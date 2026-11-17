package pounce.runtime.debugger.renderers

import com.intellij.debugger.DebuggerContext
import com.intellij.debugger.engine.evaluation.{EvaluateException, EvaluationContext}
import com.intellij.debugger.engine.{DebugProcessImpl, DebuggerUtils}
import com.intellij.debugger.settings.NodeRendererSettings
import com.intellij.debugger.ui.impl.watch.ArrayElementDescriptorImpl
import com.intellij.debugger.ui.tree.render.*
import com.intellij.debugger.ui.tree.{DebuggerTreeNode, NodeDescriptor, ValueDescriptor}
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiExpression}
import com.sun.jdi.*
import pounce.runtime.debugger.renderers.CatsLayouts.*

import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** [[Heap]] over JDI mirrors: field and array reads only, never code execution in the debuggee. */
object JdiHeap extends Heap[Value] {
  private val ArrayFetchLimit = 1000

  override def isNull(v: Value): Boolean = v == null

  override def className(v: Value): String = v match {
    case o: ObjectReference => o.referenceType().name()
    case _                  => ""
  }

  override def field(v: Value, name: String): Option[Value] = v match {
    case o: ObjectReference => Option(o.referenceType().fieldByName(name)).map(o.getValue)
    case _                  => None
  }

  override def arrayElements(v: Value): Option[IndexedSeq[Value]] = v match {
    case a: ArrayReference => Some(a.getValues(0, math.min(a.length(), ArrayFetchLimit)).asScala.toIndexedSeq)
    case _                 => None
  }
}

/**
 * Pure conversions the renderers may invoke when a layout cannot be read through fields (large Vectors, Chain
 * length). Only methods of immutable cats / Scala collections are ever invoked — never user code.
 */
private[renderers] object PureCalls {

  def invoke(ctx: EvaluationContext, target: ObjectReference, name: String, args: List[Value]): Value = {
    val methods = target.referenceType().methodsByName(name).asScala.filter(_.argumentTypeNames().size == args.size)
    val method  = methods.find(m => !m.isBridge && !m.isAbstract).orElse(methods.headOption).getOrElse {
      throw new EvaluateException(s"$name/${args.size} not found on ${target.referenceType().name()}")
    }
    ctx.getDebugProcess.invokeMethod(ctx, target, method, args.asJava)
  }

  /** First `limit` elements of any immutable Seq via `take(limit).toList`, read back through fields. */
  def firstElements(ctx: EvaluationContext, seq: Value, limit: Int): Option[Vector[Value]] = seq match {
    case o: ObjectReference =>
      val taken = invoke(ctx, o, "take", List(o.virtualMachine().mirrorOf(limit)))
      taken match {
        case t: ObjectReference => listElements(JdiHeap, invoke(ctx, t, "toList", Nil), limit)
        case _                  => None
      }
    case _ => None
  }

  def intOrLong(v: Value): Option[Long] = v match {
    case i: IntegerValue => Some(i.value().toLong)
    case l: LongValue    => Some(l.value())
    case _               => None
  }
}

/**
 * A child node showing one element; evaluation of the node as an expression is not supported.
 *
 * The `ArrayElementDescriptorImpl(project, null, index)` + `setValue` trick follows intellij-scala's (Apache-2.0)
 * `CollectionElementDescriptor` (scala/debugger).
 */
final class PounceElementDescriptor(project: Project, index: Int, value: Value, name: Option[String] = None)
    extends ArrayElementDescriptorImpl(project, null, index) {
  setValue(value)

  override def getName: String = name.getOrElse(super.getName)

  override def getDescriptorEvaluation(context: DebuggerContext): PsiExpression =
    throw new EvaluateException("Pounce renderer elements cannot be evaluated as expressions")
}

/** Shared plumbing: labels, delegation to the payload's own renderer, message nodes. */
abstract class CatsRendererBase(className: String) extends ReferenceRenderer(className) with ValueLabelRenderer with ChildrenRenderer {

  override def getUniqueId: String = getClass.getName

  // ReferenceRenderer has both `Renderer clone()` and the `Object clone()` bridge; Scala 3 needs the public one named.
  override def clone(): Renderer = super.clone().asInstanceOf[Renderer]

  protected def label(ref: ObjectReference, ctx: EvaluationContext): String

  override def calcLabel(descriptor: ValueDescriptor, ctx: EvaluationContext, listener: DescriptorLabelListener): String =
    descriptor.getValue match {
      case ref: ObjectReference =>
        try label(ref, ctx)
        catch {
          case e: EvaluateException => throw e
          case NonFatal(e)          => throw new EvaluateException(Option(e.getMessage).getOrElse(e.toString), e)
        }
      case _ => "null"
    }

  override def getChildValueExpression(node: DebuggerTreeNode, context: DebuggerContext): PsiElement =
    throw new EvaluateException("Pounce renderer children cannot be evaluated as expressions")

  protected def messageNode(builder: ChildrenBuilder, text: String): DebuggerTreeNode =
    builder.getNodeManager.createMessageNode(text)

  protected def elementNodes(builder: ChildrenBuilder, ctx: EvaluationContext, values: Seq[Value]): Seq[DebuggerTreeNode] =
    values.zipWithIndex.map { case (v, i) => builder.getNodeManager.createNode(new PounceElementDescriptor(ctx.getProject, i, v), ctx) }

  /** The renderer IntelliJ would pick for `v` on its own (Scala collection renderer, class renderer, ...). */
  protected def autoRenderer(v: Value, ctx: EvaluationContext): CompletableFuture[NodeRenderer] =
    ctx.getDebugProcess match {
      case p: DebugProcessImpl => p.getAutoRendererAsync(v.`type`())
      case _                   => CompletableFuture.completedFuture(DebugProcessImpl.getDefaultRenderer(v))
    }

  /** One-line text for a payload; mirrors the debugger's defaults (toString only when its renderer is on). */
  protected def show(v: Value, ctx: EvaluationContext): String = v match {
    case null                => "null"
    case s: StringReference  => "\"" + clip(s.value()) + "\""
    case p: PrimitiveValue   => p.toString
    case o: ObjectReference  =>
      JdiHeap.field(o, "value") match {
        case Some(p: PrimitiveValue) if o.referenceType().name().startsWith("java.lang.") => p.toString
        case _ if NodeRendererSettings.getInstance().getToStringRenderer.isEnabled =>
          try clip(DebuggerUtils.getValueAsString(ctx, o))
          catch { case NonFatal(_) => shortName(o) }
        case _ => shortName(o)
      }
    case other => other.toString
  }

  protected def shortName(o: ObjectReference): String = {
    val n = o.referenceType().name()
    s"${n.substring(n.lastIndexOf('.') + 1)}@${o.uniqueID()}"
  }

  protected def isExpandableValue(v: Value): Boolean = v match {
    case _: StringReference | _: PrimitiveValue | null => false
    case _: ObjectReference                            => true
    case _                                             => false
  }
}

/** Collection-like cats types: label `Kind(size = n)`, children `[0]`, `[1]`, ... (first [[ElementLimit]]). */
abstract class ElementsRenderer(className: String) extends CatsRendererBase(className) {
  protected def kind(ref: ObjectReference): String
  protected def size(ref: ObjectReference, ctx: EvaluationContext): Option[Long]
  protected def elements(ref: ObjectReference, ctx: EvaluationContext): Option[Vector[Value]]

  // a size that needs a (pure) method call can fail, e.g. when the thread cannot invoke methods: label without it
  override protected def label(ref: ObjectReference, ctx: EvaluationContext): String =
    sizedLabel(kind(ref), try size(ref, ctx) catch { case NonFatal(_) => None })

  override def buildChildren(value: Value, builder: ChildrenBuilder, ctx: EvaluationContext): Unit = value match {
    case ref: ObjectReference =>
      val nodes =
        try
          elements(ref, ctx) match {
            case Some(xs) =>
              val shown = elementNodes(builder, ctx, xs)
              val more  = size(ref, ctx).map(_ - xs.size).filter(_ > 0)
              shown ++ more.map(n => messageNode(builder, s"... $n more (Pounce shows the first ${xs.size})"))
            case None =>
              Seq(messageNode(builder, "Unknown layout: disable the Pounce renderer (Runtime & Debugger settings) to see raw fields"))
          }
        catch { case NonFatal(e) => Seq(messageNode(builder, s"Cannot read elements: ${e.getMessage}")) }
      builder.setChildren(nodes.asJava)
    case _ => builder.setChildren(java.util.Collections.emptyList())
  }

  override def isExpandableAsync(value: Value, ctx: EvaluationContext, parent: NodeDescriptor): CompletableFuture[java.lang.Boolean] =
    CompletableFuture.completedFuture(java.lang.Boolean.valueOf(value match {
      case ref: ObjectReference => !ref.referenceType().name().startsWith("cats.data.Chain$Empty")
      case _                    => false
    }))
}

final class NonEmptyListRenderer extends ElementsRenderer(Fqn.NonEmptyList) {
  override protected def kind(ref: ObjectReference): String = "NonEmptyList"
  override protected def size(ref: ObjectReference, ctx: EvaluationContext): Option[Long] =
    nonEmptyListSize(JdiHeap, ref).map(_.toLong)
  override protected def elements(ref: ObjectReference, ctx: EvaluationContext): Option[Vector[Value]] =
    nonEmptyListElements(JdiHeap, ref, ElementLimit)
}

final class NonEmptyVectorRenderer extends ElementsRenderer(Fqn.NonEmptyVector) {
  override protected def kind(ref: ObjectReference): String = "NonEmptyVector"

  private def vector(ref: ObjectReference): Option[Value] = JdiHeap.field(ref, "toVector")

  override protected def size(ref: ObjectReference, ctx: EvaluationContext): Option[Long] =
    vector(ref).collect { case v: ObjectReference => v }.flatMap(v => PureCalls.intOrLong(PureCalls.invoke(ctx, v, "length", Nil)))

  override protected def elements(ref: ObjectReference, ctx: EvaluationContext): Option[Vector[Value]] =
    vector(ref).flatMap(v => seqElements(JdiHeap, v, ElementLimit).orElse(PureCalls.firstElements(ctx, v, ElementLimit)))
}

/** `Chain` and `NonEmptyChain` (an opaque newtype: the same object at runtime, so both are labelled `Chain`). */
final class ChainRenderer extends ElementsRenderer(Fqn.Chain) {
  override protected def kind(ref: ObjectReference): String = "Chain"

  override protected def size(ref: ObjectReference, ctx: EvaluationContext): Option[Long] =
    if (ref.referenceType().name().startsWith("cats.data.Chain$Empty")) Some(0L)
    else if (ref.referenceType().name() == Fqn.ChainSingleton) Some(1L)
    else PureCalls.intOrLong(PureCalls.invoke(ctx, ref, "length", Nil))

  override protected def elements(ref: ObjectReference, ctx: EvaluationContext): Option[Vector[Value]] =
    chainElements(JdiHeap, ref: Value, ElementLimit)((seq, limit) => PureCalls.firstElements(ctx, seq, limit))
}

/**
 * Wrapper types shown by their payload: `Valid(42)`, `Invalid(NonEmptyList(...))`, `EitherT(IO(...))`,
 * `OptionT(...)`, `Ior.Left/Right`, `Ref(current)`. Children are the payload's own children.
 */
final class PayloadRenderer(className: String) extends CatsRendererBase(className) {

  override protected def label(ref: ObjectReference, ctx: EvaluationContext): String =
    payload(JdiHeap, ref) match {
      case Some((kind, inner)) => wrappedLabel(kind, show(inner, ctx))
      case None if ref.referenceType().name() == Fqn.IorBoth =>
        val a = JdiHeap.field(ref, "a").orNull
        val b = JdiHeap.field(ref, "b").orNull
        s"Both(${show(a, ctx)}, ${show(b, ctx)})"
      case None => show(ref, ctx)
    }

  override def buildChildren(value: Value, builder: ChildrenBuilder, ctx: EvaluationContext): Unit = value match {
    case ref: ObjectReference =>
      payload(JdiHeap, ref) match {
        case Some((_, inner)) if isExpandableValue(inner) =>
          autoRenderer(inner, ctx).thenAccept(r => r.buildChildren(inner, builder, ctx))
        case Some(_) => builder.setChildren(java.util.Collections.emptyList())
        case None if ref.referenceType().name() == Fqn.IorBoth =>
          val nodes = List("left" -> "a", "right" -> "b").zipWithIndex.map { case ((label, f), i) =>
            builder.getNodeManager.createNode(
              new PounceElementDescriptor(ctx.getProject, i, JdiHeap.field(ref, f).orNull, Some(label)),
              ctx
            )
          }
          builder.setChildren(nodes.asJava)
        case None => builder.setChildren(List(messageNode(builder, "Unknown layout")).asJava)
      }
    case _ => builder.setChildren(java.util.Collections.emptyList())
  }

  override def isExpandableAsync(value: Value, ctx: EvaluationContext, parent: NodeDescriptor): CompletableFuture[java.lang.Boolean] =
    value match {
      case ref: ObjectReference =>
        payload(JdiHeap, ref) match {
          case Some((_, inner)) if isExpandableValue(inner) =>
            autoRenderer(inner, ctx).thenCompose(r => r.isExpandableAsync(inner, ctx, parent))
          case Some(_) => CompletableFuture.completedFuture(java.lang.Boolean.FALSE)
          case None    => CompletableFuture.completedFuture(java.lang.Boolean.valueOf(ref.referenceType().name() == Fqn.IorBoth))
        }
      case _ => CompletableFuture.completedFuture(java.lang.Boolean.FALSE)
    }
}
