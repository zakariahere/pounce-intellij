package pounce.visual.endpoints

import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.search.{FilenameIndex, GlobalSearchScope, PsiSearchHelper}
import com.intellij.psi.{JavaPsiFacade, PsiElement, PsiFile, PsiManager}
import org.jetbrains.plugins.scala.extensions.*
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScCaseClause, ScReferencePattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr.*
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScMember
import pounce.visual.util.VisualPsi

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** D7: one HTTP endpoint found in the project. */
final case class Endpoint(method: String, path: String, framework: String, handler: String, file: VirtualFile, offset: Int) {

  /** Play: the controller method for `controllers.UserController.show(id: Long)`; others: the source position. */
  def target(project: Project): Option[PsiElement] =
    if (framework != Endpoints.Play) None
    else Endpoints.playControllerMethod(project, handler)

  def location: String = s"${file.getName}:${Option(com.intellij.openapi.fileEditor.FileDocumentManager.getInstance().getDocument(file)).map(_.getLineNumber(offset) + 1).getOrElse(0)}"
}

/**
 * Scanners for http4s route patterns, Play `routes` files and tapir endpoint definitions. Scanning stays cheap: only
 * files containing the words `HttpRoutes` / `AuthedRoutes` / `endpoint` (word index) and files named `routes` or
 * `*.routes` (file-name index) are looked at; parsing is syntactic (no type inference).
 */
object Endpoints {

  val Http4s = "http4s"
  val Play   = "Play"
  val Tapir  = "tapir"

  val HttpMethods: Set[String] = Set("GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS")

  def scan(project: Project): Seq[Endpoint] = {
    val scope = GlobalSearchScope.projectScope(project)
    (playEndpoints(project, scope) ++ http4sEndpoints(project, scope) ++ tapirEndpoints(project, scope))
      .sortBy(e => (e.path, e.method, e.framework))
  }

  private def filesWithWord(project: Project, scope: GlobalSearchScope, words: Seq[String]): Seq[ScalaFile] = {
    val out = mutable.LinkedHashSet.empty[ScalaFile]
    val helper = PsiSearchHelper.getInstance(project)
    words.foreach { w =>
      helper.processAllFilesWithWord(w, scope, (f: PsiFile) => {
        ProgressManager.checkCanceled()
        f match {
          case sf: ScalaFile => out += sf
          case _             =>
        }
        true
      }, true)
    }
    out.toSeq
  }

  // ------------------------------------------------------------------------------------------------------------
  // Play routes
  // ------------------------------------------------------------------------------------------------------------

  final case class RouteLine(method: String, path: String, action: String, offset: Int)

  private val RouteRe = """^\s*(GET|POST|PUT|DELETE|PATCH|HEAD|OPTIONS|\*|->)\s+(\S+)\s+(\S.*?)\s*$""".r

  /** Parses a Play routes file: `GET /users/:id controllers.UserController.show(id: Long)`. */
  def parseRoutes(text: String): Seq[RouteLine] = {
    var offset = 0
    text.linesWithSeparators.toSeq.flatMap { raw =>
      val start = offset
      offset += raw.length
      val line = raw.stripLineEnd
      if (line.trim.startsWith("#") || line.trim.startsWith("+")) None
      else
        line match {
          case RouteRe(m, p, a) => Some(RouteLine(m, p, a.stripPrefix("@"), start + line.indexOf(m)))
          case _                => None
        }
    }
  }

  private def playEndpoints(project: Project, scope: GlobalSearchScope): Seq[Endpoint] = {
    val named = FilenameIndex.getVirtualFilesByName("routes", scope).asScala.toSeq
    val ext   = FilenameIndex.getAllFilesByExt(project, "routes", scope).asScala.toSeq
    (named ++ ext).distinct.flatMap { vf =>
      val text = Option(PsiManager.getInstance(project).findFile(vf)).map(_.getText).getOrElse("")
      parseRoutes(text).map(r => Endpoint(r.method, r.path, Play, r.action, vf, r.offset))
    }
  }

  /** `controllers.UserController.show(id: Long)` → the `show` method of class `controllers.UserController`. */
  def playControllerMethod(project: Project, action: String): Option[PsiElement] = {
    val ref = action.takeWhile(_ != '(').trim
    val dot = ref.lastIndexOf('.')
    if (dot <= 0) None
    else {
      val (cls, method) = (ref.substring(0, dot), ref.substring(dot + 1))
      val scope         = GlobalSearchScope.allScope(project)
      val facade        = JavaPsiFacade.getInstance(project)
      Option(facade.findClass(cls, scope))
        .orElse(Option(facade.findClass(cls + "$", scope)))
        .flatMap(_.findMethodsByName(method, true).headOption)
    }
  }

  // ------------------------------------------------------------------------------------------------------------
  // http4s
  // ------------------------------------------------------------------------------------------------------------

  private val RoutesCall = """^(HttpRoutes|AuthedRoutes|ContextRoutes)\s*\.\s*(of|strict)\b.*""".r
  private val Binder      = """^\w+\s*@\s*""".r
  private val MethodArrow = """^(GET|POST|PUT|DELETE|PATCH|HEAD|OPTIONS)\s*->\s*(.+)$""".r

  /** `req @ (GET -> Root / "users" / IntVar(id))` → (GET, `Root / "users" / IntVar(id)`). */
  def splitCasePattern(text: String): Option[(String, String)] = {
    val noBinder = Binder.replaceFirstIn(text.replaceAll("\\s+", " ").trim, "").trim
    val inner =
      if (noBinder.startsWith("(") && closingParen(noBinder, 0) == noBinder.length - 1) noBinder.substring(1, noBinder.length - 1).trim
      else noBinder
    inner match {
      case MethodArrow(method, rest) => Some((method, rest.trim))
      case _                         => None
    }
  }

  /** Index of the parenthesis closing the one at `open` (string literals skipped), or -1. */
  private def closingParen(s: String, open: Int): Int = {
    var depth  = 0
    var inStr  = false
    var result = -1
    var i      = open
    while (i < s.length && result < 0) {
      val ch = s.charAt(i)
      if (ch == '"') inStr = !inStr
      else if (!inStr && ch == '(') depth += 1
      else if (!inStr && ch == ')') {
        depth -= 1
        if (depth == 0) result = i
      }
      i += 1
    }
    result
  }

  private def http4sEndpoints(project: Project, scope: GlobalSearchScope): Seq[Endpoint] =
    filesWithWord(project, scope, Seq("HttpRoutes", "AuthedRoutes")).flatMap(http4sIn)

  def http4sIn(file: ScalaFile): Seq[Endpoint] =
    file.depthFirst().collect { case c: ScCaseClause => c }.toSeq.flatMap { clause =>
      val inRoutes = clause.parentOfType[ScMethodCall].exists(call => RoutesCall.matches(call.getInvokedExpr.getText.trim))
      if (!inRoutes) None
      else
        clause.pattern.flatMap { p =>
          splitCasePattern(p.getText).map { case (method, rest) =>
            Endpoint(method, http4sPath(rest), Http4s, handlerName(clause), file.getVirtualFile, p.getTextRange.getStartOffset)
          }
        }
    }

  /** `Root / "users" / IntVar(id) :? Q(q)` → `/users/{id: Int}`. */
  def http4sPath(pattern: String): String = {
    val withoutQuery = topLevelCut(topLevelCut(pattern, " :? "), " as ")
    val segments = splitTopLevel(withoutQuery, '/').map(_.trim).filter(_.nonEmpty).flatMap {
      case "Root"                               => None
      case s if s.startsWith("\"")              => Some(s.stripPrefix("\"").stripSuffix("\""))
      case VarExtractor(kind, name)             => Some(s"{$name: ${kind.stripSuffix("Var")}}")
      case Extractor(name, arg)                 => Some(s"{$arg: $name}")
      case s if s.matches("[a-z_][\\w]*")       => Some(s"{$s}")
      case s                                    => Some(s)
    }
    if (segments.isEmpty) "/" else segments.mkString("/", "/", "")
  }

  private val VarExtractor = """(IntVar|LongVar|UUIDVar)\((\w+)\)""".r
  private val Extractor    = """(\w+)\((\w+)\)""".r

  private def topLevelCut(s: String, sep: String): String = {
    val i = s.indexOf(sep)
    if (i < 0) s else s.substring(0, i)
  }

  /** Splits on `sep` outside parentheses, brackets and string literals. */
  def splitTopLevel(s: String, sep: Char): Seq[String] = {
    val parts   = mutable.ArrayBuffer.empty[String]
    val current = new StringBuilder
    var depth   = 0
    var inStr   = false
    s.foreach { ch =>
      if (ch == '"') inStr = !inStr
      if (!inStr && (ch == '(' || ch == '[')) depth += 1
      if (!inStr && (ch == ')' || ch == ']')) depth -= 1
      if (ch == sep && depth == 0 && !inStr) { parts += current.toString; current.clear() }
      else current += ch
    }
    parts += current.toString
    parts.toSeq
  }

  private def handlerName(e: PsiElement): String = {
    val names = e.withContexts.collect {
      case f: ScFunctionDefinition => f.name
      case d: ScPatternDefinition  => d.declaredElements.headOption.map(_.name).getOrElse("")
      case m: (ScMember & com.intellij.psi.PsiNamedElement) if m.getName != null => m.getName
    }.filter(_.nonEmpty).toSeq.reverse
    names.distinct.mkString(".")
  }

  // ------------------------------------------------------------------------------------------------------------
  // tapir
  // ------------------------------------------------------------------------------------------------------------

  private val TapirRoots = Set("endpoint", "infallibleEndpoint")

  private def tapirEndpoints(project: Project, scope: GlobalSearchScope): Seq[Endpoint] =
    filesWithWord(project, scope, Seq("endpoint", "infallibleEndpoint")).filter(_.getText.contains("sttp.tapir")).flatMap(tapirIn)

  final case class Shape(method: Option[String], segments: List[String])

  def tapirIn(file: ScalaFile): Seq[Endpoint] = {
    val roots = file.depthFirst().collect {
      case r: ScReferenceExpression if r.qualifier.isEmpty && (TapirRoots(r.refName) || baseEndpoint(r, 0).isDefined) => r
    }.toSeq
    roots.flatMap { root =>
      val top = maximalChain(root)
      if (top == root && !TapirRoots(root.refName)) None // a bare reference to a base endpoint
      else
        shapeOf(top, 0).flatMap { shape =>
          shape.method.map { m =>
            Endpoint(m, if (shape.segments.isEmpty) "/" else shape.segments.mkString("/", "/", ""), Tapir, handlerName(top), file.getVirtualFile, top.getTextRange.getStartOffset)
          }
        }
    }.distinctBy(e => (e.offset, e.method, e.path))
  }

  /** Walks up while the parent continues the call chain (`.get`, `.in(...)`, `[T]`). */
  private def maximalChain(e: ScExpression): ScExpression = e.getParent match {
    case r: ScReferenceExpression if r.qualifier.contains(e) => maximalChain(r)
    case m: ScMethodCall if m.getInvokedExpr == e            => maximalChain(m)
    case g: ScGenericCall if g.referencedExpr == e           => maximalChain(g)
    case _                                                   => e
  }

  /** If `r` refers to a val defined by an endpoint chain (`val base = endpoint.in("api")`), that chain. */
  private def baseEndpoint(r: ScReferenceExpression, depth: Int): Option[ScExpression] =
    if (depth > 4 || TapirRoots(r.refName)) None
    else
      VisualPsi.safe(r.resolve()).flatMap {
        case p: ScReferencePattern =>
          p.nameContext match {
            case d: ScPatternDefinition => d.expr.filter(isTapirChain(_, depth + 1))
            case _                      => None
          }
        case _ => None
      }

  private def isTapirChain(e: ScExpression, depth: Int): Boolean = chainRoot(e) match {
    case Some(r) => TapirRoots(r.refName) || baseEndpoint(r, depth).isDefined
    case None    => false
  }

  private def chainRoot(e: ScExpression): Option[ScReferenceExpression] = e match {
    case r: ScReferenceExpression =>
      r.qualifier match {
        case Some(q) => chainRoot(q)
        case None    => Some(r)
      }
    case m: ScMethodCall          => chainRoot(m.getInvokedExpr)
    case g: ScGenericCall         => chainRoot(g.referencedExpr)
    case _                        => None
  }

  /** Interprets a chain: method selectors and `.in(...)` path inputs, prefixed by its base endpoint. */
  private def shapeOf(e: ScExpression, depth: Int): Option[Shape] = e match {
    case r: ScReferenceExpression =>
      r.qualifier match {
        case None =>
          if (TapirRoots(r.refName)) Some(Shape(None, Nil))
          else baseEndpoint(r, depth).flatMap(shapeOf(_, depth + 1))
        case Some(q) =>
          shapeOf(q, depth).map { s =>
            val upper = r.refName.toUpperCase
            if (HttpMethods(upper) && r.refName == r.refName.toLowerCase) s.copy(method = Some(upper)) else s
          }
      }
    case m: ScMethodCall =>
      m.getInvokedExpr match {
        case r: ScReferenceExpression if r.refName == "in" =>
          r.qualifier.flatMap(shapeOf(_, depth)).map(s => s.copy(segments = s.segments ++ m.argumentExpressions.flatMap(pathSegments)))
        case other => shapeOf(other, depth)
      }
    case g: ScGenericCall => shapeOf(g.referencedExpr, depth)
    case _                => None
  }

  /** `"users" / path[Int]("id") / "orders"` → `users`, `{id: Int}`, `orders`. Non-path inputs give nothing. */
  def pathSegments(e: ScExpression): List[String] = e match {
    case i: ScInfixExpr if i.operation.refName == "/" => pathSegments(i.left) ++ pathSegments(i.right)
    case l: ScLiteral =>
      Option[AnyRef](l.getValue).collect { case s: String => s.stripPrefix("/").split('/').filter(_.nonEmpty).toList }.getOrElse(Nil)
    case m: ScMethodCall =>
      m.getInvokedExpr match {
        case g: ScGenericCall if g.referencedExpr.getText == "path" =>
          val tpe  = g.typeArgs.getText.stripPrefix("[").stripSuffix("]")
          val name = m.argumentExpressions.headOption.collect { case l: ScLiteral => String.valueOf(l.getValue: AnyRef) }.getOrElse("param")
          List(s"{$name: $tpe}")
        case _ => Nil
      }
    case g: ScGenericCall if g.referencedExpr.getText == "path" =>
      List(s"{${g.typeArgs.getText.stripPrefix("[").stripSuffix("]")}}")
    case p: ScParenthesisedExpr => p.innerElement.toList.flatMap(pathSegments)
    case _                      => Nil
  }
}
