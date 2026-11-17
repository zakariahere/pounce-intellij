package pounce.inspections.simplifications

class SimplifyTraverseTest extends SimplificationTestBase[SimplifyTraverseInspection] {
  import SimplifyTraverse.*

  def testMapSequenceHighlight(): Unit = assertHighlight(mapSequence, ce(s"ids.${START}map(findUser).sequence$END"))
  def testMapSequenceFix(): Unit       = assertFix(mapSequence, ce("ids.map(findUser).sequence"), ce("ids.traverse(findUser)"))
  def testMapSequenceNegative(): Unit  = assertNone(ce("ids.traverse(findUser)"))

  def testMapSequence_Highlight(): Unit = assertHighlight(mapSequence_, ce(s"ids.${START}map(i => IO.println(i)).sequence_$END"))
  def testMapSequence_Fix(): Unit =
    assertFix(mapSequence_, ce("ids.map(i => IO.println(i)).sequence_"), ce("ids.traverse_(i => IO.println(i))"))

  def testTraverseIdentityHighlight(): Unit = assertHighlight(traverseIdentity, ce(s"List(io).${START}traverse(identity)$END"))
  def testTraverseIdentityFix(): Unit       = assertFix(traverseIdentity, ce("List(io).traverse(x => x)"), ce("List(io).sequence"))
  def testTraverseIdentityNegative(): Unit  = assertNone(ce("List(io).traverse(_.map(_ + 1))"))

  def testTraverse_IdentityFix(): Unit = assertFix(traverse_Identity, ce("List(io).traverse_(identity)"), ce("List(io).sequence_"))

  def testTraverseVoidHighlight(): Unit = assertHighlight(traverseVoid, ce(s"ids.${START}traverse(findUser).void$END"))
  def testTraverseVoidFix(): Unit       = assertFix(traverseVoid, ce("ids.traverse(findUser).void"), ce("ids.traverse_(findUser)"))
  def testTraverseVoidNegative(): Unit  = assertNone(ce("ids.traverse(findUser).map(_.size)"))

  /** The fix keeps the file's granular import style instead of adding `cats.syntax.all._`. */
  def testTraverseVoidAddsGranularImport(): Unit = assertFix(
    traverseVoid,
    ce("ids.traverse(findUser).void", imports = "import cats.effect._\nimport cats.syntax.traverse._"),
    ce("ids.traverse_(findUser)", imports = "import cats.effect._\nimport cats.syntax.traverse._\nimport cats.syntax.foldable._")
  )

  def testSequenceVoidFix(): Unit = assertFix(sequenceVoid, ce("List(io).sequence.void"), ce("List(io).sequence_"))
  def testSequenceVoidHighlight(): Unit = assertHighlight(sequenceVoid, ce(s"List(io).${START}sequence.void$END"))

  def testMapParSequenceHighlight(): Unit = assertHighlight(mapParSequence, ce(s"ids.${START}map(findUser).parSequence$END"))
  def testMapParSequenceFix(): Unit       = assertFix(mapParSequence, ce("ids.map(findUser).parSequence"), ce("ids.parTraverse(findUser)"))
  def testMapParSequence_Fix(): Unit =
    assertFix(mapParSequence_, ce("ids.map(findUser).parSequence_"), ce("ids.parTraverse_(findUser)"))
  def testParTraverseIdentityFix(): Unit = assertFix(parTraverseIdentity, ce("List(io).parTraverse(identity)"), ce("List(io).parSequence"))
  def testParNegative(): Unit            = assertNone(ce("ids.parTraverse(findUser)"))

  def testFlatSequenceFix(): Unit =
    assertFix(flatSequence, ce("List(IO.pure(List(1))).sequence.map(_.flatten)"), ce("List(IO.pure(List(1))).flatSequence"))
  def testFlatTraverseFix(): Unit =
    assertFix(flatTraverse, ce("ids.traverse(i => IO.pure(List(i))).map(_.flatten)"), ce("ids.flatTraverse(i => IO.pure(List(i)))"))
  def testFlatTraverseHighlight(): Unit =
    assertHighlight(flatTraverse, ce(s"ids.${START}traverse(i => IO.pure(List(i))).map(_.flatten)$END"))
  def testFlatNegative(): Unit = assertNone(ce("ids.traverse(i => IO.pure(List(i))).map(_.size)"))

  /** Not cats: `Future.sequence(...)`-style calls and plain collection code are left alone. */
  def testPlainCollectionNegative(): Unit = assertNone(ce("ids.map(_ + 1).sum"))
}

class SimplifyFoldMapTest extends SimplificationTestBase[SimplifyFoldMapInspection] {
  import SimplifyFoldMap.*

  def testMapCombineAllHighlight(): Unit = assertHighlight(mapCombineAll, ce(s"ids.${START}map(_ * 2).combineAll$END"))
  def testMapCombineAllFix(): Unit       = assertFix(mapCombineAll, ce("ids.map(_ * 2).combineAll"), ce("ids.foldMap(_ * 2)"))
  def testMapCombineAllNegative(): Unit  = assertNone(ce("ids.foldMap(_ * 2)"))

  def testFoldLeftHighlight(): Unit = assertHighlight(foldLeftCombine, ce(s"${START}ids.foldLeft(Monoid[Int].empty)(_ |+| _)$END"))
  def testFoldLeftLambdaFix(): Unit =
    assertFix(foldLeftCombine, ce("ids.foldLeft(Monoid[Int].empty)((a, b) => a.combine(b))"), ce("ids.combineAll"))
  def testFoldLeftFix(): Unit = assertFix(foldLeftCombine, ce("ids.foldLeft(Monoid.empty[Int])(_ |+| _)"), ce("ids.combineAll"))

  /** A literal zero isn't recognised as `Monoid.empty`: the user may mean a different starting value. */
  def testFoldLeftNegative(): Unit = assertNone(ce("ids.foldLeft(0)(_ |+| _)"))
}

class SimplifyOptionTraverseTest extends SimplificationTestBase[SimplifyOptionTraverseInspection] {
  import SimplifyOptionTraverse.*

  private def opt(body: String) = ce(s"val maybe: Option[User] = None\n$body")

  def testFoldHighlight(): Unit = assertHighlight(foldUnit, opt(s"${START}maybe.fold(IO.unit)(save)$END"))
  def testFoldFix(): Unit       = assertFix(foldUnit, opt("maybe.fold(IO.unit)(save)"), opt("maybe.traverse_(save)"))
  def testFoldNegative(): Unit  = assertNone(opt("maybe.fold(IO.pure(0))(u => IO.pure(u.id))"))

  def testGetOrElseHighlight(): Unit = assertHighlight(mapGetOrElseUnit, opt(s"${START}maybe.map(save).getOrElse(IO.unit)$END"))
  def testGetOrElseFix(): Unit       = assertFix(mapGetOrElseUnit, opt("maybe.map(save).getOrElse(IO.unit)"), opt("maybe.traverse_(save)"))
  def testGetOrElseNegative(): Unit  = assertNone(opt("maybe.map(u => IO.pure(u.id)).getOrElse(IO.pure(0))"))
}
