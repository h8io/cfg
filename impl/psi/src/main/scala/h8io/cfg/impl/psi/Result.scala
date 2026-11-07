package h8io.cfg.impl.psi

/** The outcome of a phase that recovers from errors: a value, possibly with holes, and the errors found on the way.
  *
  * Errors accumulate in the order found. They become a [[PsiErrors]] only at a phase gate, through [[toEither]].
  */
private[psi] final case class Result[+A](value: A, errors: List[PsiError]) {
  def map[B](f: A => B): Result[B] = Result(f(value), errors)

  def flatMap[B](f: A => Result[B]): Result[B] = {
    val next = f(value)
    Result(next.value, errors ++ next.errors)
  }

  /** Pairs two independent results, keeping the errors of both. */
  def zip[B](that: Result[B]): Result[(A, B)] = Result((value, that.value), errors ++ that.errors)

  /** The phase gate: the value if nothing went wrong, every error otherwise. */
  def toEither: Either[PsiErrors, A] =
    errors match {
      case nonEmpty @ (_ :: _) => Left(PsiErrors(nonEmpty))
      case Nil => Right(value)
    }
}

private[psi] object Result {
  def pure[A](value: A): Result[A] = Result(value, Nil)

  def failed[A](value: A, error: PsiError): Result[A] = Result(value, error :: Nil)

  /** Collects independent results, keeping every value and every error in order. */
  def sequence[A](results: Seq[Result[A]]): Result[Vector[A]] =
    Result(results.iterator.map(_.value).toVector, results.iterator.flatMap(_.errors).toList)
}
