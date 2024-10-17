/** @author: scalatheagorist */

import scala.annotation.implicitNotFound

/**
 * A ZIO-like adapted dependency injection implementation coded in Scala 3 by Martin Odersky
 *
 * This is an implementation for Scala 2.13.X.
 *
 * @see
 *   [[https://gist.github.com/johnhungerford/cc22eb5b23c7407aa45479a845a7ead8]]
 */
object di {

  /**
   * `-R` in this case, for example, if `R` is a type that represents an interface or an abstract class, then a Provider
   * for a specific type of `R` (e.g., Dog) can also be considered a Provider for Animal.
   *
   * Memoization ensures that existing instances are reused during repeated instantiations, rather than creating new
   * objects. As a result, only the instance already present in memory is utilized.
   */
  final case class Provider[-R, +A] private[di] (constructor: R => A) {
    @SuppressWarnings(Array("NullAssignment"))
    private var memo: Any = null

    @SuppressWarnings(Array("NullParameter", "AsInstanceOf"))
    @inline private[di] def apply(value: R): A =
      if (memo == null) {
        val result = constructor(value)
        memo = result
        result
      } else
        memo.asInstanceOf[A]
  }

  object Provider {
    def provideSuspended[A](value: => A): Provider[Any, A] =
      Provider((_: Any) => value)

    def provideConstructor[A, B](construct: A => B): Provider[A, B] =
      Provider(construct)

    def provideConstructor[A, B, C](construct: (A, B) => C): Provider[(A, B), C] =
      Provider { case (a, b) => construct(a, b) }

    def provideConstructor[A, B, C, D](construct: (A, B, C) => D): Provider[(A, B, C), D] =
      Provider { case (a, b, c) => construct(a, b, c) }

    def provideConstructor[A, B, C, D, E](construct: (A, B, C, D) => E): Provider[(A, B, C, D), E] =
      Provider { case (a, b, c, d) => construct(a, b, c, d) }

    def provideConstructor[A, B, C, D, E, F](construct: (A, B, C, D, E) => F): Provider[(A, B, C, D, E), F] =
      Provider { case (a, b, c, d, e) => construct(a, b, c, d, e) }

    def provideConstructor[A, B, C, D, E, F, G](construct: (A, B, C, D, E, F) => G): Provider[(A, B, C, D, E, F), G] =
      Provider { case (a, b, c, d, e, f) => construct(a, b, c, d, e, f) }

    def provideConstructor[A, B, C, D, E, F, G, H](
      construct: (A, B, C, D, E, F, G) => H
    ): Provider[(A, B, C, D, E, F, G), H] =
      Provider { case (a, b, c, d, e, f, g) => construct(a, b, c, d, e, f, g) }

    def provideConstructor[A, B, C, D, E, F, G, H, I](
      construct: (A, B, C, D, E, F, G, H) => I
    ): Provider[(A, B, C, D, E, F, G, H), I] =
      Provider { case (a, b, c, d, e, f, g, h) => construct(a, b, c, d, e, f, g, h) }

    def provideConstructor[A, B, C, D, E, F, G, H, I, J](
      construct: (A, B, C, D, E, F, G, H, I) => J
    ): Provider[(A, B, C, D, E, F, G, H, I), J] =
      Provider { case (a, b, c, d, e, f, g, h, i) => construct(a, b, c, d, e, f, g, h, i) }

    def provideConstructor[A, B, C, D, E, F, G, H, I, J, K](
      construct: (A, B, C, D, E, F, G, H, I, J) => K
    ): Provider[(A, B, C, D, E, F, G, H, I, J), K] =
      Provider { case (a, b, c, d, e, f, g, h, i, j) => construct(a, b, c, d, e, f, g, h, i, j) }

    // add more if needed
  }

  type Provided[A] = A

  def provide[A](value: A): Provided[A] = value

  def provided[A](implicit
    @implicitNotFound("Unable to provide a value of type ${A}. Make sure any dependencies are provided.")
    pr: Provided[A]
  ): A = pr

  trait LowPriorityProvided {
    implicit def providedFromProvider[R, A](implicit lyr: Provider[R, A], apr: Provided[R]): Provided[A] =
      lyr(apr)
  }

  object Provided extends LowPriorityProvided {
    implicit def providedNonEmptyTuple2[A, B](implicit apr: Provided[A], bpr: Provided[B]): Provided[(A, B)] =
      (apr, bpr)

    implicit def providedNonEmptyTuple3[A, B, C](implicit
      apr: Provided[A],
      bpr: Provided[B],
      cpr: Provided[C]
    ): Provided[(A, B, C)] =
      (apr, bpr, cpr)

    implicit def providedNonEmptyTuple4[A, B, C, D](implicit
      apr: Provided[A],
      bpr: Provided[B],
      cpr: Provided[C],
      dpr: Provided[D]
    ): Provided[(A, B, C, D)] =
      (apr, bpr, cpr, dpr)

    implicit def providedNonEmptyTuple5[A, B, C, D, E](implicit
      apr: Provided[A],
      bpr: Provided[B],
      cpr: Provided[C],
      dpr: Provided[D],
      epr: Provided[E]
    ): Provided[(A, B, C, D, E)] =
      (apr, bpr, cpr, dpr, epr)

    implicit def providedNonEmptyTuple6[A, B, C, D, E, F](implicit
      apr: Provided[A],
      bpr: Provided[B],
      cpr: Provided[C],
      dpr: Provided[D],
      epr: Provided[E],
      fpr: Provided[F]
    ): Provided[(A, B, C, D, E, F)] =
      (apr, bpr, cpr, dpr, epr, fpr)

    implicit def providedNonEmptyTuple7[A, B, C, D, E, F, G](implicit
      apr: Provided[A],
      bpr: Provided[B],
      cpr: Provided[C],
      dpr: Provided[D],
      epr: Provided[E],
      fpr: Provided[F],
      gpr: Provided[G]
    ): Provided[(A, B, C, D, E, F, G)] =
      (apr, bpr, cpr, dpr, epr, fpr, gpr)

    implicit def providedNonEmptyTuple8[A, B, C, D, E, F, G, H](implicit
      apr: Provided[A],
      bpr: Provided[B],
      cpr: Provided[C],
      dpr: Provided[D],
      epr: Provided[E],
      fpr: Provided[F],
      gpr: Provided[G],
      hpr: Provided[H]
    ): Provided[(A, B, C, D, E, F, G, H)] =
      (apr, bpr, cpr, dpr, epr, fpr, gpr, hpr)

    implicit def providedNonEmptyTuple9[A, B, C, D, E, F, G, H, I](implicit
      apr: Provided[A],
      bpr: Provided[B],
      cpr: Provided[C],
      dpr: Provided[D],
      epr: Provided[E],
      fpr: Provided[F],
      gpr: Provided[G],
      hpr: Provided[H],
      ipr: Provided[I]
    ): Provided[(A, B, C, D, E, F, G, H, I)] =
      (apr, bpr, cpr, dpr, epr, fpr, gpr, hpr, ipr)

    implicit def providedNonEmptyTuple10[A, B, C, D, E, F, G, H, I, J](implicit
      apr: Provided[A],
      bpr: Provided[B],
      cpr: Provided[C],
      dpr: Provided[D],
      epr: Provided[E],
      fpr: Provided[F],
      gpr: Provided[G],
      hpr: Provided[H],
      ipr: Provided[I],
      jpr: Provided[J]
    ): Provided[(A, B, C, D, E, F, G, H, I, J)] =
      (apr, bpr, cpr, dpr, epr, fpr, gpr, hpr, ipr, jpr)

    @SuppressWarnings(Array("MaxParameters"))
    implicit def providedNonEmptyTuple11[A, B, C, D, E, F, G, H, I, J, K](implicit
      apr: Provided[A],
      bpr: Provided[B],
      cpr: Provided[C],
      dpr: Provided[D],
      epr: Provided[E],
      fpr: Provided[F],
      gpr: Provided[G],
      hpr: Provided[H],
      ipr: Provided[I],
      jpr: Provided[J],
      kpr: Provided[K]
    ): Provided[(A, B, C, D, E, F, G, H, I, J, K)] =
      (apr, bpr, cpr, dpr, epr, fpr, gpr, hpr, ipr, jpr, kpr)

    implicit val providedEmptyTuple: Provided[Unit] = ()

    implicit def providedFromTrivialProvider[A](implicit pr: Provider[Any, A]): Provided[A] =
      pr(())
  }
}

