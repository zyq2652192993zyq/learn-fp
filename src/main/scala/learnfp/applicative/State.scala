package learnfp.applicative

import learnfp.functor.State
import learnfp.functor.StateInstance._

object StateInstance {
  implicit def stateApplicativeInstance[S] = new Applicative[({ type E[X] = State[S, X] })#E] {
    override def pure[A](a: A): State[S, A] = State(s => (s, a))

    override def <*>[A, R](fx: State[S, A => R])(a: State[S, A]): State[S, R] =
      State(s => {
        val (fs, fa)     = fx.run(s)
        val (newS, oldA) = a.run(fs)
        (newS, fa(oldA))
      })
  }

  class StatePureApplicativeOps[A](a: A) {
    def pure[S] = stateApplicativeInstance[S].pure(a)
  }

  implicit def stateToApplicativeOps[S, A, R](a: State[S, A => R]) =
    new FxApplicativeOps[A, R, ({ type E[X] = State[S, X] })#E](a)
  implicit def stateToPureOps[A](a: A) = new StatePureApplicativeOps[A](a)
}
