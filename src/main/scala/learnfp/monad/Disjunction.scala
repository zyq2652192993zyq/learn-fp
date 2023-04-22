package learnfp.monad

import learnfp.functor.Disjunction._
import learnfp.functor.DisjunctionInstance._

object DisjunctionInstance {
  implicit def disjunctionMonadInstance[L] = new Monad[({ type E[A] = Disjunction[L, A] })#E]() {
    override def pure[A](a: A): Disjunction[L, A] = right(a)

    override def flatMap[A, B](a: Disjunction[L, A])(fx: A => Disjunction[L, B]): Disjunction[L, B] =
      a match {
        case RightDisjunction(rv) => fx(rv)
        case LeftDisjunction(lv)  => left(lv)
      }
  }

  implicit def disjunctionToMonadOps[L, R](disjunction: Disjunction[L, R]) =
    new MonadOps[R, ({ type E[X] = Disjunction[L, X] })#E](disjunction)
}
