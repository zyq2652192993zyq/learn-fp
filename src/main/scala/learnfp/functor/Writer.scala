package learnfp.functor

import learnfp.monoid.Monoid

case class Writer[W, A](run: () => (W, A))(implicit val monoid: Monoid[W])

object Writer {
  def tell[W](m: W)(implicit monoid: Monoid[W]): Writer[W, Unit] = Writer(() => (m, ()))
}

object WriterInstance {
  implicit def writerInstance[W] = new Functor[({ type E[X] = Writer[W, X] })#E] {
    override def fmap[A, B](writer: Writer[W, A])(fx: A => B): Writer[W, B] = {
      val (w, a) = writer.run()
      Writer(() => (w, fx(a)))(writer.monoid)
    }
  }

  implicit def writerToFunctorOps[A, W](a: Writer[W, A]) = new FunctorOps[A, ({ type E[X] = Writer[W, X] })#E](a)

  class WriterFunctorOps[A, B](fx: A => B) {
    def `<$>`[W](a: Writer[W, A]): Writer[W, B] = a fmap fx
  }
  implicit def writerToFunctorFxOps[A, R](fx: A => R) = new WriterFunctorOps[A, R](fx)
}
