package learnfp.typeclass

trait Show[A] {
  def show(x: A): String
}

object Printer {
  def show[A](x: A)(implicit showInstance: Show[A]): String = {
    showInstance.show(x)
  }
}

object ShowInstances {
  implicit val intInstance: Show[Int] = (x: Int) => x.toString

  implicit val doubleInstance: Show[Double] = (x: Double) => x.toString

  implicit def listInstance[T](implicit xShow: Show[T]): Show[List[T]] = (xs: List[T]) =>
    s"[${xs.map(xShow.show).mkString(", ")}]"
}
