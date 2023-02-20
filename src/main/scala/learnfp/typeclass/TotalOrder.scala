package learnfp.typeclass

trait TotalOrder[A] {
  def less(lhs: A, rhs: A): Boolean
}

object TotalOrderInstances {
  implicit val intInstance: TotalOrder[Int] = (lhs: Int, rhs: Int) => lhs < rhs

  implicit val stringInstance: TotalOrder[String] = (lhs: String, rhs: String) => lhs.compareTo(rhs) < 0

  /**
    * the official answer is not clear, what if lists have different length or one of them is empty
    */
  implicit def listInstance[T](implicit suborder: TotalOrder[T]): TotalOrder[List[T]] = (lhs: List[T], rhs: List[T]) =>
//    lhs.zip(rhs).forall({ case (l, r) => suborder.less(l, r) })
    (lhs, rhs) match {
      case (Nil, Nil)               => false
      case (lHead :: _, rHead :: _) => suborder.less(lHead, rHead)
      case (Nil, rHead :: _)        => true
      case (lHead :: _, Nil)        => false
    }
}

object Comparator {
  @annotation.implicitNotFound("No instance of TotalOrder found")
  def less[A](lhs: A, rhs: A)(implicit order: TotalOrder[A]) = {
    order.less(lhs, rhs)
  }
}
