package learnfp.typeclass

trait TypeClass[A] {
  def foo(x: A): String
}

object TypeClassInstances {
  implicit val intInstance: TypeClass[Int] = (x: Int) => "int"

  implicit val stringInstance: TypeClass[String] = (x: String) => "string"
}

object TypeClassUser {
  def foo[A](x: A)(implicit typeClass: TypeClass[A]): String = {
    typeClass.foo(x)
  }
}
