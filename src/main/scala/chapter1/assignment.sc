final case class Cat(name: String, age: String, color: String)

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val catPrintable = new Printable[Cat] {
    override def format(cat: Cat) = s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
  }
}
object Printable {
  def format[A](value: A)(implicit printer:Printable[A]) = printer.format(value)

  def print[A](value: A)(implicit printer:Printable[A]): Unit = println(format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    implicit def format(implicit w: Printable[A]): String = {
      w.format(value)
    }
  }
}

import PrintableInstances._
Printable.format(Cat("meo","1","brown"))
Printable.print(Cat("peo","2","white"))
import PrintableSyntax._
Cat("paw","1","black").format
