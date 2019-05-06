trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

// Define type class
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

// Define type class objects
object JsonWriterInstances {
  // Instance of typeclass gives implementations for the types we care about
  // typeclass instances
  implicit val stringWriter = new JsonWriter[String] {
    def write(value: String): Json = JsString(value)
  }

  implicit val personWriter = new JsonWriter[Person] {
    def write(value: Person): Json = JsObject(Map("name"-> JsString(value.name),
                                      "email" -> JsString(value.email)))
  }

  implicit val doubleWriter = new JsonWriter[Double] {
    override def write(value: Double): Json = JsNumber(value)
  }

  implicit def optionJsonWriter[A](implicit w : JsonWriter[A]) = {
    new JsonWriter[Option[A]] {
      override def write(value: Option[A]): Json = {
        value match {
          case Some(v) => w.write(v)
          case None => JsNull
        }
      }
    }
  }
}

// Usage of typeclasses
// 1. Interface objects
object Json {
  // A can have type for which typeclass instances are defined
  def toJson[A](value:A)(implicit w:JsonWriter[A]) = w.write(value)
}
//2. Interface syntax
// using extension methods or type enrichment
object JsonSyntax {
  // here both w and value are implicity passed
  implicit class JsonWriterOps[A](value: A) {
    def toJs(implicit w: JsonWriter[A]) = w.write(value)
  }
}




import JsonWriterInstances._
import JsonSyntax._
// extension methods look like we are calling methods on type Person hence called extension methods
Person("mayur","abc@gmail").toJs
// JsonWriterOps(Person("mayur","abc@gmail")).toJs(personWriter)
Json.toJson(Person("aj","notdefined"))
// expanded to Json.toJson(Person("aj","notdefined"))(personWriter)

implicitly[JsonWriter[Double]] // no need of interface objects

Json.toJson(Option("434"))

