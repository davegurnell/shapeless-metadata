import play.api.libs.json._
import scala.collection.immutable.ListMap
import scala.reflect.runtime.{ universe => ru }

sealed trait Schema {
  def name: String

  lazy val descendents: Set[Schema] = this match {
    case dt: Atom     => Set(this)
    case dt: Sum      => dt.fields.foldLeft(Set(this))(_ union _._2.descendents)
    case dt: Product  => dt.subtypes.foldLeft(Set(this))(_ union _.descendents)
    case dt: Sequence => Set(this) union dt.item.descendents
  }

  def ancestorOf(that: Schema): Boolean =
    this.descendents contains that
}
final case class Atom(name: String) extends Schema
final case class Sum(name: String, fields: List[(String, Schema)] = Nil) extends Schema
final case class Product(name: String, subtypes: List[Schema] = Nil) extends Schema
final case class Sequence(name: String, item: Schema) extends Schema

object Schema {
  implicit val writes: OWrites[Schema] = new OWrites[Schema] {
    def flatten(in: Schema): Set[Schema] =
      in match {
        case in @ Atom(name) =>
          Set[Schema](in)

        case in @ Sum(name, children) =>
          children.map(_._2).foldLeft(Set[Schema](in))((a, b) => a union flatten(b))

        case in @ Product(name, children) =>
          children.foldLeft(Set[Schema](in))((a, b) => a union flatten(b))

        case in @ Sequence(name, child) =>
          Set[Schema](in) union flatten(child)
      }

    def writes(in: Schema): JsObject = {
      flatten(in).toList.sortWith(_ ancestorOf _).foldLeft(JsObject(Nil)) { (obj, item) =>
        item match {
          case item: Atom     => obj + (item.name -> atomWrites.writes(item))
          case item: Sum      => obj + (item.name -> sumWrites.writes(item))
          case item: Product  => obj + (item.name -> productWrites.writes(item))
          case item: Sequence => obj + (item.name -> sequenceWrites.writes(item))
        }
      }
    }
  }

  val atomWrites: OWrites[Atom] = new OWrites[Atom] {
    def writes(in: Atom) = Json.obj(
      "type" -> "Atom"
    )
  }

  val sumWrites: OWrites[Sum] = new OWrites[Sum] {
    def writes(in: Sum) = Json.obj(
      "type"   -> "Sum",
      "name"   -> in.name,
      "fields" -> JsObject(in.fields map { case (k, v) => k -> JsString(v.name) })
    )
  }

  val productWrites: OWrites[Product] = new OWrites[Product] {
    def writes(in: Product) = Json.obj(
      "type"     -> "Product",
      "name"     -> in.name,
      "subtypes" -> JsArray(in.subtypes map (v => JsString(v.name)))
    )
  }

  val sequenceWrites: OWrites[Sequence] = new OWrites[Sequence] {
    def writes(in: Sequence) = Json.obj(
      "type"  -> "Sequence",
      "name"  -> in.name,
      "item" -> JsString(in.item.name)
    )
  }
}
