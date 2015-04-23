import play.api.libs.json._
import scala.collection.immutable.ListMap
import scala.reflect.runtime.{ universe => ru }

sealed trait DataType {
  def name: String

  lazy val descendents: Set[DataType] = this match {
    case dt: Atom    => Set(this)
    case dt: Sum     => dt.fields.foldLeft(Set(this))(_ union _._2.descendents)
    case dt: Product => dt.subtypes.foldLeft(Set(this))(_ union _.descendents)
  }

  def ancestorOf(that: DataType): Boolean =
    this.descendents contains that
}
final case class Atom(name: String) extends DataType
final case class Sum(name: String, fields: List[(String, DataType)] = Nil) extends DataType
final case class Product(name: String, subtypes: List[DataType] = Nil) extends DataType

object DataType {
  implicit val writes: OWrites[DataType] = new OWrites[DataType] {
    def flatten(in: DataType): Set[DataType] =
      in match {
        case in @ Atom(name) =>
          Set(in)

        case in @ Sum(name, children) =>
          children.map(_._2).foldLeft(Set[DataType](in))((a, b) => a union flatten(b))

        case in @ Product(name, children) =>
          children.foldLeft(Set[DataType](in))((a, b) => a union flatten(b))
      }

    def writes(in: DataType): JsObject = {
      flatten(in).toList.sortWith(_ ancestorOf _).foldLeft(JsObject(Nil)) { (obj, item) =>
        item match {
          case item: Atom    => obj + (item.name -> atomWrites.writes(item))
          case item: Sum     => obj + (item.name -> sumWrites.writes(item))
          case item: Product => obj + (item.name -> productWrites.writes(item))
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
}
