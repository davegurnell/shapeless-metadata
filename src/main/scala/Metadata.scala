import shapeless._
import shapeless.labelled._
import shapeless.poly._

trait Metadata[A] {
  def get: Schema
}

object Metadata extends MetadataConstructors
  with LowPriorityMetadataImplicits
  with HighPriorityMetadataImplicits

trait HighPriorityMetadataImplicits {
  self: MetadataConstructors =>

  implicit val stringMetadata  : Metadata[String]  = Metadata.atom[String]
  implicit val intMetadata     : Metadata[Int]     = Metadata.atom[Int]
  implicit val booleanMetadata : Metadata[Boolean] = Metadata.atom[Boolean]

  implicit def seqMetadata[A, B[_] <: Seq[_]](implicit
    itemMeta: Metadata[A],
    collTypeable: Typeable[B[A]]
  ): Metadata[B[A]] = {
    val itemSchema = itemMeta.get
    Metadata.array(s"${collTypeable.describe}", itemSchema)
  }

  // implicit def seqMetadata[A](implicit itemMeta: Metadata[A]): Metadata[Seq[A]] = {
  //   val itemSchema = itemMeta.get
  //   Metadata.array(s"Seq[${itemSchema.name}]", itemSchema)
  // }
}

trait LowPriorityMetadataImplicits {
  self: MetadataConstructors =>

  implicit def hnilMetadata[L <: HNil]: Metadata[L] = {
    self.sum("HNil", Nil)
  }

  implicit def hconsMetadata[K <: Symbol, H, T <: HList](implicit
    key: Witness.Aux[K],
    hMeta: Lazy[Metadata[H]],
    tMeta: Lazy[Metadata[T]]
  ): Metadata[FieldType[K, H] :: T] = {
    val Sum(name, fields) = tMeta.value.get
    self.sum(name, (key.value.name -> hMeta.value.get) :: fields)
  }

  implicit def cnilMetadata: Metadata[CNil] = {
    self.product("CNil", Nil)
  }

  implicit def cconsMetadata[K <: Symbol, H, T <: Coproduct](implicit
    key: Witness.Aux[K],
    hMeta: Lazy[Metadata[H]],
    tMeta: Lazy[Metadata[T]]
  ): Metadata[FieldType[K, H] :+: T] = {
    val Product(name, subtypes) = tMeta.value.get
    self.product(name, hMeta.value.get :: subtypes)
  }

  implicit def projectMetadata[F, G](implicit
    typeable: Typeable[F],
    gen: LabelledGeneric.Aux[F, G],
    gMeta: Lazy[Metadata[G]]
  ): Metadata[F] = {
    self.project[F, G](gMeta.value)
  }
}

trait MetadataConstructors {
  private def nameOf[A](implicit typeable: Typeable[A]): String =
    typeable.describe

  def apply[A](implicit meta: Metadata[A]) =
    meta

  def atom[A](implicit typeable: Typeable[A]): Metadata[A] =
    new Metadata[A] { val get = Atom(nameOf[A]) }

  def sum[A](name: String, fields: List[(String, Schema)]): Metadata[A] =
    new Metadata[A] { val get = Sum(name, fields) }

  def product[A](name: String, subtypes: List[Schema]): Metadata[A] =
    new Metadata[A] { val get = Product(name, subtypes) }

  def array[A](name: String, item: Schema): Metadata[A] =
    new Metadata[A] { val get = Sequence(name, item) }

  def project[A: Typeable, B](bMeta: Metadata[B]): Metadata[A] =
    new Metadata[A] {
      val get = bMeta.get match {
        case Atom(name)            => Atom(nameOf[A])
        case Sum(name, fields)     => Sum(nameOf[A], fields)
        case Product(name, fields) => Product(nameOf[A], fields)
        case Sequence(name, item)  => Sequence(nameOf[A], item)
      }
    }
}