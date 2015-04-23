import shapeless._
import play.api.libs.json._

final case class Person(name: String, address: Address, pet: Pet)
final case class Address(house: Int, street: String)

sealed trait Pet
final case class Dog(name: String, iq: Int) extends Pet
final case class Cat(name: String, evil: Boolean) extends Pet

object Main extends App {
  val personMeta = Metadata[Person].get
  // Sum(
  //   "Person",
  //   List(
  //     ("name",Atom("String")),
  //     ("address",Sum(
  //       "Address",
  //       List(
  //         ("house",Atom("Int")),
  //         ("street",Atom("String"))
  //       )
  //     )),
  //     ("pet",Product(
  //       "Pet",
  //       List(
  //         Sum("Cat",List(("name",Atom("String")), ("evil",Atom("Boolean")))),
  //         Sum("Dog",List(("name",Atom("String")), ("iq",Atom("Int"))))
  //       )
  //     ))
  //   )
  // )

  val personJson = Json.toJson(personMeta)
  // {
  //   "Person" : {
  //     "type" : "Sum",
  //     "name" : "Person",
  //     "fields" : {
  //       "name" : "String",
  //       "address" : "Address",
  //       "pet" : "Pet"
  //     }
  //   },
  //   "Pet" : {
  //     "type" : "Product",
  //     "name" : "Pet",
  //     "subtypes" : [ "Cat", "Dog" ]
  //   },
  //   "Address" : {
  //     "type" : "Sum",
  //     "name" : "Address",
  //     "fields" : {
  //       "house" : "Int",
  //       "street" : "String"
  //     }
  //   },
  //   "Dog" : {
  //     "type" : "Sum",
  //     "name" : "Dog",
  //     "fields" : {
  //       "name" : "String",
  //       "iq" : "Int"
  //     }
  //   },
  //   "Int" : {
  //     "type" : "Atom"
  //   },
  //   "Cat" : {
  //     "type" : "Sum",
  //     "name" : "Cat",
  //     "fields" : {
  //       "name" : "String",
  //       "evil" : "Boolean"
  //     }
  //   },
  //   "String" : {
  //     "type" : "Atom"
  //   },
  //   "Boolean" : {
  //     "type" : "Atom"
  //   }
  // }
}
