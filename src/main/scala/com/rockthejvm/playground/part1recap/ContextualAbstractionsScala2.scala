package com.rockthejvm.playground.part1recap

import scala.language.implicitConversions

object ContextualAbstractionsScala2 {

  //implicit classes
  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  //the implicit keyword make it so that we can decorate the
  //String class with greet() method
  implicit class ImpersonableString(name: String) {
    def greet(): String =
      Person(name).greet()
  }

  // extension method
  val greeting: String = "Peter".greet()
  //identical as above.
  val explicitGreet: String = new ImpersonableString("Peter").greet()

  import scala.concurrent.duration._
  val oneSecond: FiniteDuration = 1.second

  //implicit arguments and values
  def increment(x: Int)(implicit amount: Int): Int = x + amount
  //implicit value defined will be used whenever we need an implicit arg
  //whenever it's needed
  implicit val defaultAmount: Int = 10
  val twelve: Int                 = increment(2) //implicit argument 10 passed by the compiler

  def multiply(x: Int)(implicit factor: Int) = x * factor
  val aHundred                               = multiply(10)

  //more complex example
  trait JSONSerializer[T] { def toJson(value: T): String }

  def convert2Json[T](value: T)(implicit serializer: JSONSerializer[T]): String = {
    serializer.toJson(value)
  }

//  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
//    override def toJson(person: Person): String = "\"name\" : \"" + person.name + "\"}"
//  }

  //implement a person serializer that can be used with convert2Json
  implicit val personSerializer: JSONSerializer[Person] = (person: Person) => "{\"name\" : \"" + person.name + "\"}"

  val davidsJson: String = convert2Json(Person("David")) //implicit personSerializer passed here

  //implicit defs
  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    new JSONSerializer[List[T]] {
      override def toJson(list: List[T]): String = s"[${list.map(serializer.toJson).mkString(",")}]"
    }

//  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
//    (list: List[T]) => s"[${list.map(serializer.toJson).mkString(",")}]"

  val personsJson: String = convert2Json(List(Person("Alice"), Person("Bob")))

  //implicit conversions (not recommended). Scala 3 will have guard rails against this abuse
  case class Cat(name: String) {
    def meow(): String = s"$name is meowing"
  }

  //This is how you can shoot yourself
  implicit def string2Cat(name: String): Cat = Cat(name)
  val aCat: Cat                              = "Garfield"        //call string2Cat("Garfield")
  val garfieldMeowing: String                = "Garfield".meow() // string2Cat("Garfield").meow()

  def main(args: Array[String]): Unit = {

    println(davidsJson)
    println(personsJson)
  }

}

object TypeClassesScala2 {
  case class Person(name: String, age: Int)

  //part 1 - Type class definition. What functionalities you want to have/attach to your type class
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  //part 2 - Create type class instances for the type T you want
//  implicit val stringSerializer: JSONSerializer[String] = new JSONSerializer[String] {
//    override def toJson(value: String): String = "\"" + value + "\""
//  }

  //Because an object is also a value of its type. We can do this for conciseness. Similar as above just diff syntax
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         |{ "name" : "${value.name}", "age" : ${value.age} }
         |""".stripMargin.trim
  }

  //part 3 - Create an API
  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  def convertListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  //part 4 - add extension methods

  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    println(convertListToJson(List(Person("Alice", 23), Person("Xavier", 45))))
    val bob: Person = Person("Bob", 68)

    import JSONSyntax._
    println(bob.toJson)
  }
}
