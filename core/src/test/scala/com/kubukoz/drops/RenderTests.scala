import cats.implicits._
import com.kubukoz.drops
import munit.FunSuite
import unindent._
import cats.data.NonEmptyList

class RenderTests extends FunSuite {
  def renderTest[A](name: String)(schema: drops.Schema[A], value: A, expected: String) =
    test(name)(assertEquals(schema.render[A].render(value), expected))

  renderTest("Single column")(
    drops.Schema.single("hello")(42)(drops.Render.fromShow),
    (),
    i"""| ===== |
          | hello |
          | ===== |
          | 42    |
          | ===== |""",
  )

  renderTest("Single column: value is longer")(
    drops.Schema.single("name")("value")(drops.Render.fromShow),
    (),
    i"""| ===== |
        | name  |
        | ===== |
        | value |
        | ===== |""",
  )

  case class Person(name: String, age: Int, programmer: Boolean)

  object Person {

    val schema = drops.Schema.make[Person] { col =>
      col("name")(_.name)(drops.Render.fromShow) |+|
        col("age")(_.age)(drops.Render.fromShow) |+|
        col("programmer")(_.programmer)(drops.Render.fromShow)
    }

  }

  renderTest("Multiple columns")(
    Person.schema,
    Person("Jane", 23, true),
    i"""| ==== | === | ========== |
        | name | age | programmer |
        | ==== | === | ========== |
        | Jane | 23  | true       |
        | ==== | === | ========== |""",
  )

  val jane = Person("Jane", 23, true)
  val anon = Person("Anonymous", 1337, false)

  test("Multiple columns, multiple rows") {
    assertEquals(
      drops
        .drawTable(
          NonEmptyList.of(
            jane,
            anon,
          )
        )(Person.schema)
        .mkString("\n"),
      i"""| ========= | ==== | ========== |
          | name      | age  | programmer |
          | ========= | ==== | ========== |
          | Jane      | 23   | true       |
          | Anonymous | 1337 | false      |
          | ========= | ==== | ========== |""",
    )
  }

  renderTest("Multi-line cell")(
    Person.schema,
    Person("Joe\nDoe", 30, true),
    i"""| ==== | === | ========== |
        | name | age | programmer |
        | ==== | === | ========== |
        | Joe  | 30  | true       |
        | Doe  |     |            |
        | ==== | === | ========== |""",
  )

  case class Friendship(of: Person, withPerson: Person)

  object Friendship {

    val schema = drops.Schema.make[Friendship] { col =>
      col("of")(_.of)(drops.Render.nested(Person.schema)) |+|
        col("with person")(_.withPerson)(drops.Render.nested(Person.schema))
    }

  }

  renderTest("Nesting")(
    Friendship.schema,
    Friendship(
      jane,
      anon,
    ),
    i"""| =========================== | ================================= |
        | of                          | with person                       |
        | =========================== | ================================= |
        | | ==== | === | ========== | | | ========= | ==== | ========== | |
        | | name | age | programmer | | | name      | age  | programmer | |
        | | ==== | === | ========== | | | ========= | ==== | ========== | |
        | | Jane | 23  | true       | | | Anonymous | 1337 | false      | |
        | | ==== | === | ========== | | | ========= | ==== | ========== | |
        | =========================== | ================================= |""",
  )
}
