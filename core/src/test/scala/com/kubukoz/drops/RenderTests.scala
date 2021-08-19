package com.kubukoz.drops

import cats.data.NonEmptyList
import cats.implicits._
import munit.FunSuite
import unindent._

class RenderTests extends FunSuite {
  def renderTest[A](name: String)(schema: Schema[A], value: A, expected: String) =
    test(name)(assertEquals(schema.render[A].render(value), expected))

  renderTest("Single column")(
    Schema.single("hello")(42)(Render.fromShow),
    (),
    i"""| ===== |
          | hello |
          | ===== |
          | 42    |
          | ===== |""",
  )

  renderTest("Single column: value is longer")(
    Schema.single("name")("value")(Render.fromShow),
    (),
    i"""| ===== |
        | name  |
        | ===== |
        | value |
        | ===== |""",
  )

  case class Person(name: String, age: Int, programmer: Boolean)

  object Person {

    val schema = Schema.table[Person] { col =>
      col("name")(_.name)(Render.fromShow) |+|
        col("age")(_.age)(Render.fromShow) |+|
        col("programmer")(_.programmer)(Render.fromShow)
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
      Schema
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

    val schema = Schema.table[Friendship] { col =>
      col("of")(_.of)(Render.nested(Person.schema)) |+|
        col("with person")(_.withPerson)(Render.nested(Person.schema))
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

  test("Max width") {
    assertEquals(
      Schema
        .table[Person] { col =>
          // should config be a column parameter?
          col("name")(_.name)(Render.fromShow[String].configure(Render.Config(maxWidth = Some("Very long name".length)))) |+|
            col("age")(_.age)(Render.fromShow) |+|
            col("programmer")(_.programmer)(Render.fromShow)
        }
        .render
        .render(Person("Very long nameof a person", 10, true)),
      i"""| ============== | === | ========== |
          | name           | age | programmer |
          | ============== | === | ========== |
          | Very long name | 10  | true       |
          | of a person    |     |            |
          | ============== | === | ========== |""",
    )
  }
}
