package com.kubukoz

import cats.kernel.Semigroup
import cats.Show
import cats.implicits._
import cats.data.NonEmptyList

object drops {

  trait Render[A] {
    def render(a: A): String
  }

  object Render {
    def fromShow[A: Show]: Render[A] = _.show
    def nested[A](schema: Schema[A]): Render[A] =
      a => drawTable(NonEmptyList.one(a))(schema).mkString("\n")
  }

  trait Schema[A] {
    def headers: List[String]
    def cells(a: A): List[String]

    def render: Render[A] = Render.nested(this)
    def provide(a: A): Schema[Any] = Schema.instance(headers, _ => cells(a))
  }

  object Schema {

    def instance[A](headers: List[String], cells: A => List[String]): Schema[A] = {
      val h = headers
      val c = cells
      new Schema[A] {
        val headers: List[String] = h

        def cells(a: A): List[String] = c(a)
      }
    }

    def make[A](mk: Column[A] => Schema[A]): Schema[A] = mk(new Column[A])

    implicit def semigroup[A]: Semigroup[Schema[A]] =
      (a, b) =>
        instance(
          a.headers ++ b.headers,
          s => a.cells(s) ++ b.cells(s),
        )

    def single[A](label: String)(value: A)(render: Render[A]): Schema[Unit] = {
      val rendered = List(render.render(value))

      instance(
        List(label),
        _ => rendered,
      )
    }

  }

  final class Column[A] private[drops] () {

    def apply[B](label: String)(f: A => B)(render: Render[B]): Schema[A] =
      Schema.instance(
        List(label),
        a => List(render.render(f(a))),
      )

  }

  private def cleanupColorCodes(s: String): String =
    s.replaceAll("\u001B\\[[;\\d]*m", "")

  def drawTable[A](rows: NonEmptyList[A])(schema: Schema[A]): Seq[String] = {
    val headers = schema.headers
    val renderedRows = rows.map(schema.cells).toList
    val widths = (headers, renderedRows.transpose)
      .parMapN { (header, valuesAtHeader) =>
        (header :: valuesAtHeader)
          .flatMap(_.linesIterator)
          .map(cleanupColorCodes(_).length)
          .max
      }

    renderLines(
      List(headers) ++ renderedRows,
      widths,
    )
  }

  private def renderLines(
    lines: List[List[String]],
    widths: List[Int],
  ): Seq[String] = {
    val header :: rest = lines
      .map(padHeights)
      .map { line =>
        (line, widths).parMapN { (lineContents, n) =>
          lineContents.map { s =>
            val ss = s.value
            val trimmedLength = ss.length - cleanupColorCodes(ss).length
            FlatString(s.value.padTo(n + trimmedLength, ' '))
          }
        }
      }

    val filler = header.map(
      _.map(w => FlatString("=" * cleanupColorCodes(w.value).length))
    )

    (List(filler, header, filler) ++ rest ++ Seq(filler))
      .map(renderLine)
  }

  private def renderLine(columns: List[List[FlatString]]): String =
    columns
      .transpose
      .map(_.map(_.value).mkString(" | ", " | ", " | "))
      .mkString("\n")

  private def padHeights(values: List[String]): List[List[FlatString]] = {
    val maxHeight = values.map(_.linesIterator.length).max
    values.map { value =>
      value
        .linesIterator
        .padTo(maxHeight, "")
        .map(FlatString(_))
        .toList
    }
  }

  //no-newline string
  private final case class FlatString(value: String) extends AnyVal
}
