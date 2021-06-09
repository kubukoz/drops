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
    def both(a: A): List[(String, String)]

    def headers(a: A): List[String] = both(a).map(_._1)
    def cells(a: A): List[String] = both(a).map(_._2)

    def render: Render[A] = Render.nested(this)
  }

  object Schema {
    def make[A](mk: Column[A] => Schema[A]): Schema[A] = mk(new Column[A])
    def union[A](mk: A => Schema[Unit]): Schema[A] = mk(_).both(())

    implicit def semigroup[A]: Semigroup[Schema[A]] =
      (a, b) => c => a.both(c) ++ b.both(c)

    def single[A](label: String)(value: A)(render: Render[A]): Schema[Unit] =
      _ =>
        List(
          (label, render.render(value))
        )

  }

  final class Column[A] private[drops] () {
    def apply[B](label: String)(f: A => B)(render: Render[B]): Schema[A] =
      a => Schema.single(label)(f(a))(render).both(())
  }

  private def cleanupColorCodes(s: String): String =
    s.replaceAll("\u001B\\[[;\\d]*m", "")

  def drawTable[A](rows: NonEmptyList[A])(schema: Schema[A]): Seq[String] = {
    val renderedRows = rows.map(schema.cells).toList
    val headers = schema.headers(rows.head)
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
