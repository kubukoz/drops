package com.kubukoz

import cats.SemigroupK
import cats.Show
import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Eq
import cats.kernel.Semigroup
import cats.ContravariantSemigroupal

object drops {

  trait Render[A] {
    def render(a: A): String
  }

  object Render {
    def fromShow[A: Show]: Render[A] = _.show
    def nested[A](schema: Schema[A]): Render[A] =
      a => drawTable(NonEmptyList.one(a))(schema).mkString("\n")
  }

  trait Schema[-A] {
    def headers: List[String]
    def cells(a: A): List[String]

    def render[B <: A]: Render[B] = Render.nested(this)

    def prepare[B](f: B => A): Schema[B] = Schema.instance(headers, b => cells(f(b)))
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

    def make[A](mk: Column[A] => Schema[A]): Schema[A] = mk(new Column[A](dummy = false))

    implicit def semigroup[A]: Semigroup[Schema[A]] = semigroupK.algebra[A]

    implicit val semigroupK: SemigroupK[Schema] = new SemigroupK[Schema] {
      def combineK[A](x: Schema[A], y: Schema[A]): Schema[A] = (x, y).contramapN(a => (a, a))
    }

    implicit val contravariantSemigroupal: ContravariantSemigroupal[Schema] = new ContravariantSemigroupal[Schema] {
      def product[A, B](fa: Schema[A], fb: Schema[B]): Schema[(A, B)] =
        Schema.instance(fa.headers ++ fb.headers, { case (a, b) => fa.cells(a) ++ fb.cells(b) })

      def contramap[A, B](fa: Schema[A])(f: B => A): Schema[B] = fa.prepare(f)
    }

    implicit def eq[A](implicit eqv: Eq[A => List[String]]): Eq[Schema[A]] =
      Eq.by(schema => (schema.headers, schema.cells _))

    def single[A](label: String)(value: A)(render: Render[A]): Schema[Unit] = {
      val rendered = List(render.render(value))

      instance(
        List(label),
        _ => rendered,
      )
    }

  }

  final class Column[A] private[drops] (private val dummy: Boolean) extends AnyVal {

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
          .maximumOption
          .getOrElse(0)
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

  import implicits._

  private def renderLine(columns: List[List[FlatString]]): String =
    columns
      .transpose
      .map(_.map(_.value.surround(" ")).mkString("|", "|", "|"))
      .mkString("\n")

  private def padHeights(values: List[String]): List[List[FlatString]] = {
    val maxHeight = values.map(_.linesIterator.length).maximumOption.getOrElse(0)
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

  object implicits {

    private[drops] final implicit class DropsStringOps(private val s: String) extends AnyVal {
      def surround(wrap: String): String = wrap + s + wrap
    }

  }

}
