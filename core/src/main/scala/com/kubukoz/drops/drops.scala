package com.kubukoz.drops

import cats.ContravariantSemigroupal
import cats.SemigroupK
import cats.Show
import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Semigroup
import com.kubukoz.drops.Schema.Contramap
import com.kubukoz.drops.Schema.Table

trait Render[A] {
  def render(a: A): String
}

object Render {
  def fromShow[A: Show]: Render[A] = _.show
  def nested[A](schema: Schema[A]): Render[A] =
    a => Schema.drawTable(NonEmptyList.one(a))(schema).mkString("\n")
}

sealed trait Schema[-A] {
  def render[B <: A]: Render[B] = Render.nested(this)
  //todo: reuse render?
  def draw[B <: A](values: NonEmptyList[B]): String = Schema.drawTable(values)(this).mkString("\n")
  def prepare[B](f: B => A): Schema[B] = Schema.Contramap(this, f)
  def provide(a: A): Schema[Any] = prepare(_ => a)
}

object Schema {
  final case class Table[A](headers: List[String], cells: A => List[String]) extends Schema[A]
  final case class Contramap[A, B](underlying: Schema[A], f: B => A) extends Schema[B]
  final case class Product[A, B](lhs: Schema[A], rhs: Schema[B]) extends Schema[(A, B)]

  def instance[A](headers: List[String], cells: A => List[String]): Schema[A] = Table(headers, cells)

  def table[A](mk: Column[A] => Schema[A]): Schema[A] = mk(new Column[A](dummy = false))

  implicit def semigroup[A]: Semigroup[Schema[A]] = semigroupK.algebra[A]

  implicit val semigroupK: SemigroupK[Schema] = new SemigroupK[Schema] {
    def combineK[A](x: Schema[A], y: Schema[A]): Schema[A] = (x, y).contramapN(a => (a, a))
  }

  implicit val contravariantSemigroupal: ContravariantSemigroupal[Schema] = new ContravariantSemigroupal[Schema] {
    def product[A, B](fa: Schema[A], fb: Schema[B]): Schema[(A, B)] =
      Product(fa, fb)
    //Schema.instance(fa.headers ++ fb.headers, { case (a, b) => fa.cells(a) ++ fb.cells(b) })

    def contramap[A, B](fa: Schema[A])(f: B => A): Schema[B] = fa.prepare(f)
  }

  //probably going to have to eq by rendering
  //todo - this will be needed for law tests
  // implicit def eq[A](implicit eqv: Eq[A => List[String]]): Eq[Schema[A]] =
  //   Eq.by(schema => (schema.headers, schema.cells _))

  def single[A](label: String)(value: A)(render: Render[A]): Schema[Unit] =
    instance(List(label), _ => List(render.render(value)))

  def drawTable[A](rows: NonEmptyList[A])(schema: Schema[A]): Seq[String] = {
    import utils._

    val (headers, cells) = toTable(schema)
    val renderedRows = rows.map(cells).toList
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

}

final class Column[A] private[drops] (private val dummy: Boolean) extends AnyVal {

  def apply[B](label: String)(f: A => B)(render: Render[B]): Schema[A] =
    Schema.instance(
      List(label),
      a => List(render.render(f(a))),
    )

}

private object utils {

  def cleanupColorCodes(s: String): String =
    s.replaceAll("\u001B\\[[;\\d]*m", "")

  def toTable[A](schema: Schema[A]): (List[String], A => List[String]) = schema match {
    case Table(headers, cells)   => (headers, cells)
    case c: Contramap[a, b]      =>
      toTable(c.underlying).map(_.compose(c.f))
    case p: Schema.Product[l, r] =>
      val (leftHeaders, leftValues) = toTable(p.lhs)
      val (rightHeaders, rightValues) = toTable(p.rhs)

      (leftHeaders ++ rightHeaders, { case (a, b) => leftValues(a) ++ rightValues(b) })
  }

  def renderLines(
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
