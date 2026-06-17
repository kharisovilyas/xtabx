package edo.derivation

import edo.{XmlDecoder, XmlError, XmlCursor, BadValue, MissingElement, UnknownChoice}
import scala.deriving.Mirror
import scala.compiletime.{summonAll, constValue, erasedValue}
import java.time.{LocalDate, LocalDateTime}

// ── Контекст namespace ────────────────────────────────────────────────────────

/** XML namespace для дочерней выборки; передаётся неявно в derivedProduct/derivedChoice. */
case class XmlNs(value: String)

object XmlNs:
  given XmlNs = XmlNs("urn:edo:test:document:v1")

// ── Примитивные инстансы XmlDecoder ──────────────────────────────────────────

object XmlDecoderInstances:

  given XmlDecoder[String] with
    def decode(c: XmlCursor): Either[XmlError, String] = Right(c.text)

  given XmlDecoder[Int] with
    def decode(c: XmlCursor): Either[XmlError, Int] =
      val s = c.text
      try Right(s.toInt)
      catch case _: NumberFormatException => Left(BadValue(c.label, s"not an int: $s"))

  given XmlDecoder[BigDecimal] with
    def decode(c: XmlCursor): Either[XmlError, BigDecimal] =
      val s = c.text
      try Right(BigDecimal(s))
      catch case _: NumberFormatException => Left(BadValue(c.label, s"not a decimal: $s"))

  given XmlDecoder[LocalDate] with
    def decode(c: XmlCursor): Either[XmlError, LocalDate] =
      try Right(LocalDate.parse(c.text))
      catch case _: Exception => Left(BadValue(c.label, s"not a date: ${c.text}"))

  given XmlDecoder[LocalDateTime] with
    def decode(c: XmlCursor): Either[XmlError, LocalDateTime] =
      try Right(LocalDateTime.parse(c.text))
      catch case _: Exception => Left(BadValue(c.label, s"not a dateTime: ${c.text}"))

// ── Derivation ────────────────────────────────────────────────────────────────

object XmlDecoderDerivation:

  import XmlDecoderInstances.given

  // Не приватный: используется в inline-коде на стороне call-site
  class ArrayProduct(arr: Array[Any]) extends Product:
    def canEqual(that: Any): Boolean = true
    def productArity: Int            = arr.length
    def productElement(n: Int): Any  = arr(n)

  private inline def labelsList[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].toString :: labelsList[t]

  private def toElemName(f: String): String =
    if f.isEmpty then f else f.head.toUpper.toString + f.tail

  // ── runtime builders ──────────────────────────────────────────────────────

  def buildProductDecoder[A](
    labels:   List[String],
    decoders: List[FieldDecoder[Any]],
    ns:       XmlNs,
    mirror:   Mirror.ProductOf[A]
  ): XmlDecoder[A] =
    val elemNames = labels.map(toElemName)
    new XmlDecoder[A]:
      def decode(cursor: XmlCursor): Either[XmlError, A] =
        val arr = new Array[Any](labels.length)
        var err: XmlError | Null = null
        var i = 0
        while i < labels.length && err == null do
          decoders(i).decodeFrom(cursor, ns.value, elemNames(i)) match
            case Left(e)  => err = e
            case Right(v) => arr(i) = v
          i += 1
        if err != null then Left(err.nn)
        else Right(mirror.fromProduct(ArrayProduct(arr)))

  def buildChoiceDecoder[A](
    labels:   List[String],
    decoders: List[XmlDecoder[Any]],
    ns:       XmlNs
  ): XmlDecoder[A] =
    val table: Map[String, XmlDecoder[Any]] = labels.zip(decoders).toMap
    new XmlDecoder[A]:
      def decode(cursor: XmlCursor): Either[XmlError, A] =
        // Пробуем каждый вариант: ищем дочерний элемент с именем варианта
        val found = labels.iterator.flatMap { lbl =>
          cursor.child(ns.value, lbl).map(c => table(lbl).decode(c))
        }.nextOption()
        found match
          case None    => Left(UnknownChoice(cursor.label, labels))
          case Some(r) => r.asInstanceOf[Either[XmlError, A]]

  // ── inline entry points ───────────────────────────────────────────────────

  inline def derivedProduct[A](using m: Mirror.ProductOf[A], ns: XmlNs): XmlDecoder[A] =
    buildProductDecoder[A](
      labelsList[m.MirroredElemLabels],
      summonAll[Tuple.Map[m.MirroredElemTypes, FieldDecoder]].toList.asInstanceOf[List[FieldDecoder[Any]]],
      ns,
      m
    )

  inline def derivedChoice[A](using m: Mirror.SumOf[A], ns: XmlNs): XmlDecoder[A] =
    buildChoiceDecoder[A](
      labelsList[m.MirroredElemLabels],
      summonAll[Tuple.Map[m.MirroredElemTypes, XmlDecoder]].toList.asInstanceOf[List[XmlDecoder[Any]]],
      ns
    )

  inline def derived[A](using m: Mirror.Of[A], ns: XmlNs): XmlDecoder[A] =
    inline m match
      case given Mirror.ProductOf[A] => derivedProduct[A]
      case given Mirror.SumOf[A]     => derivedChoice[A]
