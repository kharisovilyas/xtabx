package edo.streaming

import munit.CatsEffectSuite
import cats.effect.IO
import fs2.Stream
import edo.{XmlDecoder, XmlError, Item}
import edo.XmlDecoder.decodeItem
import edo.streaming.XmlPipeline.partition

class StreamingTest extends CatsEffectSuite:

  private val targetNs = "urn:edo:test:document:v1"

  private def resourceBytes(name: String): Stream[IO, Byte] =
    fs2.io.readClassLoaderResource[IO](name)

  given XmlDecoder[Item] with
    def decode(c: edo.XmlCursor): Either[XmlError, Item] = XmlDecoder.decodeItem(c)

  // ── Items streaming ───────────────────────────────────────────────────────

  test("stream items from full document — count") {
    resourceBytes("sample_full.xml")
      .through(XmlPipeline.decode[IO, Item]("Item", targetNs))
      .compile.toList
      .map { results =>
        assertEquals(results.length, 2)
        assert(results.forall(_.isRight), s"Some items failed: ${results.filter(_.isLeft)}")
      }
  }

  test("stream items — first item correct values") {
    resourceBytes("sample_full.xml")
      .through(XmlPipeline.decode[IO, Item]("Item", targetNs))
      .head.compile.toList
      .map { results =>
        val Right(item) = results.head: @unchecked
        assertEquals(item.lineNumber, 1)
        assertEquals(item.name, "Компьютер настольный")
        assertEquals(item.quantity, BigDecimal("2.000"))
        assertEquals(item.price, BigDecimal("50000.00"))
        assertEquals(item.amount, BigDecimal("100000.00"))
      }
  }

  test("stream items — second item lineNumber = 2") {
    resourceBytes("sample_full.xml")
      .through(XmlPipeline.decode[IO, Item]("Item", targetNs))
      .compile.toList
      .map { results =>
        val Right(item2) = results(1): @unchecked
        assertEquals(item2.lineNumber, 2)
      }
  }

  test("stream items from minimal document — single item") {
    resourceBytes("sample_minimal.xml")
      .through(XmlPipeline.decode[IO, Item]("Item", targetNs))
      .compile.toList
      .map { results =>
        assertEquals(results.length, 1)
        val Right(item) = results.head: @unchecked
        assertEquals(item.name, "Услуга консалтинга")
        assertEquals(item.vatRate, edo.VatRate.WithoutVat)
        assertEquals(item.vatAmount, None)
      }
  }

  // ── Dead-letter / error handling ──────────────────────────────────────────

  test("stream — bad value produces Left, not exception") {
    val badXml = """<?xml version="1.0" encoding="UTF-8"?>
      <root xmlns:edo="urn:edo:test:document:v1">
        <Item xmlns="urn:edo:test:document:v1" lineNumber="1">
          <Name>Test</Name>
          <UnitCode>796</UnitCode>
          <Quantity>NOT_A_NUMBER</Quantity>
          <Price>100.00</Price>
          <Amount>100.00</Amount>
          <VatRate>20</VatRate>
        </Item>
      </root>"""

    Stream.emit(badXml)
      .through(fs2.text.utf8.encode)
      .through(XmlPipeline.decode[IO, Item]("Item", targetNs))
      .compile.toList
      .map { results =>
        assertEquals(results.length, 1)
        assert(results.head.isLeft, s"Expected Left for bad Quantity, got: ${results.head}")
      }
  }

  test("partition — separates valid and dead-letter") {
    val xml = """<?xml version="1.0" encoding="UTF-8"?>
      <root xmlns:edo="urn:edo:test:document:v1">
        <Item xmlns="urn:edo:test:document:v1" lineNumber="1">
          <Name>Good</Name><UnitCode>796</UnitCode>
          <Quantity>1.000</Quantity><Price>100.00</Price>
          <Amount>100.00</Amount><VatRate>20</VatRate>
        </Item>
        <Item xmlns="urn:edo:test:document:v1" lineNumber="2">
          <Name>Bad</Name><UnitCode>796</UnitCode>
          <Quantity>1.000</Quantity><Price>200.00</Price>
          <Amount>200.00</Amount><VatRate>INVALID_RATE</VatRate>
        </Item>
      </root>"""

    val decoded = Stream.emit(xml)
      .through(fs2.text.utf8.encode)
      .through(XmlPipeline.decode[IO, Item]("Item", targetNs))

    val (valid, errors) = decoded.partition
    for
      validItems <- valid.compile.toList
      errorItems <- errors.compile.toList
    yield
      assertEquals(validItems.length, 1, s"expected 1 valid item")
      assertEquals(errorItems.length, 1, s"expected 1 error")
      assertEquals(validItems.head.name, "Good")
  }

  // ── EventListCursor unit tests ────────────────────────────────────────────

  test("EventListCursor — label and attr") {
    import fs2.data.xml.{XmlEvent, QName, Attr}
    val events = Vector(
      XmlEvent.StartTag(QName(None, "Item"), List(Attr(QName(None, "lineNumber"), List(XmlEvent.XmlString("42", false)))), false),
      XmlEvent.XmlString("some text", false),
      XmlEvent.EndTag(QName(None, "Item"))
    )
    val cursor = EventListCursor(events, targetNs)
    assertEquals(cursor.label, "Item")
    assertEquals(cursor.attr("lineNumber"), Some("42"))
    assertEquals(cursor.attr("missing"), None)
    assertEquals(cursor.text, "some text")
  }

  test("EventListCursor — child and children") {
    import fs2.data.xml.{XmlEvent, QName, Attr}
    val events = Vector(
      XmlEvent.StartTag(QName(None, "Parent"), Nil, false),
        XmlEvent.StartTag(QName(None, "Child"), List(Attr(QName(None, "n"), List(XmlEvent.XmlString("1", false)))), false),
          XmlEvent.XmlString("first", false),
        XmlEvent.EndTag(QName(None, "Child")),
        XmlEvent.StartTag(QName(None, "Child"), List(Attr(QName(None, "n"), List(XmlEvent.XmlString("2", false)))), false),
          XmlEvent.XmlString("second", false),
        XmlEvent.EndTag(QName(None, "Child")),
        XmlEvent.StartTag(QName(None, "Other"), Nil, false),
          XmlEvent.XmlString("other", false),
        XmlEvent.EndTag(QName(None, "Other")),
      XmlEvent.EndTag(QName(None, "Parent"))
    )
    val cursor = EventListCursor(events, targetNs)
    assertEquals(cursor.label, "Parent")
    assertEquals(cursor.child(targetNs, "Child").map(_.text), Some("first"))
    assertEquals(cursor.children(targetNs, "Child").length, 2)
    assertEquals(cursor.children(targetNs, "Child").map(_.attr("n")), List(Some("1"), Some("2")))
    assertEquals(cursor.child(targetNs, "Other").map(_.text), Some("other"))
    assertEquals(cursor.child(targetNs, "NotExist"), None)
  }

  test("EventListCursor — text excludes nested element text") {
    import fs2.data.xml.{XmlEvent, QName}
    val events = Vector(
      XmlEvent.StartTag(QName(None, "Root"), Nil, false),
        XmlEvent.XmlString("before ", false),
        XmlEvent.StartTag(QName(None, "Nested"), Nil, false),
          XmlEvent.XmlString("inner", false),
        XmlEvent.EndTag(QName(None, "Nested")),
        XmlEvent.XmlString(" after", false),
      XmlEvent.EndTag(QName(None, "Root"))
    )
    val cursor = EventListCursor(events, targetNs)
    assertEquals(cursor.text, "before  after")
  }
