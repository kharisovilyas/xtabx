package edo

import cats.effect.{IO, IOApp}
import fs2.Stream
import fs2.io.readClassLoaderResource
import scala.xml.XML

import edo.*
import edo.derivation.DerivedInstances.given
import edo.derivation.XmlDecoderInstances.given
import edo.streaming.XmlPipeline
import edo.streaming.XmlPipeline.partition

/**
 * Полный пример использования xtabx.
 *
 * Демонстрирует все 4 режима работы:
 *   1. Ручной декодер   — весь документ через scala.xml DOM
 *   2. Mirror-деривация — автоматические декодеры для вложенных типов
 *   3. assertConforms   — compile-time проверка ADT ↔ XSD
 *   4. Streaming        — fs2 pipeline, O(запись) память, dead-letter partition
 *
 * Запуск:
 *   sbt "runMain edo.UsageExample"
 */
object UsageExample extends IOApp.Simple:

  // ─────────────────────────────────────────────────────────────────────────
  // 0.  Namespace и XmlDecoder[Item] для стриминга
  // ─────────────────────────────────────────────────────────────────────────

  private val ns = "urn:edo:test:document:v1"

  /** Item декодируется вручную (атрибуты + специальные типы). */
  given XmlDecoder[Item] with
    def decode(c: XmlCursor): Either[XmlError, Item] = XmlDecoder.decodeItem(c)

  // ─────────────────────────────────────────────────────────────────────────
  // 1.  Ручной декодер полного документа (DOM)
  // ─────────────────────────────────────────────────────────────────────────

  def demo1_fullDocumentDom(): Unit =
    println("\n══════════════════════════════════════════════")
    println("  1. Полный документ — DOM-декодер")
    println("══════════════════════════════════════════════")

    val stream = getClass.getClassLoader.getResourceAsStream("sample_full.xml")
    val xml    = XML.load(stream)

    XmlDecoder.decodeDocument(xml) match
      case Left(err) =>
        println(s"  ✗ Ошибка декодирования: ${err.message}")

      case Right(doc) =>
        println(s"  Документ : ${doc.id}")
        println(s"  Версия   : ${doc.version}")
        println(s"  Тип      : ${doc.header.documentType}")
        println(s"  Номер    : ${doc.header.documentNumber}")
        println(s"  Дата     : ${doc.header.documentDate}")
        println(s"  Валюта   : ${doc.header.currency}")

        if doc.header.notes.nonEmpty then
          println(s"  Примечания:")
          doc.header.notes.foreach(n => println(s"    • $n"))

        println(s"\n  Участники (${doc.participants.length}):")
        doc.participants.foreach { p =>
          val kindStr = p.kind match
            case le: LegalEntity  => s"ЮЛ «${le.shortName.getOrElse(le.fullName)}»"
            case ind: Individual  => s"ФЛ ${ind.lastName} ${ind.firstName}"
            case fe: ForeignEntity => s"ИО ${fe.name} (${fe.countryCode})"
          val addrStr = p.address match
            case ra: RussianAddress => s"${ra.city}, ${ra.street.getOrElse("")}"
            case fa: ForeignAddress => fa.value
          println(s"    ${p.role.toString.padTo(8, ' ')} ИНН ${p.inn}  $kindStr  [$addrStr]")
        }

        println(s"\n  Позиции (${doc.body.items.length}):")
        doc.body.items.foreach { i =>
          val vatStr = i.vatAmount.map(v => s"НДС $v").getOrElse("без НДС")
          println(s"    ${i.lineNumber}. ${i.name.take(35).padTo(36, ' ')} ${i.quantity} × ${i.price} = ${i.amount}  ($vatStr)")
        }

        val t = doc.body.totals
        println(s"\n  Итого без НДС : ${t.totalWithoutVat}")
        println(s"  НДС           : ${t.totalVat}")
        println(s"  Итого с НДС   : ${t.totalWithVat}")

        doc.signatures.foreach { sigs =>
          println(s"\n  Подписей: ${sigs.length}")
          sigs.foreach(s => println(s"    ${s.algorithm}  ИНН ${s.signerInn}  ${s.signedAt}"))
        }

  // ─────────────────────────────────────────────────────────────────────────
  // 2.  Mirror-деривация — отдельные вложенные типы
  // ─────────────────────────────────────────────────────────────────────────

  def demo2_mirrorDerivation(): Unit =
    println("\n══════════════════════════════════════════════")
    println("  2. Mirror-деривация — decoded вложенные типы")
    println("══════════════════════════════════════════════")

    // LegalEntity
    val leXml = XML.loadString(
      s"""<LegalEntity xmlns="$ns">
           <FullName>ООО «Технологии Будущего»</FullName>
           <ShortName>ООО ТБ</ShortName>
           <Ogrn>9876543210987</Ogrn>
         </LegalEntity>"""
    )
    summon[XmlDecoder[LegalEntity]].decode(NodeCursor(leXml)) match
      case Right(le) => println(s"  LegalEntity  : ${le.fullName}  ОГРН ${le.ogrn}")
      case Left(e)   => println(s"  ✗ $e")

    // RussianAddress
    val addrXml = XML.loadString(
      s"""<Address xmlns="$ns">
           <RussianAddress>
             <PostalCode>620000</PostalCode>
             <RegionCode>66</RegionCode>
             <City>Екатеринбург</City>
             <Street>пр. Ленина</Street>
             <Building>д. 1</Building>
           </RussianAddress>
         </Address>"""
    )
    summon[XmlDecoder[Address]].decode(NodeCursor(addrXml)) match
      case Right(ra: RussianAddress) => println(s"  RussianAddress: ${ra.postalCode} ${ra.city}, ${ra.street.getOrElse("")}")
      case Right(other)              => println(s"  Address: $other")
      case Left(e)                   => println(s"  ✗ $e")

    // Totals
    val totalsXml = XML.loadString(
      s"""<Totals xmlns="$ns">
           <TotalWithoutVat>500000.00</TotalWithoutVat>
           <TotalVat>100000.00</TotalVat>
           <TotalWithVat>600000.00</TotalWithVat>
         </Totals>"""
    )
    summon[XmlDecoder[Totals]].decode(NodeCursor(totalsXml)) match
      case Right(t) => println(s"  Totals       : ${t.totalWithoutVat} + ${t.totalVat} = ${t.totalWithVat}")
      case Left(e)  => println(s"  ✗ $e")

    // Choice — ForeignAddress
    val foreignXml = XML.loadString(
      s"""<Address xmlns="$ns">
           <ForeignAddress>123 Main St, New York, USA</ForeignAddress>
         </Address>"""
    )
    summon[XmlDecoder[Address]].decode(NodeCursor(foreignXml)) match
      case Right(fa: ForeignAddress) => println(s"  ForeignAddress: ${fa.value}")
      case Right(other)              => println(s"  Address: $other")
      case Left(e)                   => println(s"  ✗ $e")

    // Ошибка декодирования — bad value
    val badTotalsXml = XML.loadString(
      s"""<Totals xmlns="$ns">
           <TotalWithoutVat>NOT_A_NUMBER</TotalWithoutVat>
           <TotalVat>0</TotalVat>
           <TotalWithVat>0</TotalWithVat>
         </Totals>"""
    )
    summon[XmlDecoder[Totals]].decode(NodeCursor(badTotalsXml)) match
      case Left(BadValue(path, detail)) => println(s"  ✓ Ожидаемая ошибка: BadValue($path, $detail)")
      case other                        => println(s"  Неожиданно: $other")

  // ─────────────────────────────────────────────────────────────────────────
  // 3.  Compile-time проверка assertConforms (результат уже при компиляции)
  // ─────────────────────────────────────────────────────────────────────────

  def demo3_schemaCheck(): Unit =
    println("\n══════════════════════════════════════════════")
    println("  3. Compile-time assertConforms — XSD ↔ ADT")
    println("══════════════════════════════════════════════")

    // Эти вызовы проверяются КОМПИЛЯТОРОМ при сборке.
    // Если поле в ADT не совпадает с XSD — compile error.
    derivation.SchemaCheck.assertConforms[LegalEntity]()
    derivation.SchemaCheck.assertConforms[Individual]()
    derivation.SchemaCheck.assertConforms[ForeignEntity]()
    derivation.SchemaCheck.assertConforms[RussianAddress]()
    derivation.SchemaCheck.assertConforms[Totals]()
    derivation.SchemaCheck.assertConforms[Header]()

    println("  ✓ LegalEntity     соответствует LegalEntityType в XSD")
    println("  ✓ Individual      соответствует IndividualType в XSD")
    println("  ✓ ForeignEntity   соответствует ForeignEntityType в XSD")
    println("  ✓ RussianAddress  соответствует RussianAddressType в XSD")
    println("  ✓ Totals          соответствует TotalsType в XSD")
    println("  ✓ Header          соответствует HeaderType в XSD")
    println("  (проверено компилятором, не в runtime)")

  // ─────────────────────────────────────────────────────────────────────────
  // 4.  Streaming — fs2 pipeline, O(запись) память
  // ─────────────────────────────────────────────────────────────────────────

  def demo4_streaming(): IO[Unit] =
    IO.println("\n══════════════════════════════════════════════") >>
    IO.println("  4. Streaming — fs2 pipeline O(запись)") >>
    IO.println("══════════════════════════════════════════════") >>
    {
      // 4a. Считаем позиции из полного документа по одной
      val fullStream =
        readClassLoaderResource[IO]("sample_full.xml")
          .through(XmlPipeline.decode[IO, Item]("Item", ns))

      fullStream
        .evalTap {
          case Right(item) =>
            IO.println(s"  ✓ Позиция ${item.lineNumber}: ${item.name.take(30).padTo(31,' ')} ${item.price} × ${item.quantity} = ${item.amount}")
          case Left(err)   =>
            IO.println(s"  ✗ Ошибка: ${err.message}")
        }
        .compile.drain
    } >>
    // 4b. Dead-letter partition
    IO.println("\n  -- Dead-letter partition (1 валидная, 1 битая) --") >>
    {
      val mixedXml =
        s"""<?xml version="1.0" encoding="UTF-8"?>
           |<root xmlns="$ns">
           |  <Item lineNumber="1">
           |    <Name>Принтер лазерный</Name>
           |    <UnitCode>796</UnitCode>
           |    <Quantity>3.000</Quantity>
           |    <Price>25000.00</Price>
           |    <Amount>75000.00</Amount>
           |    <VatRate>20</VatRate>
           |    <VatAmount>15000.00</VatAmount>
           |  </Item>
           |  <Item lineNumber="2">
           |    <Name>Картридж</Name>
           |    <UnitCode>796</UnitCode>
           |    <Quantity>NOT_A_NUMBER</Quantity>
           |    <Price>1500.00</Price>
           |    <Amount>0.00</Amount>
           |    <VatRate>20</VatRate>
           |  </Item>
           |</root>""".stripMargin

      val decoded =
        Stream.emit(mixedXml)
          .through(fs2.text.utf8.encode)
          .through(XmlPipeline.decode[IO, Item]("Item", ns))

      val (valid, dead) = decoded.partition

      for
        items <- valid.compile.toList
        errs  <- dead.compile.toList
      yield
        items.foreach(i => println(s"  ✓ Принято  : позиция ${i.lineNumber} «${i.name}» ${i.amount} RUB"))
        errs.foreach(e  => println(s"  ✗ Отклонено: ${e.message}"))
        println(s"\n  Итого: ${items.length} принято, ${errs.length} в dead-letter")
    } >>
    // 4c. Минимальный документ
    IO.println("\n  -- Минимальный документ (sample_minimal.xml) --") >>
    {
      readClassLoaderResource[IO]("sample_minimal.xml")
        .through(XmlPipeline.decode[IO, Item]("Item", ns))
        .evalTap {
          case Right(item) =>
            IO.println(s"  ✓ ${item.name}  НДС: ${item.vatRate}  сумма: ${item.amount}")
          case Left(err)   =>
            IO.println(s"  ✗ ${err.message}")
        }
        .compile.drain
    }

  // ─────────────────────────────────────────────────────────────────────────
  // Main
  // ─────────────────────────────────────────────────────────────────────────

  def run: IO[Unit] =
    IO(demo1_fullDocumentDom()) >>
    IO(demo2_mirrorDerivation()) >>
    IO(demo3_schemaCheck()) >>
    demo4_streaming() >>
    IO.println("\n✓ Все примеры выполнены успешно.\n")
