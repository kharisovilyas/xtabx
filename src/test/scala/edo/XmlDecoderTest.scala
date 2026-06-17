package edo

import munit.FunSuite
import scala.xml.XML
import java.time.{LocalDate, LocalDateTime}

class XmlDecoderTest extends FunSuite:

  private def loadXml(resource: String): scala.xml.Elem =
    val stream = getClass.getClassLoader.getResourceAsStream(resource)
    assert(stream != null, s"resource not found: $resource")
    XML.load(stream)

  // ─── full document ────────────────────────────────────────────────────────

  test("decode full document without errors") {
    val root   = loadXml("sample_full.xml")
    val result = XmlDecoder.decodeDocument(root)
    assert(result.isRight, s"Expected Right but got: $result")
  }

  test("document id and version") {
    val doc = loadXml("sample_full.xml")
    val Right(ed) = XmlDecoder.decodeDocument(doc): @unchecked
    assertEquals(ed.id, "a1b2c3d4-e5f6-7890-abcd-ef1234567890")
    assertEquals(ed.version, "1.0")
    assertEquals(ed.schemaUri, Some("urn:edo:test:document:v1"))
  }

  test("header fields") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_full.xml")): @unchecked
    val h = ed.header
    assertEquals(h.documentNumber, "ИНВ-2024/001")
    assertEquals(h.documentDate, LocalDate.of(2024, 3, 15))
    assertEquals(h.documentType, DocumentKind.Invoice)
    assertEquals(h.currency, "RUB")
    assertEquals(h.correctionInfo, None)
    assertEquals(h.notes, List("Счёт-фактура за март 2024", "Дополнительное примечание"))
  }

  test("participants count and roles") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_full.xml")): @unchecked
    assertEquals(ed.participants.length, 2)
    assertEquals(ed.participants(0).role, ParticipantRole.Seller)
    assertEquals(ed.participants(1).role, ParticipantRole.Buyer)
  }

  test("seller is LegalEntity with contacts") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_full.xml")): @unchecked
    val seller = ed.participants(0)
    assertEquals(seller.inn, "7701234567")
    assertEquals(seller.kpp, Some("770101001"))
    seller.kind match
      case le: LegalEntity =>
        assertEquals(le.fullName, "ООО «Ромашка»")
        assertEquals(le.shortName, Some("ООО Ромашка"))
        assertEquals(le.ogrn, "1234567890123")
      case other => fail(s"Expected LegalEntity, got $other")
    seller.address match
      case ra: RussianAddress =>
        assertEquals(ra.postalCode, "115093")
        assertEquals(ra.regionCode, "77")
        assertEquals(ra.city, "Москва")
        assertEquals(ra.street, Some("ул. Садовая"))
        assertEquals(ra.building, Some("д. 5, стр. 1"))
      case other => fail(s"Expected RussianAddress, got $other")
    val contacts = seller.contacts.getOrElse(fail("expected contacts"))
    assertEquals(contacts.phones, List("+7 495 123-45-67"))
    assertEquals(contacts.emails, List("info@romashka.ru"))
  }

  test("buyer is Individual") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_full.xml")): @unchecked
    val buyer = ed.participants(1)
    assertEquals(buyer.inn, "501234567890")
    assertEquals(buyer.kpp, None)
    buyer.kind match
      case ind: Individual =>
        assertEquals(ind.lastName, "Иванов")
        assertEquals(ind.firstName, "Иван")
        assertEquals(ind.middleName, Some("Иванович"))
        assertEquals(ind.ogrnip, None)
      case other => fail(s"Expected Individual, got $other")
    assertEquals(buyer.contacts, None)
  }

  test("body items") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_full.xml")): @unchecked
    val items = ed.body.items
    assertEquals(items.length, 2)

    val item1 = items(0)
    assertEquals(item1.lineNumber, 1)
    assertEquals(item1.name, "Компьютер настольный")
    assertEquals(item1.unitCode, "796")
    assertEquals(item1.quantity, BigDecimal("2.000"))
    assertEquals(item1.price, BigDecimal("50000.00"))
    assertEquals(item1.amount, BigDecimal("100000.00"))
    assertEquals(item1.vatRate, VatRate.Twenty)
    assertEquals(item1.vatAmount, Some(BigDecimal("20000.00")))
    assertEquals(item1.customsDeclaration, None)

    val item2 = items(1)
    assertEquals(item2.lineNumber, 2)
    assertEquals(item2.name, "Монитор 27\"")
  }

  test("body totals") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_full.xml")): @unchecked
    val t = ed.body.totals
    assertEquals(t.totalWithoutVat, BigDecimal("150000.00"))
    assertEquals(t.totalVat, BigDecimal("30000.00"))
    assertEquals(t.totalWithVat, BigDecimal("180000.00"))
  }

  test("body taxes") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_full.xml")): @unchecked
    val taxes = ed.body.taxes.getOrElse(fail("expected taxes"))
    assertEquals(taxes.length, 2)
    assertEquals(taxes(0).itemRef, 1)
    assertEquals(taxes(0).rate, VatRate.Twenty)
    assertEquals(taxes(0).base, BigDecimal("100000.00"))
    assertEquals(taxes(0).amount, BigDecimal("20000.00"))
    assertEquals(taxes(1).itemRef, 2)
  }

  test("signatures") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_full.xml")): @unchecked
    val sigs = ed.signatures.getOrElse(fail("expected signatures"))
    assertEquals(sigs.length, 1)
    val sig = sigs(0)
    assertEquals(sig.algorithm, SignAlgorithm.GOST256)
    assertEquals(sig.signerInn, "7701234567")
    assertEquals(sig.certificateThumbprint, "AABBCCDDEEFF00112233445566778899AABBCCDD")
    assertEquals(sig.signedAt, LocalDateTime.of(2024, 3, 15, 14, 30, 0))
  }

  // ─── minimal document ─────────────────────────────────────────────────────

  test("decode minimal document without errors") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_minimal.xml")): @unchecked
    assertEquals(ed.id, "00000000-0000-0000-0000-000000000001")
    assertEquals(ed.schemaUri, None)
    assertEquals(ed.header.documentType, DocumentKind.Act)
    assertEquals(ed.header.notes, Nil)
    assertEquals(ed.header.correctionInfo, None)
    assertEquals(ed.body.taxes, None)
    assertEquals(ed.signatures, None)
  }

  test("minimal: ForeignEntity and ForeignAddress") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_minimal.xml")): @unchecked
    val seller = ed.participants(0)
    seller.kind match
      case fe: ForeignEntity =>
        assertEquals(fe.name, "Acme Corp")
        assertEquals(fe.countryCode, "US")
        assertEquals(fe.taxId, None)
      case other => fail(s"Expected ForeignEntity, got $other")
    seller.address match
      case fa: ForeignAddress =>
        assertEquals(fa.value, "123 Main St, New York, USA")
      case other => fail(s"Expected ForeignAddress, got $other")
  }

  test("minimal: WithoutVat rate") {
    val Right(ed) = XmlDecoder.decodeDocument(loadXml("sample_minimal.xml")): @unchecked
    assertEquals(ed.body.items(0).vatRate, VatRate.WithoutVat)
    assertEquals(ed.body.items(0).vatAmount, None)
  }

  // ─── error cases ──────────────────────────────────────────────────────────

  test("missing required attribute returns MissingAttribute") {
    val xml = scala.xml.XML.loadString("""
      <edo:ElectronicDocument xmlns:edo="urn:edo:test:document:v1" version="1.0">
        <edo:Header/>
        <edo:Participants/>
        <edo:Body/>
      </edo:ElectronicDocument>""")
    val result = XmlDecoder.decodeDocument(xml)
    assert(result.isLeft)
    result match
      case Left(MissingAttribute(name)) => assertEquals(name, "id")
      case other => fail(s"Unexpected: $other")
  }

  test("unknown DocumentType returns BadValue") {
    val xml = scala.xml.XML.loadString("""
      <edo:ElectronicDocument xmlns:edo="urn:edo:test:document:v1" id="00000000-0000-0000-0000-000000000001" version="1.0">
        <edo:Header>
          <edo:DocumentNumber>X</edo:DocumentNumber>
          <edo:DocumentDate>2024-01-01</edo:DocumentDate>
          <edo:DocumentType>UnknownType</edo:DocumentType>
          <edo:Currency>RUB</edo:Currency>
        </edo:Header>
        <edo:Participants/>
        <edo:Body/>
      </edo:ElectronicDocument>""")
    val result = XmlDecoder.decodeDocument(xml)
    assert(result.isLeft)
    result match
      case Left(BadValue(path, _)) => assert(path.contains("DocumentType"))
      case other => fail(s"Unexpected: $other")
  }

  test("missing Body element returns MissingElement") {
    val xml = scala.xml.XML.loadString("""
      <edo:ElectronicDocument xmlns:edo="urn:edo:test:document:v1" id="00000000-0000-0000-0000-000000000001" version="1.0">
        <edo:Header>
          <edo:DocumentNumber>X</edo:DocumentNumber>
          <edo:DocumentDate>2024-01-01</edo:DocumentDate>
          <edo:DocumentType>Invoice</edo:DocumentType>
          <edo:Currency>RUB</edo:Currency>
        </edo:Header>
        <edo:Participants>
          <edo:Participant role="Seller">
            <edo:Inn>1234567890</edo:Inn>
            <edo:LegalEntity>
              <edo:FullName>Test</edo:FullName>
              <edo:Ogrn>1234567890123</edo:Ogrn>
            </edo:LegalEntity>
            <edo:Address><edo:ForeignAddress>x</edo:ForeignAddress></edo:Address>
          </edo:Participant>
          <edo:Participant role="Buyer">
            <edo:Inn>9876543210</edo:Inn>
            <edo:LegalEntity>
              <edo:FullName>Buyer</edo:FullName>
              <edo:Ogrn>9876543210123</edo:Ogrn>
            </edo:LegalEntity>
            <edo:Address><edo:ForeignAddress>y</edo:ForeignAddress></edo:Address>
          </edo:Participant>
        </edo:Participants>
      </edo:ElectronicDocument>""")
    val result = XmlDecoder.decodeDocument(xml)
    assert(result.isLeft)
    result match
      case Left(MissingElement(path)) => assert(path.contains("Body"))
      case other => fail(s"Unexpected: $other")
  }
