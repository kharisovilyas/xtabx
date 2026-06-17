package edo

import munit.FunSuite
import scala.xml.{XML, Node}
import edo.derivation.{DerivedInstances, XmlDecoderInstances, XmlNs}
import DerivedInstances.given
import XmlDecoderInstances.given

// helper: scala.xml.Node → NodeCursor для existing tests
extension [A](dec: XmlDecoder[A])
  def decodeNode(node: Node): Either[XmlError, A] = dec.decode(NodeCursor(node))

/**
 * Тесты Mirror-based деривации.
 * Загружает тот же sample_full.xml, извлекает поддеревья и декодирует
 * с помощью derived XmlDecoder[A] (не вручную написанного).
 */
class XmlDecoderDerivationTest extends FunSuite:

  private def loadXml(resource: String): scala.xml.Elem =
    val stream = getClass.getClassLoader.getResourceAsStream(resource)
    assert(stream != null, s"resource not found: $resource")
    XML.load(stream)

  private val ns = "urn:edo:test:document:v1"

  private def firstNs(root: Node, label: String): Node =
    root.descendant.find(n => n.label == label && n.namespace == ns)
      .getOrElse(fail(s"element <$label> not found"))

  // ── LegalEntity ──────────────────────────────────────────────────────────

  test("derived LegalEntity — all fields") {
    val root = loadXml("sample_full.xml")
    val node = firstNs(root, "LegalEntity")
    val Right(le) = summon[XmlDecoder[LegalEntity]].decodeNode(node): @unchecked
    assertEquals(le.fullName, "ООО «Ромашка»")
    assertEquals(le.shortName, Some("ООО Ромашка"))
    assertEquals(le.ogrn, "1234567890123")
  }

  test("derived LegalEntity — optional ShortName absent") {
    val xml = XML.loadString(
      """<LegalEntity xmlns="urn:edo:test:document:v1">
          <FullName>Test</FullName><Ogrn>1234567890123</Ogrn>
        </LegalEntity>""")
    val Right(le) = summon[XmlDecoder[LegalEntity]].decodeNode(xml): @unchecked
    assertEquals(le.shortName, None)
  }

  // ── Individual ───────────────────────────────────────────────────────────

  test("derived Individual — full") {
    val root = loadXml("sample_full.xml")
    val node = firstNs(root, "Individual")
    val Right(ind) = summon[XmlDecoder[Individual]].decodeNode(node): @unchecked
    assertEquals(ind.lastName, "Иванов")
    assertEquals(ind.firstName, "Иван")
    assertEquals(ind.middleName, Some("Иванович"))
    assertEquals(ind.ogrnip, None)
  }

  // ── ForeignEntity ─────────────────────────────────────────────────────────

  test("derived ForeignEntity — from minimal xml") {
    val root = loadXml("sample_minimal.xml")
    val node = firstNs(root, "ForeignEntity")
    val Right(fe) = summon[XmlDecoder[ForeignEntity]].decodeNode(node): @unchecked
    assertEquals(fe.name, "Acme Corp")
    assertEquals(fe.countryCode, "US")
    assertEquals(fe.taxId, None)
  }

  test("derived ForeignEntity — with optional TaxId") {
    val xml = XML.loadString(
      """<ForeignEntity xmlns="urn:edo:test:document:v1">
          <Name>ACME</Name><CountryCode>DE</CountryCode><TaxId>DE999</TaxId>
        </ForeignEntity>""")
    val Right(fe) = summon[XmlDecoder[ForeignEntity]].decodeNode(xml): @unchecked
    assertEquals(fe.taxId, Some("DE999"))
  }

  // ── RussianAddress ────────────────────────────────────────────────────────

  test("derived RussianAddress — full") {
    val root = loadXml("sample_full.xml")
    val node = firstNs(root, "RussianAddress")
    val Right(ra) = summon[XmlDecoder[RussianAddress]].decodeNode(node): @unchecked
    assertEquals(ra.postalCode, "115093")
    assertEquals(ra.regionCode, "77")
    assertEquals(ra.city, "Москва")
    assertEquals(ra.street, Some("ул. Садовая"))
    assertEquals(ra.building, Some("д. 5, стр. 1"))
  }

  test("derived RussianAddress — without optional fields") {
    val root = loadXml("sample_minimal.xml")
    val nodes = root.descendant.filter(n => n.label == "RussianAddress" && n.namespace == ns)
    // minimal has one RussianAddress without Street/Building
    val node = nodes.last
    val Right(ra) = summon[XmlDecoder[RussianAddress]].decodeNode(node): @unchecked
    assertEquals(ra.street, None)
    assertEquals(ra.building, None)
  }

  // ── Address (derivedChoice) ───────────────────────────────────────────────

  test("derived Address — RussianAddress variant") {
    val root = loadXml("sample_full.xml")
    val addrNode = firstNs(root, "Address")
    val Right(addr) = summon[XmlDecoder[Address]].decodeNode(addrNode): @unchecked
    addr match
      case ra: RussianAddress => assertEquals(ra.postalCode, "115093")
      case other              => fail(s"expected RussianAddress, got $other")
  }

  test("derived Address — ForeignAddress variant") {
    val root = loadXml("sample_minimal.xml")
    // First participant has ForeignAddress
    val addrNodes = root.descendant.filter(n => n.label == "Address" && n.namespace == ns)
    val addrNode  = addrNodes.head
    val Right(addr) = summon[XmlDecoder[Address]].decodeNode(addrNode): @unchecked
    addr match
      case fa: ForeignAddress => assertEquals(fa.value, "123 Main St, New York, USA")
      case other              => fail(s"expected ForeignAddress, got $other")
  }

  test("derived Address — UnknownChoice when no variant present") {
    val xml = XML.loadString("""<Address xmlns="urn:edo:test:document:v1"/>""")
    val result = summon[XmlDecoder[Address]].decodeNode(xml)
    assert(result.isLeft)
    result match
      case Left(UnknownChoice(parent, _)) => assertEquals(parent, "Address")
      case other => fail(s"unexpected: $other")
  }

  // ── CorrectionInfo ────────────────────────────────────────────────────────

  test("derived CorrectionInfo") {
    val xml = XML.loadString(
      """<CorrectionInfo xmlns="urn:edo:test:document:v1">
          <OriginalDocumentId>a1b2c3d4-e5f6-7890-abcd-ef1234567890</OriginalDocumentId>
          <CorrectionNumber>1</CorrectionNumber>
          <Reason>Исправление суммы</Reason>
        </CorrectionInfo>""")
    val Right(ci) = summon[XmlDecoder[CorrectionInfo]].decodeNode(xml): @unchecked
    assertEquals(ci.originalDocumentId, "a1b2c3d4-e5f6-7890-abcd-ef1234567890")
    assertEquals(ci.correctionNumber, 1)
    assertEquals(ci.reason, "Исправление суммы")
  }

  // ── Totals ────────────────────────────────────────────────────────────────

  test("derived Totals") {
    val root = loadXml("sample_full.xml")
    val node = firstNs(root, "Totals")
    val Right(t) = summon[XmlDecoder[Totals]].decodeNode(node): @unchecked
    assertEquals(t.totalWithoutVat, BigDecimal("150000.00"))
    assertEquals(t.totalVat, BigDecimal("30000.00"))
    assertEquals(t.totalWithVat, BigDecimal("180000.00"))
  }

  test("derived Totals — missing element returns MissingElement") {
    val xml = XML.loadString(
      """<Totals xmlns="urn:edo:test:document:v1">
          <TotalWithoutVat>100.00</TotalWithoutVat>
          <TotalVat>20.00</TotalVat>
        </Totals>""")
    val result = summon[XmlDecoder[Totals]].decodeNode(xml)
    assert(result.isLeft)
    result match
      case Left(MissingElement(path)) => assert(path.contains("TotalWithVat"))
      case other => fail(s"unexpected: $other")
  }

  // ── Schema descriptor sanity ──────────────────────────────────────────────

  test("EdoSchemaDescriptor — targetNamespace") {
    assertEquals(
      edo.schema.EdoSchemaDescriptor.targetNamespace,
      "urn:edo:test:document:v1"
    )
  }

  test("EdoSchemaDescriptor — DocumentKindType enum values") {
    val values = edo.schema.EdoSchemaDescriptor.enumerations("DocumentKindType")
    assertEquals(values, List("Invoice", "Act", "WaybillTorg12", "UniversalTransferDocument", "Contract"))
  }

  test("EdoSchemaDescriptor — VatRateType enum values") {
    val values = edo.schema.EdoSchemaDescriptor.enumerations("VatRateType")
    assertEquals(values, List("0", "10", "20", "WithoutVat", "10/110", "20/120"))
  }

  test("EdoSchemaDescriptor — HeaderType has required DocumentNumber") {
    import edo.schema.EdoSchemaDescriptor
    val ht = EdoSchemaDescriptor.complexTypes("HeaderType")
    val docNum = ht.elements.find(_.name == "DocumentNumber")
    assert(docNum.isDefined)
    assert(docNum.get.isRequired)
  }

  test("EdoSchemaDescriptor — HeaderType Notes is repeated") {
    import edo.schema.EdoSchemaDescriptor
    val ht = EdoSchemaDescriptor.complexTypes("HeaderType")
    val notes = ht.elements.find(_.name == "Notes")
    assert(notes.isDefined)
    assert(notes.get.isRepeated)
    assertEquals(notes.get.minOccurs, 0)
  }

  test("EdoSchemaDescriptor — AddressType is Choice") {
    import edo.schema.{EdoSchemaDescriptor, ContentKind}
    val at = EdoSchemaDescriptor.complexTypes("AddressType")
    assertEquals(at.contentKind, ContentKind.Choice)
    assert(at.elements.exists(_.name == "RussianAddress"))
    assert(at.elements.exists(_.name == "ForeignAddress"))
  }

  test("EdoSchemaDescriptor — ElectronicDocumentType has required id attribute") {
    import edo.schema.EdoSchemaDescriptor
    val edt = EdoSchemaDescriptor.complexTypes("ElectronicDocumentType")
    val idAttr = edt.attributes.find(_.name == "id")
    assert(idAttr.isDefined)
    assert(idAttr.get.required)
  }
