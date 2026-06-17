package edo

import java.time.{LocalDate, LocalDateTime}

/** Декодер: из XmlCursor в A или ошибку. */
trait XmlDecoder[A]:
  def decode(cursor: XmlCursor): Either[XmlError, A]

sealed trait XmlError:
  def message: String
case class MissingElement(path: String)            extends XmlError { def message = s"missing element: $path" }
case class MissingAttribute(name: String)          extends XmlError { def message = s"missing attribute: $name" }
case class BadValue(path: String, detail: String)  extends XmlError { def message = s"bad value at $path: $detail" }
case class UnknownChoice(path: String, variants: Seq[String]) extends XmlError {
  def message = s"unknown choice at $path, expected one of: ${variants.mkString(", ")}"
}

object XmlDecoder:

  private val ns = "urn:edo:test:document:v1"

  // ── helpers ──────────────────────────────────────────────────────────────

  private def require(cursor: XmlCursor, ns: String, name: String): Either[XmlError, XmlCursor] =
    cursor.child(ns, name).toRight(MissingElement(s"${cursor.label}/$name"))

  private def requireAttr(cursor: XmlCursor, name: String): Either[XmlError, String] =
    cursor.attr(name).toRight(MissingAttribute(name))

  private def parseBigDecimal(s: String, path: String): Either[XmlError, BigDecimal] =
    try Right(BigDecimal(s))
    catch case _: NumberFormatException => Left(BadValue(path, s"not a decimal: $s"))

  private def parseInt(s: String, path: String): Either[XmlError, Int] =
    try Right(s.trim.toInt)
    catch case _: NumberFormatException => Left(BadValue(path, s"not an int: $s"))

  // ── enum decoders ─────────────────────────────────────────────────────────

  private def decodeDocumentKind(s: String): Either[XmlError, DocumentKind] = s match
    case "Invoice"                   => Right(DocumentKind.Invoice)
    case "Act"                       => Right(DocumentKind.Act)
    case "WaybillTorg12"             => Right(DocumentKind.WaybillTorg12)
    case "UniversalTransferDocument" => Right(DocumentKind.UniversalTransferDocument)
    case "Contract"                  => Right(DocumentKind.Contract)
    case other                       => Left(BadValue("DocumentType", s"unknown: $other"))

  private def decodeParticipantRole(s: String): Either[XmlError, ParticipantRole] = s match
    case "Seller"  => Right(ParticipantRole.Seller)
    case "Buyer"   => Right(ParticipantRole.Buyer)
    case "Carrier" => Right(ParticipantRole.Carrier)
    case "Agent"   => Right(ParticipantRole.Agent)
    case other     => Left(BadValue("@role", s"unknown: $other"))

  private def decodeVatRate(s: String): Either[XmlError, VatRate] = s match
    case "0"          => Right(VatRate.Zero)
    case "10"         => Right(VatRate.Ten)
    case "20"         => Right(VatRate.Twenty)
    case "WithoutVat" => Right(VatRate.WithoutVat)
    case "10/110"     => Right(VatRate.Ten110)
    case "20/120"     => Right(VatRate.Twenty120)
    case other        => Left(BadValue("VatRate", s"unknown: $other"))

  private def decodeSignAlgorithm(s: String): Either[XmlError, SignAlgorithm] = s match
    case "GOST R 34.10-2012 256" => Right(SignAlgorithm.GOST256)
    case "GOST R 34.10-2012 512" => Right(SignAlgorithm.GOST512)
    case other                   => Left(BadValue("@algorithm", s"unknown: $other"))

  // ── node decoders ─────────────────────────────────────────────────────────

  private def decodeLegalEntity(c: XmlCursor): Either[XmlError, LegalEntity] =
    for
      fullName <- require(c, ns, "FullName").map(_.text)
      ogrn     <- require(c, ns, "Ogrn").map(_.text)
    yield LegalEntity(fullName, c.child(ns, "ShortName").map(_.text), ogrn)

  private def decodeIndividual(c: XmlCursor): Either[XmlError, Individual] =
    for
      last  <- require(c, ns, "LastName").map(_.text)
      first <- require(c, ns, "FirstName").map(_.text)
    yield Individual(last, first,
      c.child(ns, "MiddleName").map(_.text),
      c.child(ns, "Ogrnip").map(_.text))

  private def decodeForeignEntity(c: XmlCursor): Either[XmlError, ForeignEntity] =
    for
      name    <- require(c, ns, "Name").map(_.text)
      country <- require(c, ns, "CountryCode").map(_.text)
    yield ForeignEntity(name, country, c.child(ns, "TaxId").map(_.text))

  private def decodeParticipantKind(c: XmlCursor): Either[XmlError, ParticipantKind] =
    c.child(ns, "LegalEntity").map(decodeLegalEntity)
      .orElse(c.child(ns, "Individual").map(decodeIndividual))
      .orElse(c.child(ns, "ForeignEntity").map(decodeForeignEntity))
      .getOrElse(Left(UnknownChoice("Participant", Seq("LegalEntity", "Individual", "ForeignEntity"))))

  private def decodeRussianAddress(c: XmlCursor): Either[XmlError, RussianAddress] =
    for
      postal <- require(c, ns, "PostalCode").map(_.text)
      region <- require(c, ns, "RegionCode").map(_.text)
      city   <- require(c, ns, "City").map(_.text)
    yield RussianAddress(postal, region, city,
      c.child(ns, "Street").map(_.text),
      c.child(ns, "Building").map(_.text))

  private def decodeAddress(c: XmlCursor): Either[XmlError, Address] =
    c.child(ns, "RussianAddress").map(decodeRussianAddress)
      .orElse(c.child(ns, "ForeignAddress").map(fa => Right(ForeignAddress(fa.text))))
      .getOrElse(Left(UnknownChoice("Address", Seq("RussianAddress", "ForeignAddress"))))

  private def decodeContacts(c: XmlCursor): Contacts =
    Contacts(c.children(ns, "Phone").map(_.text), c.children(ns, "Email").map(_.text))

  private def decodeParticipant(c: XmlCursor): Either[XmlError, Participant] =
    for
      roleStr <- requireAttr(c, "role")
      role    <- decodeParticipantRole(roleStr)
      inn     <- require(c, ns, "Inn").map(_.text)
      kind    <- decodeParticipantKind(c)
      addrC   <- require(c, ns, "Address")
      addr    <- decodeAddress(addrC)
    yield Participant(role, inn, c.child(ns, "Kpp").map(_.text), kind, addr,
      c.child(ns, "Contacts").map(decodeContacts))

  private def decodeCorrectionInfo(c: XmlCursor): Either[XmlError, CorrectionInfo] =
    for
      origId <- require(c, ns, "OriginalDocumentId").map(_.text)
      numStr <- require(c, ns, "CorrectionNumber").map(_.text)
      num    <- parseInt(numStr, "CorrectionInfo/CorrectionNumber")
      reason <- require(c, ns, "Reason").map(_.text)
    yield CorrectionInfo(origId, num, reason)

  private def decodeHeader(c: XmlCursor): Either[XmlError, Header] =
    for
      docNum   <- require(c, ns, "DocumentNumber").map(_.text)
      dateStr  <- require(c, ns, "DocumentDate").map(_.text)
      typeStr  <- require(c, ns, "DocumentType").map(_.text)
      docType  <- decodeDocumentKind(typeStr)
      currency <- require(c, ns, "Currency").map(_.text)
      corrInfo <- c.child(ns, "CorrectionInfo") match
                    case None    => Right(None)
                    case Some(ci) => decodeCorrectionInfo(ci).map(Some(_))
    yield Header(docNum, LocalDate.parse(dateStr), docType, currency, corrInfo,
      c.children(ns, "Notes").map(_.text))

  def decodeItem(c: XmlCursor): Either[XmlError, Item] =
    for
      lineNumStr <- requireAttr(c, "lineNumber")
      lineNum    <- parseInt(lineNumStr, "@lineNumber")
      name       <- require(c, ns, "Name").map(_.text)
      unitCode   <- require(c, ns, "UnitCode").map(_.text)
      qtyStr     <- require(c, ns, "Quantity").map(_.text)
      qty        <- parseBigDecimal(qtyStr, "Item/Quantity")
      priceStr   <- require(c, ns, "Price").map(_.text)
      price      <- parseBigDecimal(priceStr, "Item/Price")
      amtStr     <- require(c, ns, "Amount").map(_.text)
      amt        <- parseBigDecimal(amtStr, "Item/Amount")
      vatRateStr <- require(c, ns, "VatRate").map(_.text)
      vatRate    <- decodeVatRate(vatRateStr)
      vatAmt     <- c.child(ns, "VatAmount") match
                      case None    => Right(None)
                      case Some(v) => parseBigDecimal(v.text, "Item/VatAmount").map(Some(_))
    yield Item(lineNum, name, unitCode, qty, price, amt, vatRate, vatAmt,
      c.child(ns, "CustomsDeclaration").map(_.text))

  private def decodeTotals(c: XmlCursor): Either[XmlError, Totals] =
    for
      wo  <- require(c, ns, "TotalWithoutVat").flatMap(v => parseBigDecimal(v.text, "TotalWithoutVat"))
      vat <- require(c, ns, "TotalVat").flatMap(v => parseBigDecimal(v.text, "TotalVat"))
      w   <- require(c, ns, "TotalWithVat").flatMap(v => parseBigDecimal(v.text, "TotalWithVat"))
    yield Totals(wo, vat, w)

  private def decodeTaxEntry(c: XmlCursor): Either[XmlError, TaxEntry] =
    for
      refStr  <- requireAttr(c, "itemRef")
      ref     <- parseInt(refStr, "@itemRef")
      rateStr <- require(c, ns, "Rate").map(_.text)
      rate    <- decodeVatRate(rateStr)
      baseStr <- require(c, ns, "Base").map(_.text)
      base    <- parseBigDecimal(baseStr, "TaxEntry/Base")
      amtStr  <- require(c, ns, "Amount").map(_.text)
      amt     <- parseBigDecimal(amtStr, "TaxEntry/Amount")
    yield TaxEntry(ref, rate, base, amt)

  private def decodeBody(c: XmlCursor): Either[XmlError, Body] =
    for
      itemsC <- require(c, ns, "Items")
      items  <- itemsC.children(ns, "Item").foldLeft[Either[XmlError, List[Item]]](Right(Nil)) {
                  (acc, ic) => acc.flatMap(lst => decodeItem(ic).map(lst :+ _))
                }
      totalsC <- require(c, ns, "Totals")
      totals  <- decodeTotals(totalsC)
      taxes   <- c.child(ns, "Taxes") match
                   case None => Right(None)
                   case Some(tc) =>
                     tc.children(ns, "TaxEntry")
                       .foldLeft[Either[XmlError, List[TaxEntry]]](Right(Nil)) {
                         (acc, te) => acc.flatMap(lst => decodeTaxEntry(te).map(lst :+ _))
                       }.map(Some(_))
    yield Body(items, totals, taxes)

  private def decodeSignature(c: XmlCursor): Either[XmlError, Signature] =
    for
      algStr <- requireAttr(c, "algorithm")
      alg    <- decodeSignAlgorithm(algStr)
      inn    <- require(c, ns, "SignerInn").map(_.text)
      thumb  <- require(c, ns, "CertificateThumbprint").map(_.text)
      atStr  <- require(c, ns, "SignedAt").map(_.text)
      sigVal <- require(c, ns, "SignatureValue").map(_.text)
    yield Signature(alg, inn, thumb, LocalDateTime.parse(atStr), sigVal)

  // ── root decoder ──────────────────────────────────────────────────────────

  /** Удобный фасад для тестов и однодокументной обработки. */
  def decodeDocument(node: scala.xml.Node): Either[XmlError, ElectronicDocument] =
    decodeDocument(NodeCursor(node))

  def decodeDocument(cursor: XmlCursor): Either[XmlError, ElectronicDocument] =
    for
      id      <- requireAttr(cursor, "id")
      ver     <- requireAttr(cursor, "version")
      headerC <- require(cursor, ns, "Header")
      header  <- decodeHeader(headerC)
      particsC <- require(cursor, ns, "Participants")
      partics <- particsC.children(ns, "Participant")
                   .foldLeft[Either[XmlError, List[Participant]]](Right(Nil)) {
                     (acc, p) => acc.flatMap(lst => decodeParticipant(p).map(lst :+ _))
                   }
      bodyC   <- require(cursor, ns, "Body")
      body    <- decodeBody(bodyC)
      sigs    <- cursor.child(ns, "Signatures") match
                   case None => Right(None)
                   case Some(sc) =>
                     sc.children(ns, "Signature")
                       .foldLeft[Either[XmlError, List[Signature]]](Right(Nil)) {
                         (acc, s) => acc.flatMap(lst => decodeSignature(s).map(lst :+ _))
                       }.map(Some(_))
    yield ElectronicDocument(id, ver, cursor.attr("schemaUri"), header, partics, body, sigs)
