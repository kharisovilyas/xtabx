package edo

import java.time.{LocalDate, LocalDateTime}
import java.math.BigDecimal as JBigDecimal

// ── Enumerations ──────────────────────────────────────────────────────────────

enum DocumentKind:
  case Invoice, Act, WaybillTorg12, UniversalTransferDocument, Contract

enum ParticipantRole:
  case Seller, Buyer, Carrier, Agent

enum VatRate:
  case Zero, Ten, Twenty, WithoutVat, Ten110, Twenty120

enum SignAlgorithm:
  case GOST256, GOST512

// ── Choice: участник по организационной форме ─────────────────────────────────

sealed trait ParticipantKind
case class LegalEntity(
  fullName:  String,
  shortName: Option[String],
  ogrn:      String
) extends ParticipantKind

case class Individual(
  lastName:   String,
  firstName:  String,
  middleName: Option[String],
  ogrnip:     Option[String]
) extends ParticipantKind

case class ForeignEntity(
  name:        String,
  countryCode: String,
  taxId:       Option[String]
) extends ParticipantKind

// ── Choice: адрес ─────────────────────────────────────────────────────────────

sealed trait Address
case class RussianAddress(
  postalCode: String,
  regionCode: String,
  city:       String,
  street:     Option[String],
  building:   Option[String]
) extends Address

case class ForeignAddress(value: String) extends Address

// ── Участник ──────────────────────────────────────────────────────────────────

case class Contacts(
  phones: List[String],
  emails: List[String]
)

case class Participant(
  role:     ParticipantRole,
  inn:      String,
  kpp:      Option[String],
  kind:     ParticipantKind,
  address:  Address,
  contacts: Option[Contacts]
)

// ── Заголовок ─────────────────────────────────────────────────────────────────

case class CorrectionInfo(
  originalDocumentId: String,
  correctionNumber:   Int,
  reason:             String
)

case class Header(
  documentNumber: String,
  documentDate:   LocalDate,
  documentType:   DocumentKind,
  currency:       String,
  correctionInfo: Option[CorrectionInfo],
  notes:          List[String]
)

// ── Тело документа ────────────────────────────────────────────────────────────

case class Item(
  lineNumber:          Int,
  name:                String,
  unitCode:            String,
  quantity:            BigDecimal,
  price:               BigDecimal,
  amount:              BigDecimal,
  vatRate:             VatRate,
  vatAmount:           Option[BigDecimal],
  customsDeclaration:  Option[String]
)

case class Totals(
  totalWithoutVat: BigDecimal,
  totalVat:        BigDecimal,
  totalWithVat:    BigDecimal
)

case class TaxEntry(
  itemRef: Int,
  rate:    VatRate,
  base:    BigDecimal,
  amount:  BigDecimal
)

case class Body(
  items:  List[Item],
  totals: Totals,
  taxes:  Option[List[TaxEntry]]
)

// ── Подписи ───────────────────────────────────────────────────────────────────

case class Signature(
  algorithm:            SignAlgorithm,
  signerInn:            String,
  certificateThumbprint:String,
  signedAt:             LocalDateTime,
  signatureValue:       String
)

// ── Корневой документ ─────────────────────────────────────────────────────────

case class ElectronicDocument(
  id:           String,
  version:      String,
  schemaUri:    Option[String],
  header:       Header,
  participants: List[Participant],
  body:         Body,
  signatures:   Option[List[Signature]]
)
