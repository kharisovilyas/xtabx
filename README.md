# xtabx

**XSD-driven ADT XML decoder** — библиотека для декодирования XML-документов в типизированные Scala 3 ADT, с генерацией схемы из XSD во время сборки, Mirror-деривацией декодеров и streaming-обработкой через fs2-data-xml.

```
bytes ──► events ──► subtree split ──► EventListCursor ──► XmlDecoder[A] ──► Either[XmlError, A]
         O(1)                O(record)                                         dead-letter safe
```

---

## Возможности

| Фаза | Что делает |
|------|-----------|
| **Ручной декодер** | `XmlDecoder[A]` поверх `XmlCursor` — явное сопоставление элементов, атрибутов, вложенных типов |
| **Генерация схемы** | sbt source generator читает `.xsd` → создаёт `EdoSchemaDescriptor` с описанием всех типов, элементов и атрибутов |
| **Mirror-деривация** | `XmlDecoderDerivation.derived[A]` выводит декодер для любого `case class` или `sealed trait` через `scala.deriving.Mirror` |
| **Compile-time проверка** | `SchemaCheck.assertConforms[A]()` — quoted macro, сверяет поля ADT с XSD прямо при компиляции |
| **Streaming** | `XmlPipeline.decode[F, A](elementName)` — pull-based `Pipe[F, Byte, Either[XmlError, A]]`, O(запись) памяти |

---

## Структура

```
src/main/scala/edo/
├── model.scala                       # ADT: ElectronicDocument, Header, Item, …
├── XmlCursor.scala                   # Интерфейс курсора + NodeCursor (DOM)
├── XmlDecoder.scala                  # Ручной декодер всего документа
├── schema/
│   └── SchemaTypes.scala             # ContentKind, ElementInfo, ComplexTypeInfo
├── derivation/
│   ├── annotations.scala             # @xmlAttr, @xmlText, @xmlInline, @xmlName
│   ├── XmlDecoderDerivation.scala    # Mirror-деривация (Product + Sum)
│   ├── FieldDecoder.scala            # Обёртка Option/List/required для полей
│   ├── DerivedInstances.scala        # Конкретные given XmlDecoder[…]
│   └── SchemaCheck.scala             # assertConforms[A]() — macro
└── streaming/
    ├── EventListCursor.scala         # XmlCursor поверх Vector[XmlEvent]
    └── XmlPipeline.scala             # fs2 Pipe + pull-based subtree splitter

project/
└── XsdDescriptorGen.scala            # sbt source generator: XSD → EdoSchemaDescriptor.scala
```

---

## Зависимости

```scala
"org.scala-lang.modules" %% "scala-xml"     % "2.3.0"
"co.fs2"                 %% "fs2-core"       % "3.11.0"
"org.typelevel"          %% "cats-effect"    % "3.5.4"
"org.gnieh"              %% "fs2-data-xml"   % "1.11.2"
"co.fs2"                 %% "fs2-io"         % "3.11.0"  // тесты
```

---

## Быстрый старт

### 1. Полный документ (DOM-декодер)

```scala
import edo.*
import scala.xml.XML

val xml  = XML.load("invoice.xml")
val doc  = XmlDecoder.decodeDocument(xml)

doc match
  case Right(d) =>
    println(s"Документ ${d.id}: ${d.header.documentType}")
    println(s"Продавец: ${d.participants.find(_.role == ParticipantRole.Seller).map(_.inn)}")
    d.body.items.foreach(i => println(s"  ${i.lineNumber}. ${i.name}  ${i.amount} RUB"))
  case Left(err) =>
    println(s"Ошибка: ${err.message}")
```

### 2. Mirror-деривация

```scala
import edo.derivation.{XmlDecoderDerivation, DerivedInstances, XmlNs}
import DerivedInstances.given

// Decoder выведен автоматически из полей case class через Mirror
val decoder = summon[XmlDecoder[LegalEntity]]
val cursor  = NodeCursor(XML.loadString("""
  <LegalEntity xmlns="urn:edo:test:document:v1">
    <FullName>ООО «Ромашка»</FullName>
    <Ogrn>1234567890123</Ogrn>
  </LegalEntity>
"""))
decoder.decode(cursor) // Right(LegalEntity("ООО «Ромашка»", None, "1234567890123"))
```

### 3. Compile-time проверка соответствия XSD

```scala
import edo.derivation.SchemaCheck

// Проверяется во время компиляции: поля case class совпадают с XSD-типом
SchemaCheck.assertConforms[LegalEntity]()   // OK
SchemaCheck.assertConforms[Header]()         // OK
SchemaCheck.assertConforms[VatRate]()        // OK — все варианты enum есть в XSD

// Если переименовать поле или добавить лишнее — compile error с указанием XSD-имени
```

### 4. Streaming — O(запись) память

```scala
import cats.effect.{IO, IOApp}
import fs2.Stream
import fs2.io.file.{Files, Path}
import edo.*
import edo.streaming.XmlPipeline
import edo.streaming.XmlPipeline.partition

object InvoiceProcessor extends IOApp.Simple:

  // XmlDecoder для Item — используем готовый из XmlDecoder
  given XmlDecoder[Item] with
    def decode(c: XmlCursor): Either[XmlError, Item] = XmlDecoder.decodeItem(c)

  def run: IO[Unit] =
    Files[IO].readAll(Path("big_invoice.xml"))
      .through(XmlPipeline.decode[IO, Item]("Item"))    // ← только имя элемента
      .through(logProgress)
      .compile.drain

  def logProgress: fs2.Pipe[IO, Either[XmlError, Item], Either[XmlError, Item]] =
    _.evalTap {
      case Right(item) => IO.println(s"✓ Позиция ${item.lineNumber}: ${item.name}")
      case Left(err)   => IO.println(s"✗ Ошибка: ${err.message}")
    }
```

### 5. Dead-letter разделение

```scala
val (valid, errors) =
  Files[IO].readAll(Path("big_invoice.xml"))
    .through(XmlPipeline.decode[IO, Item]("Item"))
    .partition  // расширение из XmlPipeline

for
  items  <- valid.compile.toList
  errs   <- errors.compile.toList
yield
  println(s"Успешно: ${items.length}, ошибок: ${errs.length}")
```

---

## Модель документа

```
ElectronicDocument
├── id, version, schemaUri
├── Header
│   ├── documentNumber, documentDate, documentType: DocumentKind
│   ├── currency, notes: List[String]
│   └── correctionInfo: Option[CorrectionInfo]
├── participants: List[Participant]
│   ├── role: ParticipantRole  (Seller | Buyer | Carrier | Agent)
│   ├── inn, kpp: Option[String]
│   ├── kind: ParticipantKind  (LegalEntity | Individual | ForeignEntity)  ← sealed trait
│   ├── address: Address       (RussianAddress | ForeignAddress)           ← sealed trait
│   └── contacts: Option[Contacts]
├── Body
│   ├── items: List[Item]
│   │   └── lineNumber, name, unitCode, quantity, price, amount
│   │       vatRate: VatRate, vatAmount: Option[BigDecimal]
│   ├── totals: Totals
│   └── taxes: Option[List[TaxEntry]]
└── signatures: Option[List[Signature]]
```

---

## Тесты

```
sbt test
```

```
65 тестов:
  XmlDecoderTest          — 16  (полный и минимальный документ, все ошибки)
  XmlDecoderDerivationTest — 20  (Mirror-деривация + EdoSchemaDescriptor)
  SchemaCheckTest          — 20  (compile-time macro, позитивные и негативные кейсы)
  StreamingTest            —  9  (fs2 pipeline + EventListCursor unit tests)
```

---

## Архитектурные решения

**Почему `XmlCursor` абстракция?**  
Один и тот же `XmlDecoder[A]` работает поверх `NodeCursor` (DOM, тесты) и `EventListCursor` (fs2 event stream, продакшн). Переключение без изменения кода декодера.

**Почему pull-based сплиттер, а не `xfilter.raw`?**  
`PartiallyAppliedFilter.raw` возвращает `Stream[F, Stream[F, XmlEvent]]` с внутренними стримами на общем источнике — `evalMap(_.compile.toVector)` создаёт дедлок. Собственный `subtreeSplitter` через `fs2.Pull` гарантирует строго последовательное потребление без блокировок.

**XSD → compile-time проверка:**  
sbt source generator при каждой сборке читает `.xsd` и генерирует `EdoSchemaDescriptor.scala`. Quoted macro `assertConforms[A]()` читает этот объект через `Expr` и сравнивает имена полей, кардинальность (`Option`/`List`/required), варианты enum — до запуска программы.

---

## Лицензия

MIT
