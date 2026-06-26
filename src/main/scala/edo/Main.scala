package edo

import cats.effect.{IO, IOApp}
import fs2.Stream
import fs2.io.file.{Files, Path}
import scala.xml.XML
import java.nio.file.{Paths, Files as NioFiles}

import edo.*
import edo.derivation.DerivedInstances.given
import edo.derivation.XmlDecoderInstances.given
import edo.streaming.XmlPipeline
import edo.streaming.XmlPipeline.partition

/**
 * Точка входа: `sbt start`
 *
 * По умолчанию читает data/sample_full.xml из директории проекта.
 * Можно передать путь к своему файлу:
 *   sbt "start data/my_invoice.xml"
 *   sbt "start /absolute/path/to/file.xml"
 */
object Main extends IOApp.Simple:

  private val ns = "urn:edo:test:document:v1"

  given XmlDecoder[Item] with
    def decode(c: XmlCursor): Either[XmlError, Item] = XmlDecoder.decodeItem(c)

  def run: IO[Unit] =
    for
      args <- IO(sys.props.get("xtabx.file").orElse(Option.empty))
      _    <- processFile(args)
    yield ()

  /** Корень проекта — передаётся из build.sbt через -Dxtabx.home. */
  private val projectHome: java.nio.file.Path =
    sys.props.get("xtabx.home")
      .map(Paths.get(_))
      .getOrElse(Paths.get("."))

  private def resolve(rel: String): java.nio.file.Path = projectHome.resolve(rel)

  private def processFile(explicit: Option[String]): IO[Unit] =
    IO(detectInputFile(explicit)).flatMap {
      case None =>
        IO.println(s"Использование: sbt start                          (читает data/sample_full.xml)") >>
        IO.println(s"               sbt \"start data/invoice.xml\"        (читает указанный файл)")     >>
        IO.raiseError(new RuntimeException(s"Файл не найден. Положите XML в $projectHome/data/"))

      case Some(path) =>
        IO.println(s"Файл: $path") >> IO.println("") >>
        runAll(Path(path))
    }

  private def detectInputFile(explicit: Option[String]): Option[String] =
    // явно переданный путь имеет приоритет
    explicit.map(resolve(_)).filter(NioFiles.exists(_)).map(_.toString).orElse {
      // дефолтные имена
      val preferred = List("data/sample_full.xml", "data/sample.xml", "data/invoice.xml")
      preferred.map(resolve(_)).find(NioFiles.exists(_)).map(_.toString).orElse {
        // любой первый .xml в data/
        val dataDir = resolve("data")
        Option(dataDir.toFile.listFiles())
          .getOrElse(Array.empty[java.io.File])
          .find(_.getName.endsWith(".xml"))
          .map(_.getAbsolutePath)
      }
    }

  // ── 1. Полный документ ────────────────────────────────────────────────────

  private def showFullDoc(path: Path): IO[Unit] =
    IO {
      val xml = XML.load(path.toString)
      XmlDecoder.decodeDocument(xml)
    }.flatMap {
      case Left(err) => IO.println(s"  ✗ ${err.message}")
      case Right(doc) =>
        IO {
          println(s"  Документ : ${doc.id}")
          println(s"  Тип      : ${doc.header.documentType}  №${doc.header.documentNumber}  от ${doc.header.documentDate}")
          println(s"  Валюта   : ${doc.header.currency}")
          if doc.header.notes.nonEmpty then
            println(s"  Примечания: ${doc.header.notes.mkString("; ")}")

          println(s"\n  Участники:")
          doc.participants.foreach { p =>
            val who = p.kind match
              case le: LegalEntity   => s"ЮЛ «${le.shortName.getOrElse(le.fullName)}»"
              case i:  Individual    => s"ФЛ ${i.lastName} ${i.firstName}"
              case fe: ForeignEntity => s"ИО ${fe.name} (${fe.countryCode})"
            println(s"    ${p.role.toString.padTo(8,' ')} ИНН ${p.inn}  $who")
          }

          println(s"\n  Позиции (${doc.body.items.length}):")
          doc.body.items.foreach { i =>
            val vat = i.vatAmount.fold("без НДС")(v => s"НДС $v")
            println(s"    ${i.lineNumber}. ${i.name.take(36).padTo(37,' ')} ${i.quantity} × ${i.price} = ${i.amount}  ($vat)")
          }

          val t = doc.body.totals
          println(s"\n  Без НДС  : ${t.totalWithoutVat}")
          println(s"  НДС      : ${t.totalVat}")
          println(s"  С НДС    : ${t.totalWithVat}")

          doc.signatures.foreach { sigs =>
            println(s"\n  Подписи (${sigs.length}):")
            sigs.foreach(s => println(s"    ${s.algorithm}  ИНН ${s.signerInn}  ${s.signedAt}"))
          }
        }
    }

  // ── 2. Streaming ──────────────────────────────────────────────────────────

  private def showStreamingItems(path: Path): IO[Unit] =
    val (valid, dead) =
      Files[IO].readAll(path)
        .through(XmlPipeline.decode[IO, Item]("Item", ns))
        .partition

    for
      items <- valid.compile.toList
      errs  <- dead.compile.toList
    yield
      println(s"  Найдено позиций: ${items.length}  /  ошибок: ${errs.length}")
      items.foreach { i =>
        val vat = i.vatAmount.fold("без НДС")(v => s"НДС $v")
        println(s"    ${i.lineNumber}. ${i.name.take(36).padTo(37,' ')} ${i.amount}  ($vat)")
      }
      if errs.nonEmpty then
        println(s"\n  Dead-letter:")
        errs.foreach(e => println(s"    ✗ ${e.message}"))

  // ── Общий запуск ─────────────────────────────────────────────────────────

  private def runAll(path: Path): IO[Unit] =
    IO.println("════════════════════════════════════════════════") >>
    IO.println("  1. DOM-декодер — полный документ") >>
    IO.println("════════════════════════════════════════════════") >>
    showFullDoc(path) >>
    IO.println("") >>
    IO.println("════════════════════════════════════════════════") >>
    IO.println("  2. Streaming — позиции Item (O(запись) память)") >>
    IO.println("════════════════════════════════════════════════") >>
    showStreamingItems(path) >>
    IO.println("")
