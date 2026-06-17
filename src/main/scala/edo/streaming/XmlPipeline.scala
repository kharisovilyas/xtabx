package edo.streaming

import cats.effect.Concurrent
import fs2.{Pipe, Pull, Stream}
import fs2.data.xml.*
import edo.{XmlDecoder, XmlError, XmlCursor}

/**
 * fs2-пайплайн: Stream[F, Byte] → Stream[F, Either[XmlError, A]]
 *
 * Архитектура:
 *   bytes → UTF-8 chars → XML events (fs2-data-xml, pull-based, O(1) на документ)
 *         → subtreeSplitter — выделяем поддеревья по имени элемента (O(запись))
 *         → EventListCursor → XmlDecoder[A]
 *         → Either[XmlError, A]
 *
 * Dead-letter: ошибки декодирования не бросают исключений —
 * они остаются как Left в потоке, можно разделить на valid/invalid.
 */
object XmlPipeline:

  /**
   * Pull-based сплиттер: разрезает поток событий на поддеревья по local-name.
   * O(запись) память — не буферизует весь документ.
   */
  private def subtreeSplitter[F[_]](elementName: String): Pipe[F, XmlEvent, Vector[XmlEvent]] =
    def go(
      in:    Stream[F, XmlEvent],
      depth: Int,
      buf:   Vector[XmlEvent]
    ): Pull[F, Vector[XmlEvent], Unit] =
      in.pull.uncons1.flatMap {
        case None => Pull.done
        case Some((ev, rest)) =>
          ev match
            case st: XmlEvent.StartTag if depth == 0 && st.name.local == elementName =>
              go(rest, 1, Vector(st))
            case st: XmlEvent.StartTag if depth > 0 =>
              go(rest, depth + 1, buf :+ st)
            case et: XmlEvent.EndTag if depth == 1 && et.name.local == elementName =>
              Pull.output1(buf :+ et) >> go(rest, 0, Vector.empty)
            case et: XmlEvent.EndTag if depth > 1 =>
              go(rest, depth - 1, buf :+ et)
            case ev if depth > 0 =>
              go(rest, depth, buf :+ ev)
            case _ =>
              go(rest, depth, buf)
      }
    in => go(in, 0, Vector.empty).stream

  /**
   * Декодирует все вхождения элемента с именем `elementName`.
   *
   * @param elementName  local-name XML-элемента (без namespace-префикса)
   * @param targetNs     namespace URI схемы
   */
  def decode[F[_]: Concurrent, A](
    elementName: String,
    targetNs:    String = "urn:edo:test:document:v1"
  )(using decoder: XmlDecoder[A]): Pipe[F, Byte, Either[XmlError, A]] =
    input =>
      input
        .through(fs2.text.utf8.decode)
        .through(events[F, String])
        .through(normalize[F])
        .through(subtreeSplitter(elementName))
        .map(evts => decoder.decode(EventListCursor(evts, targetNs)))

  /**
   * Разбивает поток на две ветки: успешно декодированные и ошибки.
   */
  extension [F[_], A](stream: Stream[F, Either[XmlError, A]])
    def partition: (Stream[F, A], Stream[F, XmlError]) =
      (stream.collect { case Right(a) => a },
       stream.collect { case Left(e)  => e })
