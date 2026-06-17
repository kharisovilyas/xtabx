package edo.streaming

import edo.XmlCursor
import fs2.data.xml.{XmlEvent, QName, Attr}

/**
 * XmlCursor поверх `Vector[XmlEvent]`, представляющего один XML-элемент.
 *
 * Событийный вектор приходит из fs2-data-xml:
 *   events(0)    — StartTag для этого элемента
 *   events(last) — соответствующий EndTag
 *   всё посередине — дочерние элементы и текстовые узлы
 *
 * Память: O(один элемент), не O(документ).
 * Для однопространственного документа matching по localName достаточен.
 */
final class EventListCursor(
  private val events:    Vector[XmlEvent],
  private val targetNs:  String            // namespace URI для данной схемы
) extends XmlCursor:

  private val startTag: XmlEvent.StartTag = events.headOption match
    case Some(st: XmlEvent.StartTag) => st
    case _ => throw IllegalArgumentException(
      s"EventListCursor must start with StartTag, got: ${events.headOption}"
    )

  // ── XmlCursor interface ───────────────────────────────────────────────────

  def label: String = startTag.name.local

  // Namespace хранится в targetNs — для однопространственных документов корректно.
  // В многопространственном документе нужно отслеживать xmlns-декларации.
  def namespace: Option[String] = Some(targetNs)

  def attr(name: String): Option[String] =
    startTag.attributes.find(_.name.local == name).map { a =>
      a.value.collect { case XmlEvent.XmlString(s, _) => s }.mkString
    }

  /** Прямой текст: только XmlString-узлы на глубине 0. */
  def text: String =
    val sb = StringBuilder()
    var depth = 0
    var i     = 1  // пропускаем наш StartTag
    val limit = events.length - 1  // пропускаем наш EndTag
    while i < limit do
      events(i) match
        case XmlEvent.XmlString(s, _) if depth == 0 => sb.append(s)
        case _: XmlEvent.StartTag                   => depth += 1
        case _: XmlEvent.EndTag                     => depth -= 1
        case _ => ()
      i += 1
    sb.toString.trim

  def child(ns: String, name: String): Option[XmlCursor] =
    directSubtrees.find(_.label == name)

  def children(ns: String, name: String): List[XmlCursor] =
    directSubtrees.filter(_.label == name)

  // ── subtree extraction ────────────────────────────────────────────────────

  /** Все прямые дочерние элементы как отдельные векторы событий. */
  private def directSubtrees: List[EventListCursor] =
    val buf   = List.newBuilder[EventListCursor]
    var depth = 0
    var start = -1
    var i     = 1
    val limit = events.length - 1
    while i < limit do
      events(i) match
        case _: XmlEvent.StartTag =>
          if depth == 0 then start = i
          depth += 1
        case _: XmlEvent.EndTag =>
          depth -= 1
          if depth == 0 then
            buf += EventListCursor(events.slice(start, i + 1), targetNs)
            start = -1
        case _ => ()
      i += 1
    buf.result()

  override def toString: String = s"EventListCursor(<${startTag.name.local}>)"
