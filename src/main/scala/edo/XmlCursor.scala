package edo

/**
 * Абстракция над XML-узлом.
 *
 * Две реализации:
 *   - NodeCursor   — обёртка над scala.xml.Node (тесты, малые документы)
 *   - EventListCursor — курсор над Vector[XmlEvent] из fs2-data-xml
 *                       (потоковая обработка, O(запись) memory)
 *
 * Декодеры и деривация работают только с этим интерфейсом —
 * бэкенд-парсер подставляется снаружи.
 */
trait XmlCursor:
  /** Локальное имя текущего элемента (без namespace-префикса). */
  def label: String

  /** Namespace URI текущего элемента. */
  def namespace: Option[String]

  /** Значение XML-атрибута по имени. */
  def attr(name: String): Option[String]

  /** Текстовое содержимое элемента (trim, без текста вложенных элементов). */
  def text: String

  /** Первый дочерний элемент с заданным namespace и локальным именем. */
  def child(ns: String, name: String): Option[XmlCursor]

  /** Все дочерние элементы с заданным namespace и локальным именем. */
  def children(ns: String, name: String): List[XmlCursor]

// ── NodeCursor ────────────────────────────────────────────────────────────────

/** Backward-compatible обёртка над scala.xml.Node. */
final class NodeCursor(private val node: scala.xml.Node) extends XmlCursor:
  def label     = node.label
  def namespace = Option(node.namespace).filter(_.nonEmpty)
  def attr(name: String) = Option(node \@ name).filter(_.nonEmpty)
  def text      = node.text.trim

  def child(ns: String, name: String): Option[XmlCursor] =
    node.child
      .find(c => c.label == name && c.namespace == ns)
      .map(NodeCursor(_))

  def children(ns: String, name: String): List[XmlCursor] =
    node.child
      .filter(c => c.label == name && c.namespace == ns)
      .toList
      .map(NodeCursor(_))
