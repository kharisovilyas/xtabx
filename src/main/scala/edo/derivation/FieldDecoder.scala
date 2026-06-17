package edo.derivation

import edo.{XmlDecoder, XmlError, XmlCursor, MissingElement}

/**
 * Извлекает поле типа A из родительского курсора по имени дочернего элемента.
 *
 * - Обычный A (required): ищет ровно один дочерний элемент.
 * - Option[A]:            ищет 0 или 1 дочерний элемент.
 * - List[A]:              ищет все совпадающие дочерние элементы.
 */
trait FieldDecoder[A]:
  def decodeFrom(parent: XmlCursor, ns: String, elemName: String): Either[XmlError, A]

object FieldDecoder:

  given [A](using xd: XmlDecoder[A]): FieldDecoder[A] with
    def decodeFrom(parent: XmlCursor, ns: String, name: String): Either[XmlError, A] =
      parent.child(ns, name) match
        case None    => Left(MissingElement(s"${parent.label}/$name"))
        case Some(c) => xd.decode(c)

  given [A](using xd: XmlDecoder[A]): FieldDecoder[Option[A]] with
    def decodeFrom(parent: XmlCursor, ns: String, name: String): Either[XmlError, Option[A]] =
      parent.child(ns, name) match
        case None    => Right(None)
        case Some(c) => xd.decode(c).map(Some(_))

  given [A](using xd: XmlDecoder[A]): FieldDecoder[List[A]] with
    def decodeFrom(parent: XmlCursor, ns: String, name: String): Either[XmlError, List[A]] =
      parent.children(ns, name).foldLeft[Either[XmlError, List[A]]](Right(Nil)) {
        (acc, c) => acc.flatMap(lst => xd.decode(c).map(lst :+ _))
      }
