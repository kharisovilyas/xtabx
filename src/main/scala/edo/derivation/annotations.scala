package edo.derivation

import scala.annotation.StaticAnnotation

/** Переопределяет XML-имя элемента или атрибута для поля/кейса. */
final class xmlName(val name: String) extends StaticAnnotation

/** Поле читается из XML-атрибута, а не дочернего элемента. */
final class xmlAttr extends StaticAnnotation

/** Поле читается из текстового содержимого текущего узла (без вложенного элемента). */
final class xmlText extends StaticAnnotation

/**
 * Поле с типом sealed trait/enum декодируется из текущего родительского узла (inline choice),
 * а не из именованного дочернего элемента.
 */
final class xmlInline extends StaticAnnotation
