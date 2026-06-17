package edo.schema

enum ContentKind:
  case Sequence, Choice

/** Описание одного дочернего элемента в complexType. maxOccurs = -1 означает unbounded. */
case class ElementInfo(
  name:      String,
  typeName:  String,
  minOccurs: Int,
  maxOccurs: Int
):
  def isOptional: Boolean   = minOccurs == 0 && maxOccurs == 1
  def isRepeated: Boolean   = maxOccurs == -1 || maxOccurs > 1
  def isRequired: Boolean   = minOccurs >= 1 && maxOccurs == 1

/** Описание XML-атрибута. */
case class AttributeInfo(
  name:     String,
  typeName: String,
  required: Boolean
)

/** Описание одного complexType из XSD. */
case class ComplexTypeInfo(
  name:        String,
  contentKind: ContentKind,
  elements:    List[ElementInfo],
  attributes:  List[AttributeInfo]
)
