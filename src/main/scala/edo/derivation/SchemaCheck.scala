package edo.derivation

import scala.quoted.*
import edo.schema.{EdoSchemaDescriptor, ComplexTypeInfo, ContentKind, ElementInfo, AttributeInfo}

/**
 * Compile-time conformance check: сверяет Mirror-структуру ADT-типа A
 * с XSD-дескриптором, сгенерированным из edo_test_schema.xsd.
 *
 * Конвенция имён: тип `Foo` → XSD-тип `FooType`.
 *
 * Проверяет:
 *   - Case class: каждое поле соответствует XSD-элементу или атрибуту;
 *     кардинальность (required / Option / List) совпадает.
 *   - Enum: каждый case-вариант имеет совпадающее значение в XSD enumeration
 *     или аннотацию @xmlName с корректным значением.
 *   - Sealed trait / choice: каждый вариант-класс присутствует как XSD-элемент.
 *
 * Тяжёлый разбор XSD — в build-time генераторе (XsdDescriptorGen).
 * Макрос делает только лёгкую сверку с готовым дескриптором.
 */
object SchemaCheck:

  /** Вызов compile-time проверки. Вставляется в derived-цепочку или пишется вручную. */
  inline def assertConforms[A](): Unit = ${ assertConformsImpl[A] }

  // ── macro implementation ────────────────────────────────────────────────────

  def assertConformsImpl[A: Type](using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val tpe      = TypeRepr.of[A]
    val sym      = tpe.typeSymbol
    val symName  = sym.name
    // Convention: FooType → try "FooType" first, then "Foo" as fallback
    val xsdName  = EdoSchemaDescriptor.complexTypes.keys
                     .find(k => k == symName + "Type" || k == symName)
                     .getOrElse(symName + "Type")

    if sym.flags.is(Flags.Enum) then
      checkEnum(sym, xsdName)
    else if sym.flags.is(Flags.Sealed) && !sym.flags.is(Flags.Case) then
      EdoSchemaDescriptor.complexTypes.get(xsdName) match
        case None    => report.warning(xsdMissing(symName, xsdName, "sealed trait"))
        case Some(d) => checkSealedTrait(sym, xsdName, d)
    else if sym.flags.is(Flags.Case) && !sym.flags.is(Flags.Module) then
      EdoSchemaDescriptor.complexTypes.get(xsdName) match
        case None    => report.warning(xsdMissing(symName, xsdName, "case class"))
        case Some(d) => checkCaseClass(tpe, sym, xsdName, d)
    else
      report.warning(s"[assertConforms] $symName: not a case class, enum or sealed trait — skipping")

    '{ () }

  // ── helpers ─────────────────────────────────────────────────────────────────

  private def xsdMissing(name: String, xsdName: String, kind: String): String =
    s"[assertConforms] $kind '$name': XSD type '$xsdName' not found in descriptor — " +
    s"skipping check. If the type is not in the schema, this is expected."

  private def capitalize(s: String): String =
    if s.isEmpty then s else s.head.toUpper.toString + s.tail

  private def decapitalize(s: String): String =
    if s.isEmpty then s else s.head.toLower.toString + s.tail

  // ── enum check ──────────────────────────────────────────────────────────────

  private def checkEnum(using Quotes)(sym: quotes.reflect.Symbol, xsdName: String): Unit =
    import quotes.reflect.*
    EdoSchemaDescriptor.enumerations.get(xsdName) match
      case None =>
        report.warning(s"[assertConforms] enum '${sym.name}': XSD enumeration '$xsdName' not found")
      case Some(xsdValues) =>
        val xsdSet = xsdValues.toSet
        for child <- sym.children do
          val caseName = child.name
          // Try to find @xmlName annotation
          val xmlNameAnnot = child.annotations.collectFirst {
            case annot if annot.tpe =:= TypeRepr.of[xmlName] =>
              annot match
                case Apply(_, List(Literal(StringConstant(v)))) => v
                case _ => caseName
          }
          val effectiveName = xmlNameAnnot.getOrElse(caseName)
          if !xsdSet.contains(effectiveName) then
            val hint =
              if xsdSet.contains(caseName) then ""
              else
                val candidates = xsdValues.filter(v => v.toLowerCase == caseName.toLowerCase)
                if candidates.nonEmpty then
                  s" (closest XSD value: '${candidates.head}' — add @xmlName(\"${candidates.head}\"))"
                else
                  s" (available XSD values: ${xsdValues.map(v => s"\"$v\"").mkString(", ")})"
            report.error(
              s"[assertConforms] enum '${sym.name}.${caseName}': " +
              s"effective name '$effectiveName' is not in XSD enumeration '$xsdName'.$hint"
            )

  // ── sealed trait check ──────────────────────────────────────────────────────

  private def checkSealedTrait(using Quotes)(
    sym:     quotes.reflect.Symbol,
    xsdName: String,
    desc:    ComplexTypeInfo
  ): Unit =
    import quotes.reflect.*
    if desc.contentKind != ContentKind.Choice then
      report.warning(
        s"[assertConforms] sealed trait '${sym.name}': XSD type '$xsdName' is not a choice — " +
        s"expected ContentKind.Choice, found ${desc.contentKind}"
      )
    val xsdElemNames = desc.elements.map(_.name).toSet
    for child <- sym.children do
      val childName = child.name
      if !xsdElemNames.contains(childName) then
        report.error(
          s"[assertConforms] sealed trait '${sym.name}': variant '$childName' " +
          s"has no matching element in XSD choice '$xsdName'.\n" +
          s"  Available choice elements: ${xsdElemNames.mkString(", ")}"
        )

  // ── case class check ────────────────────────────────────────────────────────

  private def checkCaseClass(using Quotes)(
    tpe:    quotes.reflect.TypeRepr,
    sym:    quotes.reflect.Symbol,
    xsdName: String,
    desc:   ComplexTypeInfo
  ): Unit =
    import quotes.reflect.*

    val elemByName = desc.elements.map(e => e.name -> e).toMap
    val attrByName = desc.attributes.map(a => a.name -> a).toMap

    // ── per-field checks ──────────────────────────────────────────────────────
    for field <- sym.caseFields do
      val fieldName = field.name
      val xsdElem   = capitalize(fieldName)
      val fieldTpe  = tpe.memberType(field).dealias
      val (isOpt, isList) = cardinalityOf(fieldTpe)

      val hasAttrAnnot   = field.annotations.exists(_.tpe =:= TypeRepr.of[xmlAttr])
      val hasInlineAnnot = field.annotations.exists(_.tpe =:= TypeRepr.of[xmlInline])

      if hasInlineAnnot then
        // @xmlInline: поле декодируется из родительского узла (inline choice) — не ищем элемент
        ()
      else if hasAttrAnnot then
        // @xmlAttr: ожидаем атрибут, не элемент
        attrByName.get(xsdElem) match
          case None =>
            report.error(
              s"[assertConforms] '${sym.name}.$fieldName' (@xmlAttr): " +
              s"no attribute '$xsdElem' in XSD type '$xsdName'.\n" +
              s"  Available attributes: ${attrByName.keys.mkString(", ")}"
            )
          case Some(_) => // OK
      else
        // обычное поле → ищем элемент
        elemByName.get(xsdElem) match
          case None =>
            // Может быть атрибут — мягкая подсказка
            if attrByName.contains(xsdElem) then
              report.warning(
                s"[assertConforms] '${sym.name}.$fieldName': element '$xsdElem' not found, " +
                s"but there IS an attribute with that name. Add @xmlAttr if this field is an attribute."
              )
            else
              report.error(
                s"[assertConforms] '${sym.name}.$fieldName': no element '$xsdElem' in XSD '$xsdName'.\n" +
                s"  Available elements:   ${elemByName.keys.mkString(", ")}\n" +
                s"  Available attributes: ${attrByName.keys.mkString(", ")}"
              )
          case Some(ei) =>
            checkFieldCardinality(sym.name, fieldName, xsdElem, ei, isOpt, isList)

    // ── missing required elements ─────────────────────────────────────────────
    for ei <- desc.elements.filter(_.isRequired) do
      val camelName = decapitalize(ei.name)
      val hasField  = sym.caseFields.exists(_.name == camelName)
      if !hasField then
        // Возможно это inline choice — не бьём ошибку, только предупреждаем
        report.warning(
          s"[assertConforms] '${sym.name}': required XSD element '${ei.name}' (minOccurs=${ei.minOccurs}) " +
          s"has no matching field '$camelName'. If it's an inline choice, annotate the field with @xmlInline."
        )

  // ── cardinality helpers ───────────────────────────────────────────────────

  private def cardinalityOf(using Quotes)(tpe: quotes.reflect.TypeRepr): (Boolean, Boolean) =
    tpe.asType match
      case '[Option[?]] => (true, false)
      case '[List[?]]   => (false, true)
      case _            => (false, false)

  private def checkFieldCardinality(using Quotes)(
    typeName:  String,
    fieldName: String,
    xsdElem:   String,
    ei:        ElementInfo,
    isOpt:     Boolean,
    isList:    Boolean
  ): Unit =
    import quotes.reflect.*
    // required в XSD, но Option в Scala → предупреждение (разрешённое ослабление)
    if ei.isRequired && isOpt then
      report.warning(
        s"[assertConforms] '$typeName.$fieldName': XSD element '$xsdElem' is required " +
        s"(minOccurs=${ei.minOccurs}) but Scala field is Option. " +
        s"This relaxes the schema constraint — intentional?"
      )
    // optional в XSD, но не Option в Scala → предупреждение
    if ei.isOptional && !isOpt && !isList then
      report.warning(
        s"[assertConforms] '$typeName.$fieldName': XSD element '$xsdElem' is optional " +
        s"(minOccurs=0, maxOccurs=1) but Scala field is not Option. " +
        s"Consider: $fieldName: Option[...]"
      )
    // repeated в XSD, но не List в Scala → ошибка
    if ei.isRepeated && !isList then
      report.error(
        s"[assertConforms] '$typeName.$fieldName': XSD element '$xsdElem' is repeated " +
        s"(maxOccurs=${ei.maxOccurs}) but Scala field is not List. " +
        s"Expected: $fieldName: List[...]"
      )
    // не repeated в XSD, но List в Scala → ошибка
    if !ei.isRepeated && isList then
      report.error(
        s"[assertConforms] '$typeName.$fieldName': Scala field is List but XSD element '$xsdElem' " +
        s"is not repeated (maxOccurs=${ei.maxOccurs}). " +
        s"Expected: $fieldName: T (not List)"
      )
