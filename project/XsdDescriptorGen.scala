import sbt._
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.{Element, Node => DomNode}

/** sbt source generator: читает XSD и эмитит EdoSchemaDescriptor.scala в managed sources. */
object XsdDescriptorGen {

  private val XS_NS = "http://www.w3.org/2001/XMLSchema"

  // ── DOM helpers ────────────────────────────────────────────────────────────

  private def childElems(e: Element): List[Element] = {
    val nl = e.getChildNodes
    (0 until nl.getLength).toList
      .map(nl.item)
      .collect { case c: Element => c }
  }

  private def xsKids(e: Element, localName: String): List[Element] =
    childElems(e).filter(c => c.getLocalName == localName && c.getNamespaceURI == XS_NS)

  private def stripPrefix(t: String): String =
    if (t.contains(":")) t.split(":")(1) else t

  private def attr(e: Element, name: String, default: String = ""): String = {
    val v = e.getAttribute(name)
    if (v == null || v.isEmpty) default else v
  }

  // ── parsing ────────────────────────────────────────────────────────────────

  private case class ElemDesc(name: String, typeName: String, minOccurs: Int, maxOccurs: Int)
  private case class AttrDesc(name: String, typeName: String, required: Boolean)
  private case class ComplexDesc(name: String, isChoice: Boolean, elems: List[ElemDesc], attrs: List[AttrDesc])

  private def parseElement(e: Element): ElemDesc = {
    val name    = attr(e, "name")
    val typeName = stripPrefix(attr(e, "type", "xs:string"))
    val minOcc  = attr(e, "minOccurs", "1").toInt
    val maxOcc  = attr(e, "maxOccurs", "1") match {
      case "unbounded" => -1
      case s           => s.toInt
    }
    ElemDesc(name, typeName, minOcc, maxOcc)
  }

  private def parseAttribute(e: Element): AttrDesc = {
    val name     = attr(e, "name")
    val typeName = stripPrefix(attr(e, "type", "xs:string"))
    val required = attr(e, "use", "optional") == "required"
    AttrDesc(name, typeName, required)
  }

  private def parseComplexType(ct: Element): Option[ComplexDesc] = {
    val name = attr(ct, "name")
    if (name.isEmpty) return None

    // Look for sequence or choice (direct child or wrapped in complexContent/extension)
    val seqs = xsKids(ct, "sequence")
    val chcs = xsKids(ct, "choice")

    val (isChoice, contentNode) =
      if (seqs.nonEmpty) (false, Some(seqs.head))
      else if (chcs.nonEmpty) (true, Some(chcs.head))
      else (false, None)

    val elems = contentNode.toList.flatMap { cn =>
      xsKids(cn, "element").map(parseElement) ++
        // xs:choice nested inside xs:sequence
        xsKids(cn, "choice").flatMap(c => xsKids(c, "element").map(parseElement)) ++
        // xs:sequence nested inside xs:choice
        xsKids(cn, "sequence").flatMap(s => xsKids(s, "element").map(parseElement))
    }

    val attrElems = xsKids(ct, "attribute").map(parseAttribute)

    Some(ComplexDesc(name, isChoice, elems, attrElems))
  }

  private def parseSimpleTypeEnums(st: Element): Option[(String, List[String])] = {
    val name = attr(st, "name")
    if (name.isEmpty) return None
    val restrictions = xsKids(st, "restriction")
    restrictions.headOption.flatMap { r =>
      val enums = xsKids(r, "enumeration").map(e => attr(e, "value"))
      if (enums.nonEmpty) Some(name -> enums) else None
    }
  }

  // ── code generation ────────────────────────────────────────────────────────

  private def q(s: String): String = s""""$s""""

  private def emitElemInfo(e: ElemDesc): String =
    s"ElementInfo(${q(e.name)}, ${q(e.typeName)}, ${e.minOccurs}, ${e.maxOccurs})"

  private def emitAttrInfo(a: AttrDesc): String =
    s"AttributeInfo(${q(a.name)}, ${q(a.typeName)}, ${a.required})"

  private def emitComplexType(cd: ComplexDesc): String = {
    val ck    = if (cd.isChoice) "ContentKind.Choice" else "ContentKind.Sequence"
    val elems = cd.elems.map(emitElemInfo).mkString(",\n          ")
    val attrs = cd.attrs.map(emitAttrInfo).mkString(",\n          ")
    s"""    ${q(cd.name)} -> ComplexTypeInfo(
          name        = ${q(cd.name)},
          contentKind = $ck,
          elements    = List($elems),
          attributes  = List($attrs)
        )"""
  }

  // ── entry point ───────────────────────────────────────────────────────────

  def generate(outDir: File, xsdFile: File): Seq[File] = {
    val dbf = DocumentBuilderFactory.newInstance()
    dbf.setNamespaceAware(true)
    val doc  = dbf.newDocumentBuilder().parse(xsdFile)
    val root = doc.getDocumentElement

    val complexDescs = xsKids(root, "complexType").flatMap(parseComplexType)
    val enumDescs    = xsKids(root, "simpleType").flatMap(parseSimpleTypeEnums)
    val targetNs     = Option(root.getAttribute("targetNamespace")).getOrElse("")

    val sb = new StringBuilder
    sb.append("package edo.schema\n\n")
    sb.append("// AUTO-GENERATED from edo_test_schema.xsd — do not edit manually\n\n")
    sb.append("object EdoSchemaDescriptor:\n\n")
    sb.append(s"  val targetNamespace: String = ${q(targetNs)}\n\n")

    // enumerations
    sb.append("  val enumerations: Map[String, List[String]] = Map(\n")
    enumDescs.zipWithIndex.foreach { case ((name, vals), i) =>
      val comma = if (i < enumDescs.length - 1) "," else ""
      val valsStr = vals.map(q).mkString(", ")
      sb.append(s"    ${q(name)} -> List($valsStr)$comma\n")
    }
    sb.append("  )\n\n")

    // complexTypes
    sb.append("  val complexTypes: Map[String, ComplexTypeInfo] = Map(\n")
    complexDescs.zipWithIndex.foreach { case (cd, i) =>
      val comma = if (i < complexDescs.length - 1) "," else ""
      sb.append(emitComplexType(cd))
      sb.append(s"$comma\n")
    }
    sb.append("  )\n")

    val outFile = outDir / "edo" / "schema" / "EdoSchemaDescriptor.scala"
    IO.createDirectory(outFile.getParentFile)
    IO.write(outFile, sb.toString)
    Seq(outFile)
  }
}
