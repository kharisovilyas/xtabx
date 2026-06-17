package edo.derivation

import edo.*

object DerivedInstances:
  import XmlDecoderInstances.given
  import XmlDecoderDerivation.*

  given XmlDecoder[LegalEntity]    = derivedProduct[LegalEntity]
  given XmlDecoder[Individual]     = derivedProduct[Individual]
  given XmlDecoder[ForeignEntity]  = derivedProduct[ForeignEntity]
  given XmlDecoder[RussianAddress] = derivedProduct[RussianAddress]
  given XmlDecoder[CorrectionInfo] = derivedProduct[CorrectionInfo]
  given XmlDecoder[Totals]         = derivedProduct[Totals]

  given XmlDecoder[ForeignAddress] with
    def decode(c: XmlCursor): Either[XmlError, ForeignAddress] =
      Right(ForeignAddress(c.text))

  given XmlDecoder[Address] = derivedChoice[Address]
