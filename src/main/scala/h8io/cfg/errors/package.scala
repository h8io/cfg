package h8io.cfg

import cats.data.NonEmptyChain

package object errors {
  implicit class CfgErrorOps(val self: CfgError) extends AnyVal {
    def |(other: CfgError): OrError =
      (self, other) match {
        case (OrError(leftErrors), OrError(rightErrors)) => OrError(leftErrors ++ rightErrors)
        case (OrError(leftErrors), _) => OrError(leftErrors :+ other)
        case (_, OrError(rightErrors)) => OrError(self +: rightErrors)
        case _ => OrError(NonEmptyChain(self, other))
      }

    def &(other: CfgError): AndError =
      (self, other) match {
        case (AndError(leftErrors), AndError(rightErrors)) => AndError(leftErrors ++ rightErrors)
        case (AndError(leftErrors), _) => AndError(leftErrors :+ other)
        case (_, AndError(rightErrors)) => AndError(self +: rightErrors)
        case _ => AndError(NonEmptyChain(self, other))
      }
  }
}
