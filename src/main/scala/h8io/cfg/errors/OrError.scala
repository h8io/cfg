package h8io.cfg.errors

import cats.data.NonEmptyChain
import h8io.cfg.CfgError

final case class OrError(errors: NonEmptyChain[CfgError]) extends CfgError
