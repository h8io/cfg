package h8io.cfg.errors

import cats.data.NonEmptyChain
import h8io.cfg.CfgError

final case class AndError(errors: NonEmptyChain[CfgError]) extends CfgError
