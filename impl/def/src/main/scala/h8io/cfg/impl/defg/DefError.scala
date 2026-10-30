package h8io.cfg.impl.defg

import h8io.cfg.CfgError

import java.io.IOException

/** An error found while loading a DEF configuration. Every error points at the place it is about. */
sealed trait DefError {
  def location: DefLocation
}

/** The source could not be read at all; the error points at its start. */
final case class SourceUnreadable(source: String, cause: IOException) extends DefError {
  def location: SourceLocation = SourceLocation(source, 1, 1)
}

/** Every error a load reported, in the order found. The loader reports as many as it can without inventing any. */
final case class DefErrors(errors: ::[DefError]) extends CfgError
