package h8io.cfg

import cats.Functor
import cats.data.Validated
import h8io.cfg.errors.Thrown
import h8io.cfg.raw.{Id, Node}

object DecoderOps extends Functor[UniversalDecoder] {
  def map[A, B](fa: UniversalDecoder[A])(f: A => B): UniversalDecoder[B] =
    node =>
      try fa(node).map(f)
      catch {
        case e: Exception => Validated.invalidNec(Thrown(node.id, node.origin, e))
      }

  @inline def apply[N <: Node[Id]]: Functor[λ[T => Decoder[N, T]]] =
    DecoderOps.asInstanceOf[Functor[λ[T => Decoder[N, T]]]]
}
