package h8io.cfg

import h8io.cfg.raw.Node

trait Decoder[-N <: Node[?], +T] extends (N => T) {}
