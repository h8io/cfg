package h8io.cfg.raw

import com.typesafe.config.ConfigFactory

import java.net.URL

package object hocon {
  def read(urls: URL*): MapImpl[Id.Root] =
    MapImpl(Id.Root,
      urls.iterator
        .map(ConfigFactory.parseURL)
        .reduceOption((p, n) => n withFallback p)
        .map(_.resolve())
        .getOrElse(ConfigFactory.load).root())
}
