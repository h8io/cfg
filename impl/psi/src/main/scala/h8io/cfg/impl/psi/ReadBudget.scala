package h8io.cfg.impl.psi

import java.io.IOException
import java.time.Duration

/** What reading the sources of one load may take, in all: `bytes`, every read counted, a source read twice twice, and
  * `time`, from the start of the load. The first read to go past either is the load's error; every read after it is
  * dropped, its source taken as empty, and not reported.
  */
private[psi] final class ReadBudget(bytes: Int, time: Duration) {
  private val started = System.nanoTime

  /** `time` in nanoseconds, a duration too long for them taken as forever. */
  private val nanos = if (time.compareTo(Duration.ofNanos(Long.MaxValue)) > 0) Long.MaxValue else time.toNanos

  private var read = 0L

  /** The error of the read that went past, after which nothing more is read. */
  private var exceeded: Option[PsiError] = None

  def error: Option[PsiError] = exceeded

  def exhausted: Boolean = exceeded.nonEmpty

  /** How long the next wait of reading `source` may be, in milliseconds: at most `timeout`, and at least 1, 0 meaning
    * no timeout at all; past the time, the read fails.
    */
  def timeout(source: String, timeout: Int): Int = {
    val left = nanos - (System.nanoTime - started)
    if (left <= 0) exceed(ReadTimeExceeded(source, time))
    // a millisecond more than what is left, so that the wait ends after the time, and is never 0
    else math.min(timeout.toLong, left / 1000000 + 1).toInt
  }

  /** Counts `n` bytes more read of `source`; past the limit, the read fails. */
  def count(source: String, n: Int): Unit = {
    read += n
    if (read > bytes) exceed(ReadLimitExceeded(source, bytes))
  }

  private def exceed(error: PsiError): Nothing = {
    exceeded = Some(error)
    throw new ReadBudget.Exceeded(error)
  }
}

private[psi] object ReadBudget {

  /** How many bytes the sources of a load may take, every read counted, unless the builder says otherwise. */
  def DefaultBytes: Int = 10000000

  /** How long reading the sources of a load may take unless the builder says otherwise. */
  val DefaultTime: Duration = Duration.ofSeconds(60)

  /** A read stopped by the budget, which holds its error. */
  final class Exceeded(val error: PsiError) extends IOException(error.message)
}
