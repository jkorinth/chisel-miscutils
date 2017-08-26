package chisel.miscutils
import  chisel3._
import  Logging._
import  scala.util.Properties.{lineSeparator => NL}

trait Logging {
  self: Module =>
    def info(msg: => core.Printable)(implicit level: Level)  { log(Level.Info, msg) }
    def warn(msg: => core.Printable)(implicit level: Level)  { log(Level.Warn, msg) }
    def error(msg: => core.Printable)(implicit level: Level) { log(Level.None, msg) }

    def log(msgLevel: Level, msg: => core.Printable)(implicit l: Level): Unit = msgLevel match {
      case Level.Info if (l == Level.Info)                    => printf(p"[INFO] $className: $msg$NL")
      case Level.Warn if (l == Level.Info || l == Level.Warn) => printf(p"[WARN] $className: $msg$NL")
      case Level.None                                         => printf(p"[ERROR] $className: $msg$NL")
      case _                                                  => ()
    }

    def cinfo(msg: => String)(implicit level: Level)  { clog(Level.Info, msg) }
    def cwarn(msg: => String)(implicit level: Level)  { clog(Level.Warn, msg) }
    def cerror(msg: => String)(implicit level: Level) { clog(Level.None, msg) }

    def clog(msgLevel: Level, msg: => String)(implicit l: Level): Unit = msgLevel match {
      case Level.Info if (l == Level.Info)                    => println(s"[INFO] $className: $msg")
      case Level.Warn if (l == Level.Info || l == Level.Warn) => println(s"[WARN] $className: $msg")
      case Level.None                                         => println(s"[ERROR] $className: $msg")
      case _                                                  => ()
    }
    private[this] final lazy val className = self.getClass.getSimpleName
}

object Logging {
  sealed trait Level
  object Level {
    final case object Info extends Level
    final case object Warn extends Level
    final case object None extends Level
  }
}
