package utils

object Logging {
  implicit class Logger(val message: String) extends AnyVal {
    def log: Unit = println(message+Console.RESET)
  }
}
