
import Parsing._

import scala.util.{Failure, Success}
object Main extends App {
  val consoleIn = scala.Console.in
  val inputString = try {
    consoleIn.readLine()
  }
  finally {
    consoleIn.close()
  }
  allCardOnTable(inputString = inputString) match {
    case Failure(exception) =>println(exception.getMessage)
    case Success((players, table)) => println(s"Win ${Winner(players,table)}")
  }

}


