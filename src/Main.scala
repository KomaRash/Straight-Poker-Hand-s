
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
    case Success((players, table)) => val range=RangePlayers(players,table)
      range.foldLeft(range.head){(prevPlayer,player)=>
        if(! (prevPlayer._1 equals player._1)) {
          GameOrdering.compare(prevPlayer,player) match {
            case 0=> print('=')
            case _=> print(' ')
          }
        }
          print(player._1)
          player

      }

    }
}



