
import Parsing._
object Main extends App {
  val consoleIn = scala.Console.in
  val inputString=try {
    consoleIn.readLine()
  }
  finally {
    consoleIn.close()
  }

  val (players,table)=allCardOnTable(inputString = inputString)
  val combinationOnTable=playerCombination(table)(_)
  players.map(combinationOnTable(_)).foreach(println)
}


