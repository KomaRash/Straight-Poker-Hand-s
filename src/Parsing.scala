import Combinations.CardCharacteristic._
import Combinations.{Combination, Flush, HighCard, LinearCombination, StraightFlush}

import scala.util.Try
object Parsing {
  /**
   * implicit value for compare power of player's Cards
   */
  implicit val GameOrdering:Ordering[(Player,(List[Combination],Combination))]=(x,y)=>CombinationOrdering.compare(x._2._2 ,y._2._2) match {
    case 0 => x._2._1.zip(y._2._1).foldLeft(0){(compare,compareComb)=>if(compare ==0)0 else CombinationOrdering.compare(compareComb._1,compareComb._2)}
    case order=>order
  }

  /**
   * implicit class for convert two cards in Player
   * @param listCard -convertible sheet
   */
  implicit class CardList(listCard: List[Card]) {

    def playerFromList(): Player = Player(listCard.head, listCard.last)
  }

  /**
   * convect rank from input string to Rank
   * @param rank - input char
   * @return
   */
    def fromCharToDignity(rank:Char): List[Rank] =
      rank match {
        case rank if rank >= '1' & rank <= '9' => List(rank.toInt - '0')
        case 'T' => List(10)
        case 'J' => List(11)
        case 'Q' => List(12)
        case 'K' => List(13)
        case 'A' => List(14, 1)
        case _=>throw new ParseCardError(rank)
      }

  /**
   *  parse input string from console to Tuple (Card on table,Players)
   * @param inputString  input in Console String
   * @return  all card in playing table with all Player's or exception
   */
  def allCardOnTable(inputString: String):Try[(List[Player],List[Card])]={
 Try(inputString.split(" ").
   map(cards=>
     (cards.grouped(2).toList
     flatMap {
      case string: String=> fromCharToDignity(string.head).map(dignity=>Card(dignity,string.last))
     })
     ) match {case Array(table,players@_*)=>(players.map{_.playerFromList()}.toList,table)})
}

  /**
   *
   * @param table list card on table in game
   * @param player  specific player
   * @return all possible player combinations
   */
  def playerCombination(table:List[Card])(player: Player):List[Combination]={
    @scala.annotation.tailrec
    def updateCombinationTree(list:List[Card])(combination:Combination):Combination={
      if(list.isEmpty)
        combination
      else updateCombinationTree(list.tail)(combination.combine(list.head))
    }
    val cardRange:List[Card]=(table:::player.listCard).sorted
    val combinationTree=updateCombinationTree(cardRange)(player.getStartCombination)
    val allCombination=combinationTree.
      getCombinationList.
      combinations(2).
      map(_.reduce(_ combine _)).
      distinct .toList.sorted
    allCombination
  }

  /**
   * getting winner Player in game
   * @param players - list Player's
   * @param table - list card on table in game
   * @return Player with highest combination
   */
  def Winner(players: List[Player],table:List[Card]):Player={
    val combinationOnTable = playerCombination(table)(_)
    val playersHighCombination = players.map {
      player =>
        val playerCombination = combinationOnTable(player)
        val (highCards, combinations) = playerCombination.partition(_.isInstanceOf[HighCard])
        val highCombination = combinations.max
        (player,((highCombination match {
          case comb@(_: Flush | _: StraightFlush) =>
            val (suitCard, otherCard) =
              highCards.partition(highCombination.asInstanceOf[LinearCombination].getSuit equals _)
            (suitCard.take(5)) ::: otherCard
          case _ => highCards
        }).reverse, highCombination))
    }
    playersHighCombination.max._1

  }


}
