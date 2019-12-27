import Combinations.CardCharacteristic._
import Combinations.Combination
object Parsing {
  implicit val CardOrdering: Ordering[Card] = (x: Card, y: Card) => x.dignity compareTo y.dignity
  implicit val CombinationOrdering: Ordering[Combination] = (x: Combination, y: Combination) => x.powerCombination compareTo y.powerCombination

  implicit class CardList(listCard: List[Card]) {

    def playerFromList(): Player = Player(listCard.head, listCard.last)
  }
    def fromCharToDignity(card:Char): List[Dignity] = card match {
    case x if x >= '1' & x <= '9' => List(x.toInt-'0')
    case 'T' => List(10)
    case 'J' => List(11)
    case 'Q' => List(12)
    case 'K' => List(13)
    case 'A' => List(14,1)
  }
  def allCardOnTable(inputString: String):(List[Player],List[Card])={
 inputString.split(" ").
   map(cards=>(cards.grouped(2).toList
     flatMap {
      case string: String=> fromCharToDignity(string.head).map(dignity=>Card(dignity,string.last))
     })
   ) match {case Array(table,players@_*)=>(players.map{_.playerFromList()}.toList,table)}
}
  def playerCombination(table:List[Card])(player: Player):(Player,List[Combination])={
    @scala.annotation.tailrec
    def updateCombinationTree(list:List[Card])(combination:Combination):Combination={
      if(list.isEmpty)
        combination
      else updateCombinationTree(list.tail)(combination.combine(list.head))
    }
    val cardRange:List[Card]=(table:::player.listCard).sorted
    val combinationTree=updateCombinationTree(cardRange)(player.getStartCombination())
    val allCombination=combinationTree.
      getCombinationList.
      combinations(2).
      map(_.reduce(_|_)).
      distinct .toList
    (player,allCombination)
  }

}
