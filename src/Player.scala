import Combinations.CardCharacteristic.Card
import Combinations.{Combination, NoCard}

case class Player(firstCard:Card,secondCard:Card){
  def getStartCombination: Combination = NoCard
  def listCard():List[Card]=List(firstCard,secondCard)
}
