import Combinations.CardCharacteristic.Card
import Combinations.{Combination, NoCard}

/**
 * class represent player
 * @param firstCard - first card player
 * @param secondCard - second card player
 */
case class Player(firstCard:Card,secondCard:Card){
  def getStartCombination: Combination = NoCard
  def listCard():List[Card]=List(firstCard,secondCard)
  override def toString: String = s"player with card:$secondCard;$firstCard"
}
