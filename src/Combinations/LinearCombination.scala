package Combinations

import Combinations.CardCharacteristic.{Card, Dignity}

sealed  class LinearCombination(highCard:Card) extends Combination{
  override def getHighDignity: Dignity = highCard.dignity

}

case class StraightFlush(highCard:Card) extends LinearCombination(highCard:Card) {
  override def powerCombination: Int =8*10000+highCard.dignity

}
case class Flush(highCard:Card) extends LinearCombination(highCard:Card){
  override def powerCombination: Int = 5*10000+highCard.dignity
}
case class Straight(highCard:Card) extends LinearCombination(highCard:Card){
  override def powerCombination: Int = 4*10000+highCard.dignity
}

