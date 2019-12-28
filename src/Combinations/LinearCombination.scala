package Combinations

import Combinations.CardCharacteristic.{Card, Rank, Suit}

/**
 * class represent all line combination
 * @param highCard - high card in combination
 */
sealed  class LinearCombination(highCard:Card) extends Combination{
  override def getHighRank: Rank = highCard.rank
  def getSuit:Suit=highCard.suit
}

/**
 * class represent poker combination Straight Flush
 * @param highCard - high card  combination
 */
case class StraightFlush(highCard:Card) extends LinearCombination(highCard:Card) {
  override def powerCombination: Int =8*10000+highCard.rank

}

/**
 * class represent poker combination Straight
 * @param highCard - high card  combination
 */
case class Flush(highCard:Card) extends LinearCombination(highCard:Card){
  override def powerCombination: Int = 5*10000+highCard.rank
}

/**
 * class represent poker combination Straight
 * @param highCard - high card  combination
 */
case class Straight(highCard:Card) extends LinearCombination(highCard:Card){
  override def powerCombination: Int = 4*10000+highCard.rank
}

