package Combinations

import Combinations.CardCharacteristic.{Card, Rank}


/**
 * class represent not full line combination
 * @param highCard - card with high rank in combination
 * @param lowCard - card with low rank in combination
 * @param length - length not full combination
 */
sealed abstract  class  PossibleLinearCombination(highCard: Card, lowCard:Card, length:Int) extends Combination{
  override def size: Rank = 0
  override def powerCombination: Int = -1
  val lowerCardDignity: Rank =lowCard.getLowerRank
  override def combine(combination: Combination): Combination = combination

  override def getCombinationList: List[Combination] = List()
}

/**
 * class represent not full Straight
 * @param highCard - card with high rank in combination
 * @param lowCard - card with low rank in combination
 * @param length - length not full combination
 */
case class PossibleStraight(highCard: Card, lowCard:Card, length:Int) extends
  PossibleLinearCombination(highCard: Card, lowCard:Card, length:Int){

  override def combine(card: Card): Combination = {
    card match {
      case Card(this.lowerCardDignity, _) if this.size < 4 => this.copy(lowCard = card, length = this.length + 1)
      case Card(this.lowerCardDignity, _) if this.size == 4 => Straight(highCard)
      case card: Card=>this
    }
  }
}

/**
 *  class represent not full Flush
 * @param highCard - card with high rank in combination
 * @param lowCard - card with low rank in combination
 * @param length - length not full combination
 */
case class PossibleFlush(highCard: Card, lowCard:Card, length:Int) extends
  PossibleLinearCombination(highCard: Card, lowCard:Card, length:Int){

  override def combine(card: Card): Combination = card match {
    case Card(dignity,highCard.suit) if (this.size < 4 && dignity!=1) =>this.copy(lowCard=card,length = this.length+1)
    case Card(dignity,highCard.suit) if (this.size == 4 && dignity!=1) =>Flush(highCard)
    case card: Card=>this
  }
}

/**
 *  class represent not full Straight Flush
 * @param highCard - card with high rank in combination
 * @param lowCard - card with low rank in combination
 * @param length - length not full combination
 */
case class PossibleStraightFlush(highCard: Card, lowCard:Card, length:Int) extends
  PossibleLinearCombination(highCard: Card, lowCard:Card, length:Int)   {
  override def combine(card: Card): Combination = {
    card match {
      case Card(this.lowerCardDignity, lowCard.suit) if (this.size < 4) => Node(this, PossibleStraightFlush(highCard, card, size + 1))
      case Card(this.lowerCardDignity,lowCard.suit) if (this.size == 4) => StraightFlush (highCard)
      case Card(this.lowerCardDignity, _) if (this.size < 4) => Node(this, PossibleStraight(highCard, card, size + 1))
      case Card(this.lowerCardDignity, _) if (this.size == 4) => Straight(highCard)
      case Card(_, highCard.suit)  if (this.size < 4) => Node(this, PossibleFlush(highCard, card, size + 1))
      case Card(_, highCard.suit) if (this.size == 4) => Flush(highCard)
      case card: Card=>this

    }
  }
}
