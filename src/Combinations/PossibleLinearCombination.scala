package Combinations

import Combinations.CardCharacteristic.{Card, Dignity}

sealed abstract  class  PossibleLinearCombination(highCard: Card, lowCard:Card, length:Int) extends Combination{
  override def powerCombination: Int = -1
  val lowerCardDignity: Dignity =lowCard.getlowerDignity
  override def |(combination: Combination): Combination = combination

  override def getCombinationList: List[Combination] = List()
}

case class PossibleStraight( highCard: Card, lowCard:Card,length:Int) extends
  PossibleLinearCombination(highCard: Card, lowCard:Card, length:Int){

  override def combine(card: Card): Combination = {
    card match {
      case Card(this.lowerCardDignity, _) if this.length < 4 => this.copy(lowCard = card, length = this.length + 1)
      case Card(this.lowerCardDignity, _) if this.length == 4 => Straight(highCard)
      case card: Card=>this
    }
  }
}

case class PossibleFlush( highCard: Card, lowCard:Card,length:Int) extends
  PossibleLinearCombination(highCard: Card, lowCard:Card, length:Int){

  override def combine(card: Card): Combination = card match {
    case Card(dignity,highCard.suit) if (this.length < 4 && dignity!=1) =>this.copy(lowCard=card,length = this.length+1)
    case Card(dignity,highCard.suit) if (this.length == 4 && dignity!=1) =>Flush(highCard)
    case card: Card=>this
  }
}

case class PossibleStraightFlush( highCard: Card, lowCard:Card,length:Int) extends
  PossibleLinearCombination(highCard: Card, lowCard:Card, length:Int)   {
  override def combine(card: Card): Combination = {
    card match {
      case Card(this.lowerCardDignity, lowCard.suit) if (this.length < 4) => Comb(this, PossibleStraightFlush(highCard, card, length + 1))
      case Card(this.lowerCardDignity,lowCard.suit) if (this.length == 4) => StraightFlush (highCard)
      case Card(this.lowerCardDignity, _) if (this.length < 4) => Comb(this, PossibleStraight(highCard, card, length + 1))
      case Card(this.lowerCardDignity, _) if (this.length == 4) => Straight(highCard)
      case Card(_, highCard.suit)  if (this.length < 4) => Comb(this, PossibleFlush(highCard, card, length + 1))
      case Card(_, highCard.suit) if (this.length == 4) => Flush(highCard)
      case card: Card=>this

    }
  }
}
