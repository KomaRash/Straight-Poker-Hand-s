package Combinations

import Combinations.CardCharacteristic.{Dignity,Card}


trait Combination{
  def |(combination: Combination):Combination = combination match {
    case combination:PossibleLinearCombination  => this
    case NoCard=>this
    case _=>this
  }
  def getHighDignity: Dignity= ???
  def powerCombination:Int= ???
  def combine(card:Card):Combination= this
  def getCombinationList:List[Combination]=List(this)

}

case class FourOfKind(dignity:Dignity) extends Combination{
  override def powerCombination: Int = 7*10000+getHighDignity
  override def getHighDignity: Dignity=dignity

}
case class FullHouse(threeOfKind: ThreeOfKind,pair: Pair) extends Combination{
  override def powerCombination: Int = 6*10000+threeOfKind.getHighDignity*100+pair.getHighDignity
}
case class ThreeOfKind(firstCard:Card,secondCard:Card,thirdCard:Card) extends Combination{
  override def powerCombination: Int = 3*10000+getHighDignity

  override def |(combination: Combination): Combination = combination match {
    case pair: Pair if(pair.getHighDignity!=this.getHighDignity)=> FullHouse(pair = pair, threeOfKind = this)
    case _ =>this
  }
  override def getHighDignity: Dignity = firstCard.dignity
  override def combine(card: Card): Combination = card match {
    case Card(firstCard.dignity, _)=>FourOfKind(firstCard.dignity)
    case _=>this
  }
}
case class TwoPairs(firstPair: Pair,secondPair: Pair ) extends Combination{
  override def powerCombination: Int = 2*10000+ {
    if (firstPair.getHighDignity.compareTo(secondPair.getHighDignity) equals firstPair.getHighDignity)
      firstPair.getHighDignity * 100 + secondPair.getHighDignity
    else
      secondPair.getHighDignity * 100 + firstPair.getHighDignity
  }
  override def combine(card: Card): Combination = card match {
    case Card(secondPair.firstCard.dignity,_)=>FullHouse(pair = firstPair,threeOfKind=(secondPair combine card ).asInstanceOf[ThreeOfKind])
    case Card(firstPair.firstCard.dignity,_)=>FullHouse((firstPair combine card ).asInstanceOf[ThreeOfKind],secondPair)
    case card=>this
  }
}
case class Pair(firstCard:Card,secondCard:Card) extends Combination{
  override def powerCombination: Int = 1*10000+firstCard.dignity
  override def getHighDignity: Dignity = firstCard.dignity
  override def combine(card: Card): Combination = card match{
    case Card(firstCard.dignity,_) => ThreeOfKind(firstCard,secondCard,card)
    case _=>this
  }

  override def |(combination: Combination): Combination = combination match {
    case pair: Pair if( pair.getHighDignity != this.getHighDignity) => TwoPairs(firstPair = this,secondPair= pair)
    case threeOfKind: ThreeOfKind if(threeOfKind.getHighDignity !=this.getHighDignity)=>FullHouse(pair = this,threeOfKind = threeOfKind)
    case _=>this
  }


}
case class HighCard(card: Card) extends Combination{
  override def powerCombination: Int = card.dignity
  val lowerCardDignity: Dignity = this.card.getlowerDignity

  override def combine(nextCard: Card): Combination = {
     nextCard match {
       case Card(this.lowerCardDignity, card.suit) => Comb(this, PossibleStraightFlush(card, nextCard, 2))
      case Card(this.lowerCardDignity, _) => Comb(this, PossibleStraight(card, nextCard, 2))
       case Card(_, card.suit) => Comb(this, PossibleFlush(card, nextCard, 2))
       case Card(card.dignity, _) => Comb(this, Pair(card, nextCard))
       case _ => this
    }
  }
}
case object NoCard extends Combination {
  override def powerCombination: Int = -1

  override def combine(card: Card): Combination = card match {
    case Card(1,suit)=>this
    case _=>Comb (HighCard(card),NoCard)

  }
}

case class Comb(leftCombination: Combination, rightCombination: Combination) extends Combination{
  override def combine(card: Card): Combination = Comb(leftCombination.combine(card),rightCombination.combine(card))
  override def getCombinationList: List[Combination] = List(leftCombination,rightCombination) flatMap { combination=>combination.getCombinationList}
}


