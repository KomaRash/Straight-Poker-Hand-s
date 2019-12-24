import CardCharacteristic._

//Выбор структуры классов с множествами -

sealed trait Combination{

  def |(combination: Combination):Combination = if(this.powerCombination < combination.powerCombination) combination else this
  def powerCombination:Dignity= ???
  def combine(card:Card):Combination= this
  def getCombination:List[Combination]=List(this)
}
case class StraightFlush(highCard:Card) extends Combination{
  override def powerCombination: Dignity =8

}
case class FourOfKind(dignity:Dignity) extends Combination{
  override def powerCombination: Dignity = 7
}
case class FullHouse(pair: Pair,threeOfKind: ThreeOfKind) extends Combination{
  override def powerCombination: Dignity = 6
}
case class ThreeOfKind(firstCard:Card,secondCard:Card,thirdCard:Card) extends Combination{
  override def powerCombination: Dignity = 3

  override def combine(card: Card): Combination = card match {
    case Card(firstCard.dignity, _)=>FourOfKind(firstCard.dignity)
    case _=>this
  }
}
case class TwoPairs(firstPair: Pair,secondPair: Pair ) extends Combination{
  override def powerCombination: Dignity = 2

  override def combine(card: Card): Combination = card match {
    case Card(secondPair.firstCard.dignity,suit)=>FullHouse(firstPair,(secondPair combine card ).asInstanceOf[ThreeOfKind])
    case card=>this
  }
}
case class Pair(firstCard:Card,secondCard:Card) extends Combination{
  override def powerCombination: Dignity = 1

  override def combine(card: Card): Combination = card match{
    case Card(firstCard.dignity,_) => ThreeOfKind(firstCard,secondCard,card)
    case _=>this
  }
}
case class HighCard(card: Card) extends Combination{
  override def powerCombination: Dignity = 0
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
case object NoCard extends Combination{
  override def powerCombination: Dignity = -1
  override def combine(card: Card): Combination = Comb(HighCard(card),NoCard)
}
case class Flush(highCard:Card) extends Combination{
  override def powerCombination: Dignity = 5
}
case class Straight(highCard:Card) extends Combination{
  override def powerCombination: Dignity = 4
}
case class Comb(leftCombination: Combination, rightCombination: Combination) extends Combination{
  override def powerCombination: Dignity = (leftCombination | rightCombination).powerCombination
  override def combine(card: Card): Combination = Comb(leftCombination.combine(card),rightCombination.combine(card))
  override def getCombination: List[Combination] = List(leftCombination,rightCombination) flatMap { combination=>combination.getCombination}
}
case class PossibleStraight(highCard: Card, lowCard:Card, length:Int) extends Combination{
  override def powerCombination: Dignity = -1
  val lowerCardDignity: Dignity =lowCard.getlowerDignity

  override def combine(card: Card): Combination = {
    card match {
      case Card(this.lowerCardDignity, _) if this.length < 4 => this.copy(lowCard = card, length = this.length + 1)
      case Card(this.lowerCardDignity, _) if this.length >= 4 => Straight(highCard)
      case card: Card=>this
    }
  }
}
case class PossibleFlush(highCard: Card, lowCard:Card, length:Int) extends Combination{
  override def powerCombination: Dignity = -1

  override def combine(card: Card): Combination = card match {
    case Card(dignity,highCard.suit) if (this.length < 4 && dignity!=1) =>this.copy(lowCard=card,length = this.length+1)
    case Card(dignity,highCard.suit) if (this.length == 4 && dignity!=1) =>Flush(highCard)
    case card: Card=>this
  }
}
case class PossibleStraightFlush(highCard :Card, lowCard: Card, length: Int) extends Combination {
  override def powerCombination: Dignity = -1

  val lowerCard: Dignity =this.lowCard.getlowerDignity
  override def combine(card: Card): Combination = {
    card match {
      case Card(this.lowerCard, lowCard.suit) if (this.length < 4) => Comb(this, PossibleStraightFlush(highCard, card, length + 1))
      case Card(this.lowerCard,lowCard.suit) if (this.length == 4) => StraightFlush (highCard)
      case Card(this.lowerCard, _) if (this.length < 4) => Comb(this, PossibleStraight(highCard, card, length + 1))
      case Card(this.lowerCard, _) if (this.length == 4) => Straight(highCard)
      case Card(_, highCard.suit) if (this.length < 4) => Comb(this, PossibleFlush(highCard, card, length + 1))
      case Card(_, highCard.suit) if (this.length == 4) => Flush(highCard)
      case card: Card=>this

    }
  }
}