import CardCharacteristic.Dignity

//Выбор структуры классов с множествами -

trait Combination{
  def |(combination: Combination):Combination = combination match {
    case combination:PossibleLinearCombination  => this
    case NoCard=>this
    case _=> compareCombination(combination)
  }
  def getHighDignity: Dignity= ???
  def compareCombination(combination: Combination):Combination= {

    combination.powerCombination match {
      case power if power>this.powerCombination=>combination
      case power if power<this.powerCombination =>this
      case _ =>compareEqualsCombination(combination)
    }

  }
  protected def compareEqualsCombination(combination:Combination):Combination=if(combination.asInstanceOf[this.type].getHighDignity > this.getHighDignity)
    combination
  else
    this
  def powerCombination:Int= ???
  def combine(card:Card):Combination= this
  def getCombination:List[Combination]=List(this)

}

case class FourOfKind(dignity:Dignity) extends Combination{
  override def powerCombination: Int = 7
  override def getHighDignity: Dignity=dignity

}
case class FullHouse(pair: Pair,threeOfKind: ThreeOfKind) extends Combination{
  override def powerCombination: Int = 6
 override def compareEqualsCombination(combination: Combination): Combination =this.threeOfKind.compareCombination(combination.asInstanceOf[FullHouse].threeOfKind)
 match {
    case threeOfKind if this.threeOfKind equals threeOfKind =>this
    case threeOfKind :ThreeOfKind =>combination
    case Comb(leftCombination, rightCombination)=>
      if(this.pair.compareCombination(combination.asInstanceOf[FullHouse].pair) equals(this.pair)) this else combination
  }

}
case class ThreeOfKind(firstCard:Card,secondCard:Card,thirdCard:Card) extends Combination{
  override def powerCombination: Int = 3

  override def getHighDignity: Dignity = firstCard.dignity
  override def combine(card: Card): Combination = card match {
    case Card(firstCard.dignity, _)=>FourOfKind(firstCard.dignity)
    case _=>this
  }
}
case class TwoPairs(firstPair: Pair,secondPair: Pair ) extends Combination{
  override def powerCombination: Int = 2
  override protected def compareEqualsCombination(combination: Combination): Combination = {
    val twoPairs=combination.asInstanceOf[TwoPairs]
    def  F(firstPair:Pair,secondPair:Pair)(elseValue:Combination):Combination=
      {
        val combination=firstPair.compareEqualsCombination(secondPair)
        if(combination.equals(firstPair)) this
      else if(combination equals secondPair)twoPairs
      else elseValue
      }
    F(this.getHighPair,twoPairs.getHighPair)(F(this.getLowPair,twoPairs.getLowPair)(this))
    }

  override def combine(card: Card): Combination = card match {
    case Card(secondPair.firstCard.dignity,_)=>FullHouse(pair = firstPair,threeOfKind=(secondPair combine card ).asInstanceOf[ThreeOfKind])
    case Card(firstPair.firstCard.dignity,_)=>FullHouse(secondPair,(firstPair combine card ).asInstanceOf[ThreeOfKind])
    case card=>this
  }

  private def getHighPair:Pair=if (this.firstPair.firstCard > this.secondPair.firstCard) firstPair else secondPair
  private def getLowPair:Pair=if (this.firstPair.firstCard > this.secondPair.firstCard) secondPair else firstPair
}
case class Pair(firstCard:Card,secondCard:Card) extends Combination{
  override def powerCombination: Int = 1
  override def getHighDignity: Dignity = firstCard.dignity
  override def combine(card: Card): Combination = card match{
    case Card(firstCard.dignity,_) => ThreeOfKind(firstCard,secondCard,card)
    case _=>this
  }


}
case class HighCard(card: Card) extends Combination{
  override def powerCombination: Int = 0
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
  override def powerCombination: Int = -1
  override def combine(card: Card): Combination = Comb(HighCard(card),NoCard)
}

case class Comb(leftCombination: Combination, rightCombination: Combination) extends Combination{
  override def combine(card: Card): Combination = Comb(leftCombination.combine(card),rightCombination.combine(card))
  override def getCombination: List[Combination] = List(leftCombination,rightCombination) flatMap { combination=>combination.getCombination}
}


