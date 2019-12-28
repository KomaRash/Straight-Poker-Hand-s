package Combinations

import Combinations.CardCharacteristic.{Card, Rank}

/**
 * upper type for all Poker Combination
 */
trait Combination{
  /**
   * getting highest rank of combination
   * @return high rank combination
   */
  def getHighRank: Rank= ???

  /**
   * getting power of combination
   * @return value used to identify the strongest combination
   */
  def powerCombination:Int= ???

  /**
   * combine Combination and Card and return new node of Three.
   * @param card used for combine with combination
   * @return new Node Three considering param card. if Combination and Card don't combine return  this Combination.
   */
  def combine(card:Card):Combination= this
  /**
   * Make List[Combination] from Tree
   * @return List[Combination] with all combination
   */
  def getCombinationList:List[Combination]=List(this)
//
  /**
   * combines two combinations of cards and
   * if possible it returns a new combination
   * @param combination second combination of cards for combines
   * @return new combination
   */
  def combine(combination: Combination):Combination = this

}

/**
 * class represent Poker combination Four of Kind (Four cards of the same rank)
 * @param rank - rank Four of Kind
 */
case class FourOfKind(rank:Rank) extends Combination{
  override def powerCombination: Int = 7*10000+getHighRank
  override def getHighRank: Rank=rank

}

/**
 * class represent  Poker combination a Full House  (consist of combination Three of kind any rank and  pair of  another rank).
 * @param threeOfKind -combination three of kind
 * @param pair - combination pair
 */
case class FullHouse(threeOfKind: ThreeOfKind,pair: Pair) extends Combination{
  override def powerCombination: Int = 6*10000+threeOfKind.getHighRank*100+pair.getHighRank
}

/**
 * class represent  Poker combination Three of Kind (Three cards of the same rank)
 * @param firstCard first Card consisting in combination
 * @param secondCard second Card consisting in combination
 * @param thirdCard third Card consisting in combination
 */
case class ThreeOfKind(firstCard:Card,secondCard:Card,thirdCard:Card) extends Combination{
  override def powerCombination: Int = 3*10000+getHighRank

  /**combines two combinations of cards and
   * if the pair parameter of another rank returns a full house else this combination
   * @param combination second combination of cards for combines
   * @return new combination
   */
  override def combine(combination: Combination): Combination = combination match {
    case pair: Pair if(pair.getHighRank!=this.getHighRank)=> FullHouse(pair = pair, threeOfKind = this)
    case _ =>this
  }
  override def getHighRank: Rank = firstCard.rank
  override def combine(card: Card): Combination = card match {
    case Card(firstCard.rank, _)=>FourOfKind(firstCard.rank)
    case _=>this
  }
}

/**
 * class represent  Poker combination a Two Pair  (consist two pair of  any different rank).
 * @param firstPair - first pair consisting in combination
 * @param secondPair - second pair consisting in combination
 */
case class TwoPairs(firstPair: Pair,secondPair: Pair ) extends Combination{
  override def powerCombination: Int = 2*10000+ {
    if (firstPair.getHighRank.compareTo(secondPair.getHighRank) equals firstPair.getHighRank)
      firstPair.getHighRank * 100 + secondPair.getHighRank
    else
      secondPair.getHighRank * 100 + firstPair.getHighRank
  }
  override def combine(card: Card): Combination = card match {
    case Card(secondPair.firstCard.rank,_)=>FullHouse(pair = firstPair,threeOfKind=(secondPair combine card ).asInstanceOf[ThreeOfKind])
    case Card(firstPair.firstCard.rank,_)=>FullHouse((firstPair combine card ).asInstanceOf[ThreeOfKind],secondPair)
    case card=>this
  }
}

/**
 * class represent Poker combination a Pair  (consist two card any rank).
 *
 * @param firstCard  -first card consisting in combination
 * @param secondCard -second card consisting in combination
 */
case class Pair(firstCard:Card,secondCard:Card) extends Combination{
  override def powerCombination: Int = 1*10000+firstCard.rank
  override def getHighRank: Rank = firstCard.rank
  override def combine(card: Card): Combination = card match{
    case Card(firstCard.rank,_) => ThreeOfKind(firstCard,secondCard,card)
    case _=>this
  }
  override def combine(combination: Combination): Combination = combination match {
    case pair: Pair if( pair.getHighRank != this.getHighRank) => TwoPairs(firstPair = this,secondPair= pair)
    case threeOfKind: ThreeOfKind if(threeOfKind.getHighRank !=this.getHighRank)=>FullHouse(pair = this,threeOfKind = threeOfKind)
    case _=>this
  }


}

/**
 * class represent Poker combination a HighCard(consist HighCard).
 * @param card - high Card
 */
case class HighCard(card: Card) extends Combination{
  override def powerCombination: Int = card.rank

  val lowerCardRank: Rank = this.card.getLowerRank
  override def combine(nextCard: Card): Combination = {
     nextCard match {
       case Card(this.lowerCardRank, card.suit) => Node(this, PossibleStraightFlush(card, nextCard, 2))
      case Card(this.lowerCardRank, _) => Node(this, PossibleStraight(card, nextCard, 2))
       case Card(_, card.suit) => Node(this, PossibleFlush(card, nextCard, 2))
       case Card(card.rank, _) => Node(this, Pair(card, nextCard))
       case _ => this
    }
  }
}

/**
 *  empty object in Tree
 */
case object NoCard extends Combination {
  override def powerCombination: Int = -1

  override def combine(card: Card): Combination = card match {
    case Card(1,suit)=>this
    case _=>Node (HighCard(card),NoCard)

  }
}

/**
 * class represent node tree with two branch
 * @param leftCombination -left branch
 * @param rightCombination-right branch
 */
case class Node(leftCombination: Combination, rightCombination: Combination) extends Combination{
  override def combine(card: Card): Combination = Node(leftCombination.combine(card),rightCombination.combine(card))
  override def getCombinationList: List[Combination] = List(leftCombination,rightCombination) flatMap { combination=>combination.getCombinationList}
}


