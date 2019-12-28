package Combinations

/**
 *  object contains description  class Card and card characteristic
 */
package object CardCharacteristic
{
  type Rank = Int
  type Suit = Char

  /**
   * implicit method for  cast from card to Combination:> HighCard
   * @param card
   * @return HighCard(card)
   */
  implicit def toHighCard(card: Card): HighCard =HighCard(card)

  /**
   * implicit Ordering for compare two cards
   */
  implicit val CardOrdering: Ordering[Card] = (x: Card, y: Card) => x.rank compareTo y.rank
  /**
   *
   */
  implicit val CombinationOrdering: Ordering[Combination] = (x: Combination, y: Combination) => x.powerCombination compareTo y.powerCombination


  /**
   *  class stores information about one card on the table
   * @param rank - card the rank
   * @param suit - card suit (one of "h", "d", "c", "s")
   */
  case class Card(rank: Rank, suit: Suit) {
    /**
     * value with less rank
     */
    val getLowerRank: Rank = rank-1

    override def toString: String = s"${rank match{
      case 10=>"T"
      case 11=>"J"
      case 12=>"Q"
      case 13=>"K"
      case 14=>"A"
      case rank=>rank
    }
    }$suit"
  }
}
