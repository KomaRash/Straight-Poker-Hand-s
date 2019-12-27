package Combinations

package object CardCharacteristic
{
  type Dignity = Int
  type Suit = Char
  implicit def toHighCard(card: Card): HighCard =HighCard(card)

  case class Card(dignity: Dignity, suit: Suit) {
    val getlowerDignity: Dignity = dignity-1
  }
}
