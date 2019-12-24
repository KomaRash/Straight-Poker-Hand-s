
object CardCharacteristic
{
  type Dignity = Int
  type Suit = Char
  implicit def toHighCard(card: Card): HighCard =HighCard(card)

}