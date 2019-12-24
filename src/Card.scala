import CardCharacteristic.{Dignity, Suit}

case class Card(dignity: Dignity, suit: Suit) {
  val getlowerDignity:Dignity=dignity-1
}