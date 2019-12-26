import CardCharacteristic.Suit

case class Card(dignity: Dignity, suit: Suit) {
  val getlowerDignity: Dignity = dignity.getPrevDignity
}