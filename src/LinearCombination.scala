import CardCharacteristic.Dignity

sealed  class LinearCombination(highCard:Card) extends Combination{
  override def compareCombination(combination: Combination): Combination = combination match {
    case combination if(combination.powerCombination>this.powerCombination) =>combination
    case combination if(combination.powerCombination<this.powerCombination) =>this
    case combination:LinearCombination if(combination.getHighDignity >= this.getHighDignity)=>combination
    case _=>this
  }
  override def getHighDignity: Dignity = highCard.dignity

}

case class StraightFlush(highCard:Card) extends LinearCombination(highCard:Card) {
  override def powerCombination: Int =8

}
case class Flush(highCard:Card) extends LinearCombination(highCard:Card){
  override def powerCombination: Int = 5
}
case class Straight(highCard:Card) extends LinearCombination(highCard:Card){
  override def powerCombination: Int = 4
}

