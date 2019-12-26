import CardCharacteristic.Dignity

object Main extends App {
  implicit val CardOrdering: Ordering[Card] = (x: Card, y: Card) => y.dignity compareTo x.dignity

  implicit def fromCharToDignity(card:Char): List[Dignity] = card match {
    case x if x >= '1' & x <= '9' => List(x.toInt-'0')
    case 'T' => List(10)
    case 'J' => List(11)
    case 'Q' => List(12)
    case 'K' => List(13)
    case 'A' => List(14,1)
  }

  def SeqSymbolToCard(value:String): List[Card] =value.
    grouped(2).
    toList.flatMap(card=>fromCharToDignity(card.head).map(dignity=>Card(dignity,card.last))).sorted

  val q=SeqSymbolToCard("4a3a2a5aAc2v2c2d")
  def x(combination:Combination,list:List[Card]):Combination={

    if(list.isEmpty)
    combination
    else x(combination.combine(list.head),list.tail)

  }
  val a=x(NoCard,q)
  val b=a.getCombination.filter{
    case NoCard => false
    case _=>true
  }.reduce(_ | _)
  println(b)
  //b.foreach(println)
}
