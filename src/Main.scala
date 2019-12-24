

object Main extends App {
//  implicit val CardOrdering: Ordering[Card] = (x: Card, y: Card) => y.dignity compareTo x.dignity

  implicit def fromCharToDignity(dignity: Char): Int = dignity match {
    case x if x >= '1' & x <= '9' => x.toInt-'0'
    case 'T' => 10
    case 'J' => 11
    case 'Q' => 12
    case 'K' => 13
    case 'A' => 14
  }

  def SeqSymbolToCard(value:String): List[Card] =value.
    grouped(2).
    toList.
    map(preCard  => Card(fromCharToDignity(preCard.head), preCard.last)).sortBy(_.dignity).reverse

  val q=SeqSymbolToCard("4a3a2a5a1c2v2c2d")
  def x(combination:Combination,list:List[Card]):Combination={
    if(list.isEmpty)
      combination
    else x(combination.combine(list.head),list.tail)

  }
  val a=x(NoCard,q)
  val b=a.getCombination.filter{
    case NoCard => false
    case _=>true
  }.reduce(_|_)
  println(b)
}
