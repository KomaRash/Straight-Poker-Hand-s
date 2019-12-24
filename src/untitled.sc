/*

type Dignity = Int
type Suit = Char

case class Card( dignity: Dignity, suit: Suit)

val  b="4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d  KdKs 9hJh".split(" ").
  toList match {
  case head::tail=>Some(head,tail)
  case _=>None
  }
  object tmp {





*/
/*
val A=for(x<-0.until(15)) yield {
  Card(x,'D')
}
A.sortBy()
*//*
def x(combination:Combination,list:List[Card]):Combination={
  if(list.isEmpty)
    combination
    else x(combination.combine(list.head),list.tail)

}
*/