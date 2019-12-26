
object CardCharacteristic
{
  type Dignity = Int
  type Suit = Char
  implicit def toHighCard(card: Card): HighCard =HighCard(card)

}/*
sealed trait Dignity
{
  def getNextCardDignity:Dignity= ???
  def getPrevDignity:Dignity= ???

}
case class Two() extends Dignity{
  override def getNextCardDignity: Dignity = Three()
  override def getPrevDignity: Dignity = Ace()

}
case class Three() extends Dignity{

    override def getNextCardDignity: Dignity = Four()
    override def getPrevDignity: Dignity = Two()


}
case class Four() extends Dignity{
  override def getNextCardDignity: Dignity = Five()
  override def getPrevDignity: Dignity = Three()
}

case class Five() extends Dignity{
  override def getNextCardDignity: Dignity = Six()
  override def getPrevDignity: Dignity = Four()
}
case class Six() extends Dignity{
  override def getNextCardDignity: Dignity = Seven()
  override def getPrevDignity: Dignity = Five()
}
case class Seven() extends Dignity{
  override def getNextCardDignity: Dignity = Eight()
  override def getPrevDignity: Dignity = Six()
}
case class Eight() extends Dignity{
  override def getNextCardDignity: Dignity = Nine()
  override def getPrevDignity: Dignity = Seven()
}
case class Nine() extends Dignity{
  override def getNextCardDignity: Dignity = Ten()
  override def getPrevDignity: Dignity = Eight()
}
case class Ten() extends Dignity{
  override def getNextCardDignity: Dignity = Jack()
  override def getPrevDignity: Dignity = Nine()
}
case class Jack() extends Dignity{
  override def getNextCardDignity: Dignity = Queen()
  override def getPrevDignity: Dignity = Ten()
}
case class Queen() extends Dignity{
  override def getNextCardDignity: Dignity = King()
  override def getPrevDignity: Dignity = Queen()
}
case class King() extends Dignity{
  override def getNextCardDignity: Dignity = King()
  override def getPrevDignity: Dignity = Eight()
}
case class Ace() extends Dignity{
  override def getNextCardDignity: Dignity = Two()
  override def getPrevDignity: Dignity = King()
}*/