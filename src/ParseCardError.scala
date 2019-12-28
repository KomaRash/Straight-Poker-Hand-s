/**
 * class for exception uncorrected parsing input data
 * @param errorRank - bad character
 */
class ParseCardError(errorRank: Char) extends Error{
  override def getMessage: String = s"Incorrect value for card rank ($errorRank), the rank one of symbol from [A, K, Q, J, T] and [2-9]"

}
