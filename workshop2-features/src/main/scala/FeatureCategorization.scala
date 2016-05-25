case class BidWin(price: Double, categories: Seq[String])

class FeatureCategorization(allCategories: Seq[String], groups: Map[String, Seq[String]]) {

  def categorize(wins: Seq[BidWin]): Seq[BidWin] = {
    ???
  }

}

object Sample {
  val win1 = BidWin(1.0, Seq("IAB2-0"))
  val win2 = BidWin(2.0, Seq("IAB2-0", "finance"))
  val win3 = BidWin(3.0, Seq("IAB2-0", "IAB3-0", "IAB5-0"))
  val win4 = BidWin(4.0, Seq("IAB3-0", "IAB3-1", "IAB3-5", "sport"))
  val win5 = BidWin(5.0, Seq("IAB3-1", "IAB5-0", "IAB9-0", "moto"))

  val allWins = Seq(win1, win2, win3, win4, win5)

  val allCategories = Seq("IAB1-0", "IAB1-1", "IAB1-2", "IAB2-0", "IAB2-1", "IAB3-0",
    "IAB3-1", "IAB3-2", "IAB5-0", "IAB6-0", "IAB7-0",
    "finance", "sport", "sports", "leisure", "vacancy", "car")

  val groups = Map(
    "financial" -> Seq("IAB2-0", "finance"),
    "sports" -> Seq("IAB3-0", "IAB3-1", "IAB3-2", "sport", "sports"),
    "leisures" -> Seq("IAB5-0", "IAB6-0", "leisure", "vacancy"),
    "car" -> Seq("IAB1-0", "IAB1-1", "car")
  )
}