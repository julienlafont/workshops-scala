import models._

// Specs format (WordSpec here) http://www.scalatest.org/user_guide/selecting_a_style
// Matchers documentation: http://www.scalatest.org/user_guide/using_matchers

class DnaToolsSpec extends UnitTest {

  "Nucleobase factory" should {
    "Safely parse base from String" in {
      Base.get('A') === Some(A)
      Base.get('T') === Some(T)
      Base.get('C') === Some(C)
      Base.get('G') === Some(G)
    }

    "Reject invalid chars by returning None" in {
      Base.get('B') === None
    }

    "Reject invalid chars by returning None (type-checked)" in {
      forAll { (c: Char) =>
        whenever (c != 'A' & c != 'T' & c != 'C' & c != 'G') {
          Base.get(c) === None
        }
      }
    }
  }

}
