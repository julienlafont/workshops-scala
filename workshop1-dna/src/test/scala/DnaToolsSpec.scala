import models._

// Specs format (WordSpec here) http://www.scalatest.org/user_guide/selecting_a_style
// Matchers documentation: http://www.scalatest.org/user_guide/using_matchers

class DnaToolsSpec extends UnitTest {

  "Nucleobase factory" should {
    "Safely parse base from String" in {
      Base.get('A') shouldEqual Some(A)
      Base.get('T') shouldEqual Some(T)
      Base.get('C') shouldEqual Some(C)
      Base.get('G') shouldEqual Some(G)
    }

    "Reject invalid chars by returning None" in {
      Base.get('B') shouldEqual None
    }

    "Reject invalid chars by returning None (type-checked)" in {
      forAll { (c: Char) =>
        whenever (c != 'A' & c != 'T' & c != 'C' & c != 'G') {
          Base.get(c) shouldEqual None
        }
      }
    }
  }

  "DNA tools" should {

    "calculate the complementary of a DNA sequence" in {
      DnaTools.complementary(Seq()) shouldEqual Seq()
      DnaTools.complementary(Seq(A, T, C, G)) shouldEqual Seq(T, A, G, C)
      DnaTools.complementary(Seq(A, A, G, C, T, T, G, A)) shouldEqual Seq(T, T, C, G, A, A, C, T)
    }

    "Count the number of each base" in {
      DnaTools.countBases(Seq()) shouldEqual Map(A -> 0, T -> 0, C -> 0, G -> 0)
      DnaTools.countBases(Seq(A, T, C, G)) shouldEqual Map(A -> 1, T -> 1, C -> 1, G -> 1)
      DnaTools.countBases(Seq(A, A, G, C, T, T, G, A)) shouldEqual Map(A -> 3, T -> 2, C -> 1, G -> 2)
    }

    "Check if the `subsequence` is contained in the main DNA sequence" in {
      DnaTools.contains(Seq(), Seq()) shouldEqual true
      DnaTools.contains(Seq(), Seq(A)) shouldEqual false
      DnaTools.contains(Seq(A, A, G, C, T, T, G, A), Seq(T, T, G)) shouldEqual true
      DnaTools.contains(Seq(A, A, G, C, T, T, G, A), Seq(A, T, G)) shouldEqual false
    }

    "Insert a subsequence in a DNA sequence" in {
      DnaTools.insertSubsequence(Seq(), Seq(), 0) shouldEqual Seq()
      DnaTools.insertSubsequence(Seq(A, G), Seq(T, C), 1) shouldEqual Seq(A, T, C, G)
    }

    "Count the differences in two DNA sequences" in {
      DnaTools.hammingDistance(Seq(A, T, C, G), Seq(A, T, C, G)) shouldEqual 0
      DnaTools.hammingDistance(Seq(A, T, C, G), Seq(A, T)) shouldEqual 0
      DnaTools.hammingDistance(Seq(A, T, C, G), Seq()) shouldEqual 0
      DnaTools.hammingDistance(Seq(A, T, C, G), Seq(G, C, T, A)) shouldEqual 4
      DnaTools.hammingDistance(Seq(A, T, C, G), Seq(A, C, C, A)) shouldEqual 2
    }

    "Return the positions of the differences between two DNA sequences" in {
      DnaTools.basesDifferences(Seq(A, T, C, G), Seq(A, T, C, G)) shouldEqual Seq()
      DnaTools.basesDifferences(Seq(A, T, C, G), Seq(A, T)) shouldEqual Seq()
      DnaTools.basesDifferences(Seq(A, T, C, G), Seq()) shouldEqual Seq()
      DnaTools.basesDifferences(Seq(A, T, C, G), Seq(G, C, T, A)) shouldEqual Seq(0, 1, 2, 3)
      DnaTools.basesDifferences(Seq(A, T, C, G), Seq(A, C, C, A)) shouldEqual Seq(1, 3)
    }
  }

}
