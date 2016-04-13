import models._

object DnaTools {

  type DNA = Seq[Base]

  /**
    * Try to parse a DNA sequence from String as a sequence of nucleobase
    */
  def optParseDNA(str: String): Option[DNA] = ???

  /**
    * Parse a DNA sequence from String as a sequence of nucleobase
    * @throws IllegalArgumentException if the sequence is not valid
    */
  def parseDNA(str: String): DNA = ???

  /**
    * Process the Hamming distance of two DNA sequences.
    *
    * The hamming distance is calculated by comparing two DNA strands
    * and counting how many of the nucleotides are different from their equivalent
    * in the other string.
    *
    * Note: The Hamming distance is only defined for sequences of equal length.
    * You must count the differences in the first N bases, where `N = min(dna1.size, dna2.size)`
    *
    *  Eg:
    *   - Distance ATCG & ATGG = 1
    *   - Distance ATCG & TAGC = 4
    *   - Distance TTAAT & TTAAGCA = 1
    *
    * @return the hamming distance of dna1 and dna2
    */
  def hammingDistance(dna1: DNA, dna2: DNA): Long = ???

  /**
    * Search the differences between two DNA sequences.
    *
    * Sames rules as the Hamming distance
    * @return The indices (0 based) of the differences between the two sequences
    */
  def basesDifferences(dna1: DNA, dna2: DNA): Seq[Int] = ???

  /**
    * Return the complementary sequences of a DNA sequence.
    *
    * Nucleobase A/T are complements of each other, as C and G.
    */
  def complementary(dna: DNA): DNA = ???

  /**
    * Count the number of each base in the DNA sequence
    */
  def countBases(dna: DNA): Map[Base, Int] = ???

  private val translationTableSource = """
     |FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG
     |TTTTTTTTTTTTTTTTCCCCCCCCCCCCCCCCAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGG
     |TTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGG
     |TCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAG
   """.trim.stripMargin

  /**
    * Translate a DNA sequence in 6 frames
    *
    * In genetics a reading frame is a way to divide a sequence of nucleotides (DNA bases) into a set of consecutive
    * non-overlapping triplets (also called codon).
    * Each of this triplets is translated into an amino-acid during a translation process to create proteins.
    *
    * Eg: AGGTGACACCGCAAGCCTTATATTAGC has 3 frames
    * Frame 1: AGG·TGA·CAC·CGC·AAG·CCT·TAT·ATT·AGC
    * Frame 2: A·GGT·GAC·ACC·GCA·AGC·CTT·ATA·TTA·GC
    * Frame 3: AG·GTG·ACA·CCG·CAA·GCC·TTA·TAT·TAG·C
    *
    * Translate all these frame with the "standard table of genetic code" (available in `translationTableSource`.
    * Line1 : translated tri-nucléotide. Line 2, 3 and 4 : the three elements of the tri-nucleotide
    *
    * Eg:
    *  - AGG = R
    *  - TGA = *
    *
    * Translations of the 3 frames defined above: R*HRKPYIS, GDTASLIL, VTPQALY*
    *
    * @return the 3 possible translations for a DNA sequence
    */
  def translate(dna: DNA): Seq[String] = ???

  /**
    * Count the longest streak (uninterrupted sequence) of each nucleobase in the given DNA sequence
    *
    * Eg: ATTTTAACCCCGCG
    * Returns: A->2, T->4, C->4, G->1
    *
    * @return Map of the longest streak by nucleobase
    */
  def longestSequences(dna: DNA): Map[Base, Int] = ???
}
