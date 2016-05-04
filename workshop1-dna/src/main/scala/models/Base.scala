package models

sealed trait Base
case object A extends Base
case object T extends Base
case object G extends Base
case object C extends Base

object Base {

  /**
    * Try to parse a character as a valid nucleobase
    * Return the corresponding Base if valid (A, T, C, G), else return None
    */
  def get(char: Char): Option[Base] = {
    char match {
      case 'A' => Some(A)
      case 'T' => Some(T)
      case 'C' => Some(C)
      case 'G' => Some(G)
      case _ => None
    }
  }

  /**
    * Parse a character as a valid nucleobase
    * @throws IllegalArgumentException if the `char` parameter doesn't represent a valid nucleobase
    */
  def apply(char: Char): Base = get(char).getOrElse(throw new IllegalArgumentException(s"$char not found"))

}
