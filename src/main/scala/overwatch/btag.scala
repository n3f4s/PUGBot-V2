package Overwatch {
  case class BattleTag(val name: String, val discriminator: String) {

    require((BattleTag is_valid full), (() ⇒ s"Invalid battletag ${full}"))

    override def toString = s"${name}#${discriminator}"
    def full = toString
    override def hashCode = toString.hashCode
    override def equals(x: Any): Boolean = x match {
      case b: BattleTag ⇒ b.full == full
    }
  }
  object BattleTag {
    def from_string(str: String) = {
      val astr = str split "#"
      astr match {
        case Array(name, disc) => Some(BattleTag(name, disc))
        case _ => None
      }
    }
    def is_valid(str: String): Boolean = {
      import scala.util.matching.Regex
      val pattern = "[a-zA-Z0-9]+#[0-9]+".r
      (pattern findFirstMatchIn str).isDefined
    }
  }
}
