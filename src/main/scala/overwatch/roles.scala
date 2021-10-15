package Overwatch {
  enum Role(val name: String, val icon: String) {
    case Tank extends Role("Tank", "https://static.playoverwatch.com/img/pages/career/icon-tank-8a52daaf01.png")
    case Damage extends Role("Damage", "https://static.playoverwatch.com/img/pages/career/icon-offense-6267addd52.png")
    case Support extends Role("Support", "https://static.playoverwatch.com/img/pages/career/icon-support-46311a4210.png")
  }
  object Role {
      def from_str(role: String): Option[Role] = {
        if(role == Role.Tank.name) {
          Some(Role.Tank)
        } else if(role == Role.Support.name) {
          Some(Role.Support)
        } else if(role == Role.Damage.name) {
          Some(Role.Damage)
        } else {
          None
        }
      }
  }
}
