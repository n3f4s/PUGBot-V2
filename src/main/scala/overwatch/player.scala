import requests._
import io.circe.JsonObject

package Overwatch {
  object Account {
    enum AccountError {
      case QueryError(code: Int)
      case ParsingError(e: io.circe.Error)
    }
    private def make_query(btag: BattleTag): Either[Int, String] = {
      val url_btag = s"${btag.name}-${btag.discriminator}"
      val url = s"https://ow-api.com/v1/stats/pc/EU/$url_btag/profile"
      val obj_url = java.net.URI.create(url).toASCIIString
      val resp = (requests get (obj_url,
                                params = Map("user-agent" -> "Mozilla/5.0 (X11; Linux x86_64; rv:71.0) Gecko/20100101 Firefox/71.0")) )
      if(resp.statusCode != 200) {
        Left(resp.statusCode)
      } else {
        Right(resp.text)
      }
    }
    private def query_account_impl(btag: BattleTag) =
      make_query(btag) match {
        case Right(text) => {
          import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
          decode[Impl.Json.PlayerStats](text) match {
            case Right(v) => Right(v)
            case Left(err) => Left(AccountError.ParsingError(err))
          }
        }
        case Left(err) => {
          Left(AccountError.QueryError(err))
        }
      }

    def query_account(btag: BattleTag) = query_account_impl(btag).map(s => Account(btag, s))
    def from_json(content: String) = {
      import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
      decode[Impl.Json.PlayerStats](content) match {
        case Right(v) => {
          Right(v).map {inner => {
                          Account(BattleTag.from_string(inner.name).get, inner)
                        }}
        }
        case Left(err) => Left(AccountError.ParsingError(err))
      }
    }
  }
  case class SkillRating(val max: Int, val current: Int)
  case class AccountRating(val tank: SkillRating, val damage: SkillRating, val support: SkillRating)
  class Account(btag: BattleTag, inner: Impl.Json.PlayerStats) {
    // FIXME TODO: update max SR
    private var tank_max_sr = 0
    private var dmge_max_sr = 0
    private var supp_max_sr = 0

    def battle_tag = btag
    def is_private = inner.`private`
    def skill_rating: Option[AccountRating] = {
      var curr_tank = 0
      var curr_dps = 0
      var curr_sup = 0
      inner.ratings.foreach { rating => rating match {
                               case Impl.Json.Rating(sr, _, "Tank", _) => curr_tank = sr
                               case Impl.Json.Rating(sr, _, "Damage", _) => curr_dps = sr
                               case Impl.Json.Rating(sr, _, "Support", _) => curr_sup = sr
                               case Impl.Json.Rating(_, _,_, _) => {}
                         } }
      Some(AccountRating(
             SkillRating(tank_max_sr, curr_tank),
             SkillRating(dmge_max_sr, curr_dps),
             SkillRating(supp_max_sr, curr_sup),
             ))
    }
    def to_json = {
      import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
      inner.asJson.noSpaces
    }
  }

  package Impl {
    package Json {
      package Complete {
        case class Awards(cards: Int, medals: Int, medalsBronze: Int, medalsSilver: Int, medalsGold: Int)
        case class Games(played: Int, won: Int)
        // case class GeneralStats(
        //   assists: Map[String, Int],
        //   average: Map[String, String], // String or Int, fix when neede
        //   best: Map[String, String], // String or Int, fix when neede
        //   combat: Map[String, String], // String or Int, fix when neede
        //   deaths: Option[Int],
        //   heroSpecific: Option[Map[String, String]], // Don't know what that is
        //   game: GamesPlayed,
        //   matchAwards: Awards,
        //   miscellaneous: Map[String, Int],
        // )
        // FIXME: better handling of stats
        case class ModeStats(awards: Awards, games: Games, careerStats: Map[String, JsonObject], topHeroes: Map[String, Map[String, String]])
        case class Rating(level: Int, rankIcon: String, role: String, roleIcon: String)

        case class PlayerStats(
          competitiveStats: Option[ModeStats],
          endorsement: Int,
          endorsementIcon: String,
          gamesWon: Int,
          icon: String,
          level: Int,
          levelIcon: String,
          name: String,
          prestige: Int,
          prestigeIcon: String,
          `private`: Boolean,
          quickPlayStats: Option[ModeStats],
          rating: Int,
          ratingIcon: String,
          ratings: List[Rating]
        )
      }
      case class Awards(cards: Int, medals: Int, medalsBronze: Int, medalsSilver: Int, medalsGold: Int)
      case class Games(played: Int, won: Int)
      case class ModeStats(awards: Awards, games: Games)
      case class Rating(level: Int, rankIcon: String, role: String, roleIcon: String)
      case class PlayerStats(
        competitiveStats: Option[ModeStats],
        endorsement: Int,
        endorsementIcon: String,
        gamesWon: Int,
        icon: String,
        level: Int,
        levelIcon: String,
        name: String,
        prestige: Int,
        prestigeIcon: String,
        `private`: Boolean,
        quickPlayStats: Option[ModeStats],
        rating: Int,
        ratingIcon: String,
        ratings: List[Rating]
      )
    }
  }
}
