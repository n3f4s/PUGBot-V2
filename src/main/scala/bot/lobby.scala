import discord4j.core.`object`.entity.channel.VoiceChannel
import discord4j.common.util.Snowflake
import discord4j.core._

import io.circe.{ Decoder, Encoder, HCursor, Json }

package Bot {
  case class Lobby(val name: String,
              val waiting: VoiceChannel,
              val blue: VoiceChannel,
              val red: VoiceChannel) {
    val guild = waiting.getGuild.block
    def to_str = s"""Lobby __*${name}*__:
    🟣 Waiting room: *${waiting.getName}*
    🔴 Red team: *${red.getName}*
    🔵 Blue team: *${blue.getName}*
"""
    def to_json_c = LobbyJson(
      name,
      guild.getId.asString,
      waiting.getId.asString,
      blue.getId.asString,
      red.getId.asString,
    )
  }
  case class LobbyJson(
    val name: String,
    val guild: String,
    val waiting: String,
    val blue: String,
    val red: String
  ) {
    def validate(client: GatewayDiscordClient): Lobby = {
      val guild_t = (client getGuildById Snowflake.of(guild) block)
      Lobby(
        name,
        guild_t.getChannelById(Snowflake.of(waiting)).block.asInstanceOf[VoiceChannel],
        guild_t.getChannelById(Snowflake.of(blue)).block.asInstanceOf[VoiceChannel],
        guild_t.getChannelById(Snowflake.of(red)).block.asInstanceOf[VoiceChannel],
      )
    }
  }

  object Lobbies {
    def from_json(str: String, client: GatewayDiscordClient): Lobbies = {
      import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
      val lobbies = new Lobbies()
      decode[Array[LobbyJson]](str).foreach(l ⇒ l.foreach(l ⇒ lobbies.add_lobby(l.validate(client))))
      lobbies
    }
  }

    class Lobbies {
      import scala.collection.mutable.{ Map, Set }

      private val lobbies: Map[String, Lobby] = Map()
      private val lobbies_set: Set[Snowflake] = Set()
      private val waiting_to_lobby: Map[Snowflake, Lobby] = Map()
      private val red_to_lobby: Map[Snowflake, Lobby] = Map()
      private val blue_to_lobby: Map[Snowflake, Lobby] = Map()

      def list_lobbies = lobbies.values

      def add_lobby(lobby: Lobby) = {
        lobbies += (lobby.name -> lobby)
        lobbies_set += lobby.waiting.getId
        lobbies_set += lobby.red.getId
        lobbies_set += lobby.blue.getId
        waiting_to_lobby += (lobby.waiting.getId -> lobby)
        red_to_lobby += (lobby.red.getId -> lobby)
        blue_to_lobby += (lobby.blue.getId -> lobby)
      }
      def get_lobby(name: String): Option[Lobby] = lobbies.get(name)
      def remove_lobby(name: String) = {
        (lobbies get name) match {
          case None ⇒ {}
          case Some(lobby) ⇒ {
            lobbies -= name
            lobbies_set -= lobby.waiting.getId
            lobbies_set -= lobby.red.getId
            lobbies_set -= lobby.blue.getId
            waiting_to_lobby -= lobby.waiting.getId
            red_to_lobby -= lobby.red.getId
            blue_to_lobby -= lobby.blue.getId

          }
        }
      }
      def get_by_waiting_id(id: Snowflake): Option[Lobby] = {
        waiting_to_lobby get id
      }
      def get_by_red_id(id: Snowflake): Option[Lobby] = {
        red_to_lobby get id
      }
      def get_by_blue_id(id: Snowflake): Option[Lobby] = {
        blue_to_lobby get id
      }
      def is_pug_chan(id: Snowflake): Boolean = {
        lobbies_set contains id
      }
      def to_str = lobbies.values.map(_.to_str).mkString("\n")
      def to_json = {
        import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
        lobbies.values.toArray.map(_.to_json_c).asJson.noSpaces
      }
  }
}
