import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ Behavior, ActorRef }

import org.slf4j.LoggerFactory
import discord4j.core.`object`.entity.User
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import discord4j.common.util.Snowflake
import Overwatch.BattleTag
import discord4j.core.GatewayDiscordClient
import scala.io.{ Source => IOSource }

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.actor.typed.ActorRef
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{ Sink, Source }
import akka.stream.typed.scaladsl.ActorSource
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.stream.Materializer
import akka.actor.typed.ActorSystem

import com.hubspot.jinjava

import collection.JavaConverters._
import scala.concurrent.Future

package Backend {

  /* TODO backend:
   1. make the communication between the front/back
   */

  class Player(disc_acc: User, owacc: ArrayBuffer[Overwatch.Account] = ArrayBuffer[Overwatch.Account]()) {
    private val ow_accs: ArrayBuffer[Overwatch.Account] = owacc
    def battletags = ow_accs.map(_.battle_tag.full)
    def discord_name = disc_acc.getUsername
    def discord_id = disc_acc.getId
    def add_account(btag: BattleTag): Either[Overwatch.Account.AccountError, Overwatch.BattleTag] = {
      Overwatch.Account.query_account(btag) match {
        case Right(acc) => {
          ow_accs += acc
          Right(acc.battle_tag)
        }
        case Left(e) => { Left(e) }
      }
    }
    def to_jsonable = Player.PlayerJson(disc_acc.getId.asString, (ow_accs map (_.to_json) toArray))
  }
  object Player {
    case class PlayerJson(did: String, ow: Array[String])
  }

  class Lobby(name: String) {
    val players: mutable.Set[Snowflake] = mutable.Set()
  }

  object Backend {
    enum SSEMessage {
      case JSON(str: String)
      case Complete
      case Fail(e: Exception)
    }
    private val logger = LoggerFactory.getLogger("backend")
    private lazy val renderer = jinjava.Jinjava()
    def render_index(values: Map[String, String]): String = {
      val content = IOSource.fromResource("index.html", getClass.getClassLoader).mkString
      renderer.render(content, values.asJava)
    }
    def routes(implicit system: ActorSystem[Nothing]) = {
      import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._

      val source: Source[ServerSentEvent, ActorRef[SSEMessage]] =
        ActorSource.actorRef[SSEMessage](
          completionMatcher = { case SSEMessage.Complete => },
          failureMatcher = { case SSEMessage.Fail(ex) => ex },
          bufferSize = 8,
          overflowStrategy = OverflowStrategy.fail
        ).map{ case SSEMessage.JSON(s) => ServerSentEvent(s) }

      val ref = source.run()
      (concat(
         pathSingleSlash {
           get {
              // FIXME: see what to send and how to send (pass in route args?)
             complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                                 render_index(Map()))) }
             },
             path("event") { // FIXME check URL
               complete {
                 source
               }
             }),
       ref
      )
    }
    def add_btag_player(players: ArrayBuffer[Player], player: User, btag: Overwatch.BattleTag) =
      players
        .filter(_.discord_id == player.getId)
        .map(_.add_account(btag))
        .foreach { res =>
          import Overwatch.Account.AccountError
          res match {
            case Right(btag) => player
                .getPrivateChannel
                .flatMap(_.createMessage(s"${btag.full} accepted"))
                .doOnNext(_ => save_players(players))
                .doOnNext(_ => logger.info(s"Battletag ${btag.full} successfully added to player ${player.getUsername}"))
                .block
            case Left(AccountError.QueryError(404)) => {
              player
                .getPrivateChannel
                .flatMap(_.createMessage(s"Battle tag ${btag.full} not found check the spelling and try again"))
                .doOnNext(_ => logger.info(s"Battletag ${btag.full} unsuccessfully added to player ${player.getUsername}: HTTP code 404 received"))
                .block
            }
            case Left(AccountError.QueryError(code)) => {
              player
                .getPrivateChannel
                .flatMap(_.createMessage(s"There was an error when querrying data for ${btag.full}"))
                .doOnNext(_ => logger.info(s"Battletag ${btag.full} unsuccessfully added to player ${player.getUsername}: HTTP code ${code} received"))
                .block
            }
            case Left(AccountError.ParsingError(e)) => {
              player
                .getPrivateChannel
                .flatMap(_.createMessage(s"There was an error when parsing data for ${btag.full}"))
                .doOnNext(_ => logger.info(s"Battletag ${btag.full} unsuccessfully added to player ${player.getUsername}: error while parsing JSON: ${e.toString}"))
                .block
            }
          } }

    def save_players(ary: ArrayBuffer[Player]) = {
      import java.io._
      import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
      val filename = "players.json"
      logger.info(s"Saving players to ${filename}")
      val content = (ary
                       .map(_.to_jsonable)
                       .toArray)
      val pw = new PrintWriter(new File(filename))
      pw.write(content.asJson.noSpaces)
      pw.close
    }
    def load_players(filename: String, client: GatewayDiscordClient): mutable.ArrayBuffer[Player] = {
      import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
      logger.info(s"Loading players from ${filename}")
      if (new java.io.File(filename).exists) {
        val sn = IOSource
          .fromFile(filename)
          .getLines
          .mkString
        val res = decode[Array[Player.PlayerJson]](sn)
                     .map{ _.map{ case Player.PlayerJson(id, pj) => {
                                    val players = pj
                                                     .map(Overwatch.Account.from_json(_))
                                                     .map{ case Right(e) => e  }
                                    val user = client.getUserById(Snowflake.of(id)).block
                                    Player(user, ArrayBuffer(players: _*))
                                 }
                          }
                     }
        res match {
          case Right(ary) => ArrayBuffer(ary: _*)
          case Left(_) => ArrayBuffer()
        }
      } else {
        mutable.ArrayBuffer()
      }
    }
    def apply(ref: ActorRef[ActorMessage]): Behavior[ActorMessage] = {
      (Behaviors setup { context =>

         logger.info("Starting the backend actor")
         var lobbies = Bot.Lobbies() // FIXME have a 3rd actor for DB?
         val players: ArrayBuffer[Player] = ArrayBuffer()
         val lobbies_player: mutable.Map[String, Lobby] = mutable.Map()
         implicit val system = context.system
         // implicit val ec = system.dispatchers
         implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

         val (route, ref) = routes

         // val f = for { bindingFuture <- Http().bindAndHandleAsync(route, "localhost", 63081)
         val f = for { bindingFuture <- Http().newServerAt("localhost", 63081).bind(route)
                       waitOnFuture  <- Future.never }
                 yield waitOnFuture

         (Behaviors receive { (context, message) =>
           message match {
             case ActorMessage.PlayerJoinLobby(player, lobby_name, _) => {
               logger.info(s"Player ${player.getUsername} joined lobby ${lobby_name}")
               lobbies_player.get(lobby_name).foreach(l => l.players += player.getId)
               Behaviors.same
             }
             case ActorMessage.PlayerLeaveLobby(player, lobby_name, _) => {
               logger.info(s"Player ${player.getUsername} left lobby ${lobby_name}")
               lobbies_player.get(lobby_name).foreach(l => l.players -= player.getId)
               Behaviors.same
             }
             case ActorMessage.RegisterPlayer(player, btag, _) => {
               logger.info(s"Player ${player.getUsername} registered battletag ${btag.full}")
               if (players.exists(_.discord_id == player.getId)) {
                 logger.debug(s"Adding battletag ${btag.full} to ${player.getUsername}")
                 add_btag_player(players, player, btag)
               } else {
                 logger.info(s"Registering new player ${player.getUsername} with battletag ${btag.full}")
                 val tmp = Player(player)
                 import Overwatch.Account.AccountError
                 tmp.add_account(btag) match {
                   case Right(btag) => player
                       .getPrivateChannel
                       .flatMap(_.createMessage(s"${btag.full} accepted"))
                       .doOnNext(_ => save_players(players))
                       .doOnNext(_ => logger.info(s"Battletag ${btag.full} successfully added to player ${player.getUsername}"))
                       .block
                   case Left(AccountError.QueryError(404)) => {
                     player
                       .getPrivateChannel
                       .flatMap(_.createMessage(s"Battle tag ${btag.full} not found check the spelling and try again"))
                       .doOnNext(_ => logger.info(s"Battletag ${btag.full} unsuccessfully added to player ${player.getUsername}: HTTP code 404 received"))
                       .block
                   }
                   case Left(AccountError.QueryError(code)) => {
                     player
                       .getPrivateChannel
                       .flatMap(_.createMessage(s"There was an error when querrying data for ${btag.full}"))
                       .doOnNext(_ => logger.info(s"Battletag ${btag.full} unsuccessfully added to player ${player.getUsername}: HTTP code ${code} received"))
                       .block
                   }
                   case Left(AccountError.ParsingError(e)) => {
                     player
                       .getPrivateChannel
                       .flatMap(_.createMessage(s"There was an error when parsing data for ${btag.full}"))
                       .doOnNext(_ => logger.info(s"Battletag ${btag.full} unsuccessfully added to player ${player.getUsername}: error while parsing JSON: ${e.toString}"))
                       .block
                   }
                 }
                 players += tmp
               }
               save_players(players)
               Behaviors.same
             }
             case ActorMessage.Die(_) ⇒ {
               logger.info("Stopping backend")
               Behaviors.stopped
             }
             case ActorMessage.NewLobby(l) => {
               logger.info(s"Adding lobby ${l.name}")
               lobbies.add_lobby(l)
               Behaviors.same
             }
             case ActorMessage.TriggerLoad(c) => {
               players ++= load_players("players.json", c)
               Behaviors.same
             }
             case ActorMessage.DebugReqMsg(msg) => {
               val str = players.map(p => {
                                       val name = p.discord_name
                                       val btags = p.battletags.mkString(",")
                                       s"**${name}**: ${btags}"
                                     })
                 .mkString("\n")
               msg.editReply(s"__**_Players registered_**__:\n${str}").block
               Behaviors.same
             }
             case ActorMessage.Ping(_) => {Behaviors.same}
             case ActorMessage.None(_) => {Behaviors.same}
             // case _ => {
             //   (logger debug "Recieved a message")
             //   Behaviors.same
             // }
           }
         })
       })
    }
  }
}
