import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }

import discord4j.core.`object`.entity.User

import org.slf4j.LoggerFactory

/* TODO
 1- add logging (akka logging?)
 2- start v1

 TODO V1:
 2.2.0- test people moving VC
 2.2.3- check code formatting (esp chain calling)
 3- Make backend with persistence (list OW data, disc <-> OW and who's in which lobby)
 4- Make the website backend
 5- Check with the already existing frontend stuff
 6- Make the frontend work
 DONE V1
 1- Add logging
 1- Add config slash command
 2- Persistence for lobbies
 2.3- Logging
 2.1- Ask btag when someone join
 2.2- Save btag <-> disc id
 2.2.0- handle people having multiple btag
 2.2.2- disc -> btag persistence
 3- When joining ask bnet and send it to backend


 **V1 features:**
 - Basic player info in website (SR, role and hero played)
 - Basic balancing info in website (per role and team wise SR average)
 - Checkbox selector for role in website
 - Store current data for player
 - Persistence (disc id < = > btags, career profile data)
 - Handle joining, leaving lobbies
 - Handle moving in the different VC for a lobby

 */

enum Sender:
  case Bot
  case Backend

enum TeamColour:
  case Red
  case Blue

enum ActorMessage(val sender: Sender):
  case Ping(sender2: Sender) extends ActorMessage(sender2)
  case Die(sender2: Sender) extends ActorMessage(sender2)
  case None(sender2: Sender) extends ActorMessage(sender2)
  case PlayerJoinLobby(player: User, lobby_name: String, sender2: Sender) extends ActorMessage(sender2)
  case RegisterPlayer(player: User, btag: Overwatch.BattleTag, sender2: Sender) extends ActorMessage(sender2)
  case PlayerLeaveLobby(player: User, lobby_name: String, sender2: Sender) extends ActorMessage(sender2)
  case PlayerGoTeam(player: User, lobby_name: String, team: TeamColour, sender2: Sender) extends ActorMessage(sender2)
  case PlayerRejoinLobby(player: User, lobby_name: String, sender2: Sender) extends ActorMessage(sender2)
end ActorMessage

object Dispatch {
  private val logger = LoggerFactory.getLogger("Dispatch")
  def apply(): Behavior[ActorMessage] = {
    Behaviors.setup { context =>
      val mainThread = Thread.currentThread();
      logger.info("Starting dispatcher")
      val backend = context.spawn(Backend.Backend(context.self), "Backend")
      val bot = context.spawn(Bot.Bot(context.self), "Bot")
      context.watch(backend)
      context.watch(bot)
      logger.debug("Bot and backend started")
      Behaviors.receiveMessage { msg =>
        logger.debug("Recieved a message")
        msg match {
          case ActorMessage.Ping(Sender.Bot) => {
            backend ! msg
            Behaviors.same
          }
          case ActorMessage.Ping(Sender.Backend) => {
            bot ! msg
            Behaviors.same
          }
          case ActorMessage.Die(_) => {
            bot ! msg
            backend ! msg
            Behaviors.stopped
          }
          case _ => {
            Behaviors.same
          }
        }
      }
    }
  }
}
@main def hello: Unit = {
  val ref = ActorSystem(Dispatch(), "PUGBot")
  val logger = LoggerFactory.getLogger("Main")
  sys.addShutdownHook({
                        logger.info("Shutting down everything")
                        ref ! ActorMessage.Die(Sender.Bot)
                        Thread.sleep(1000)
                      })
  logger.info("Shutdown hook added")

}
