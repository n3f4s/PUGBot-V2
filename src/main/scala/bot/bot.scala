import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ Behavior, ActorRef }

import discord4j.core._
import discord4j.core.event.domain.lifecycle._
import discord4j.core.event.domain.message._
import discord4j.core.event.domain._
import discord4j.core.`object`.entity._
import discord4j.core.`object`.entity.channel.{ VoiceChannel, Channel }
import discord4j.core.`object`.VoiceState

import discord4j.discordjson.json.{ ApplicationCommandRequest, ApplicationCommandOptionData}
import discord4j.core.event.ReactiveEventAdapter
import reactor.core.publisher.Mono
import discord4j.core.event.domain.interaction.ChatInputInteractionEvent

import discord4j.rest.util.ApplicationCommandOptionType

import collection.JavaConverters._
import reactor.core.scala.publisher._
import reactor.core.scala.publisher.ScalaConverters._

import scala.language.postfixOps
import scala.io.Source

import org.slf4j.LoggerFactory
import scala.collection.mutable
import discord4j.common.util.Snowflake
import discord4j.core.`object`.entity.channel.PrivateChannel


package Bot{
  object VoiceChange {
    def make(status: VoiceStateUpdateEvent, lobbies: Lobbies) = {
      def is_in_lobby(l: Lobby, vs: VoiceState) =
        vs.getChannelId.map(id ⇒ l.waiting.getId == id
                              || l.red.getId == id
                              || l.blue.getId == id).getOrElse(false)
      val from_lobby = status.getOld.flatMap(_.getChannelId).map(id ⇒ lobbies.is_pug_chan(id)).getOrElse(false)
      val to_lobby = status.getCurrent.getChannelId.map(id ⇒ lobbies.is_pug_chan(id)).getOrElse(false)
      if (status.isLeaveEvent && from_lobby) Leave(status.getOld.get)
      else if (status.isMoveEvent && from_lobby && !to_lobby) Leave(status.getOld.get)
      else if (status.isMoveEvent && from_lobby && to_lobby) Move(status.getOld.get, status.getCurrent)
      else if (status.isMoveEvent && !from_lobby && to_lobby) Join(status.getCurrent)
      else if (status.isJoinEvent && to_lobby) Join(status.getCurrent)
      else NOOP
    }
  }
  enum VoiceChange {
    case Leave(chan: VoiceState)
    case Join(chan: VoiceState)
    case Move(from: VoiceState, to: VoiceState)
    case NOOP
  }

  object Bot {
    private val logger = LoggerFactory.getLogger("DiscordBot")
    def reply(evt: ChatInputInteractionEvent, lobbies: Lobbies): Mono[Void] = {
      implicit class J2SOpt[T](val x: java.util.Optional[T]) {
        def toScala = if(x.isPresent) { Some(x.get) } else { None }
      }
      def get_chan(evt: ChatInputInteractionEvent, name: String): Channel = {
        evt
          .getInteraction()
          .getCommandInteraction()
          .toScala
          .flatMap(_.getOption(name).toScala)
          .flatMap(_.getValue.toScala)
          .map(_.asChannel().block)
          .get
      }
      val lobby_name = evt
        .getInteraction()
        .getCommandInteraction()
        .toScala
        .flatMap(_.getOption("lobby_name").toScala)
        .flatMap(_.getValue.toScala)
        .map(_.asString())
        .get
      val waiting_room = get_chan(evt, "waiting_room")
      val red_room = get_chan(evt, "red_team")
      val blue_room = get_chan(evt, "blue_team")
      // FIXME: add names of bad channel
      // TODO: add in the pug bot
      val res = (waiting_room, blue_room, red_room) match {
        case (w: VoiceChannel, b: VoiceChannel, r: VoiceChannel) ⇒ {
          val waiting_room = w.getName
          val blue_room = b.getName
          val red_room = r.getName
          lobbies.add_lobby(new Lobby(lobby_name, w, b, r))
          {
            import java.io._
            val pw = new PrintWriter(new File("lobbies.json"))
            pw.write(lobbies.to_json)
            pw.close
          }
          s"Lobby *${lobby_name}*: Waiting room: *${waiting_room}*, red team: *${red_room}*, blue team: *${blue_room}*"
        }
        case (_, b: VoiceChannel, r: VoiceChannel) ⇒ {
          "The waiting room isn't a voice channel"
        }
        case (w: VoiceChannel, _, r: VoiceChannel) ⇒ {
          "The blue team channel isn't a voice channel"
        }
        case (w: VoiceChannel, b: VoiceChannel, _) ⇒ {
          "The red team channel isn't a voice channel"
        }
        case (_, _, r: VoiceChannel) ⇒ {
          "The waiting room and blue team aren't voice channel"
        }
        case (_, b: VoiceChannel, _) ⇒ {
          "The waiting room and red team aren't voice channel"
        }
        case (w: VoiceChannel, _, _) ⇒ {
          "The blue team and red team aren't voice channel"
        }
        case (_, _, _) ⇒ {
          "None of the channels are voice channels"
        }
      }
      evt.reply(res)
    }

    def save_d2b(d2b: mutable.Set[Snowflake], filename: String) = {
      import java.io._
      val content = d2b.map(_.asString).mkString(",")
      val pw = new PrintWriter(new File(filename))
      pw.write(content)
      pw.close
    }
    def load_d2b(filename: String): mutable.Set[Snowflake] = {
      if (new java.io.File(filename).exists) {
        val sn = Source
          .fromFile(filename)
          .getLines
          .mkString
          .split(",")
          .map(s => Snowflake.of(s))
        mutable.Set(sn: _*)
      } else {
        mutable.Set()
      }
    }
    def register_btag(msg: Message, d2b: mutable.Set[Snowflake]) = {
      // FIXME: error handling
      val user = msg.getAuthor.get
      val id = user.getId
      val btag = Overwatch.BattleTag.from_string(msg.getContent).get
      d2b += id
      save_d2b(d2b, "reg_users")
      (user, btag)
    }

    def apply(ref: ActorRef[ActorMessage]): Behavior[ActorMessage] = {

      // FIXME: Error handling
      Behaviors.setup { context ⇒
        implicit class J2SOpt[T](val x: java.util.Optional[T]) {
          def toScala = if(x.isPresent) { Some(x.get) } else { None }
        }

        logger.info("Starting the bot")

        val token = sys.env("DISCORD_PUG_BOT")

        val client: GatewayDiscordClient = (DiscordClient.create(token).login().block())

        logger.debug("Client started")

        val lobbies = if (new java.io.File("lobbies.json").exists) {
          logger.info("Reading from file")
          Lobbies.from_json(Source.fromFile("lobbies.json").getLines.mkString, client)
        } else {
          logger.warn("No config found")
          Lobbies()
        }

        logger.debug("Lobbies created")

        def get_lobby(vs: VoiceState): Lobby =
          (lobbies
             get_by_waiting_id (vs.getChannelId.get)
             orElse (lobbies get_by_red_id (vs.getChannelId.get))
             orElse (lobbies get_by_blue_id (vs.getChannelId.get))
             get
          )

        val rest_client = client.getRestClient
        val app_id = rest_client.getApplicationId.block
        val conf_cmd = (ApplicationCommandRequest.builder()
                          .name ("addlobby")
                          .description ("Register three voice channel as PUG voice channel (a lobby and two team voice channel)")
                          .addOption(ApplicationCommandOptionData.builder()
                                       .name("lobby_name")
                                       .description("Lobby name")
                                       .`type`(ApplicationCommandOptionType.STRING.getValue())
                                       .required(true)
                                       .build())
                          .addOption(ApplicationCommandOptionData.builder()
                                       .name("waiting_room")
                                       .description("Lobby's voice channel")
                                       .`type`(ApplicationCommandOptionType.CHANNEL.getValue())
                                       .required(true)
                                       .build())
                          .addOption(ApplicationCommandOptionData.builder()
                                       .name("red_team")
                                       .description("Red team's voice channel")
                                       .`type`(ApplicationCommandOptionType.CHANNEL.getValue())
                                       .required(true)
                                       .build())
                          .addOption(ApplicationCommandOptionData.builder()
                                       .name("blue_team")
                                       .description("Blue team's voice channel")
                                       .`type`(ApplicationCommandOptionType.CHANNEL.getValue())
                                       .required(true)
                                       .build())
                          .build)
        val print_cmd = (ApplicationCommandRequest.builder()
                           .name ("printcfg")
                           .description ("Print the configuration for this server")
                           .build)
        logger.debug("Command created")

        ((rest_client.getApplicationService)
           createGuildApplicationCommand (app_id, 823930184067579954L, conf_cmd)
           doOnError (err ⇒ logger.error(s"Can't create command: ${err}"))
           onErrorResume (e ⇒ Mono.empty())
           block)
        ((rest_client.getApplicationService)
           createGuildApplicationCommand (app_id, 823930184067579954L, print_cmd)
           doOnError (err ⇒ logger.error(s"Can't create command: ${err}"))
           onErrorResume (e ⇒ Mono.empty())
           block)
        logger.debug("Command registered")

        ((client.getEventDispatcher) on classOf[ReadyEvent]
          subscribe(ready => logger.info("Logged in as " + ready.getSelf().getUsername())))

        (client on classOf[ChatInputInteractionEvent]
           filter (evt ⇒ Array("addlobby", "printcfg") contains evt.getCommandName)
           flatMap (evt ⇒
             if (evt.getCommandName == "addlobby") {
               logger.info("Got command add lobby")
               Bot.reply(evt, lobbies)
             }
             else if(evt.getCommandName == "printcfg") {
               logger.info("Got command print cfg")
               logger.debug(lobbies.to_str)
               evt.reply("__**_Lobbies_**__:\n" + lobbies.to_str)
             }
             else {Mono.empty()})
           subscribe
        )

        val disc2btag: mutable.Set[Snowflake] = load_d2b("reg_users")
        (client on classOf[MessageCreateEvent]
           filter (evt ⇒ evt.getMessage.getAuthor.toScala.map(a ⇒ !a.isBot()).getOrElse(false))
           map (evt ⇒ evt.getMessage)
           filter (message ⇒ !message.getContent.isEmpty)
           filterWhen (_.getChannel.map(_.isInstanceOf[PrivateChannel]))
           map (msg => register_btag(msg, disc2btag))
           doOnNext { (user, btag) => ref ! ActorMessage.RegisterPlayer(user, btag, Sender.Bot) }
           subscribe
        )

        logger.debug("Slash command listener set up")

        ((client.getEventDispatcher.on(classOf[VoiceStateUpdateEvent]).asScala)
           map (e ⇒ VoiceChange.make(e, lobbies))
           doOnNext {
             case VoiceChange.Leave(chan) ⇒ {
               val user = chan.getUser.block
               val lobby = get_lobby(chan)
               logger.info(s"Player ${user.getUsername} left lobby ${lobby.name}")
               ref ! ActorMessage.PlayerLeaveLobby(user, lobby.name, Sender.Bot)
             }
             case VoiceChange.Join(chan) ⇒ {
               val user = chan.getUser.block
               val lobby = get_lobby(chan)
               logger.info(s"Player ${user.getUsername} joined lobby ${lobby.name}")
               if (!(disc2btag contains user.getId)) {
                 user.getPrivateChannel.flatMap(_.createMessage("Give me your battletag")).block
               }
               ref ! ActorMessage.PlayerJoinLobby(user, lobby.name, Sender.Bot)
             }
             case VoiceChange.Move(from, to) ⇒ {
               val user = from.getUser.block
               val lobby_f = get_lobby(from)
               val lobby_t = get_lobby(to)
               logger.info(s"Player ${user.getUsername} joined lobby ${lobby_f.name} and left ${lobby_t.name}")
               ref ! ActorMessage.PlayerJoinLobby(user, lobby_t.name, Sender.Bot)
               ref ! ActorMessage.PlayerLeaveLobby(user, lobby_f.name, Sender.Bot)
             }
             case VoiceChange.NOOP ⇒ {}}
           subscribe
        )

        logger.debug("Voice channel event listener set up")
        Behaviors.receiveMessage { message ⇒
          message match {
            case ActorMessage.Die(_) ⇒ {
              logger.info("Stopping client")
              client.logout.block
              Behaviors.stopped
            }
            case _ ⇒ {Behaviors.same}
          }
        }
      }
    }
  }
}
