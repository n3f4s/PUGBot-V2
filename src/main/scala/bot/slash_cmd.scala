import discord4j.discordjson.json.ApplicationCommandOptionData
import scala.collection.mutable.ArrayBuffer
import discord4j.discordjson.json.ApplicationCommandRequest
import discord4j.core.GatewayDiscordClient
import discord4j.core.`object`.entity.Guild
import discord4j.core.event.domain.interaction.ChatInputInteractionEvent
import scala.util.{ Try, Success, Failure}
import reactor.core.publisher.Mono
import collection.JavaConverters._
import discord4j.core.`object`.command.ApplicationCommandOption
import scala.collection.immutable.StringOps

package Bot {
  package Impl {
    type Argument = ApplicationCommandOptionData
    type CmdCallback = (ChatInputInteractionEvent => Mono[Void])

    class SlashCommandReg(inner: ApplicationCommandRequest,
                          client: GatewayDiscordClient,
                          callback: CmdCallback) {
      val id = client.getRestClient.getApplicationId.block
      val client_app = client.getRestClient.getApplicationService
      def register_guild(guild_id: Long): Try[CmdCallback] =
        (client_app
           createGuildApplicationCommand (id, guild_id, inner)
           `then` (Mono just Success(callback))
           onErrorResume (e => Mono just Failure(e))
           block)
      def register_guild(guild: Guild): Try[CmdCallback] =
        (client_app
           createGuildApplicationCommand (id, guild.getId.asLong, inner)
           `then` (Mono just Success(callback))
           onErrorResume (e => Mono just Failure(e))
           block)
      def register(): Try[CmdCallback] =
        (client_app
           createGlobalApplicationCommand (id, inner)
           `then` (Mono just Success(callback))
           onErrorResume (e => Mono just Failure(e))
           block)
    }

    class SlashCommandBuilder(name: String, client: GatewayDiscordClient) { // FIXME: add client
      private val args: ArrayBuffer[Argument] = ArrayBuffer()
      private var desc = ""
      private var callback: CmdCallback = (_ => Mono.empty)
      def --(arg: Argument): SlashCommandBuilder = {
        args += arg
        this
      }
      def desc(str: String): SlashCommandBuilder = {
        desc = str
        this
      }
      def on_error(fn: (Throwable => Unit)) = {
        // FIXME: TODO
        this
      }
      def executing(fn: CmdCallback) = {
        callback = fn
        this
      }
      def build = {
        SlashCommandReg(
          ApplicationCommandRequest
            .builder
            .name(name)
            .description(desc)
            .addAllOptions(args.toList.asJava)
            .build,
          client,
          callback
        )
      }
    }

    class Client2SlashBuilder(client: GatewayDiscordClient) {
      def /(name: String) = {
        SlashCommandBuilder(name, client)
      }
    }

    class ArgumentBuilder(name: String) {
      private var desc = ""
      private var atype = SlashCmd.Arguments.Type.Int
      private var is_req = false
      def desc(str:  String): ArgumentBuilder = {
        desc = str
        this
      }
      def of_type(at: SlashCmd.Arguments.Type): ArgumentBuilder = {
        atype = at
        this
      }
      def is_required(req: Boolean): ArgumentBuilder = {
        is_req = req
        this
      }
      def build =
        atype match {
          case SlashCmd.Arguments.Type.Channel(ctype) =>
            ApplicationCommandOptionData
              .builder()
              .name(name)
              .description(desc)
              .`type`(atype.value.getValue)
              .channelTypes(Iterable(ctype.value).asJava)
              .required(is_req)
              .build
          case _ => ApplicationCommandOptionData
              .builder()
              .name(name)
              .description(desc)
              .`type`(atype.value.getValue)
              .required(is_req)
              .build
        }
    }
  }

  package SlashCmd{
    package Arguments {
      package Channel { // FIXME make it so that we can restrict to multiple channels
        enum Type(val value: Integer) {
          case GuildText extends Type(0)
          case DM extends Type(1)
          case GuildVoice extends Type(2)
          case GroupDM extends Type(3)
          case GroupCategory extends Type(4)
          case GuildNews extends Type(5)
          case GuildStore extends Type(6)
          case GuildNewsThread extends Type(10)
          case GuildPublicThread extends Type(11)
          case GuildPrivateThread extends Type(12)
          case GuildStageVoice extends Type(13)
        }
      }
      enum Type(val value: ApplicationCommandOption.Type) {
        case String extends Type(ApplicationCommandOption.Type.STRING)
        case Int extends Type(ApplicationCommandOption.Type.INTEGER)
        case Boolean extends Type(ApplicationCommandOption.Type.BOOLEAN)
        case User extends Type(ApplicationCommandOption.Type.USER)
        case Role extends Type(ApplicationCommandOption.Type.ROLE)
        case Channel(chan_type: Arguments.Channel.Type) extends Type(ApplicationCommandOption.Type.CHANNEL)
        case Mentionable extends Type(ApplicationCommandOption.Type.MENTIONABLE)
        case Number extends Type(ApplicationCommandOption.Type.NUMBER)
        case SubCmd extends Type(ApplicationCommandOption.Type.SUB_COMMAND)
        case SubCmdGroup extends Type(ApplicationCommandOption.Type.SUB_COMMAND_GROUP)
      }
    }
    given Conversion[GatewayDiscordClient, Impl.Client2SlashBuilder] = Impl.Client2SlashBuilder(_)
    given Conversion[Impl.SlashCommandBuilder, Impl.SlashCommandReg] = _.build

    given Conversion[Impl.ArgumentBuilder, Impl.Argument] = _.build
    given Conversion[String, Impl.ArgumentBuilder] = Impl.ArgumentBuilder(_)
  }


}
