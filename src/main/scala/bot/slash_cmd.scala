import discord4j.rest.util.ApplicationCommandOptionType
import discord4j.discordjson.json.ApplicationCommandOptionData
import scala.collection.mutable.ArrayBuffer
import discord4j.discordjson.json.ApplicationCommandRequest
import discord4j.core.GatewayDiscordClient
import discord4j.core.`object`.entity.Guild
import discord4j.core.event.domain.interaction.ChatInputInteractionEvent
import scala.util.{ Try, Success, Failure}
import reactor.core.publisher.Mono
import collection.JavaConverters._

package Bot {

  type Argument = ApplicationCommandOptionData
  type CmdCallback = (ChatInputInteractionEvent => Unit)

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

  class SlashCommandBuilder(name: String) {
    private val args: ArrayBuffer[Argument] = ArrayBuffer()
    private var desc = ""
    private var cli: Option[GatewayDiscordClient] = None
    private var callback: CmdCallback = (_ => ())
    def --(arg: Argument): SlashCommandBuilder = {
      args += arg
      this
    }
    def desc(str: String): SlashCommandBuilder = {
      desc = str
      this
    }
    def in_client(client: GatewayDiscordClient) = {
      cli = Some(client)
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
          .build, // XXX what if option empty?
        cli.get,
        callback
      )
    }
  }
  given Conversion[String, SlashCommandBuilder] = SlashCommandBuilder(_)

  class ArgumentBuilder(name: String) {
    private var desc = ""
    private var atype = ApplicationCommandOptionType.INTEGER
    private var is_req = false
    def desc(str:  String): ArgumentBuilder = {
      desc = str
      this
    }
    def arg_type(at: ApplicationCommandOptionType): ArgumentBuilder = {
      atype = at
      this
    }
    def is_required(req: Boolean): ArgumentBuilder = {
      is_req = req
      this
    }
    def build = ApplicationCommandOptionData
      .builder()
      .name(name)
      .description(desc)
      .`type`(atype.getValue)
      .required(is_req)
      .build
  }
  given Conversion[ArgumentBuilder, Argument] = _.build
  given Conversion[String, ArgumentBuilder] = ArgumentBuilder(_)
}
