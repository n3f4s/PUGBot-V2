import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ Behavior, ActorRef }

import org.slf4j.LoggerFactory

package Backend {
  object Backend {
    private val logger = LoggerFactory.getLogger("backend")
    def apply(ref: ActorRef[ActorMessage]): Behavior[ActorMessage] = {
      Behaviors.receive { (context, message) =>
        message match {
          case ActorMessage.Die(_) ⇒ {
            logger.info("Stopping backend")
            Behaviors.stopped
          }
          case _ => {
            logger.debug("Recieved a message")
            ref ! ActorMessage.Ping(Sender.Backend)
            Behaviors.same
          }
        }
      }
    }
  }
}
