package me.shadaj.romeojuliet

import akka.actor.{Terminated, SupervisorStrategy, Props, Status, ActorRef, Actor}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.language.postfixOps

import akka.pattern.ask
import akka.pattern.pipe

case class FallInLove(lover: ActorRef)

case object DoYouLoveMe
case object ILoveYou

case class YouHaveToMarry(that: ActorRef)

case object HelpMe

case object SleepingPotion
case class WakingUp(actorRef: ActorRef)

object Tragedy {
  final val Name = "tragedy"
  def props: Props = Props(new Tragedy)
}

/* The single top-level actor. */
class Tragedy extends Actor {

  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  val romeo = context.watch(context.actorOf(Romeo.props, Romeo.Name))
  val friar = context.watch(context.actorOf(FriarLawrence.props, FriarLawrence.Name))
  val juliet = context.watch(context.actorOf(Juliet.props(friar), Juliet.Name))
  val paris = context.watch(context.actorOf(Paris.props, Paris.Name))

  private var isJulietAlive = true
  private var isRomeoAlive = true

  romeo ! FallInLove(juliet)
  juliet ! FallInLove(romeo)

  context.system.scheduler.scheduleOnce(5 seconds) {
    println(
      """CAPULET
        |How, how, how, how? Chopped logic! What is this?
        |“Proud,” and “I thank you,” and “I thank you not,”
        |And yet “not proud”? Mistress minion you,
        |Thank me no thankings, nor proud me no prouds,
        |But fettle your fine joints 'gainst Thursday next
        |To go with Paris to Saint Peter’s Church,
        |Or I will drag thee on a hurdle thither.
        |Out, you green sickness, carrion! Out, you baggage!
        |You tallow face!
        |""".stripMargin)
    juliet ! YouHaveToMarry(paris)
  }

  override def receive = {
    case Terminated(actor) if actor.path.name == Romeo.Name =>
      println("OMG, Romeo died!")
      isRomeoAlive = false
      potentialEndOfGame()

    case Terminated(actor) if actor.path.name == Juliet.Name =>
      println("OMG, Juliet died!")
      isJulietAlive = false
      potentialEndOfGame()
  }

  private def potentialEndOfGame() = if (!isRomeoAlive && !isJulietAlive) {
    println(s"Shutting down because Romeo and Juliet died!")
    context.system.shutdown()
  }
}

object Romeo {
  final val Name = "romeo"
  def props: Props = Props(new Romeo)
}

class Romeo extends Actor {
  implicit val timeout = Timeout(5 seconds)

  def receive: Receive = {
    case FallInLove(lover) =>
      context.become(loving(lover))
  }

  def loving(lover: ActorRef): Receive = {
    case DoYouLoveMe =>
      println(
        """ROMEO
          |I love you Juliet
          |""".stripMargin)
      sender() ! ILoveYou

      context.system.scheduler.scheduleOnce(1 second) {
        println(
          """ROMEO
            |Do you love me Juliet?
            |""".stripMargin)
        (lover ? DoYouLoveMe).pipeTo(self)
      }
    case ILoveYou =>
      println(
        """ROMEO
          |Yay Juliet loves me
          |""".stripMargin)
    case Status.Failure(e) =>
      println(e)
      println(
        """ROMEO
          |Here’s to my love! (drinks the poison) O true apothecary,
          |Thy drugs are quick. Thus with a kiss I die.
          |""".stripMargin)
      throw new IllegalStateException(
        """ROMEO
          |I cannot live without my lover
          |""".stripMargin)
  }
}

object Juliet {
  final val Name = "juliet"
  def props(friar: ActorRef): Props = Props(new Juliet(friar))
}

class Juliet(friar: ActorRef) extends Actor {
  implicit val timeout = Timeout(5 seconds)

  def receive: Receive = {
    case FallInLove(lover) =>
      context.become(loving(lover))
      (lover ? DoYouLoveMe).pipeTo(self)
  }

  def loving(lover: ActorRef): Receive = {
    case DoYouLoveMe =>
      println(
        """JULIET
          |I love you Romeo
          |""".stripMargin)
      lover ! ILoveYou

      context.system.scheduler.scheduleOnce(1 second) {
        println(
          """JULIET
            |Do you love me Romeo?
            |""".stripMargin)
        (lover ? DoYouLoveMe).pipeTo(self)
      }
    case SleepingPotion =>
      println(
        """JULIET
          |Romeo, Romeo, Romeo! Here’s drink. I drink to thee.
          |""".stripMargin)
      context.become(sleeping)
      context.system.scheduler.scheduleOnce(10 seconds, self, WakingUp(lover))

    case YouHaveToMarry(toMarry) =>
      if (toMarry != lover) {
        println( """JULIET
                   |O, shut the door! And when thou hast done so,
                   |Come weep with me, past hope, past cure, past help.
                   |""".stripMargin)
        friar ! HelpMe
      }
    case ILoveYou =>
      println(
        """JULIET
          |Yay Romeo loves me
          |""".stripMargin)
    case Status.Failure(e) =>
      println(e)
      println(
        """JULIET
          |O happy dagger,
          |This is thy sheath. There rust and let me die.
          |(stabs herself with ROMEO’s dagger and dies)
          |""".stripMargin)
      throw new IllegalStateException(
        """JULIET
          |I cannot live without my lover
          |""".stripMargin)
  }

  def sleeping: Receive = {
    case WakingUp(lover) =>
      context.become(loving(lover))
      println("""JULIET
                |O comfortable Friar! Where is my lord?
                |I do remember well where I should be,
                |And there I am. Where is my Romeo?
                |""".stripMargin)
      (lover ? DoYouLoveMe).pipeTo(self)
  }
}

object FriarLawrence {
  final val Name = "friar-lawrence"
  def props: Props = Props(new FriarLawrence)
}

class FriarLawrence extends Actor {
  override def receive: Receive = {
    case HelpMe =>
      println("""FRIAR LAWRENCE
                |Take thou this vial, being then in bed,
                |And this distillèd liquor drink thou off,
                |When presently through all thy veins shall run
                |A cold and drowsy humor, for no pulse
                |Shall keep his native progress, but surcease.
                |""".stripMargin)
      sender() ! SleepingPotion
  }
}

object Paris {
  final val Name = "paris"
  def props: Props = Props(new Paris)
}

class Paris extends Actor {
  override def receive: Receive = Actor.emptyBehavior
}