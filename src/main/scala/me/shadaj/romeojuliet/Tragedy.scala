package me.shadaj.romeojuliet

import akka.actor.{Status, ActorRef, Actor}
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
      context.system.scheduler.scheduleOnce(10 second) {
        context.become(loving(lover))
        println("""JULIET
                  |O comfortable Friar! Where is my lord?
                  |I do remember well where I should be,
                  |And there I am. Where is my Romeo?
                  |""".stripMargin)
        (lover ? DoYouLoveMe).pipeTo(self)
      }

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
    case _ =>
  }
}

class FriarLawrence extends Actor {
  def receive: Receive = {
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

class Paris extends Actor {
  def receive: Receive = {
    case _ =>
  }
}