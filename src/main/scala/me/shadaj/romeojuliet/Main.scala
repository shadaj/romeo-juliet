package me.shadaj.romeojuliet

import akka.actor.{Props, ActorSystem}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.language.postfixOps

object Main extends App {
  val tragedy = ActorSystem("tragedy")

  val romeo = tragedy.actorOf(Props[Romeo])
  val friar = tragedy.actorOf(Props[FriarLawrence])
  val juliet = tragedy.actorOf(Props(new Juliet(friar)))
  val paris = tragedy.actorOf(Props[Paris])

  romeo ! FallInLove(juliet)
  juliet ! FallInLove(romeo)

  tragedy.scheduler.scheduleOnce(5 seconds) {
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
}
