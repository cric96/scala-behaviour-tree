package me.vacuum

import me.behaviour.core.{Actuation, Environment, Agent}
import me.vacuum.Robot._

class Robot(env : Room, var position : (Int, Int)) extends Agent[VacuumActuation, RoomPerception] {
  var orientation : Orientation = Up
  var hit : Boolean = false
  override def act(actuation: Seq[VacuumActuation]): Unit = actuation foreach {
    case Reset => hit = false
    case Turn(o) => orientation = o
    case Step => move()
  }

  private def move() : Unit = {
    val newPos = orientation match {
      case Up => (position._1,position._2 + 1)
      case Down => (position._1,position._2 - 1)
      case Right => (position._1 + 1,position._2)
      case Left => (position._1 - 1,position._2)
    }
    if(env.obstacle(newPos._1, newPos._2))
      hit = true
    else
      position = newPos
  }

  override def sense() : RoomPerception = RoomPerception(hit, orientation)
}

object Robot {
  sealed trait Orientation
  case object Up extends Orientation
  case object Down extends Orientation
  case object Right extends Orientation
  case object Left extends Orientation

  sealed trait VacuumActuation extends Actuation

  case class Turn(orientation: Orientation) extends VacuumActuation
  case object Step extends VacuumActuation
  case object Reset extends VacuumActuation

  case class RoomPerception(hit : Boolean, o : Orientation) extends Environment

  def clockwise(o : Orientation) : Orientation = o match {
    case Up => Right
    case Right => Down
    case Down => Left
    case Left => Up
  }
}