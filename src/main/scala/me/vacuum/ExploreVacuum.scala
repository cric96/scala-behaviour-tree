package me.vacuum

import me.behaviour.core.NodeResult.{Failure, Success}
import me.behaviour.core.tree.BehaviourNode.ActionNode
//Example of usage.
//TODO comment all example
object ExploreVacuum extends App {
  import me.behaviour.core.tree.BehaviourNodeLang._
  import Robot._
  val turnCondition = cond[RoomPerception, VacuumActuation](env => if(env.hit) Failure else Success)
  val turnAction : ActionNode[RoomPerception, VacuumActuation] = actionNode().success(
    (e : RoomPerception) => true,
    (e : RoomPerception) => {
      val turned: VacuumActuation = Turn(clockwise(e.o))
      Seq(turned, Reset)
    }
  )
  val move : ActionNode[RoomPerception, VacuumActuation] = actionNode().success(
    (e : RoomPerception) => true,
    (e : RoomPerception) => {
      Seq[VacuumActuation](Step)
    }
  )
  val width = 3
  val height = 3
  val tree = (turnCondition ? turnAction) -> (move)
  val room = new Room(Set((2,2), (2,3)), width, height)
  val robot = new Robot(room, (1,1))
  val system = new VacuumSystem(robot,tree)

  def printWorld = {
    for(i <- 0 to width) {
      for(j <- 0 to height) {
        if(robot.position == (i, j)) {
          print("o")
        } else if(room.obstacle(i, j)) {
          print("x")
        } else {
          print(".")
        }
      }
      println()
    }
  }
  while(true) {
    printWorld
    println(system.tick())
    Thread.sleep(1000);
  }
  println(system.tick())
}
