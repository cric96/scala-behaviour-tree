package me.vacuum
import me.behaviour.core.System
import me.behaviour.core.tree.BehaviourNode
class VacuumSystem(robot : Robot, val behaviourNode: BehaviourNode[Robot.RoomPerception, Robot.VacuumActuation]) extends System {
  override type E = Robot.RoomPerception
  override type A = Robot.VacuumActuation

  def behaviour: BehaviourNode[E, A] = behaviourNode

  def agent: Robot = robot
}
