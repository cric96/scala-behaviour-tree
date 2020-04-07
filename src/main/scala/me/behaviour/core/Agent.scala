package me.behaviour.core

/**
 * An entity that lives in an environment.
 * The entity is situated and can percept environment.
 * This is a logically mutable entity.
 * @tparam E the type of environment used to compute action in the tree
 * @tparam A the type of the output produced by node
 */
trait Agent [A <: Actuation, E <: Environment] {
  /**
   * act the actuation that change environment and the robot state.
   * @param actuation the actuation that the robot must do.
   */
  def act(actuation : Seq[A]) : Unit

  /**
   * collect information of environment and create a snapshot of it.
   * @return the environment sensed by the situated agent.
   */
  def sense() : E
}
