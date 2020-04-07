package me.behaviour.core

import me.behaviour.core.tree.BehaviourNode

/**
 * describe a system in which an agent are controlled by a behaviour tree.
 * the environment is what the agent percept at each tick.
 * A system, in summary, is a collector of:
 *  - a environment "shape" see by an agent
 *  - the agent that is "animated" by actuations
 *  - a behaviour tree that produce actuation from the environment sensed.
 */
trait System {
  type E <: Environment
  type A <: Actuation

  def behaviour : BehaviourNode[E, A]

  def agent : Agent[A, E]

  final def tick() : (NodeResult, Seq[A]) = {
    val env = agent.sense()
    val state = behaviour.tick(env)
    state match {
      case (_, actuations) => agent.act(actuations)
    }
    state
  }
}
