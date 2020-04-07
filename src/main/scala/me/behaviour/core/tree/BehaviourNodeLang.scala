package me.behaviour.core.tree

import me.behaviour.core.tree.BehaviourNode.{ActionNode, ConditionNode, FallbackNode, ParallelNode, SequenceNode}
import me.behaviour.core.{Actuation, Environment, NodeResult}

/**
 * some utility used to create the behaviour tree.
 */
//TODO produce better documentation
object BehaviourNodeLang {
  def sequence[E <: Environment, A <: Actuation](behaviourNodes: BehaviourNode[E, A] *) : SequenceNode[E, A] = SequenceNode(behaviourNodes)
  def fallback[E <: Environment, A <: Actuation](behaviourNodes: BehaviourNode[E, A] *) : FallbackNode[E, A] = FallbackNode(behaviourNodes)
  def parallel[E <: Environment, A <: Actuation](thr : Int, behaviourNodes: BehaviourNode[E, A] *) : ParallelNode[E, A] = ParallelNode(behaviourNodes, thr)
  def cond[E <: Environment, A <: Actuation](fun : E => NodeResult): ConditionNode[E, A] = ConditionNode(fun)

  implicit def funToCond[E <: Environment, A <: Actuation](fun : E => NodeResult) = ConditionNode[E, A](fun)

  implicit class CollectorLang[E <: Environment, A <: Actuation](p : BehaviourNode[E, A]) {
    def ? (behaviourNode: BehaviourNode[E, A]): FallbackNode[E, A] = p match {
      case FallbackNode(nodes) => FallbackNode(nodes :+ behaviourNode)
      case _ => fallback(p, behaviourNode)
    }
    def ->(behaviourNode: BehaviourNode[E, A]) = p match {
      case SequenceNode(nodes) => SequenceNode(nodes :+ behaviourNode)
      case _ => sequence(p, behaviourNode)
    }
  }

  class ActionBuilder[E <: Environment, A <: Actuation] {
    private var successStrategy : (E => Boolean, E => Seq[A]) = null
    private var runningStrategy : (E => Boolean, E => Seq[A]) = null
    private var failCondition : (E => Boolean) = null

    def success(guard : E => Boolean, action : E => Seq[A]) : ActionBuilder[E, A] = {
      require(successStrategy == null)
      successStrategy = (guard, action)
      this
    }
    def fail(guard : E => Boolean) : ActionBuilder[E, A] = {
      require(failCondition == null)
      failCondition = guard
      this
    }
    def running(guard : E => Boolean, action: E => Seq[A]) : ActionBuilder[E, A] = {
      require(runningStrategy == null)
      runningStrategy = (guard, action)
      this
    }
    def build : ActionNode[E, A] = {
      if(failCondition == null) {
        failCondition = e => false
      }
      if(runningStrategy == null) {
        runningStrategy = ((e : E) => false, (e : E) => Seq.empty)
      }
      require(successStrategy != null )
      ActionNode[E,A](
        failCondition, successStrategy._1, runningStrategy._1, runningStrategy._2, successStrategy._2
      )
    }
  }

  def actionNode[E <: Environment, A <: Actuation]() : ActionBuilder[E, A] = new ActionBuilder[E,A]()

  implicit def builderToAction[E <: Environment, A <: Actuation](actionBuilder: ActionBuilder[E,A]) : ActionNode[E, A] = actionBuilder.build
}
