package me.behaviour.core.tree

import me.behaviour.core.NodeResult.{Failure, Running, Success}
import me.behaviour.core.{Actuation, Environment, NodeResult}

/**
 * behaviour node is a node in a behaviour tree.
 * Following scala idea, this tree is "conceptually" immutable.
 * This abstraction are created using following references:
 *  - libgdx (a game framework) : https://github.com/libgdx/gdx-ai/wiki/Behavior-Trees;
 *  - book Behavior Trees in Robotics and AI;
 * This tree has only "behaviour" structure, @see [me.behaviour.tree.System] to see how
 * this element is used to actuate the behaviour.
 * each node has tick method, that wrap the logic of node. a tick result could be
 *  1- Failure: some condition isn't respected
 *  2- Success: node computation is successful
 *  3- Running: a node (or a set of node) is in running mode to achieve its goal
 * the type of nodes are:
 *  1- collector : (fallback, sequence, parallel);
 *  2- condition;
 *  3- action;
 *  4- decorator;
 *
 * @tparam E the type of environment used to compute action in the tree
 * @tparam A the type of the output produced by node
 */
trait BehaviourNode[E <: Environment, A <: Actuation] {
  /**
   * give the token to the current node, it starts the computation.
   * the execution leads in a NodeResult. The actuation that the "agent" must do to
   * achieve the behaviour describe in the tree are collected in a Seq[A]
   * @param env: snapshot of environment used to compute the behaviour from the tree
   * @return a Tuple of NodeResult and Sequence of Actuation
   */
  def tick(env : E) : (NodeResult, Seq[A])
}

object BehaviourNode {
  /**
   * shortcut to produce a result with no actuation
   * @param nodeResult the result expected
   * @tparam A the type of the output produced by node
   * @return a tuple formed by (nodeResult, Seq.empty)
   */
  def result[A <: Actuation](nodeResult: NodeResult) : (NodeResult, Seq[A]) = (nodeResult, Seq.empty)
  //internal use, improve code quality
  private def tickUntil[E <: Environment, A <: Actuation](iterator : Iterator[BehaviourNode[E, A]], env : E, condiction : NodeResult => Boolean) : (NodeResult, Seq[A]) = {
    var elems = Seq.empty[A]
    var current = iterator.next().tick(env)
    elems = elems ++ current._2
    while(iterator.hasNext && condiction(current._1)) {
      current = iterator.next().tick(env)
      elems = elems ++ current._2
    }
    (current._1, elems)
  }

  /**
   * root type of all collector node: a node that is not a leaf and has a sequence of child node.
   * @param children an iterable of children associated to this root node.
   * @tparam E the type of environment used to compute action in the tree
   * @tparam A the type of the output produced by node
   */
  abstract class CollectorNode[E <: Environment, A <: Actuation](children : Iterable[BehaviourNode[E,A]]) extends BehaviourNode [E,A]{
    require(children.nonEmpty)
  }

  /**
   *
   * @param children an iterable of children associated to this root node.
   * @tparam E the type of environment used to compute action in the tree
   * @tparam A the type of the output produced by node
   */
  case class SequenceNode[E <: Environment, A <: Actuation](children : Seq[BehaviourNode[E, A]]) extends CollectorNode[E, A](children) {
    override def tick(env: E): (NodeResult, Seq[A]) = tickUntil[E, A](children.iterator, env, _ == Success)
  }

  /**
   *
   * @param children an iterable of children associated to this root node.
   * @param thr
   * @tparam E the type of environment used to compute action in the tree
   * @tparam A the type of the output produced by node
   */
  case class ParallelNode[E <: Environment, A <: Actuation](children : Iterable[BehaviourNode[E, A]], thr : Int) extends CollectorNode[E, A](children) {
    private val len = children.size
    require(thr > 0 && thr <= len)

    override def tick(env: E): (NodeResult, Seq[A]) = {
      val eval = children.map(_.tick(env))
      val actuations = eval.foldLeft(Seq.empty[A])((acc, curr) => acc ++ curr._2)
      val statistics = eval.map {
        case (Success, _) => (1,0)
        case (Failure,_) => (0,1)
        case _ => (0,0)
      }.foldLeft(0,0)((acc, elem) => (acc._1 + elem._1, acc._2 + elem._2))

      statistics match {
        case (successCount,_) if successCount > thr => (Success, actuations)
        case (_,failureCount) if failureCount > (len - thr) => (Failure, actuations)
        case _ => (Running, actuations)
      }
    }
  }

  /**
   *
   * @param children an iterable of children associated to this root node.
   * @tparam E the type of environment used to compute action in the tree
   * @tparam A the type of the output produced by node
   */
  case class FallbackNode[E <: Environment, A <: Actuation](children : Seq[BehaviourNode[E, A]]) extends CollectorNode[E, A](children) {
    override def tick(env: E): (NodeResult, Seq[A]) = tickUntil[E, A](children.iterator, env, _ == Failure)
  }

  /**
   *
   * @param evalLogic
   * @tparam E the type of environment used to compute action in the tree
   * @tparam A the type of the output produced by node
   */
  case class ConditionNode[E <: Environment, A <: Actuation](evalLogic : E => NodeResult) extends BehaviourNode[E, A] {
    override final def tick(env: E): (NodeResult, Seq[A]) = result(evalLogic(env))
  }

  /**
   *
   * @param failureCondition
   * @param successCondition
   * @param runningCondition
   * @param onRunning
   * @param onSuccess
   * @tparam E the type of environment used to compute action in the tree
   * @tparam A the type of the output produced by node
   */
  case class ActionNode[E <: Environment, A <: Actuation](failureCondition : E => Boolean,
                                                          successCondition : E => Boolean,
                                                          runningCondition : E => Boolean,
                                                          onRunning : E => Seq[A],
                                                          onSuccess : E => Seq[A]) extends BehaviourNode[E, A] {
    override final def tick(env: E): (NodeResult, Seq[A]) = if(failureCondition(env)) {
      result(Failure)
    } else if(runningCondition(env)) {
      (Running, onRunning(env))
    } else if(successCondition(env)) {
      (Success, onSuccess(env))
    } else {
      result(Failure)
    }

    /**
     *
     * @param decore
     * @tparam E the type of environment used to compute action in the tree
     * @tparam A the type of the output produced by node
     */
    abstract class Decorator[E <: Environment, A <: Actuation](decore : BehaviourNode[E,A]) extends BehaviourNode[E,A]
  }
}
