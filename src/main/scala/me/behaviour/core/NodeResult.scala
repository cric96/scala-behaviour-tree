package me.behaviour.core

/**
 * describe the node result of a behaviour tree.
 */
sealed trait NodeResult

object NodeResult {
  case object Success extends NodeResult
  case object Failure extends NodeResult
  case object Running extends NodeResult
}
