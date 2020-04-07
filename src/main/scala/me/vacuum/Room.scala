package me.vacuum

class Room(obstacleSet : Set[(Int, Int)], width : Int, height : Int) {
  def obstacle(x : Int, y : Int) : Boolean = (outBounds(x, y)) || obstacleSet.contains((x,y))

  def outBounds(x : Int, y : Int) : Boolean = outWidth(x) || outHeight(y)

  private def outWidth(x : Int) : Boolean = x < 0 || x > width

  private def outHeight(y : Int) : Boolean = y < 0 || y > height
}
