case class Frame(var score: Int, var pinsRolled: Array[Int]) {

  def getPinsRolled() : Array[Int] = {
    pinsRolled
  }

  def add(pins: Int): Unit = {
    if (isCompleted) {
      throw new IllegalArgumentException("can't add pins: " + pins)
    }
    score += pins
    pinsRolled = pinsRolled :+ pins
  }

  def getScore(): Int = {
    score
  }

  def isCompleted: Boolean = {
    (pinsRolled.length == 2 || isStrike || isSpare)
  }

  def isStrike: Boolean = {
    (pinsRolled.length == 1 && pinsRolled.sum == 10)
  }

  def isSpare: Boolean = {
    (pinsRolled.length == 2 && pinsRolled.sum == 10);
  }

  def pinsFitInThisFrame(pins: Int): Boolean = {
    !(pinsRolled.sum + pins > 10 || pinsRolled.length == 2)
  }

}

object Frame {

  def apply(pins: Int) :Frame = {
    val pinsRolled :Array[Int] = Array[Int]{pins}
    new Frame(pins, pinsRolled)
  }

  def apply(): Frame = {
    new Frame(0, Array.empty[Int])
  }

}
