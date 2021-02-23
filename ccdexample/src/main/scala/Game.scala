case class Game() {
  var frames = Array.empty[Frame]

  def AddRoll(pins: Int) : Unit = {
    addPinsToGame(pins)
    handlePreviousScore(pins)
  }

  def Frames(): Array[Frame] = {
    frames
  }

  def TotalScore(): Int = {
    frames.map(_.score).sum
  }

  def isNewFrameNeeded: Boolean = {
    frames.isEmpty || frames.last.isCompleted
  }

  def addPinsToGame(pins: Int): Unit = {
    if(isNewFrameNeeded) {
      addRollToNewFrame(pins)
    } else {
      addRollToCurrentFrame(pins)
    }
  }

  //  def Over() : Boolean = {}

  def addRollToNewFrame(pins: Int): Unit = {
    frames = frames :+ Frame(pins)
  }

  def addRollToCurrentFrame(pins: Int): Unit = {
    frames.last.add(pins)
  }

  def handlePreviousScore(pins: Int): Unit = {
    if(previousFrameIsStrike) {
      frames(frames.length - 2).score += pins
      if(firstRollOfCurrentFrame && prePreviousFrameIsStrike) {
        frames(frames.length - 3).score += pins
      }
    } else if (firstRollOfCurrentFrame && previousFrameIsSpare){
      frames(frames.length - 2).score += pins
    }
  }

  def previousFrame: Frame = frames(frames.length - 2)

  def prePreviousFrame: Frame = frames(frames.length - 3)

  def hasAtLeastTwoFrames: Boolean = frames.length > 2

  def firstRollOfCurrentFrame: Boolean = frames.last.pinsRolled.length == 1

  def prePreviousFrameIsStrike: Boolean = hasAtLeastTwoFrames && prePreviousFrame.isStrike

  def previousFrameIsStrike: Boolean = frames.length > 1 && previousFrame.isStrike

  def previousFrameIsSpare: Boolean = frames.length > 1 && previousFrame.isSpare

}