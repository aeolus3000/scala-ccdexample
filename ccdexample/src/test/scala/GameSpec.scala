import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class GameSpec extends FlatSpec with TableDrivenPropertyChecks {

  private val frameSpare = Frame(10, Array(5,5))
  private val frameStrike = Frame(10, Array(10))
  private val frameSimple = Frame(3, Array(2,1))
  private val frameUnfinished = Frame(6, Array(6))

  "Game" should "return 0 if no roll was added" in {
    val game = Game()
    assert(game.TotalScore() == 0)
  }

  private val examples0 =
    Table(
      ("rolls", "expectedFrames", "expectedScores"),
      (Array(2,1,2,1), Array[Frame](frameSimple, frameSimple), 6),
      (Array(8,2,3,1), Array[Frame](Frame(13, Array(8,2)), Frame(4, Array(3,1))), 17),
      (Array(10,3,1), Array[Frame](Frame(14, Array(10)), Frame(4, Array(3,1))), 18),
      (Array(10,10,3,1), Array[Frame](Frame(23, Array(10)), Frame(14, Array(10)), Frame(4, Array(3,1))), 41),
      (Array(10,10,3), Array[Frame](Frame(23, Array(10)), Frame(13, Array(10)), Frame(3, Array(3))), 39),
    )
  it should "adding rolls should build up the game" in {
    forAll(examples0) { (rolls, expectedFrames, expectedScores) =>
      val game = Game()
      //given: adding rolls
      rolls.foreach(game.AddRoll)
      //then: totalScore should match expectedScores
      assertFrames(game.frames, expectedFrames)
      assert(game.TotalScore() == expectedScores)
    }
  }

  private val examples =
    Table(
      ("frames", "expectedScores"),
      (Array[Frame](frameSimple, frameSimple), 6),
      (Array[Frame](frameStrike, frameSimple), 13),
      (Array[Frame](frameSpare, frameSimple), 13),
      (Array.empty[Frame], 0),
    )
  it should "return the total score as accumulation of all frame scores" in {
    val game = Game()
    forAll(examples) { (frames, expectedScores) =>
      //given: an unfinished game
      game.frames = frames
      //when:
      val totalScore = game.TotalScore()
      //then: totalScore should match expectedScores
      assert(totalScore == expectedScores)
    }
  }

  it should "return one frame after adding a roll" in {
    val game = Game()
    game.AddRoll(5)
    assert(!game.Frames().isEmpty)
  }

  it should "return one frame after adding a roll with the number of pins" in {
    val numberOfPins = 5
    val game = Game()
    game.AddRoll(numberOfPins)
    assert(!game.Frames().isEmpty)
    val pinsRolled: Array[Int] = game.Frames()(0).getPinsRolled()
    assert(!pinsRolled.isEmpty)
    assert(pinsRolled(0) == numberOfPins)
  }

  it should "return true on a new game" in {
    val game = Game()
    assert(game.isNewFrameNeeded)
  }

  private val examples3 =
    Table(
      ("pins","expected"),
      (Array(5), false),
      (Array(10), true),
      (Array(3), false),
      (Array(1,9), true),
      (Array(9), false),
    )
  it should "return if a new frame is needed in existing game" in {
    val game = Game()
    forAll(examples3) { (pins, expected) =>
      game.frames = Array[Frame](Frame(pins.sum, pins))
      assert(game.isNewFrameNeeded == expected)
    }

  }

  private val examples5 =
    Table(
      ("frames", "expectedScores"),
      (Array[Frame](frameSimple), Array[Int](3)),
      (Array[Frame](frameSpare), Array[Int](10)),
      (Array[Frame](frameStrike), Array[Int](10)),
      (Array[Frame](frameSimple, frameSimple), Array[Int](3,3)),
      (Array[Frame](frameSimple, frameSpare), Array[Int](3,10)),
      (Array[Frame](frameSpare, frameSimple), Array[Int](10,3)),
      (Array[Frame](frameSpare, frameUnfinished), Array[Int](16,6)),
      (Array[Frame](frameSimple, frameStrike), Array[Int](3,10)),
      (Array[Frame](frameStrike, frameStrike), Array[Int](20,10)),
      (Array[Frame](frameStrike, frameUnfinished), Array[Int](16,6)),
      (Array[Frame](frameSimple, frameSimple, frameSimple), Array[Int](3,3,3)),
      (Array[Frame](frameSimple, frameSimple, frameSpare), Array[Int](3,3,10)),
      (Array[Frame](frameSimple, frameSimple, frameUnfinished), Array[Int](3,3,6)),
      (Array[Frame](frameSimple, frameSpare, frameSpare), Array[Int](3,10,10)),
      (Array[Frame](frameSpare, frameSpare, frameSpare), Array[Int](10,10,10)),
      (Array[Frame](frameSimple, frameSimple, frameStrike), Array[Int](3, 3,10)),
      (Array[Frame](frameSimple, frameStrike, frameStrike), Array[Int](3,20,10)),
      (Array[Frame](frameStrike, frameStrike, frameStrike), Array[Int](20,20,10))
    )
  it should "correctly manipulate the score of previous frames" in {
    val game = Game()
    forAll(examples5) { (frames, expected) =>
      //given: an unfinished game
      game.frames = frames.map(frame => Frame(frame.score, frame.pinsRolled))
      //when:
      game.handlePreviousScore(game.frames.last.pinsRolled.last)
      //then:
      game.frames.zipWithIndex.foreach(frameAndIndex => assert(frameAndIndex._1.score == expected(frameAndIndex._2)))
    }
  }

  private val examples6 =
    Table(
      ("frames", "addPins", "expectedLastFrame"),
      (Array[Frame](frameSimple), 6, frameUnfinished),
      (Array[Frame](frameSimple, frameSimple), 6, frameUnfinished),
      (Array[Frame](frameSpare), 6, frameUnfinished),
      (Array[Frame](frameStrike), 6, frameUnfinished),
      (Array.empty[Frame], 6, frameUnfinished)
    )
  it should "add a new frame when invoking addToNewFrame" in {
    val game = Game()
    forAll(examples6) { (frames, addPins, expectedLastFrame) =>
      //given: an unfinished game
      game.frames = frames.map(frame => Frame(frame.score, frame.pinsRolled))
      //when:
      game.addRollToNewFrame(addPins)
      //then:
      assert(frames.length + 1 == game.frames.length)
      assertFrames(game.frames.take(frames.length), frames)
      assertFrame(game.frames.last, frameUnfinished)
    }
  }

  private val examples7 =
    Table(
      ("frames", "addPins", "expectedLastFrame"),
      (Array[Frame](frameUnfinished), 4, Frame(10, Array(6,4))),
      (Array[Frame](frameUnfinished), 1, Frame(7, Array(6,1))),
      (Array[Frame](frameSimple, frameUnfinished), 1, Frame(7, Array(6,1))),
    )
  it should "reuse the current frame when invoking addToCurrentFrame" in {
    val game = Game()
    forAll(examples7) { (frames, addPins, expectedLastFrame) =>
      //given: an unfinished game
      game.frames = frames.map(frame => Frame(frame.score, frame.pinsRolled))
      //when:
      game.addRollToCurrentFrame(addPins)
      //then:
      assert(frames.length == game.frames.length)
      assertFrames(game.frames.take(frames.length - 1), frames.take(frames.length - 1))
      assertFrame(game.frames.last, expectedLastFrame)
    }
  }

  //  it should "return two frames after adding two rolls with the number of pins" in {
//    val numberOfPins = Array(5, 5)
//    val game = Game()
//    for(x <- numberOfPins) {
//      game.AddRoll(x)
//    }
//    assert(!game.Frames().isEmpty)
//    val pinsRolled: Array[Int] = game.Frames()(0).getPinsRolled()
//    assert(!pinsRolled.isEmpty)
//    assert(pinsRolled == numberOfPins)
//  }

  it should "return an empty frame array if no roll was added" in {
    val game = Game()
    assert(game.Frames().isEmpty)
  }

  def assertFrames(actual: Array[Frame], expected: Array[Frame]): Unit = {
    assert(actual.length == expected.length)
    actual.zip(expected).foreach { case (a, e) => {
      assertFrame(a, e)
    }
    }
  }

  def assertFrame(actual: Frame, expected: Frame): Unit = {
    assert(actual.score == expected.score)
    assert(actual.pinsRolled sameElements expected.pinsRolled)
  }

}
