import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class FrameSpec extends FlatSpec with TableDrivenPropertyChecks {

  "Frame" should "add number of Pins to pinsRolled" in {
    val frame = Frame()
    frame.add(5)
    frame.add(4)
    assert(frame.getPinsRolled() sameElements Array(5, 4))
  }

  val examples =
    Table(
      ("pins","expectedScore"),
      (Array(5), 5),
      (Array(7), 7),
      (Array(9,3), 7),
    )

  it should "add number of pins to the current score" in {
    forAll(examples) { (pins, expected) =>
      var currentScore = 0
      val frame = Frame()
      for(rollPins <- pins) {
        frame.add(rollPins)
        currentScore += rollPins
        assert(frame.getScore == currentScore)
      }
    }
  }

  val examples2 =
    Table(
      ("pins","expectedScore"),
      (Array(10), 10),
      (Array(3,7), 10),
      (Array(1,9), 10),
    )

  it should "limit number of pins rolled" in {
    forAll(examples2) { (pins, expected) =>
      val frame = Frame()
      for(rollPins <- pins) {
        frame.add(rollPins)
      }
      assertThrows[IllegalArgumentException](frame.add(1))
    }
  }

  val examples3 =
    Table(
      ("pins","expected"),
      (Array(10), true),
      (Array(3), false),
      (Array(1,9), true),
      (Array(9), false),
    )
  it should "return if a frame is closed" in {
    forAll(examples3) { (pins, expected) =>
      val frame = Frame()
      for(rollPins <- pins) {
        frame.add(rollPins)
      }
      assert(frame.isCompleted == expected)
    }
  }

  val examples4 =
    Table(
      ("pins","expectedStrike", "expectedSpare"),
      (Array(10), true, false),
      (Array(3), false, false),
      (Array(1,9), false, true),
      (Array(9), false, false),
    )
  it should "return true on strike" in {
    forAll(examples4) { (pins, expectedStrike, expectedSpare) =>
      val frame = Frame()
      for(rollPins <- pins) {
        frame.add(rollPins)
      }
      assert(frame.isStrike == expectedStrike)
      assert(frame.isSpare == expectedSpare)
    }
  }

  val examples5 =
    Table(
      ("pins", "addPins", "expected"),
      (Array(10), 1, false),
      (Array(3), 7, true),
      (Array(3), 8, false),
      (Array(1,9), 0, false),
      (Array(1,9), 1, false),
      (Array(1,8), 1, false),
      (Array(1,7), 2, false),
      (Array(9), 2, false),
      (Array(9), 1, true),
      (Array(), 11, false),
      (Array(), 10, true),
      (Array(0), 10, true),
      (Array(1), 10, false),
    )
  it should "return expected whether the pins fit in the current frame" in {
    forAll(examples5) { (pins, addPins, expected) =>
      val frame = Frame()
      for(rollPins <- pins) {
        frame.add(rollPins)
      }
      assert(frame.pinsFitInThisFrame(addPins) == expected)
    }
  }

}
