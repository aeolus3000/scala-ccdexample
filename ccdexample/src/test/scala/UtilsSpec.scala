import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class UtilsSpec extends FlatSpec with TableDrivenPropertyChecks {

  val examples =
    Table(
      ("pins","expected"),
      (10, true),
      (9, false),
    )

  "Utils.has10pins" should "return true if pins = 10" in {
    val has10Pins =
      forAll(examples) { (pins, expected) =>
        val has10Pins: Boolean = Utils.has10Pins(pins)
        assert(has10Pins == expected)
      }
  }

}
