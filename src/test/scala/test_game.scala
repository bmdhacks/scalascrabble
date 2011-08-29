import org.scalatest.FlatSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.ShouldMatchers
import com.bmdhacks.scrabble._

class GameSpec extends FlatSpec with ShouldMatchers with PrivateMethodTester {

  "WWFGame" should "be cool" in {
	val spacetypes = List('n, 'tw, 'dw, 'tl, 'dl)
	assert(WWFGame.spaces.length == 15)
	assert(WWFGame.spaces(0).length == 15)
	assert(WWFGame.boardwidth == 15)
	for (row <- WWFGame.spaces) {
	  for (space <- row) {
		spacetypes.contains(space)
	  }
	}
  }

}
