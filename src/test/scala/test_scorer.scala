import org.scalatest.FlatSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.ShouldMatchers
import com.bmdhacks.scrabble._

class ScorerSpec extends FlatSpec with ShouldMatchers {

  val scorer = new Scorer(WWFGame)

  "Scorer" should "tally simple tiles" in {

	assert(scorer.calculateScore == 0)
	scorer.addTile(new Tile('f',4))
	assert(scorer.calculateScore == 4)
	scorer.addTile(new Tile('o', 1))
	assert(scorer.calculateScore == 5)
	scorer.addTile(new Tile('z', 0))
	assert(scorer.calculateScore == 5)
  }

  it should "tally letter multipliers" in {
	/* triple letter */
	scorer.addTileAtSpace(new Tile('o', 1), 0, 6)
	assert(scorer.calculateScore == 8)

	/* double letter */
	scorer.addTileAtSpace(new Tile('u', 2), 1, 2)
	assert(scorer.calculateScore == 12)
  }

  it should "tally word multipliers" in {
	/* triple word */
	scorer.addTileAtSpace(new Tile('o', 0), 0, 3)
	assert(scorer.calculateScore == 36)

	/* double word */
	scorer.addTileAtSpace(new Tile('x', 8), 1, 5)
	assert(scorer.calculateScore == 120)
  }

  it should "tally bingos" in {
	scorer.addTileAtSpace(new Tile('m', 0), 14, 0)
	scorer.addTileAtSpace(new Tile('m', 0), 14, 1)
	scorer.addTileAtSpace(new Tile('m', 0), 14, 2)
	info("score is " + scorer.calculateScore)
	assert(scorer.calculateScore == 155)
  }
}
