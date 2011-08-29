import org.scalatest.FlatSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.ShouldMatchers
import com.bmdhacks.scrabble._
import scala.collection.immutable.BitSet

class BoardSpec extends FlatSpec with ShouldMatchers with PrivateMethodTester {

  val tiles1 = new TileSet(List(new Tile('f', 4), new Tile('o', 1), new Tile('o', 1)))
  val tiles2 = new TileSet("foo", WWFGame)
  val tiles3 = new TileSet("llamas", WWFGame)
  val tiles4 = new TileSet("bri?n", WWFGame)

  val dictionary = new GameDictionary("foo,bar,llamas,cheese,of,oo,lo,fool,fo")
  val board = new Board(WWFGame, dictionary)

  "Board" should "add items" in {
	board.addWord(tiles2, 0, 8, 'right)
	board.addWord(tiles3, 4, 8, 'down)
  }

  it should "stringify" in {
	val text:String = board.mkString
	assert(text == 
		   "               \n" +
		   "               \n" +
		   "               \n" +
		   "               \n" +
		   "               \n" +
		   "               \n" +
		   "               \n" +
		   "               \n" +
		   "foo l          \n" +
		   "    l          \n" +
		   "    a          \n" +
		   "    m          \n" +
		   "    a          \n" +
		   "    s          \n" +
		   "               \n")
  }

  it should "report playable spaces" in {
	val isPlayableSpace = PrivateMethod[Boolean]('isPlayableSpace)
	assert((board invokePrivate isPlayableSpace(15, 15)) == false)
	assert((board invokePrivate isPlayableSpace(2, 8)) == false)
	assert((board invokePrivate isPlayableSpace(0, 0)) == false)
	assert((board invokePrivate isPlayableSpace(3, 8)) == true)
	assert((board invokePrivate isPlayableSpace(2, 7)) == true)
	assert((board invokePrivate isPlayableSpace(2, 9)) == true)
  }
  
  it should "find playable lengths" in {
	val checkMap1 = board.checkSpace(5,7)
	assert(checkMap1('right) == BitSet.empty)
	assert(checkMap1('down) == BitSet(2,3,4,5,6,7))
	val checkMap2 = board.checkSpace(4,7)
	assert(checkMap2('right) == BitSet(1,2,3,4,5,6,7))
	assert(checkMap2('down) == BitSet(1,2))
  }

  it should "check perpendicular words" in {
	val checkPerpendicularWord = PrivateMethod[Int]('checkPerpendicularWord)
	
	/* double word */
	val of = board invokePrivate checkPerpendicularWord('f', 4, 1, 9, 'down)
	assert(of == 10)

	/* no multiplier */
	val fo = board invokePrivate checkPerpendicularWord('f', 4, 1, 7, 'down)
	assert(fo == 5)

	/* double letter */
	val lo = board invokePrivate checkPerpendicularWord('o', 1, 5, 9, 'right)
	assert(lo == 5)

	/* no word made */
	val nw = board invokePrivate checkPerpendicularWord('o', 1, 2, 2, 'right)
	assert(nw == 0)

	/* invalid word */
	val iw = board invokePrivate checkPerpendicularWord('o', 1, 4, 7, 'down)
	assert(iw == -1)
  }

  val dictionary2= new GameDictionary(io.Source.fromFile("wordlist.txt"))
  val board2 = new Board(WWFGame, dictionary2)
  board2.addWord(new TileSet("posies", WWFGame), 7, 7, 'down)
  board2.addWord(new TileSet("hoop", WWFGame), 4, 7, 'right)
  board2.addWord(new TileSet("pelt", WWFGame), 3, 6, 'right)

  it should "check moves" in {
	val score = board.checkMove(tiles2, 1, 9, 'right)
	/* double word "of" = 10, "oo" = 2, double word "fool" = 16 */
	assert(score == 10+2+16)
	
	val yaud = new TileSet("yaud", WWFGame)
	val score2 = board2.checkMove(yaud, 6, 11, 'down)
	assert(score2 == 18)
  }

  val myTiles = new TileSet("aadyuer", WWFGame)

  it should "find the best move in a space" in {
	val moves = board2.findMovesAtSpace(myTiles, 6, 11).sortWith((x,y) => x.score > y.score)
	val best = moves.head

	assert(best.word == "yaud")
	assert(best.x == 6)
	assert(best.y == 11)
	assert(best.score == 18)
  }


  it should "find the best move" in {
	val moves = board2.findMoves(myTiles).sortWith((x,y) => x.score > y.score)
	val best = moves.head

	assert(best.word == "yed")
	assert(best.x == 6)
	assert(best.y == 8)
	assert(best.score == 21)
  }
}
