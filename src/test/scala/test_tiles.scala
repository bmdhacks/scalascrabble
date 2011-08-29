import org.scalatest.FlatSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.ShouldMatchers
import com.bmdhacks.scrabble._

class TilesSpec extends FlatSpec with ShouldMatchers {

  val tiles1 = new TileSet(List(new Tile('f', 4), new Tile('o', 1), new Tile('o', 1)))
  val tiles2 = new TileSet("foo", WWFGame)
  val tiles3 = new TileSet("llamas", WWFGame)
  val tiles4 = new TileSet("bri?n", WWFGame)
  val tiles5 = new TileSet("b?", WWFGame)

  "TileSet" should "stringify" in {
	assert(tiles1.mkString == "foo")
	assert(tiles3.mkString == "llamas")
	assert(tiles4.mkString == "bri?n")
  }

  it should "produce all combinations" in {
	/* pruning duplicates - IE: only one copy of 'fo' depite two different o's */
	val combinations1 = List(
	  new TileSet(List(new Tile('f', 4), new Tile('o', 1))),
	  new TileSet(List(new Tile('o', 1), new Tile('f', 4))),
	  new TileSet(List(new Tile('o', 1), new Tile('o', 1))))

	val cw1 = combinations1.map(_.letters.mkString).toList
	val fcw1 = tiles1.combinations(2).map(_.letters.mkString).toList
	assert(cw1.diff(fcw1) == Nil)

	/* multiplicative properties of blanks. */
	val combinations5 = List(
	  new TileSet(List(new Tile('b',4), new Tile('a',0))),
	  new TileSet(List(new Tile('a',0), new Tile('b',4))),
	  /* note the 0 pointed tile in both spots here, thus having to equivalent strings but different possible scores */
	  new TileSet(List(new Tile('b',4), new Tile('b',0))),
	  new TileSet(List(new Tile('b',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('c',0))),
	  new TileSet(List(new Tile('c',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('d',0))),
	  new TileSet(List(new Tile('d',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('e',0))),
	  new TileSet(List(new Tile('e',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('f',0))),
	  new TileSet(List(new Tile('f',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('g',0))),
	  new TileSet(List(new Tile('g',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('h',0))),
	  new TileSet(List(new Tile('h',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('i',0))),
	  new TileSet(List(new Tile('i',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('j',0))),
	  new TileSet(List(new Tile('j',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('k',0))),
	  new TileSet(List(new Tile('k',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('l',0))),
	  new TileSet(List(new Tile('l',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('m',0))),
	  new TileSet(List(new Tile('m',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('n',0))),
	  new TileSet(List(new Tile('n',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('o',0))),
	  new TileSet(List(new Tile('o',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('p',0))),
	  new TileSet(List(new Tile('p',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('q',0))),
	  new TileSet(List(new Tile('q',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('r',0))),
	  new TileSet(List(new Tile('r',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('s',0))),
	  new TileSet(List(new Tile('s',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('t',0))),
	  new TileSet(List(new Tile('t',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('u',0))),
	  new TileSet(List(new Tile('u',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('v',0))),
	  new TileSet(List(new Tile('v',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('w',0))),
	  new TileSet(List(new Tile('w',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('x',0))),
	  new TileSet(List(new Tile('x',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('y',0))),
	  new TileSet(List(new Tile('y',0), new Tile('b',4))),
	  new TileSet(List(new Tile('b',4), new Tile('z',0))),
	  new TileSet(List(new Tile('z',0), new Tile('b',4))))

	assert(tiles5.combinations(2).map(_.letters.mkString).toList.diff(combinations5.map(_.letters.mkString).toList) == Nil)
	assert(tiles5.combinations(2).map(_.scores.toList).toList.diff(combinations5.map(_.scores.toList).toList) == Nil)
  }
}
