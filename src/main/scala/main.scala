import scala.io.Source
import com.bmdhacks.scrabble._

object Main {
  def main(args: Array[String]) = {

	val dictionary = new GameDictionary(io.Source.fromFile("wordlist.txt"))
	val board = new Board(WWFGame, dictionary)

	board.addWord(new TileSet("qat", WWFGame), 7, 1, 'right)
	board.addWord(new TileSet("gin", WWFGame), 12, 1, 'right)
	board.addWord(new TileSet("woven", WWFGame), 8, 2, 'right)
	board.addWord(new TileSet("nam", WWFGame), 14, 1, 'down)
	board.addWord(new TileSet("dew", WWFGame), 7, 3, 'right)
	board.addWord(new TileSet("xu", WWFGame), 11, 3, 'right)
	board.addWord(new TileSet("ka", WWFGame), 6, 4, 'right)
	board.addWord(new TileSet("sos", WWFGame), 10, 4, 'right)
	board.addWord(new TileSet("rah", WWFGame), 4, 5, 'down)
	board.addWord(new TileSet("pan", WWFGame), 9, 5, 'right)
	board.addWord(new TileSet("yech", WWFGame), 5, 6, 'down)
	board.addWord(new TileSet("dragon", WWFGame), 7, 6, 'down)
	board.addWord(new TileSet("ef", WWFGame), 9, 6, 'right)
	board.addWord(new TileSet("the", WWFGame), 3, 7, 'right)
	board.addWord(new TileSet("rages", WWFGame), 7, 7, 'right)
	board.addWord(new TileSet("turfs", WWFGame), 3, 7, 'down)
	board.addWord(new TileSet("see", WWFGame), 11, 7, 'down)
	board.addWord(new TileSet("clay", WWFGame), 5, 8, 'right)
	board.addWord(new TileSet("re", WWFGame), 10, 8, 'right)
	board.addWord(new TileSet("bi", WWFGame), 13, 8, 'right)
	board.addWord(new TileSet("brush", WWFGame), 13, 8, 'down)
	board.addWord(new TileSet("czar", WWFGame), 0, 9, 'right)
	board.addWord(new TileSet("cob", WWFGame), 6, 10, 'right)
	board.addWord(new TileSet("muds", WWFGame), 0, 11, 'right)
	board.addWord(new TileSet("nettles", WWFGame), 7, 11, 'right)
	board.addWord(new TileSet("load", WWFGame), 11, 11, 'down)
	board.addWord(new TileSet("jovial", WWFGame), 7, 13, 'right)
	board.addWord(new TileSet("hod", WWFGame), 6, 14, 'right)
	board.addWord(new TileSet("dope", WWFGame), 11, 14, 'right)
	println(board.mkString)

	val myTiles = new TileSet("toileii", WWFGame)

/*	println("Hit enter to begin")
	Console.readLine() */

	val allmoves = board.findMoves(myTiles)

/*	println("...and we're done")
	Console.readLine() */

	val moves = allmoves.sortWith((x,y) => x.score > y.score).take(10)
	val best = moves.head

	moves.foreach(println(_))

	board.addWord(new TileSet(best.word, WWFGame), best.x, best.y, best.direction)
	println(board.mkString) 
  }
}
