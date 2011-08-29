import scala.collection.mutable.ListBuffer

package com.bmdhacks.scrabble {

  /*****************************************************************************
   * The representation of each tile
   */
  case class Tile(val letter: Char, val score: Int) 

  /*****************************************************************************
   * And a set of tiles to make a word
   */
  case class TileSet(val letters: Array[Char], val scores: Array[Int]) {

	def this(tiles: List[Tile]) {
	  this(tiles.map(_.letter).toArray, tiles.map(_.score).toArray)
	}

	def this(word: String, game: Game) = {
	  this(word.toArray, word.map(game.scores(_)).toArray)
	}

	/* cache for our combinations */
	var comboCache: Array[Iterable[TileSet]] = Array.fill(7)(Iterable.empty)

	def tiles() : List[Tile] = {
	  (for (i <- 0 until letters.length) yield new Tile(letters(i), scores(i))).toList
	}

	/* a list of all possible tile combinations of the given length */
	def combinations(length: Int) : Iterable[TileSet] = {

	  if (comboCache(length-1).isEmpty) {

		/* permute a blank to all possible letters */
		def replaceBlank(tilelist: List[Tile]) : List[List[Tile]] = {
		  val blank = tilelist.indexWhere(_.letter == '?')
		  if (blank == -1) {
			List(tilelist)
		  }
		  else {
			val expanded: List[List[Tile]] = ('a' to 'z').map(x => tilelist.updated(blank, new Tile(x, 0))).toList
			expanded.flatMap(replaceBlank(_)).toList
		  }
		}

		/* return a listbuffer of lists of tiles.  Either with ? subbed for a-z or just a single element, our original list */
		val replaced = replaceBlank(tiles)
		
		/* take every distinct combination of letters, and remove duplicates */
		val combos = replaced.flatMap(_.combinations(length)).distinct

		/* now arrange each combo... */
		comboCache(length-1) = combos.flatMap( c =>
		  /* into every permutation (arrangement) of those letters */
		  c.permutations.map( p => 
			/* and turn it into a TileSet object, converting the final flat mapped batch to a list */
			new TileSet(p)))
	  }
/*	  else {
		if (length == 1 && printed == false) {
		  printed = true
		  println("done filling cache")
		  Console.readLine()
		}
	  } 
*/
	  comboCache(length-1)
	}

//	var printed = false

	def mkString() : String = {
	  letters.mkString
	}
  }
}
