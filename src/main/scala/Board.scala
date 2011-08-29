import scala.io.Source
import scala.collection.immutable.BitSet
import scala.collection.mutable.ListBuffer

package com.bmdhacks.scrabble {

  case class Move(word: String, x: Int, y: Int, direction: Symbol, score: Int)

  /*****************************************************************************
   * An actual board with it's tiles during a game
   */
  class Board(val game: Game, val dictionary: GameDictionary) {
	var tiles: Array[Array[Option[Tile]]] = Array.fill(game.boardwidth, game.boardwidth)(None)

	var letters: Array[Array[Char]] = Array.fill(game.boardwidth, game.boardwidth)(' ')
	var scores: Array[Array[Int]] = Array.fill(game.boardwidth, game.boardwidth)(0)

	var firstMove: Boolean = true

	/* used for navigating the board horizontally or vertically */
	class Cursor(var x: Int, var y: Int, val direction: Symbol) {
	  def otherDirection : Symbol = {
		if (direction == 'right)
		  'left
		else
		  'right
	  }
	  def isValid() : Boolean = {
		return (x<game.boardwidth && x>=0 && 
				y<game.boardwidth && y>=0)
	  }
	  def traverse(n: Int) : Unit = {
		if (direction == 'right) {
		  x+=n
		}
		else {
		  y+=n
		}
	  }
	}

	/* find all possible moves in all valid spaces and return a list of them, sorted by score */
	def findMoves(tset: TileSet) : List[Move] = {
	  var allspaces = for (x <- 0 until game.boardwidth; y <- 0 until game.boardwidth; if letters(y)(x) == ' ') yield (x,y)
	  allspaces.toList.flatMap(space => findMovesAtSpace(tset, space._1, space._2))
	}
	
	/* find all possible moves at the given space and return a list of them */
	def findMovesAtSpace(tset: TileSet, x: Int, y: Int) : List[Move] = {
	  val moves = new ListBuffer[Move]

	  val lengths = checkSpace(x,y)

	  List('right,'down).foreach { direction =>
		lengths(direction).foreach { length =>
		  tset.combinations(length).foreach { combo =>
			val score = checkMove(combo, x, y, direction)
			if (score > 0)
			  moves.append(new Move(combo.mkString, x, y, direction, score))
		  }
		}
	  }

	  moves.toList
	}
	
	/* given a space, returns a map containing all the possible lengths legal words going down, and
	 * the lengths of words going right.  Legal being fitting on the board and connected to at least one letter. */
	def checkSpace(x: Int, y: Int) : Map[Symbol,BitSet] = {
	  val tupleList = 
		List('right, 'down).map { direction =>
		  var cursor = new Cursor(x,y,direction)
		  var lengths = new scala.collection.mutable.BitSet
		  var foundValid = false						 

		  /* skip over leading blanks (although there really shouldn't be any) */
		  while (cursor.isValid && letters(cursor.y)(cursor.x) != ' ') {
			cursor.traverse(1)
		  }
		  for (length <- 1 to 7; if cursor.isValid) {
			if (isPlayableSpace(cursor.x, cursor.y) || foundValid) {
			  foundValid = true
			  lengths += length
			}

			do {
			  cursor.traverse(1)
			} while (cursor.isValid && letters(cursor.y)(cursor.x) != ' ') 
		  }
		  (direction, lengths.toImmutable)
	  }

	  Map.empty ++ tupleList
	}

	/* Given a tileset, coordinates, and direction, see if a valid move results and return the score */
	def checkMove(tset: TileSet, x: Int, y: Int, direction: Symbol) : Int = {
	  var cursor = new Cursor(x, y, direction)
	  var word = new StringBuilder
	  var secondaryScore = 0
	  val scorer = new Scorer(game)

	  /* get any letters behind us */
	  cursor.traverse(-1)
	  while (cursor.isValid && letters(cursor.y)(cursor.x) != ' ') {
		word.append(letters(cursor.y)(cursor.x))
		scorer.addScore(scores(cursor.y)(cursor.x))
		cursor.traverse(-1)
	  }
	  /* these were added while going backwards so we need to reverse them */
	  word.reverseContents
	  cursor.x=x
	  cursor.y=y

	  var i=0
	  while (i<tset.letters.length) {
		if (! cursor.isValid) {
		  println("ran off the board with cursor x =" + cursor.x + " and cursor y = " + cursor.y)
		  /* we ran off the board */
		  return -1
		}

		val perpendicular = checkPerpendicularWord(tset.letters(i), tset.scores(i), cursor.x, cursor.y, cursor.otherDirection)
		if (perpendicular != -1) {
		  secondaryScore += perpendicular
		}
		else {
		  /* a perpendicular word was formed that is not a real word, invalidating this move */
		  return -1
		}
		word.append(tset.letters(i))
		scorer.addScoreAtSpace(tset.scores(i), cursor.x, cursor.y)
		cursor.traverse(1)

		/* skip any existing tiles as we go */
		while (cursor.isValid && letters(cursor.y)(cursor.x) != ' ') {
		  scorer.addScore(scores(cursor.y)(cursor.x))
		  word.append(letters(cursor.y)(cursor.x))
		  cursor.traverse(1)
		}

		i+=1
	  }

	  if (dictionary.contains(word.toString)) 
		return scorer.calculateScore + secondaryScore
	  else
		return -1
	}

	/* given a tile we're considering placing in the location and
	 direction, see if a word has been created by the placement of
	 this tile and return it's score (or -1 if the resulting word is invalid) */
	private def checkPerpendicularWord(letter: Char, score: Int, x: Int, y: Int, direction: Symbol) : Int = {
	  var cursor = new Cursor(x, y, direction)

	  var word = new StringBuilder
	  val scorer = new Scorer(game)

	  /* get the tiles before our guy */
	  cursor.traverse(-1)
	  while (cursor.isValid && letters(cursor.y)(cursor.x) != ' ') {
		word.append(letters(cursor.y)(cursor.x))
		scorer.addScore(scores(cursor.y)(cursor.x))
		cursor.traverse(-1)
	  }
	  /* flip it around since it was added in reverse */
	  word.reverseContents()
	  word.append(letter)

	  /* and the tiles after our guy */
	  cursor.x = x 
	  cursor.y = y
	  cursor.traverse(1)
	  while(cursor.isValid && letters(cursor.y)(cursor.x) != ' ') {
		word.append(letters(cursor.y)(cursor.x))
		scorer.addScore(scores(cursor.y)(cursor.x))
		cursor.traverse(1)
	  }

	  /* if our guy just stands on his own and makes no new word, return 0 */
	  if (word.length == 1) {
		return 0
	  }
	  else {
		if (dictionary.contains(word.toString)) {
		  /* if it's a valid word, calculate the score */
		  scorer.addScoreAtSpace(score, x, y)
		  return scorer.calculateScore
		}
		else {
		  /* not a valid word */
		  return -1
		}
	  }
	}

	/* see if this space has any connected tiles. IE: should we explore laying down words here */
	private def isPlayableSpace(x: Int, y: Int) : Boolean = {
	  /* special case the first move of the game */
	  if (firstMove && x==7 && y==7) {
		return true
	  }

	  try {
		/* first, this space has to be empty */
		if (letters(y)(x) == ' ') {
		  if (y+1 < game.boardwidth && letters(y+1)(x) != ' ') return true
		  if (x+1 < game.boardwidth && letters(y)(x+1) != ' ') return true
		  if (y-1 >= 0 && letters(y-1)(x) != ' ') return true
		  if (x-1 >= 0 && letters(y)(x-1) != ' ') return true
		}
	  }
	  catch {
		case e:ArrayIndexOutOfBoundsException => false
	  }
	  return false
	}

	/* add the word on the board, throwing an exception if it does not fit */
	def addWord(tset: TileSet, x: Int, y: Int, direction: Symbol) {
	  val cursor = new Cursor(x, y, direction)

	  for (tile <- tset.tiles) {
		letters(cursor.y)(cursor.x) = tile.letter
		scores(cursor.y)(cursor.x) = tile.score
		cursor.traverse(1)
	  }
	  
	  firstMove = false
	}

	def mkString() : String = {
	  letters.map { row =>
		row.mkString + "\n"
	  }.mkString
	}
  }
}

