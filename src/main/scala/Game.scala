package com.bmdhacks.scrabble {

  /*****************************************************************************
   * Abstract class for game boards, to be implemented by various boards such
   * as scrabble, words with friends, etc.
   */
  abstract class Game {
	val scores: Map[Char,Int]
	val spaces: List[List[Symbol]]
	val boardwidth: Int

	def applyMultiplier(score: Int, multiplier: Int, x: Int, y: Int) : Tuple2[Int, Int] = {
	  spaces(y)(x) match {
		case 'n => (score, multiplier)
		case 'dl => (2*score, multiplier)
		case 'tl => (3*score, multiplier)
		case 'dw => (score, multiplier*2)
		case 'tw => (score, multiplier*3)
	  }
	}
  }

  /*****************************************************************************
   * Implements the Words with Friends game board
   */
  object WWFGame extends Game {

	/* words with friends tile scores */
	val scores = Map(
	  'a' -> 1, 
	  'b' -> 4,
	  'c' -> 4,
	  'd' -> 2,
	  'e' -> 1,
	  'f' -> 4,
	  'g' -> 3,
	  'h' -> 3,
	  'i' -> 1,
	  'j' -> 10,
	  'k' -> 5,
	  'l' -> 2,
	  'm' -> 4,
	  'n' -> 2,
	  'o' -> 1,
	  'p' -> 4,
	  'q' -> 10,
	  'r' -> 1,
	  's' -> 1,
	  't' -> 1,
	  'u' -> 2,
	  'v' -> 5,
	  'w' -> 4,
	  'x' -> 8,
	  'y' -> 3,
	  'z' -> 10,
	  '?' -> 0)

	/* words with friends board */
	val spaces = List(
	  List('n,  'n,  'n, 'tw,  'n,  'n, 'tl,  'n, 'tl,  'n,  'n, 'tw,  'n,  'n,  'n),
	  List('n,  'n,  'dl, 'n,  'n,  'dw, 'n,  'n,  'n,  'dw, 'n,  'n,  'dl, 'n,  'n),
	  List('n,  'dl, 'n,  'n,  'dl, 'n,  'n,  'n,  'n,  'n,  'dl, 'n,  'n,  'dl, 'n),
	  List('tw, 'n,  'n,  'tl, 'n,  'n,  'n,  'dw, 'n,  'n,  'n,  'tl, 'n,  'n,  'tw),
	  List('n,  'n,  'dl, 'n,  'n,  'n,  'dl, 'n,  'dl, 'n,  'n,  'n,  'dl, 'n,  'n),
	  List('n,  'dw, 'n,  'n,  'n,  'tl, 'n,  'n,  'n,  'tl, 'n,  'n,  'n,  'dw, 'n),
	  List('tl, 'n,  'n,  'n,  'dl, 'n,  'n,  'n,  'n,  'n,  'dl, 'n,  'n,  'n,  'tl),
	  List('n,  'n,  'n,  'dw, 'n,  'n,  'n,  'n,  'n,  'n,  'n,  'dw, 'n,  'n,  'n),
	  List('tl, 'n,  'n,  'n,  'dl, 'n,  'n,  'n,  'n,  'n,  'dl, 'n,  'n,  'n,  'tl),
	  List('n,  'dw, 'n,  'n,  'n,  'tl, 'n,  'n,  'n,  'tl, 'n,  'n,  'n,  'dw, 'n),
	  List('n,  'n,  'dl, 'n,  'n,  'n,  'dl, 'n,  'dl, 'n,  'n,  'n,  'dl, 'n,  'n),
	  List('tw, 'n,  'n,  'tl, 'n,  'n,  'n,  'dw, 'n,  'n,  'n,  'tl, 'n,  'n,  'tw),
	  List('n,  'dl, 'n,  'n,  'dl, 'n,  'n,  'n,  'n,  'n,  'dl, 'n,  'n,  'dl,  'n),
	  List('n,  'n,  'dl, 'n,  'n,  'dw, 'n,  'n,  'n,  'dw, 'n,  'n,  'dl,  'n,  'n),
	  List('n,  'n,  'n,  'tw, 'n,  'n,  'tl, 'n,  'tl, 'n,  'n,  'tw,  'n,  'n,  'n)
	)

	val boardwidth = spaces.length
  }
}
