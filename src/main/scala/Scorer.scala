package com.bmdhacks.scrabble {

  class Scorer(val game: Game) {
	var multiplier = 1
	var tally = 0
	var tilesUsed = 0

	/* add a tile without concern for the space it's on */
	def addTile(tile: Tile) : Unit = {
	  tally += tile.score
	}

	def addScore(score: Int) : Unit = {
	  tally += score
	}

	def addScoreAtSpace(score: Int, x: Int, y: Int) {
	  tilesUsed += 1
	  game.spaces(y)(x) match {
		case 'n => tally += score
		case 'dl => tally += score*2
		case 'tl => tally += score*3
		case 'dw => 
		  tally += score
		  multiplier *= 2
		case 'tw =>
		  tally += score
		  multiplier *= 3
	  }
	}

	def addTileAtSpace(tile: Tile, x: Int, y: Int) = addScoreAtSpace(tile.score, x, y)

	def calculateScore() : Int = {
	  val bingoBonus = if (tilesUsed == 7) 35 else 0
	  return tally*multiplier + bingoBonus
	}
  }
}
