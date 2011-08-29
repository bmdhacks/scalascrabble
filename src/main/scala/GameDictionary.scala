import scala.io.Source
import scala.collection.immutable.HashSet

package com.bmdhacks.scrabble {

  /*****************************************************************************
   * List of possible words in the dictionary
   */
  class GameDictionary(val wordlist: HashSet[String]) extends Iterable[String] {
	def this(file: scala.io.Source) = {
	  this(HashSet() ++ file.getLines())
	}

	def this(text: String) = {
	  this(HashSet() ++ text.split(",").toList)
	}

	def contains(word: String) = wordlist.contains(word)
	
	def iterator = wordlist.iterator
  }
}
