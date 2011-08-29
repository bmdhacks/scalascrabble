import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.bmdhacks.scrabble._

class GameDictionarySpec extends FlatSpec with ShouldMatchers {

  val wordlist = new GameDictionary("foo,bar,llamas,cheese")
  val dictionary = new GameDictionary(io.Source.fromFile("wordlist.txt"))

  "A GameDictionary" should "match words" in {
	assert(wordlist.contains("foo"))
	assert(wordlist.contains("bar"))
	assert(! wordlist.contains("cornucopia"))
  }

  it should "iterate" in {
	val iterlist = for (word <- wordlist) yield word
	assert(iterlist.toList.diff(List("llamas", "foo", "bar", "cheese")) == Nil)
  }
}
