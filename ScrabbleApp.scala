import scala.annotation.tailrec

/**
  * Created by filipmqv on 31.03.16.
  *
  * Zaimplementuj w Scali program znajdujący najbardziej punktowane możliwe do ułożenia legalne słowo w Scrabble.
  * Dany jest alfabet znaków A. Dany jest wektor P(c) który określa wartość punktową każdego znaku c w alfabecie.
  * Dany jest też wektor C(c) który dla każdego znaku w alfabecie określa jego liczność. Słowo to dowolna sekwencja znaków.
  * Mając dane słowo w jego punktacja P(w) to suma P(c) dla wszystkich c zawartych w w. Dany jest słownik D zawierający
  * legalne słowa.
  * Gracz posiada multizbiór R zawierający znaki z A. Znak c występuje w R nie więcej niż C(c) razy.
  * Napisz funkcję losującą R. Napisz funkcję znajdującą w R takie legalne słowo w którego P(w) jest największe spośród
  * legalnych słów możliwych do utworzenia z R.
  * Standardowe punkty znaków (i liczności):
  * 0 pt: blank (2)
  * 1 pt: E (12), A (9), I (9), O (8), N (6), R (6), T (6), L (4), S (4), U (4)
  * 2 pt: D (4), G (3)
  * 3 pt: B (2), C (2), M (2), P (2)
  * 4 pt: F (2), H (2), V (2), W (2), Y (2)
  * 5 pt: K (1)
  * 8 pt: J (1), X (1)
  * 10 pt: Q (1), Z (1)
  * Słowniki: np. /usr/share/dict/
  * Luźne uwagi: |w| ≤ |R|, sugeruje tylko małe/wielkie litery, można wczytać słownik z pliku
  */

class Scrabble(letterValueMap: Map[Char, Int], letterCountMap: Map[Char, Int], dictionary: Set[String]) {

  @tailrec
  private def getLetter(letter: Char, count: Int, acc: List[Char] = Nil): List[Char] = {
    count match {
      case 0 => acc
      case _ => getLetter(letter, count - 1, letter :: acc)
    }
  }

  private def getLettersListFromCountMap(): List[Char] = {
    val keys = letterCountMap.keysIterator
    keys.map((x: Char) => getLetter(x, letterCountMap(x))).toList.flatten
  }

  @tailrec
  private def getRandomLetters(count: Int, lettersList: List[Char], acc: List[Char] = Nil): List[Char] = {
    count match {
      case 0 => acc
      case _ =>
        val randomNumber = scala.util.Random.nextInt(lettersList.length)
        getRandomLetters(count - 1,
          lettersList.take(randomNumber) ::: lettersList.takeRight(lettersList.length - randomNumber - 1),
          lettersList(randomNumber) :: acc)
    }
  }

  def getRandomSet(size: Int = 7): List[Char] = {
    getRandomLetters(size, getLettersListFromCountMap())
  }

  //================


  private def getWordValue(word: List[Char]): Int = {
    word.foldLeft(0)((acc: Int, elem: Char) => acc + letterValueMap(elem))
  }

  @tailrec
  private def lettersMatchWord(lettersList: List[Char], word: List[Char],
                               acc: (Boolean, List[Char]) = (true, Nil)): (Boolean, List[Char]) = {
    word match {
      case letter :: tail =>
        if (lettersList.contains(letter))
          lettersMatchWord(lettersList.diff(List(letter)), tail, (true, letter :: acc._2))
        else if (lettersList.contains('-'))
          lettersMatchWord(lettersList.diff(List('-')), tail, (true, '-' :: acc._2))
        else
          lettersMatchWord(Nil, Nil, (false, Nil))
      case Nil => (acc._1, acc._2.reverse)
    }
  }

  @tailrec
  private def searchDictionary(lettersList: List[Char], dict: List[String] = dictionary.toList,
                               acc: (String, String, Int) = ("", "", 0)): (String, String, Int) = {
    dict match {
      case word :: tail => {
        if (word.length > lettersList.length) {
          // skip word impossible to construct due to its length
          searchDictionary(lettersList, tail, acc)
        } else {
          val (correct, constructedWord) = lettersMatchWord(lettersList, word.toList)
          if (correct && getWordValue(constructedWord) > acc._3)
            searchDictionary(lettersList, tail, (word, constructedWord.mkString, getWordValue(constructedWord)))
          else
            searchDictionary(lettersList, tail, acc)
        }
      }
      case Nil => acc
    }
  }

  /**
    * Construct best word from set of letters
    *
    * @param lettersList players set of letters
    * @return (word from dictionary, constructed word (possibly with '-' sign), points of constructed word)
    */
  def getBestWord(lettersList: List[Char]): (String, String, Int) = {
    // przejscie po słowniku, rozbicie każdego słowa na litery, porównanie listy liter posiadanych i liter ze słowa
    // słownikowego; uwzględnij puste puste znaki
    searchDictionary(lettersList)
  }
}

object ScrabbleApp extends App {

  /**
    * Choose language:
    * en - english, pl - polish
    */
  val USE_LANG = "pl"

  val letterValueMap = Map(
    'a' -> 1, 'b' -> 3, 'c' -> 3, 'd' -> 2, 'e' -> 1, 'f' -> 4, 'g' -> 2, 'h' -> 4, 'i' -> 1, 'j' -> 8, 'k' -> 5,
    'l' -> 1, 'm' -> 3, 'n' -> 1, 'o' -> 1, 'p' -> 3, 'q' -> 10, 'r' -> 1, 's' -> 1, 't' -> 1, 'u' -> 1, 'v' -> 4,
    'w' -> 4, 'x' -> 8, 'y' -> 4, 'z' -> 10, '-' -> 0
  )

  val letterCountMap = Map(
    'a' -> 9, 'b' -> 2, 'c' -> 2, 'd' -> 4, 'e' -> 12, 'f' -> 2, 'g' -> 3, 'h' -> 2, 'i' -> 9, 'j' -> 1, 'k' -> 1,
    'l' -> 4, 'm' -> 2, 'n' -> 6, 'o' -> 8, 'p' -> 2, 'q' -> 1, 'r' -> 6, 's' -> 4, 't' -> 6, 'u' -> 4, 'v' -> 2,
    'w' -> 2, 'x' -> 1, 'y' -> 2, 'z' -> 1, '-' -> 2
  )

  val letterCountMapPl = Map(
    'a' -> 9, 'ą' -> 1, 'b' -> 2, 'c' -> 3, 'ć' -> 1, 'd' -> 3, 'e' -> 7, 'ę' -> 1, 'f' -> 1, 'g' -> 2,
    'h' -> 2, 'i' -> 8, 'j' -> 2, 'k' -> 3, 'l' -> 3, 'ł' -> 2, 'm' -> 3, 'n' -> 5, 'ń' -> 1, 'o' -> 6,
    'ó' -> 1, 'p' -> 3, 'r' -> 4, 's' -> 4, 'ś' -> 1, 't' -> 3, 'u' -> 2, 'w' -> 4, 'y' -> 4, 'z' -> 5,
    'ź' -> 1, 'ż' -> 1, '-' -> 2
  )

  val letterValueMapPl = Map(
    'a' -> 1, 'ą' -> 5, 'b' -> 3, 'c' -> 2, 'ć' -> 6, 'd' -> 2, 'e' -> 1, 'ę' -> 5, 'f' -> 5, 'g' -> 3,
    'h' -> 3, 'i' -> 1, 'j' -> 3, 'k' -> 2, 'l' -> 2, 'ł' -> 3, 'm' -> 2, 'n' -> 1, 'ń' -> 7, 'o' -> 1,
    'ó' -> 5, 'p' -> 2, 'r' -> 1, 's' -> 1, 'ś' -> 5, 't' -> 2, 'u' -> 3, 'w' -> 1, 'y' -> 2, 'z' -> 1,
    'ź' -> 9, 'ż' -> 5, '-' -> 0
  )

  println("Loading dictionary")
  val path = if (USE_LANG == "en") "/usr/share/dict/american-english" else "/usr/share/dict/polish"
  val dictionary = scala.io.Source.fromFile(path).getLines.toSet.map((x: String) => x.toLowerCase)
  println("Dictionary size: " + dictionary.size)

  val s = if (USE_LANG == "en") new Scrabble(letterValueMap, letterCountMap, dictionary)
  else new Scrabble(letterValueMapPl, letterCountMapPl, dictionary)

  println("Creating random set")
  val randomSet = s.getRandomSet(50)
  println(randomSet)

  println("Searching")
  val (bestWord, constructedWord, points) = s.getBestWord(randomSet)
  println("\nWord found: " + bestWord + ",\t\t constructed: " + constructedWord + "\t\t points: " + points)

}
