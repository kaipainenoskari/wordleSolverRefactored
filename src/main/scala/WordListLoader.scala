import scala.io.Source
import scala.util.Random

class WordListLoader(lang: String, wordLength: Int):
	private val rand = Random()

	def loadWordListFromFile(filePath: String): Vector[String] =
		var ret = Source.fromFile(filePath).getLines.toVector.map(word => word.toLowerCase())
		if lang == "fi" then
			// Deal with umlauts -> change ä to uppercase A, ö to uppercase O
			ret = ret.map(string =>
				string.filter(char => char.toInt != 227 && char != '-').map {
					case char if char.toInt == 164 || char.toInt == 8222 => 'A'
					case char if char.toInt == 182 || char.toInt == 8211 => 'O'
					case char => char
				}
			)
		
		rand.shuffle(ret.filter(word => word.length == wordLength))
	
	def generateNumble(wordLength: Int): Vector[String] =
		def inner(start: String, i: Int): Seq[String] =
			if (i == wordLength) then
				Vector("0", "1", "2", "3", "4", "5", "6", "7", "8", "9").map(start + _)
			else
				Vector("0", "1", "2", "3", "4", "5", "6", "7", "8", "9").flatMap(l => inner(start + l, i + 1))
		inner("", 1).toVector

	// TODO: figure out how to work with files
	def loadAnswerList: Vector[String] =
		val path = "utils/"
		val file = lang match
			case "en" if wordLength == 5 => "wordle-answers-alphabetical.txt"
			case "en" => "words_alpha.txt"
			case "fi" => "kaikki-suomen-sanat.txt"
			case _ => return generateNumble(wordLength)

		loadWordListFromFile(path + file)

	def loadValidWords: Vector[String] =
		val path = "utils/"
		val file = lang match
			case "en" if wordLength == 5 => "valid-wordle-words.txt"
			case "en" => "words_alpha.txt"
			case _ => "kaikki-suomen-sanat.txt"
		
		loadWordListFromFile(path + file)
