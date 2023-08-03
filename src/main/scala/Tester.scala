object WordleSolver extends App {

	// Filter the given sequence of words based on the provided guess and colors.
	def filterWords(words: Vector[String], guess: String, colors: String): Vector[String] =
		// Extract indices of green, yellow, and blank positions from the colors string.
		val c = colors.toList.zipWithIndex
		// Update the maps containing indices of green, yellow, and blank positions along with the characters.
		val _indexOfGreens = c.filter(_._1 == 'g').map(x => x._2 -> Set(guess(x._2))).toMap
		val _indexOfYellows = c.filter(_._1 == 'y').map(x => x._2 -> Set(guess(x._2))).toMap
		val _indexOfBlanks = c.filter(_._1 == 'b').map(x => x._2 -> Set(guess(x._2))).toMap

		// Start with the complete list of words and filter it step by step.
		var filteredWords = words
		// Filter words based on the green positions.
		filteredWords = filteredWords.filter { word =>
			_indexOfGreens.forall { (index, charSet) =>
				charSet.forall(char => word(index) == char)
			}
		}
		// Filter words based on the yellow positions.
		filteredWords = filteredWords.filter { word =>
			_indexOfYellows.forall { (index, charSet) =>
				charSet.forall(char => word.contains(char) && word(index) != char)
			}
		}
		// Filter words based on the blank positions.
		filteredWords = filteredWords.filter { word =>
			val sb = new StringBuilder(word)
			_indexOfBlanks.forall { (index, charSet) =>
				charSet.forall { char =>
					!word.contains(char) ||
					(!_indexOfGreens.filter(x =>
						for (i <- _indexOfGreens.filter(x => x._2.contains(char))) {
							sb(i._1) = '.'
						};
						x._2.contains(char)).isEmpty &&
						!sb.contains(char)) ||
						(word(index) != char &&
						!_indexOfYellows.filter(x => x._2.contains(char)).isEmpty)
				}
			}
		}
		return filteredWords

	// Test the filterWords method with sample data.
	val words = Vector("apple", "berry", "grape", "lemon", "melon", "banan", "orang")
	val guess = "apple"
	val colors = "yybbg"

	val startTime = System.nanoTime()
	val result = filterWords(words, guess, colors)
	val endTime = System.nanoTime()
	val elapsedTime = (endTime - startTime) / 1e6 // Convert to milliseconds

	println(s"Filtered Words: $result")
	println(s"Elapsed Time: $elapsedTime ms")
}
