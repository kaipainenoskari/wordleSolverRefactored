// A class to filter words based on provided green, yellow, and blank positions.
class WordFilter:
	var indexOfGreens = Map[Int, Set[Char]]()
	var indexOfYellows = Map[Int, Set[Char]]()
	var indexOfBlanks = Map[Int, Set[Char]]()

	// Filter the given sequence of words based on the provided guess and colors.
	def filterWords(words: Vector[String], guess: String, colors: String): Vector[String] =
		// Extract indices of green, yellow, and blank positions from the colors string.
		val c = colors.toList.zipWithIndex
		// Update the maps containing indices of green, yellow, and blank positions.
		val _indexOfGreens = merge(indexOfGreens, c.filter(_._1 == 'g').map(x => x._2 -> Set(guess(x._2))).toMap)
		val _indexOfYellows = merge(indexOfYellows, c.filter(_._1 == 'y').map(x => x._2 -> Set(guess(x._2))).toMap)
		val _indexOfBlanks = merge(indexOfBlanks, c.filter(_._1 == 'b').map(x => x._2 -> Set(guess(x._2))).toMap)

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

	// Update the maps of green, yellow, and blank positions based on the provided colors and guess.
	def updateColorIndexes(colors: String, guess: String): Unit =
		val c = colors.toList.zipWithIndex
		indexOfGreens = merge(indexOfGreens, c.filter(_._1 == 'g').map(x => x._2 -> Set(guess(x._2))).toMap)
		indexOfYellows = merge(indexOfYellows, c.filter(_._1 == 'y').map(x => x._2 -> Set(guess(x._2))).toMap)
		indexOfBlanks = merge(indexOfBlanks, c.filter(_._1 == 'b').map(x => x._2 -> Set(guess(x._2))).toMap)

	// Check if the WordFilter has no information stored (no greens, yellows, or blanks).
	def isEmpty: Boolean =
		indexOfGreens.isEmpty && indexOfYellows.isEmpty && indexOfBlanks.isEmpty

	// Get all indices in a string.
	def indices: String =
		return s"Greens: ${indexOfGreens.mkString(", ")} \nYellows: ${indexOfYellows.mkString(", ")} \nBlanks: ${indexOfBlanks.mkString(", ")} \n"

	// Merge two maps containing sets of characters based on the same index.
	private def merge(m1: Map[Int, Set[Char]], m2: Map[Int, Set[Char]]): Map[Int, Set[Char]] =
		(m1.toSeq ++ m2).groupMap(_._1)(_._2).view.mapValues(_.flatten.toSet).toMap
