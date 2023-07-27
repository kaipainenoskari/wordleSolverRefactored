class Guesser(lang: String, wordLength: Int):
    private val permutations = new Permutations(wordLength)

    // Method to find the best guess from the answer list based on a set of criteria
    def bestGuess(answerList: Seq[String], validWordList: Vector[String], wordFilter: WordFilter): String =
        var sizes = scala.collection.mutable.Map[String, Vector[Int]]()
        var sizeAverages = scala.collection.mutable.Map[String, Double]()
        var validWords = validWordList
        val startSize = validWords.length.toDouble

        // Loop through the list of valid words and compute their sizes
        while validWords.nonEmpty do
            val current_word = validWords.head
            val sizeNow = validWords.length
            val percent = math.ceil(((startSize - sizeNow) / startSize) * 100).toInt
            val progress = "#" * percent
            val toRun = "-" * (100 - percent)
            print("\r[%s%s] %d %%".format(progress, toRun, percent))

            // Generate permutations for the current word based on word filters
            val perm =
                if (wordFilter.isEmpty)
                    then permutations.permutationSet
                    else permutations.minimizePermutations(current_word, wordFilter.indexOfGreens, wordFilter.indexOfYellows, wordFilter.indexOfBlanks).toVector

            // Iterate through the permutations to compute the sizes of matching words
            var i = 0
            for p <- perm do
                val currentSize = wordFilter.filterWords(answerList, current_word, p).length
                if currentSize != 0 then
                    // Store sizes of matching words for the current word
                    sizes(current_word) = sizes.get(current_word) match
                        case Some(value) => value :+ currentSize
                        case None => Vector(currentSize)
                i += 1

            // Find the maximum candidate size for the current word
            val candidateSize =
                if sizes.contains(current_word) then sizes(current_word).max
                else Int.MaxValue

            // If the word is a single candidate and exists in the answer list, return it as the best guess
            if candidateSize == 1 && answerList.contains(current_word) then
                return current_word

            // Compute the average size for the current word and store it in the sizeAverages map
            sizeAverages += current_word -> sizes(current_word).sum / sizes(current_word).length.toDouble
            validWords = validWords.tail

        // Process the sizeAverages map to find the top 10 best guesses based on certain criteria
        val topGuesses = sizeAverages.toVector
            .map((word, average) => if answerList.contains(word) then
                (word, average - (1 / answerList.length.toDouble))
            else (word, average))
            .sortBy(_._2)
            .take(10)
            .map((word, average) => (word, (average * 100).round / 100.0))
            
        topGuesses.mkString(", ")
