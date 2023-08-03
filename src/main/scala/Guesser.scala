//import me.tongfei.progressbar._

class Guesser(wordLength: Int):
    private val permutations = new Permutations(wordLength)

    // Method to find the best guess from the answer list based on a set of criteria
    def bestGuess(answerList: Vector[String], validWordList: Vector[String], wordFilter: WordFilter): String =
        var sizes = scala.collection.mutable.Map[String, Vector[Int]]()
        var sizeAverages = scala.collection.mutable.Map[String, Double]()
        var validWords = validWordList
        val startSize = validWords.length.toDouble
        //val progressBar = new ProgressBar(max = startSize)
        var wordfilterTime = 0L
        var permutationTime = 0L
        var everythingElse = System.nanoTime()
        var printTime = 0L

        // Loop through the list of valid words and compute their sizes
        while validWords.nonEmpty do
            val p1 = System.nanoTime()
            val current_word = validWords.head
            val sizeNow = validWords.length
            val percent = math.ceil(((startSize - sizeNow) / startSize) * 100).round.toInt
            val progress = "#" * percent
            val toRun = "-" * (100 - percent)
            print("\r[%s%s] %d %%".format(progress, toRun, percent))
            //progressBar.update(sizeNow)
            printTime += System.nanoTime() - p1

            val t1 = System.nanoTime()
            // Generate permutations for the current word based on word filters
            val perm =
                if (wordFilter.isEmpty)
                    then permutations.permutationSet
                    else permutations.minimizePermutations(current_word, wordFilter).toVector
            permutationTime += System.nanoTime() - t1

            // Iterate through the permutations to compute the sizes of matching words
            var i = 0
            while i < perm.size do
                val t1 = System.nanoTime()
                val currentSize = wordFilter.filterWords(answerList, current_word, perm(i)).size
                wordfilterTime += System.nanoTime() - t1
                if currentSize != 0 then
                    // Store sizes of matching words for the current word
                    sizes(current_word) = sizes.get(current_word) match
                        case Some(value) => value :+ currentSize
                        case None => Vector(currentSize)
                i += 1

            // Find the maximum candidate size for the current word
            val candidateSize =
                if sizes.keySet.contains(current_word) then sizes(current_word).max
                else Int.MaxValue

            // If the word is a single candidate and exists in the answer list, return it as the best guess
            if candidateSize == 1 && answerList.contains(current_word) then
                return current_word

            // Compute the average size for the current word and store it in the sizeAverages map
            sizeAverages += current_word -> sizes(current_word).sum / sizes(current_word).length.toDouble
            validWords = validWords.tail
        //printTime -= wordfilterTime + permutationTime

        // Process the sizeAverages map to find the top 10 best guesses based on certain criteria
        val topGuesses = sizeAverages.toVector
            .map((word, average) => if answerList.contains(word) then
                (word, average - (1 / answerList.length.toDouble))
            else (word, average))
            .sortBy(_._2)
            .take(10)
            .map((word, average) => (word, (average * 100).round / 100.0))
        everythingElse = System.nanoTime() - everythingElse - wordfilterTime - permutationTime - printTime
            
        println(s"\nFilter: ${(wordfilterTime / 1e9d).round}s")
        println(s"Permutations: ${(permutationTime / 1e9d).round}s")
        println(s"Printing : ${(printTime / 1e9d).round}s")
        println(s"Everything else: ${(everythingElse / 1e9d).round}s")
        println(s"Summed: ${((wordfilterTime + permutationTime + everythingElse + printTime) / 1e9d).round}s")
        //progressBar.close()
        topGuesses.mkString(", ")
