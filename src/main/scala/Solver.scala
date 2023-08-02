import scala.io.StdIn.readLine
import scala.util.Random

// A class that handles the word guessing game
class Solver:
    // Use descriptive variable names to improve readability
    private var newGame = true
    private var lang: String = _
    private var wordLength: Int = _
    private var answerList: Vector[String] = _
    private var validWordList: Vector[String] = _
    private var wordFilter: WordFilter = _
    private var bestGuessSolver: Guesser = _

    // Method to start the game
    def start(): Unit =
        while newGame do
            lang = readLanguage()
            wordLength = readWordLength()
            loadLists()
            wordFilter = new WordFilter()
            bestGuessSolver = new Guesser(lang, wordLength)

            var continueGame = true
            while continueGame do
                val guess = readGuess()
                if guess == "exit" then
                    continueGame = false
                    newGame = false
                else if guess == "list" then
                    println(answerList)
                else if guess == "answer" then
                    // TODO: Improve this section to avoid code duplication
                    if answerList.length <= 2 then
                        println(Random.shuffle(answerList).head)
                    else
                        // Measure the time it takes to find the best guess and print it
                        val t1 = System.nanoTime()
                        println("Running...")
                        println("\n" + bestGuessSolver.bestGuess(answerList, validWordList, wordFilter))
                        val duration = ((System.nanoTime() - t1) / 1e9d).round
                        println(s"Duration $duration seconds")
                else if guess.length == wordLength && !guess.contains(' ') then
                    val colors = readColors(wordLength)
                    if colors == "g" * wordLength then
                        continueGame = false
                    else
                        wordFilter.updateColorIndexes(colors, guess)
                        val filteredWords = wordFilter.filterWords(answerList, guess, colors).toVector
                        updateAnswerList(filteredWords)
                        // Consider refactoring this block to reduce nesting
                        if answerList.size <= 10 then
                            if answerList.isEmpty then
                                println("No answers match...")
                            else
                                println(answerList.mkString(", "))
                else
                    println("Incorrect input...")

    // Load the wordlists based on the selected language and word length
    private def loadLists(): Unit =
        val wordListLoader = new WordListLoader(lang, wordLength)
        answerList = wordListLoader.loadAnswerList
        validWordList = wordListLoader.loadValidWords

    // Method to update the answer list with filtered words
    private def updateAnswerList(filteredWords: Vector[String]): Unit =
        answerList = filteredWords

    // Read the selected language from the user
    private def readLanguage(): String =
        var lang = readLine("Select language fi/en\n").toLowerCase()
        while lang != "en" && lang != "fi" do
            lang = readLine("Select language fi/en\n").toLowerCase()
        lang

    // Read the word length from the user
    private def readWordLength(): Int =
        var lengthString = readLine("Select length\n")
        while !lengthString.forall(_.isDigit) do
            lengthString = readLine("Select length\n")
        lengthString.toInt

    // Read the player's guess from the user
    private def readGuess(): String =
        readLine("guess\n")

    // Read the colors from the user and validate the input
    private def readColors(wordLength: Int): String =
        var colors = readLine("colors\n").toLowerCase()
        while colors.length != wordLength || !colors.forall(c => c == 'g' || c == 'y' || c == 'b') do
            println("Incorrect input...")
            colors = readLine("colors\n").toLowerCase()
        colors
