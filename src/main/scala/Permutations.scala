// A class to generate permutations and filter them based on specific criteria.
class Permutations(wordLength: Int):
    // Generate all possible permutations of the given word length.
    val permutationSet = generatePermutations(wordLength)

    // Get all indices of a specific character in a given word.
    def getAllIndices(word: String, char: Char): Seq[Int] =
        word.zipWithIndex.collect { case (c, i) if c == char => i }

    // Generate all possible permutations of the given word length.
    def generatePermutations(wordLength: Int): Vector[String] =
        val chars = Vector("g", "y", "b")
        def inner(start: String, i: Int, acc: Vector[String]): Vector[String] =
            if (i == wordLength) then
                acc :+ start
            else
                chars.foldLeft(acc) { (result, digit) =>
                    inner(start + digit, i + 1, result)
                }
        inner("", 0, Vector.empty)

    // Filter the permutationSet based on the provided criteria for green, yellow, and blank positions.
    def minimizePermutations(word: String, filter: WordFilter): Vector[String] =
        val indexOfGreens = filter.indexOfGreens
        val indexOfYellows = filter.indexOfYellows
        val indexOfBlanks = filter.indexOfBlanks
        var perm = permutationSet
        perm = perm.filter(p =>
            indexOfGreens.forall((index, charSet) =>
                charSet.forall(char => (p(index) == 'g' && word(index) == char) || (p(index) != 'g' && word(index) != char)))
            && indexOfYellows.forall((index, charSet) =>
                charSet.forall(char => (word(index) != char) || (p(index) != 'g' && word(index) == char)))
            && indexOfBlanks.forall((index, charSet) =>
                charSet.forall(char => word(index) != char || (word(index) == char && p(index) == 'b') )))
        perm.filter(permutation => word.forall(char => !indexOfBlanks.values.toList.flatten.toSet.contains(char) || (getAllIndices(word, char).forall(index => permutation(index) == 'b') || (indexOfGreens.values.toList.flatten.contains(char) || indexOfYellows.values.toList.flatten.contains(char) ))))

