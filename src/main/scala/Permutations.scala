// A class to generate permutations and filter them based on specific criteria.
class Permutations(wordLength: Int):
    // Generate all possible permutations of the given word length.
    val permutationSet = generatePermutations(wordLength)

    // Get all indices of a specific character in a given word.
    def getAllIndices(word: String, char: Char): Seq[Int] =
        word.zipWithIndex.collect { case (c, i) if c == char => i }

    // Generate all possible permutations of the given word length.
    def generatePermutations(wordLength: Int): Vector[String] =
        def inner(start: String, i: Int): Seq[String] =
        if (i == wordLength) then
            Vector("g", "y", " ").map(start + _)
        else
            Vector("g", "y", " ").flatMap(l => inner(start + l, i + 1))
        inner("", 1).toVector

    // Filter the permutationSet based on the provided criteria for green, yellow, and blank positions.
    def minimizePermutations(word: String, indexOfGreens: Map[Int, Set[Char]], indexOfYellows: Map[Int, Set[Char]], indexOfBlanks: Map[Int, Set[Char]]): Vector[String] =
        var perm = permutationSet
        perm = perm.filter(p =>
            indexOfGreens.forall((index, charSet) =>
                charSet.forall(char => (p(index) == 'g' && word(index) == char) || (p(index) != 'g' && word(index) != char)))
            && indexOfYellows.forall((index, charSet) =>
                charSet.forall(char => (word(index) != char) || (p(index) != 'g' && word(index) == char)))
            && indexOfBlanks.forall((index, charSet) =>
                charSet.forall(char => word(index) != char || (word(index) == char && p(index) == ' ') )))
        perm.filter(permutation => word.forall(char => !indexOfBlanks.values.toList.flatten.toSet.contains(char) || (getAllIndices(word, char).forall(index => permutation(index) == ' ') || (indexOfGreens.values.toList.flatten.contains(char) || indexOfYellows.values.toList.flatten.contains(char) ))))

