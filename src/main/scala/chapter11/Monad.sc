import chapter11._

val listMonad = SampleMonads.listMonad
val optionMonad = SampleMonads.optionMonad

// generates lists of length 'n' containing the provided elements (permutations without repetition)
listMonad.replicateM(3, List("a", "b"))

// in the Some() case, generates a Some(List()) with the List() containing the element n times
// None remains None
optionMonad.replicateM(3, Some("a"))
optionMonad.replicateM(3, None)