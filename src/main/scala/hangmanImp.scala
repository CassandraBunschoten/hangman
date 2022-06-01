import hangmanzio.Hangman.GuessResult
import hangmanzio.Hangman.State
import scala.io.Source
import scala.util.Random


object HangmanImp extends App {

  type Guess = Char

  lazy val words: List[String] = Source.fromFile("src/main/scala/resources/words.txt").getLines.toList

  def playHangman: Unit = {
    println("Welcome, let's play Hangman!")
    val word = words(Random.nextInt(words.length))
    val state = State(word)

    gameLoop(state)
  }

  def getLetter: Option[Guess] = {
    println("Please guess a letter: ")
    val letter = scala.io.StdIn.readLine().toCharArray().headOption
    letter
  }

  def printState(state: State): Unit = {
    val word = state.word.toList.map(c => 
      if (state.guesses.contains(c)) s" $c" else " _ ").mkString("") 

    val guesses = "Guesses: " + state.guesses.toList.mkString("")

    println("\n" + word + "\n\n" + guesses + "\n\n")
  }

  def newGuessResult(oldState: State, newState: State, guess: Guess): GuessResult = {
    if (oldState.guesses.contains(guess))           GuessResult.Unchanged
    else if (newState.playerWon)                    GuessResult.Won
    else if (newState.playerLost)                   GuessResult.Lost
    else if (oldState.word.contains(guess.toChar))  GuessResult.Correct
    else                                            GuessResult.Incorrect
  }

  def gameLoop(state: State): Unit = {
    import hangmanzio.Hangman.GuessResult._

    printState(state)
    val l: Guess          = getLetter.getOrElse(sys.error("Player did not input a letter"))
    val newState: State   = state.copy(guesses = state.guesses + l) 
    val res: GuessResult  = newGuessResult(state, newState, l)

    res match {
      case Unchanged  => println("You already tried that letter, try again "); gameLoop(newState)  
      case Won        => println("Congrats! You won!"); printState(newState)
      case Lost       => println(s"You lost D: \n The correct word was ${state.word}")
      case Correct    => println("You guessed a correct letter, carry on!"); gameLoop(newState)
      case Incorrect  => println("You guessed an incorrect letter, keep trying!"); gameLoop(newState)
    }

  }

  println(playHangman)
    
}
  
