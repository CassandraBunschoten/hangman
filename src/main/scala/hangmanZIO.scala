package hangmanzio

import zio._
import zio.console._
import zio.random._
import scala.io.Source
import java.io.IOException

object Hangman extends App {

  lazy val Words: List[String] = Source.fromFile("src/main/scala/resources/words.txt").getLines.toList

  type Guess = Char

  case class State(word: String, guesses: Set[Guess] = Set.empty[Guess]) {
    def failures: Int                 = (guesses -- word.toSet).size
    def playerLost: Boolean           = failures > 10
    def playerWon: Boolean            = (word.toSet -- guesses).size == 0
    def addGuess(guess: Guess): State  = new State(word, guesses + guess)
  }

  sealed trait GuessResult 
  object GuessResult {
    case object Won       extends GuessResult
    case object Lost      extends GuessResult
    case object Correct   extends GuessResult
    case object Incorrect extends GuessResult
    case object Unchanged extends GuessResult
  }

  def newGuessResult(oldState: State, newState: State, guess: Guess): GuessResult = 
    if (oldState.guesses.contains(guess))           GuessResult.Unchanged
    else if (newState.playerWon)                    GuessResult.Won
    else if (newState.playerLost)                   GuessResult.Lost
    else if (oldState.word.contains(guess.toChar))  GuessResult.Correct
    else                                            GuessResult.Incorrect

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = 
    (for {
      word <- chooseWord
      _    <- gameLoop(State(word))
    } yield ()).exitCode

  val hangman = for {
    _     <- putStrLn("Welcome to hangman. Let's play!")
    word  <- chooseWord
    state = State(word, Set())
    _     <- renderState(state)
    _     <- gameLoop(state)
  } yield ()


  val getLetter: ZIO[Console, IOException, Guess] = for {
    _       <- putStrLn("Please enter a letter")
    letter  <- getStrLn
    char    <- letter.toLowerCase.trim.headOption match {
        case None     => putStrLn(s"You did not enter a character") *> getLetter
        case Some(x)  => ZIO.succeed(x) 
          } 
  } yield char

  lazy val chooseWord: URIO[Random, String] = for {
    index <- nextIntBounded(Words.length)
  } yield Words.lift(index).getOrElse("boom")

  def renderState(state: State): ZIO[Console, IOException, Unit] = {
    val word    = state.word.toList.map(c => 
       if (state.guesses.contains(c)) s" $c " else " _ "
       ).mkString("")
    val guesses = "Guesses: " + state.guesses.toList.sorted.mkString("")
    val text    = "\n" + word + "\n\n" + guesses + "\n\n"
    putStrLn(text)
  }

  def resultOfGuess(guess: Char, oldState: State, newState: State) = ???

  def gameLoop(state: State): ZIO[Console, IOException, Unit] = {
     import Hangman.GuessResult._
     for {
        _         <- renderState(state)
        guess     <- getLetter
        newState  <- ZIO.succeed(state.copy(guesses = state.guesses + guess))
        result    =  newGuessResult(state, newState, guess)
        _         <- result match {
          case Won        => putStrLn("\n Congratulations, you won the game") *> renderState(newState) 
          case Lost       => putStrLn(s"\n Sorry you lost the game. Word was ${state.word}") *> renderState(newState) 
          case Correct    => putStrLn("\n Good guess!") *> gameLoop(newState)
          case Incorrect  => putStrLn("\n That's wrong, but keep trying!") *> gameLoop(newState)
          case Unchanged  => putStrLn("\n You already tried that letter") *> gameLoop(newState)
        }
    } yield ()
  }

}
