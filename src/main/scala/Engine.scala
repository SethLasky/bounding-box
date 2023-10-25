import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.{Files, Path}
import BoxOperations._

object Engine extends IOApp {
  def run(args: List[String]): IO[ExitCode] = for {
    file <- readFile(args.head)
    characters = translateToCharacters(file)
    boxes = getBoxes(characters.head, characters)
    finalBox = largestBox(boxes)
    _ <- IO.println(formatOutput(finalBox))
  } yield ExitCode.Success
}
