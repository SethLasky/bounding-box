import cats.effect.IO
import fs2.io.file.{Files, Path}

object BoxOperations {
  def readFile(file: String) = Files[IO].readUtf8(Path(file)).compile.string

  def isOverlapping(box1: Box, box2: Box) = (getAllCoordinates(box1) intersect getAllCoordinates(box2)).nonEmpty

  def getAllCoordinates(box: Box) = for {
    line <- box.leftCorner.character to box.rightCorner.character
    char <- box.leftCorner.line to box.rightCorner.line
  } yield Coordinates(line, char)

  def getBoxCoordinates(currentChar: Character, chars: List[Character], boxCoordinates: List[Coordinates] = List.empty): List[Coordinates] = {
    if (currentChar.char == '-') {
      boxCoordinates
    }
    else {
      val rightBox = currentChar.coordinates.right(chars).map(getBoxCoordinates(_, chars, boxCoordinates)).getOrElse(List.empty)
      val downBox = currentChar.coordinates.down(chars).map(getBoxCoordinates(_, chars, boxCoordinates)).getOrElse(List.empty)
      rightBox ++ downBox ++ boxCoordinates :+ currentChar.coordinates
    }
  }

  def getBoxes(currentChar: Character, chars: List[Character], boxes: List[Box] = List.empty): List[Box] = {
    val right = currentChar.coordinates.right(chars)
    val down = currentChar.coordinates.down(chars)
    if (currentChar.char == '*') {
      val coordinates = getBoxCoordinates(currentChar, chars)
      boxes :+ getBox(coordinates)
    }
    else if (right.nonEmpty) {
      val downList = if (down.nonEmpty) getBoxes(down.get, chars, boxes) else List.empty
      getBoxes(right.get, chars, boxes) ++ downList
    }
    else boxes
  }

  def getCorner(checkMin: Boolean, coordinates: List[Coordinates]): Coordinates = coordinates.reduceLeft { (last, curr) =>
    val condition1 = if (checkMin) curr.line < last.line else curr.line > last.line
    val condition2 = if (checkMin) curr.character < last.character else curr.character > last.character
    if (condition1) curr
    else if (curr.line == last.line) {
      if (condition2) curr
      else last
    }
    else last
  }

  def getBox(coordinates: List[Coordinates]) = {
    val leftCorner = getCorner(checkMin = true, coordinates)
    val rightCorner = getCorner(checkMin = false, coordinates)
    Box(leftCorner, rightCorner)
  }

  def translateToCharacters(text: String): List[Character] = text.split("\n").zipWithIndex.flatMap {
    case (line, lineNumber) => line.toList.zipWithIndex.map {
      case (char, charNumber) => Character(Coordinates(lineNumber + 1, charNumber + 1), char)
    }
  }.toList

  def largestBox(boxes: List[Box]): Box = boxes.maxBy { box =>
    (box.rightCorner.line - box.leftCorner.line + 1) * (box.rightCorner.character - box.leftCorner.character + 1)
  }

  def formatOutput(box: Box): String = s"(${box.leftCorner.line},${box.leftCorner.character})(${box.rightCorner.line},${box.rightCorner.character})"
}

case class Character(coordinates: Coordinates, char: Char)

case class Coordinates(line: Int, character: Int){
  def right(chars: List[Character]) = chars.find(_.coordinates == Coordinates(line, character + 1))
  def down(chars: List[Character]) = chars.find(_.coordinates == Coordinates(line + 1, character))
}
case class Box(leftCorner: Coordinates, rightCorner: Coordinates)
