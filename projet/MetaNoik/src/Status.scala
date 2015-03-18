abstract class CardinalPoint
case class North() extends CardinalPoint
case class South() extends CardinalPoint
case class East() extends CardinalPoint
case class West() extends CardinalPoint

class Pos(x: Int, y: Int)

class AntState(id: Int, controlled : Boolean, pos : Pos, orientation : CardinalPoint, hp : Int, mp : Int)

abstract class MapSquare
case class Grass() extends MapSquare
case class Food(level: Int) extends MapSquare
case class Water() extends MapSquare

class WorldMap(squares: List[List[MapSquare]])

class Status(turn: Int, map: WorldMap, ants: List[AntState])
