abstract class CardinalPoint
case class North() extends CardinalPoint
case class South() extends CardinalPoint
case class East() extends CardinalPoint
case class West() extends CardinalPoint

class Pos(val x: Int, val y: Int)

class AntState(
     val id : Int,
     val controlled  : Boolean,
     val pos         : Pos,
     val orientation : CardinalPoint,
     val hp          : Int,
     val mp          : Int)

abstract class MapSquare
case class Grass() extends MapSquare
case class Food(level: Int) extends MapSquare
case class Water() extends MapSquare

class WorldMap(val squares: Array[Array[MapSquare]])

class Status(val turn: Int, val map: WorldMap, val ants: Array[AntState])
