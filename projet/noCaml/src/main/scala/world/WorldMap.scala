package map

class Map {
  
  def init(size:Int) {
    var x = Array.ofDim[Position](size, size);
    for (i <- 0 to size) {
      for (j <- 0 to size) {
        x(i)(j) = new Position();
      }
    }
    createMap(x);
  } 
  
  def createMap(init: Array[Array[Position]]) {
    var newMap = Array.ofDim[Position](init.length, init(0).length);
     for (i <- 0 to init.length) {
        for (j <- 0 to init(0).length) {
           newMap(i)(j) = init(i)(j).makePosition("");
        }
     }
     //var newInfos = getInfos();
     //for (i in newInfos) {
     // add ground & ants
     //}
     
     //send the new map to ants
     createMap(newMap);
  }
  
}