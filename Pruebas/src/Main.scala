

object Main {
  def main(args: Array[String]) {
    var l = Array.ofDim[Int](2,2)
    l(0)(0) = 1
    l(0)(1) = 2
    l(1)(0) = 3
    l(1)(1) = 4
    var x = Array.ofDim[Int](2,2)
    x(0)(0) = 1
    x(0)(1) = 2
    x(1)(0) = 3
    x(1)(1) = 5
    if (compararMatrices(l, x)){
      println("Hola")
    }
  }
  
  def compararMatrices(matriz1:Array[Array[Int]], matriz2:Array[Array[Int]]) : Boolean = {
    for( i <- 0 until 2){
      for( j <- 0 until 2){
        if (matriz1(i)(j) != matriz2(i)(j)){
          return false
        }
      }
    }
    return true
  }
}