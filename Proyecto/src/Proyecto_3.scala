import scala.util.control._
import sun.security.util.Length

object Proyecto3 {
  // variables globales
  // matrices estados representan el tablero en X estado
  var estadoMeta = Array.ofDim[Int](0,0)
  var estadoInicial = Array.ofDim[Int](0,0)
  var estadoActual = Array.ofDim[Int](0,0) //Matriz donde se trabaja para no modificar el estado incial
  var estadoHeuristica = Array.ofDim[Int](0,0)//Matriz aux para no modificar el estado actual 
  var rango = 0 //Dimension de la matriz
  var ultimoE = 0 // Ultimo indice evaluado 
  var indiceF=0
  
  var Open: List[Array[Array[Int]]] = List() //Posibles estados, contiene todas la matrices estados
  var Closed: List[Int] = List() // Contiene el indice de los Estados utilizados 
  var H: List[Int]=List() // Lista que contiene las heuristicas de las matrices segun el indice
  var Padre: List[Int]=List() //Lista que tiene el indice de los padres 
  var LStr: List[String] = List()
  
  var distanciaM = 0  // Distancia de cada iteración de la suma de Manhattan
  var loop = new Breaks //Clase breaks para cortar ciclos 
  var resultadoHeuristica = 0 //Resultado de la evaluacion de las heuristicas
  var posicionCero:Array[Int] = new Array[Int](2) //Coordenadas del 0 en estado Actual
  
  def main(args: Array[String]) {
    println("\n¡Bienvenido!\n\nProyecto 3: Solucionador del rompecabezas deslizante - Puzzle N\n\nUtiliza las heurísticas: Distance Manhattan, Misplaced Tiles y Tiles Out of Row and Column\n\nBy:\n\tAdrián J. Barboza Prendas \n\tAdán Mora Fallas\n")
    tableroInicial()//Inicializa el tablero
    loop.breakable {
      while(true){
        println("\n Menú de opciones: \n1) Cambiar Estado Inicial\n2) Cambiar el estado Meta\n3) Usar Distancia de Manhattan\n4) Usar Misplaced Tiles\n5) Usar Tiles Out Row and Column\n6) Salir\n" )
        val input = scala.io.StdIn.readLine()
        if (input == "1"){
          tableroInicial()
        } else if (input == "2"){
          CambiarEstadoMeta()
        } else if (input == "3"){
          AlgoritmoVoraz1(distanciaManhattan())
        } else if (input == "4"){
          AlgoritmoVoraz1(Misplaced_Tiles())
        } else if (input == "5"){
          AlgoritmoVoraz1(Tiles_Out_RowColumn())
        }else if (input == "6"){
          loop.break
        } else {
          println("\nOpción incorrecta.")
        }
      }
    }
  }
  
  // Función que ejecuta el algoritmo voraz para la búsqueda de la solución.
  def AlgoritmoVoraz1(heuristica: => String) {
    //Inicializando 
    Open= List()
    Closed = List() 
    H=List()
    Padre=List()
    Open = Open ::: List(estadoActual)//Guardamos el estado inicial en los utilizados
    Closed = Closed ::: List(0) //Agrego el estado incial a la lista de closed 
    H = H ::: List(0) //Default de la heuristica del Inicial
    Padre= Padre ::: List(0) //Padre default del inicial
    ultimoE=1 //ultimo por defecto
    LStr=List()
    estadoHeuristica = Array.ofDim[Int](rango,rango)
    estadoHeuristica = copiarMatrices(estadoActual)
    LStr=LStr ::: List(heuristica)//Es solo para generar el calculo del estado final   
    //ultimoE = 0 
    indiceF=0 
    //Start
    println("\n +++++++++++++ Camino ++++++++++++++\n")
    ImprimeCamino()
    println("\t***************** AAAAAAAAAAAAAA *****************")
    imprimirMatriz(estadoInicial)
    imprimirMatriz(estadoMeta)
    println("\t***************** AAAAAAAAAAAAAA *****************")
    loop.breakable {
      while (true){
        calcularPosiblesMovimientos() //Calcula los movimiento segun la matriz actual
        calcularSiguienteEstado(heuristica)//calcula las heuristicas de las matrices añadidas

        if(compararMatrices(estadoActual, estadoMeta)){          
          loop.break
        }
      }
    }
    println("\n +++++++++++++ Camino ++++++++++++++\n")
    ImprimeCamino()
    estadoActual = copiarMatrices(estadoInicial)// Reinicio de la matriz actual a la inicial para probar otras heuristicas
    loop.breakable {
      for( i <- 0 until rango){
        for( j <- 0 until rango){
          if (estadoActual(i)(j) == 0){
            posicionCero(0) = i
            posicionCero(1) = j
            loop.break
          }
        }
      }
    }
  }
  // Función auxiliar para debugguear el paso a paso de los resultados de las heurísticas. 
  def ImprimeStr(){
    print("Tamaño de LSTR: "+LStr.length+"\n")
    print("Tamaño de Open: "+Open.length+"\n")
    for (n <-0 until LStr.length){
      println("\nIndice "+n)
      imprimirMatriz(Open.apply(n))
      println(LStr.apply(n))
    }
  }
  // Función que imprime en consola los pasos a seguir para resolver el tablero y los resultados de la heurística.
  def ImprimeCamino(){
    println("Indice Final: "+indiceF)
    var Ult= indiceF
    println("*****************************\nPasos: ")
    var P = ContarP(Ult)
    println(P-1)
    ImprimeCaminoAux(Ult)
    println("*****************************")
    println("EL algoritmo: ")
    ImprimePasos(Ult)
    
  }
  
  // Función que cuenta el número de pasos realizados.
  def ContarP(indic: Int):Int={
    if(indic==0){
      return 1
    }else{
      return ContarP(Padre.apply(indic))+1
    }
  }
  
  // Función que imprime los pasos de de la heurística evaluada.
  def ImprimePasos(indic:Int){
    if(indic==0){
      println("Indice: "+indic)
      imprimirMatriz(Open.apply(indic))
      println(LStr.apply(indic))
    }else{
      ImprimePasos(Padre.apply(indic))
      println("Indice: "+indic)
      imprimirMatriz(Open.apply(indic))
      println(LStr.apply(indic))
    }
  }
  // Función que imprime los pasos a seguir para la reolución del tablero.
  def ImprimeCaminoAux(indic:Int){
    if(indic==0){
      imprimirMatriz(estadoInicial)
    }else{
      ImprimeCaminoAux(Padre.apply(indic))
      imprimirMatriz(Open.apply(indic))
    }
  }
  //Funcion que calcula los posibles movimientos segun la matriz actual
  def calcularSiguienteEstado(h: => String) {
    var cantMovimientos = Open.length //Tamaño de Open para saber hasta donde evaluar
    estadoHeuristica = Array.ofDim[Int](rango,rango) //Matriz auxiliar reiniciada.
    for (n <- ultimoE until cantMovimientos){ //ultimoE indice del ultimo elemento evaluada, a partir de este no hay evaluados
      //esto es para no calcular heuristicas previamente calculadas
      estadoHeuristica = copiarMatrices(Open.apply(n))
      LStr=LStr ::: List(h) //Agregamos el string de evaluar la heuristica a la lista
      H = H ::: List(resultadoHeuristica) //Guardando resultado del calculo de la euristica
      resultadoHeuristica = 0//Reiniciamos resultado
      ultimoE=ultimoE+1 //aumentamos ultimo evaluado
    }
    
    //Comparando Heuristicas
    var indice = -1
    for (i <- 1 until cantMovimientos){//Empieza en 1 para no verificar la heuristica de la inicial
      if (IsInClosed(i) == false){
        if(indice == -1){
          indice=i
        }
        if (H(i) < H(indice)){
          indice = i
          if(H(i)==0){
            indiceF=i
          }
        } 
      }
    }
    println("INDICE: "+indice)
    estadoActual = Open.apply(indice)
    imprimirMatriz(estadoActual)
    Closed = Closed ::: List(indice)
    loop.breakable {
      for( i <- 0 until rango){
        for( j <- 0 until rango){
          if (estadoActual(i)(j) == 0){
            posicionCero(0) = i
            posicionCero(1) = j
            loop.break
          }
        }
      }
    }
  }
  
  // Función que determina si una matriz está cerrada
  def IsInClosed(indic: Int):Boolean={
    for(n <- Closed){
      if (indic == n){
        return true
      }
    }
    return false
  }
  
  // Función que determina si una matriz ya está en estados abiertos.
  def isInUtilizados(matriz:Array[Array[Int]]): Boolean ={
    var matA = Array.ofDim[Int](rango,rango)
    for (n <-  Closed){
      matA = copiarMatrices(Open.apply(n))
      if (compararMatrices(matriz,matA)){
        return true
      }
    }
    return false
  }
  
  // Función que calcula los posibles movimientos del estado actual.
  def calcularPosiblesMovimientos() {
    var matrizAux  = Array.ofDim[Int](rango, rango)
    matrizAux = copiarMatrices(estadoActual)    
    //Posicion 0 son coordenadas x,y
    var izquierda = posicionCero(1) - 1
    var derecha = posicionCero(1) + 1
    var arriba = posicionCero(0) - 1
    var abajo = posicionCero(0) + 1
        
    if (arriba >= 0){
      matrizAux(posicionCero(0))(posicionCero(1)) = estadoActual(arriba)(posicionCero(1))
      matrizAux(arriba)(posicionCero(1)) = 0
      if(! isInUtilizados(matrizAux)){
        Open = Open ::: List(matrizAux)
        Padre = Padre::: List(Open.indexOf(estadoActual) )
      }
      matrizAux = copiarMatrices(estadoActual)
    }
    
    if (izquierda >= 0){
      matrizAux(posicionCero(0))(posicionCero(1)) = estadoActual(posicionCero(0))(izquierda)
      matrizAux(posicionCero(0))(izquierda) = 0
      if(! isInUtilizados(matrizAux)){
        Open = Open ::: List(matrizAux)
        Padre = Padre::: List(Open.indexOf(estadoActual) )
      }
      matrizAux = copiarMatrices(estadoActual)
    }
    
    if (derecha != rango){
      matrizAux(posicionCero(0))(posicionCero(1)) = estadoActual(posicionCero(0))(derecha)
      matrizAux(posicionCero(0))(derecha) = 0
      if(! isInUtilizados(matrizAux)){
        Open = Open ::: List(matrizAux)
        Padre = Padre::: List(Open.indexOf(estadoActual) )
      }
      matrizAux = copiarMatrices(estadoActual)
    }

    if (abajo != rango){
      matrizAux(posicionCero(0))(posicionCero(1)) = estadoActual(abajo)(posicionCero(1))
      matrizAux(abajo)(posicionCero(1)) = 0
      if(! isInUtilizados(matrizAux)){
        Open = Open ::: List(matrizAux)
        Padre = Padre::: List(Open.indexOf(estadoActual) )
      }
    }
  }
  // Función que verifica si una ficha ya existe en el tablero
  def isInTablero(ficha:Int): Boolean ={
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        if (estadoInicial(i)(j) == ficha){
          return true
        }
      }
    }
    return false
  }
  
  // Función que inicializa el tablero o Estado Inicial del Rompecabezas
  def tableroInicial() {
    loop.breakable {
      Open= List()
      Closed = List() 
      H=List()
      Padre=List() 
    
      println("\n********** Iniciarlizar Tablero *****************\n")
      while(true){
        println("\nDigite el rango del tablero inicial: ")
        try {
          rango = (scala.io.StdIn.readLine()).toInt
          if (rango > 1){
            loop.break
          } else {
            println("\nERROR: El rango debe ser mayor a 1.")
          }
        } catch {
          case ex: NumberFormatException => {
            println("\nERROR: El rango debe ser un número.")
          }
        }
      }
    }
    // Inicialización del Estado Meta según el rango digitado por el usuario
    estadoMeta = Array.ofDim[Int](rango,rango)
    var n = 1
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        estadoMeta(i)(j) = n
        n += 1
      }
    }
    estadoMeta(rango - 1)(rango - 1) = 0
    
    estadoInicial = Array.ofDim[Int](rango,rango)
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        estadoInicial(i)(j) = -1
      }
    }
    
    var error = false
    var fila = 0
    var columna = 0
    while( fila != rango){
      error = false
      println("\nDigite la ficha de la fila " + (fila+1).toString() + " columna " + (columna+1).toString() + " : ")
      try {
        val ficha = (scala.io.StdIn.readLine()).toInt
        if (ficha < 0 || ficha >= (rango*rango) && error == false){
          error = true
          println("\nERROR: La ficha no está dentro del rango del tablero.")
        }
        if (isInTablero(ficha) && error == false){
          error = true
          println("\nERROR: La ficha ya existe en el tablero.")
        }
        if (error == false){
          estadoInicial(fila)(columna) = ficha
          columna += 1
          if (ficha == 0){
            posicionCero(0) = fila
            posicionCero(1) = columna -1
          }
          if (columna == rango){
            fila += 1
            columna = 0
          }
        }
      } catch {
        case ex: NumberFormatException => {
          println("\nERROR: Ficha inválida, debe ser un número.")
        }
      }
    }
    println("\n******************** Tableros*************************\n")
    println("\nEste es el estado inicial del tablero:")
    imprimirMatriz(estadoInicial)
    println("\nEste es el estado meta por defecto del tablero:")
    imprimirMatriz(estadoMeta)
    println("\n*******************************************************\n")
    
    estadoActual = copiarMatrices(estadoInicial) //Cambie esto por aquello para preservar el estado Inicial
    ///estadoActual = estadoInicial
  }
  
  /*Función que imprime en consola una matriz de forma legible
   * Recibe la matriz a imprimir.
  */
  def imprimirEstados(){
    for (n <- Closed){
      imprimirMatriz(Open.apply(n))
    }
  }
  def imprimirOpen(){
    for (n <- 0 until Open.length){
      println("***** Matriz Indice "+n+" ******" )
      imprimirMatriz(Open.apply(n))
    }
  }
  def imprimirMatriz(matriz:Array[Array[Int]]){
    print("\n")
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        print(matriz(i)(j))
        print("\t")
      }
      print("\n")
    }
    print("\n")
  }
  // Función que copia los valores de una matriz a otra dada por parámetro.
  def copiarMatrices(matriz:Array[Array[Int]]) : Array[Array[Int]] = {
    var copia = Array.ofDim[Int](rango, rango)
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        copia(i)(j) = matriz(i)(j)
      }
    }
    return copia
  }
  
  // Función booleana que compara dos matrices, retorna TRUE si son iguales, FALSE si no o son
  def compararMatrices(matriz1:Array[Array[Int]], matriz2:Array[Array[Int]]) : Boolean = {
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        if (matriz1(i)(j) != matriz2(i)(j)){
          return false
        }
      }
    }
    return true
  }
  
  // Función auxiliar de la distancia de Manhattan.
  def calcularDistancia(ficha:Int, posX:Int, posY:Int): String = {
    var r = ""
    loop.breakable {
      for( i <- 0 until rango){
        for( j <- 0 until rango){
          if (estadoMeta(i)(j) == ficha){
            distanciaM = ((posX - (i+1)).abs + (posY - (j+1)).abs)
            r += ("Para la ficha " + ficha + " la distacia Manhattan es (" +  posX + "," + posY + "), (" +  (i+1).toString() + "," + (j+1).toString() + ") = |" +  posX + " - " + (i+1).toString() + "| + |" +  posY + " - " + (j+1).toString() + "| = " + distanciaM)
            loop.break
          }
        }
      }
    }
    return r
  }
  // Función que calcula la heurística Distancia Manhattan.
  def distanciaManhattan(): String ={
    var pasos = "\n"
    var lineaSuma = "\n\nH(S) = "
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        pasos += calcularDistancia(estadoHeuristica(i)(j), i+1, j+1) + "\n"
        lineaSuma += distanciaM.toString()
        if (!(i == rango -1 && j == rango -1)){
          lineaSuma  += " + "
        }
        resultadoHeuristica = resultadoHeuristica + distanciaM
      }
    }
    lineaSuma += " = " + resultadoHeuristica.toString()
    pasos += lineaSuma
    return pasos
  }
  // Función que calcula la heurística Misplaced Tiles.
  def Misplaced_Tiles(): String ={
    var pasos = "\n\tSe ha seleccionado la heurística Misplaced Tiles  \n\n"
    var resultado = 0
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        if((estadoHeuristica(i)(j) != estadoMeta(i)(j))&&(estadoHeuristica(i)(j)!=0)){
          pasos+= ("La ficha ( "+estadoHeuristica(i)(j)+" ) que se encuentra en la posicion ("+(i+1).toString()+ ") ("+ (j+1).toString()+ ") esta en una posicion incorrecta. +1\n" )
          resultado = resultado+1
        }
      }
    }
    pasos+= ("\nEl resultado de Misplaced Tiles es: "+resultado)
    resultadoHeuristica = resultado
    return pasos 
  }
  // Función que calcula la heurística Tiles Out of Row and Column.
  def Tiles_Out_RowColumn():String = {
  var pasos ="\n\tSe ha seleccionado la heurística Tiles Out Row and Column  \n\n"
  var resultado = 0
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        
        for (f <- 0 until rango){
          for (c <- 0 until rango){
            if(estadoHeuristica(i)(j) == estadoMeta(f)(c)){
              if(i != f){
                pasos+= ("La ficha ( "+estadoHeuristica(i)(j)+" ) que se encuentra en la fila ("+(i+1).toString()+ ") esta en una fila incorrecta. +1\n" )
                resultado = resultado+1
              }
              if(j != c){
                pasos+= ("La ficha ( "+estadoHeuristica(i)(j)+" ) que se encuentra en la columna ("+ (j+1).toString()+ ") esta en una columna incorrecta. +1\n" )
                resultado = resultado+1
              }
            }
          }
        }
      }
    }
    pasos+= ("\nEl resultado de Tiles Out Row and Column es: "+resultado)
    resultadoHeuristica= resultado
    return pasos 
  return pasos
  }
  // Función que determina si una ficha se encuentra en el tablero meta al configurarlo.
  def isInTableroMeta(ficha:Int): Boolean ={
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        if (estadoMeta(i)(j) == ficha){
          return true
        }
      }
    }
    return false
  }
    
  def CambiarEstadoMeta() {
    println("\n\t¡Hola aca puedes cambiar el tablero meta!") 
    // Inicialización del Estado Meta según el rango digitado por el usuario
    estadoMeta = Array.ofDim[Int](rango,rango)
    Closed = List()
    
    for( i <- 0 until rango){
      for( j <- 0 until rango){
        estadoMeta(i)(j) = -1
      }
    }
    var error = false
    var fila = 0
    var columna = 0
    while( fila != rango){
      error = false
      println("\nDigite la ficha de la fila " + (fila+1).toString() + " columna " + (columna+1).toString() + " : ")
      try {
        val ficha = (scala.io.StdIn.readLine()).toInt
        if (ficha < 0 || ficha >= (rango*rango) && error == false){
          error = true
          println("\nERROR: La ficha no está dentro del rango del tablero.")
        }
        if (isInTableroMeta(ficha) && error == false){
          error = true
          println("\nERROR: La ficha ya existe en el tablero.")
        }
        if (error == false){
          estadoMeta(fila)(columna) = ficha
          columna += 1
          if (columna == rango){
            fila += 1
            columna = 0
          }
        }
      } catch {
        case ex: NumberFormatException => {
          println("\nERROR: Ficha inválida, debe ser un número.")
        }
      }
    }
    println("\n******************** Tablero Meta*************************\n")
    println("\nEste es el estado inicial del tablero:")
    imprimirMatriz(estadoInicial)
    println("\nEste es el estado meta por defecto del tablero:")
    imprimirMatriz(estadoMeta)
    println("\n*******************************************************\n")
  }
}