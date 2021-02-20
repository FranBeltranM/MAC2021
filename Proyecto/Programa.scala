import scala.math.BigDecimal.double2bigDecimal

object Programa {

  /* Fichero: traduce.hs */
  def traduceX(valor: Int): Char = {
    valor match {
      case 1 => 'A'
      case 2 => 'B'
      case 3 => 'C'
    }
  }

  /* Fichero: factorial.hs */
  def factorial(numero: Int): Int = {
    if (numero >= 0) {
      if (numero == 0)
        1
      else {
        numero * factorial(numero - 1)
      }
    } else
      -1
  }

  /* Fichero: divisible.hs */
  def divisible(x: Int, y: Int): Boolean = {
    if ((x % y) == 0)
      true
    else
      false
  }

  /* Fichero: divisores_ricardo.hs */
  def calculaDivisores(numero: Int): List[Int] = {
    for (i <- List.range(1, numero + 1) if numero % i == 0)
      yield i
  }

  /* Fichero: suma.hs */
  def suma(x: Int, y: Int): Int = {
    x + y
  }

  /* Fichero: suma3.hs */
  def suma(x: Int, y: Int, z: Int): Int = {
    x + y + z
  }

  def suma(x: Double, y: Double, z: Double): Double = {
    x + y + z
  }

  /* Fichero: soloPrimero.hs */
  def soloPrimero[A](seq: Seq[A]): A = {
    seq.head
  }

  /* Fichero: suma_lista.hs */
  def sumaLista(lista: List[Int]): Int = {
    lista.sum
  }

  /* Fichero: miXor.hs */
  def miXor(x: Boolean, y: Boolean): Boolean = {
    if ((x || y) && !(x && y))
      true
    else
      false
  }

  /* Fichero: predecesor.hs */
  def predecesor(valor: Int): Int = {
    valor - 1
  }

  /* Fichero: prog1.hs */
  def positivo(valor: Int): Boolean = {
    valor >= 0
  }

  /* Fichero: quitaBlancos.hs */
  /* Fichero: quitaBlancos2.hs */
  def quitaBlancos(cadena: String): String = {
    cadena.replace(" ", "")
  }

  /* Fichero: euler2_ricardo.hs */
  def Fibonacci(numero: Int) : Int = {
    if (numero >= 0) {
      if (numero == 0)
        0
      else if ( numero == 1 )
        1
      else if ( numero == 2 )
        2
      else {
        Fibonacci(numero - 1) + Fibonacci(numero - 2)
      }
    } else
      -1
  }

  /* Fichero: euler3_ricardo.hs */
  def Primo(numero: Any) : Boolean = {
    calculaDivisores(numero.toString.toInt).size == 2
  }

  /* Fichero: euler3.hs */
  def calculaPrimos(numero: Int) : List[Int] = {
    for( i <- List.range(1, numero+1) if Primo(i) )
        yield i
  }

  /* Fichero secondElem.hs */
  def secondElem(lista : List[Int]) : Any = {
    if(lista.size > 1) {
      lista(1)
    }
    else
      null
  }

  def calculaSumMultiplo3y5(numero: Int) : Int = {
    var a = List[Int]()

    for( i <- List.range(1, numero+1) if i % 3 == 0 || i % 5 == 0 )
      a = i :: a

    a.sum
  }

  /* Fichero firstElement.hs */
  def firstElement(lista : List[Int]) : Any = {
    if(lista.size > 0) {
      lista(0)
    }
    else
      null
  }

  /* Fichero: euler4.hs */
  def maximo(lista : List[Int]) : Any = {
    if(lista.size > 0)
      lista.max
  }

  /* Fichero: euler4.hs */
  def palindromo(x : Any) : Boolean = {
    if(x.toString.length > 1)
      if(x.toString == x.toString.reverse)
        true
      else
        false
    else
      false
  }

  /* Fichero: euler4.hs */
  def listPalindromos(valor : Int) : List[Int] = {
    for( i <- List.range(1, valor+1) if palindromo(i) )
      yield i
  }

  /* Fichero: funcionLet.hs */
  def funcionLet(lista : List[Any]) : List[Any] = {
    if(lista.size > 2)
      lista.drop(2)
    else
      null
  }

  /* Fichero: funcionLista.hs */
  def funcionLista(lista : List[Any]) : Any = {
    if(lista.size > 0)
      lista.foreach( x => print(x + ", ") )
    else
      null
  }

  /* Fichero: integral_manuel_jesus.hs */
  /* Fichero: integral.hs */
  def calculaIntegral(funcion: Double => Double, a : Double, b : Double, t : Double) : Double = {
    if( b <= a )
      0
    else if( (a + t) > b )
      funcion(a) * (b-a)
    else
      funcion(a) * t + calculaIntegral(funcion, (a+t), b, t)
  }

  /* Fichero: lambda.hs */
  def suma2[A](valor1: A, valor2: A) (implicit num: Numeric[A]): A = {
    import num._
    valor1 + valor2
  }

  /* Fichero aprox_pi_manuel_jesus.hs */
  def calcupaPi(aproximacion: Double) : Double = {
    val listaX = (-1 + aproximacion / 2 to 1 - aproximacion / 2 by aproximacion).toList
    val listaY = (-1 + aproximacion / 2 to 1 - aproximacion / 2 by aproximacion).toList
    var valor = 0.0;

    for( xi <- listaX)
      for( yi <- listaY)
        if(Math.sqrt( (xi*xi + yi*yi).bigDecimal.doubleValue() ) <= 1)
          valor += aproximacion*aproximacion

    valor
  }

  /* Ficheros: miFuncion.hs, miFuncion2.hs, miFuncion2Nicolas.hs */
    def miFuncion(funciones: List[(Any) => Any], valores: List[Any]) : List[Any] = {
    if(funciones.size > 0) {
      if(valores.size > 0) {
        funciones(0)(valores(0)) :: miFuncion(funciones.drop(1), valores.drop(1))
      } else
        Nil
    }
    else
      Nil
  }

  def miFuncion2(funciones: List[(List[Any]) => List[Any]], valores: List[Any]) : List[Any] = {
    if(funciones.size > 0) {
      if(valores.size > 0) {
        funciones(0)(valores) :: miFuncion2(funciones.drop(1), valores)
      } else
        Nil
    } else
      Nil
  }

  def suma1(X: List[Any]): List[Any] = {
    for (x <- X)
      yield x.toString.toDouble + 1
  }

  def suma2(valor: Any) : Any = {
    valor.toString.toDouble + 1
  }

  /* Fichero: funcionTupla.hs */
  def funcionTupla(funcion1: Double => Double, funcion2: Double => Double, valor : Double) : Double = {
    funcion1(valor) + funcion2(valor)
  }

  /* Fichero: funcionTuplaBool.hs */
  def funcionTuplaBool(funcion1: Any => Boolean, funcion2: Any => Boolean, valor : Any) : Boolean = {
    funcion1(valor) || funcion2(valor)
  }

  def suma5(valor: Double) : Double = {
    valor + 5
  }

  /* Fichero: programaTuplas.hs */
  def programaTuplas(tupla: List[List[Any]], valor: Any) : Any = {
    for(t <- tupla if t.contains(valor))
      yield t
  }

  /* Fichero: solucion_pregunta_map.hs */
  /* Fichero: mapea_y_filtra_ricardo_diaz.hs */
  def funcionMap(funcion: Any => Any, valores: List[Any]) : List[Any] = {
    if(valores.size > 0) {
      funcion(valores(0)) :: funcionMap(funcion, valores.drop(1))
    } else
      Nil
  }

  def funcionFiltra(funcion: Any => Boolean, valores: List[Any]) : List[Any] = {
    if(valores.size > 0) {
      if( funcion(valores(0)) )
        valores(0) :: funcionFiltra(funcion, valores.drop(1))
      else
        funcionFiltra(funcion, valores.drop(1))
    } else
      Nil
  }

  /* Fichero: funcion_hash_ricardo.hs */
  def generaPrimos(iteracion: Int) : List[Int] = {
    var Lista : List[Int] = List()
    var contador = 0

    while(Lista.size < iteracion) {
      if( Primo(contador) ) {
        Lista ::= contador
      }

      contador += 1
    }

    Lista
  }

  def funcionHash(valores: List[Int]) : Int = {
    if(valores.size > 0) {
      var sum = 0
      val listaPrimos = generaPrimos(valores.size)

      for( i <- 0 to valores.size -1)
        sum += valores(i) * listaPrimos(i)

      sum
    } else
      -1
  }

  def main(args: Array[String]): Unit = {
    //////////////////////////////////////////////////////////////////////////
    println("Función traduce: 1, 2, 3")
    println(traduceX(1))
    println(traduceX(2))
    println(traduceX(3))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Factorial de 16:")
    println(factorial(16))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("División exacta:")
    println("Números 6,2 -> " + divisible(6, 2))
    println("Números 5,2 -> " + divisible(5, 2))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Calcula los posibles divisores del número 20:")
    println(calculaDivisores(20))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Función suma 0 + 1:")
    println(suma(0, 1))
    println("Función suma 1 + 1:")
    println(suma(1, 1))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Función suma3 1 + 1 + 0:")
    println(suma(1, 1, 0))
    println("Función suma3 1 + 1 + 1:")
    println(suma(1, 1, 1))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Función suma3 1.5 + 0.5 + 1:")
    println(suma(1.5, 0.5, 1))
    println()
    //////////////////////////////////////////////////////////////////////////
    val listChar: List[Char] = List('A', 'B', 'C')
    val listInt: List[Any] = List(1, 2, 3)
    println("Primer elemento de una lista: ")
    println(soloPrimero(listChar))
    println(soloPrimero(listInt))
    println()
    //////////////////////////////////////////////////////////////////////////
    val listaNumeros: List[Int] = List(1, 4, 3, 4, 5, 6, 7, 8, 9)
    println("Suma de los elementos de una lista " + listaNumeros)
    println(sumaLista(listaNumeros))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Puerta XOR:")
    println("False, False -> " + miXor(false, false))
    println("True, False -> " + miXor(true, false))
    println("False, True -> " + miXor(false, true))
    println("True, True -> " + miXor(true, true))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Predecesor de 5:")
    println(predecesor(5))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("¿Es positivo?")
    println("5 -> " + positivo(5))
    println("-1 -> " + positivo(-1))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Quita blancos de una cadena:")
    println("Cadena Original -> 01 200 00 22 1")
    println("Cadena sin blancos -> " + quitaBlancos("01 200 00 22 1"))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Serie de Fibonacci:")
    println("0 -> " + Fibonacci(0))
    println("1 -> " + Fibonacci(1))
    println("2 -> " + Fibonacci(2))
    println("5 -> " + Fibonacci(5))
    println("10 -> " + Fibonacci(10))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("¿Es primo?")
    println("Numero 2 -> " + Primo(2))
    println("Numero 13 -> " + Primo(13))
    println("Numero 25 -> " + Primo(25))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Calcula los números primos:")
    println("Numero 13 -> " + calculaPrimos(13))
    println("Numero 113 -> " + calculaPrimos(113))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Devuelve el segundo elemento de una Lista:")
    println(listaNumeros + " -> " + secondElem(listaNumeros))
    println("[1] -> " + secondElem(List(1)))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Devuelve la suma de los divisores de 3 y 5 hasta el numero 15:")
    println(calculaSumMultiplo3y5(15))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Devuelve el primer elemento de una Lista:")
    println(listaNumeros + " -> " + firstElement(listaNumeros))
    println("[1] -> " + firstElement(List(1)))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Devuele el máximo de una lista:")
    println(listaNumeros + " -> " + maximo(listaNumeros))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Comprueba si un elemento es palindromo:")
    println("Número: 212 -> " + palindromo(212))
    println("Palabra: holaaloh -> " + palindromo("holaaloh"))
    println("Palabra: holaalo -> " + palindromo("holaalo"))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Devuelve una lista de números palindromos:")
    println(listPalindromos(2000))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Devuelve una lista sin los 2 primeros elementos:")
    println(listaNumeros + " -> " + funcionLet(listaNumeros))
    println(List() + " -> " + funcionLet(List()))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Lista los elementos de una lista:")
    println(listaNumeros)
    print(funcionLista(listaNumeros))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Calcula la integral:")
    def funcion(x: Double) : Double = {
      x*x
    }
    println(calculaIntegral(funcion, 0, 2, 1))
    println(calculaIntegral(funcion, 0, 2, 0.75))
    println(calculaIntegral(funcion, 0, 2, 0.5))
    println(calculaIntegral(funcion, 0, 2, 0.25))
    println(calculaIntegral(funcion, 0, 2, 0.05))
    //////////////////////////////////////////////////////////////////////////
    println("Número PI con aproximacion 0.001:")
    println(calcupaPi(0.001))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("miFuncion:")
    println(miFuncion(List(palindromo, suma2, suma2, suma2), List(5, 4, 8, 5)))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("miFuncion2:")
    println(miFuncion2(List(funcionLet, suma1), List(5.2, 4.1, 8.3, 5.4)))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("funcionTupla:")
    println(funcionTupla(suma5, calcupaPi, 0.001))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("funcionTuplaBool:")
    println(funcionTuplaBool(palindromo, Primo, 4))
    println()
    //////////////////////////////////////////////////////////////////////////
    var A = List("1",122,5)
    var B = List("2",444,6)
    var C = List("3",5,74)
    var D = List(1,"2",54)
    println("programaTuplas:")
    println(programaTuplas(List(A,B,C,D),"2"))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Funcion MAP:")
    println(funcionMap(suma2, List(4,2,5)))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Funcion Filtra:")
    println(funcionFiltra(palindromo, List("ABA","BAB","CCCA")))
    println()
    //////////////////////////////////////////////////////////////////////////
    println("Funcion Hash:")
    println("Lista(1,2,3,4) -> " + funcionHash(List(1, 2, 3, 4)))
    println()
    //////////////////////////////////////////////////////////////////////////
  }
}
