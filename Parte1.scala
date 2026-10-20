object Parte1 {
    //ejercicio 1
    def main (args: Array[String]): Unit = {
        val duplicar: Int => Int = a => a*2 
        val cuadrado: Int => Int = a => a*a
        val esPar: Int => Boolean = a => if a%2==0 then true else false
        println(duplicar(5))
        println(cuadrado(4))
        println(esPar(8))
        
        //ejercicio 2
        val cubo: Int => Int = a => a*a*a
        val promedio: (Double, Double) => Double = (a,b) => (a+b)/2
        val mayor: (Int, Int) => Int = (a,b) => if a>b then a else b
        val esMultiploDeTres: Int => Boolean = a => if a%3 == 0 then true else false

        //ejercicio 3
        def transformar(numero:Int, operacion: Int => Int): Int = {
            operacion(numero)
        }

        println(transformar(5, x=> x*2))
        transformar(5, x => x*x)
        transformar(5, x => x+10)
        transformar(5,x=> -x)

        //ejercicio 4
        def calcular(a: Double, b:Double, operacion:(Double, Double) => Double): Double = {
            operacion(a,b) 
        }
        calcular(5,4,(a,b)=>a+b)
        calcular(5,4,(a,b)=>a-b)
        calcular(5,4,(a,b)=>a*b)
        calcular(5,4,(a,b)=>a/b)
        calcular(5,4,(a,b)=>(a+b)/2)

        //ejercicio 5
        def crearMultiplicador(factor: Int): Int => Int =  {
            (numero:Int) => numero*factor
        }
        val duplicar5 = crearMultiplicador(2)
        val triplicar = crearMultiplicador(3)
        val porDiez = crearMultiplicador(10)

        println(duplicar5(7))
        println(porDiez(10))

        //ejercicio 6
        def aplicarDescuento(precio:Double, porcentaje:Double): Double = {
             precio - precio * porcentaje
        }
        val descuento10: Double => Double = precioOriginal => aplicarDescuento(precioOriginal,0.1)
        val descuento20: Double => Double = precioOriginal => aplicarDescuento(precioOriginal,0.2)
        val descuento50: Double => Double = precioOriginal => aplicarDescuento(precioOriginal,0.5)

        println(descuento10(500))
        println(descuento50(500))

        //ejercicio 7
        def elevar(exponente: Int)(base: Int): Int = {
            def potencia (b:Int ,e:Int ): Int = {
                if e == 0 then 1
                else b * potencia(b,e-1)
            }
            potencia(base,exponente)
        }
        val cuadrado7: Int => Int = n => elevar(2)(n)
        val cubo7: Int => Int = n => elevar(3)(n)
        val cuartaPotencia: Int => Int = n => elevar(4)(n)

        println(cuadrado7(5))
        println(cubo7(5))
        println(cuartaPotencia(5))

        //ejercicio 8
        val limpiar: String => String = texto => texto.substring(1, texto.length-1)
        val convertirMinusculas: String => String = texto => texto.toLowerCase()
        val longitud: String => Int = texto => texto.length()
        val esLongitudValida: Int => Boolean = n => if n >= 5 then true else false

        val validarTexto = limpiar andThen convertirMinusculas andThen longitud andThen esLongitudValida
        println(validarTexto("LAKSJJK"))

        //ejercicio 9
        def sumar9(desde:Int, hasta:Int, transformar9: Int => Int): Int ={
            if desde > hasta then 0
            else transformar9(desde) + sumar9(desde+1, hasta,transformar9)
        }
        def producto9(desde:Int, hasta:Int, transformar9: Int => Int): Int ={
            if desde > hasta then 1
            else transformar9(desde) * producto9(desde+1, hasta,transformar9)
        }
        println(sumar9(1,4,x=>x))
        println(sumar9(1,3,x=>x*x))
        println(producto9(1,4,x=>x))

        //ejercicio 10
    def transformarTexto(texto:String, transformacion10: Char => Char): String ={
        def recorrer (t:String, acum:String): String = {
            if (t.isEmpty) then acum
            else recorrer(t.tail, acum + transformacion10(t.head))
        }
        recorrer(texto, "")
    }

    def buscarCaracter(texto:String, condicion: Char => Boolean): Boolean ={
        def buscar (t:String): Boolean = {
            if (t.isEmpty) then false
            else if condicion(t.head) then true
            else buscar(t.tail)
        }
        buscar(texto)
    }

    println(transformarTexto("scala", c => c.toUpper))
    println(buscarCaracter("Scala2026", c => c.isDigit))
    println(buscarCaracter("Scala", c => c =='z'))
    }
}