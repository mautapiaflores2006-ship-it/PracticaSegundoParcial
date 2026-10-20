//ejercicio 1

class Pelicula(
    val titulo:String, 
    val duracionMinutos: Int,
    val clasificacion: String
)
object Pelicula {
    def apply(titulo: String, duracionMinutos: Int, clasificacion: String): Pelicula = {
        if duracionMinutos <= 0 then new Pelicula(titulo, 90, clasificacion)
        else new Pelicula(titulo, duracionMinutos, clasificacion)
    }
}

//ejercicio 2

class FuncionCine(
    val pelicula: Pelicula,
    val sala: Int,
    val hora: String,
    val precioEntrada: Double, 
    val asientosDisponibles: Int
) {
    //ejercicio 3
     def puedeReservar(cantidad:Int): Boolean = {
        if cantidad <= asientosDisponibles then true else false
    // fin ejercicio 3
     }
    //ejercicio 4
    def costoReserva(cantidad: Int): Double = {
        cantidad * precioEntrada
    } 
    //fin ejercicio 4

    //ejercicio 5
    def reservar(cantidad: Int): FuncionCine = {
        if cantidad <= asientosDisponibles then new FuncionCine(pelicula,sala,hora,precioEntrada,asientosDisponibles-cantidad)
        else new FuncionCine(pelicula,sala,hora,precioEntrada,asientosDisponibles)
    }
    //fin ejercicio 5

    // ejercicio 6
    def descripcion: String = {
        s"${pelicula.titulo} \n" +
        s"Sala: $sala\n" +
        s"Hora: $hora\n" +
        s"Precio: $precioEntrada\n" +
        s"Asientos disponibles: $asientosDisponibles"
    }
    //fin ejercicio 6

}
object FuncionCine {
    def apply (
        pelicula: Pelicula,
        sala: Int,
        hora: String,
        precioEntrada: Double,
        asientosDisponibles: Int): FuncionCine = {
        val salaValida = if sala >= 1 then sala else 1
        val precioValido = if precioEntrada >= 0 then precioEntrada else 0.0
        val asientoValido = if asientosDisponibles >= 0 then asientosDisponibles else 0
        new FuncionCine(pelicula,salaValida,hora,precioValido,asientoValido)
    }
// fin ejercicio 2

    //ejercicio 7
    def funcionEstandar(
        pelicula: Pelicula,
        sala: Int,
        hora: String): FuncionCine = {
            new FuncionCine(pelicula,sala,hora, 30, 50)
        }
    //fin ejercicio 7

} 

object Parte2 {
    def main (args: Array[String]): Unit = {
        /*val pelicula = Pelicula("Interstellar",0,"PG-13")
        println(pelicula.duracionMinutos)

        val funcion = FuncionCine(pelicula, 0, "12:00", -1, -1)
        println(funcion.precioEntrada )
        println(funcion.asientosDisponibles)
        println(funcion.sala)

        val funcion02 = FuncionCine(pelicula, 3, "13:00", 70,20)
        println(funcion02.puedeReservar(21))

        println(funcion02.costoReserva(4))

        val funcion3 = funcion02.reservar(5)
        println(funcion3.asientosDisponibles)

        println(funcion02.descripcion)

        val funcion4 = FuncionCine.funcionEstandar(
                pelicula,
                2,
                "18:30"
            )
        println(funcion4.precioEntrada)*/

        //ejercicio 8
        val pelicula1 = Pelicula("Dune", 155, "PG-13")
        val funcion1 = FuncionCine(pelicula1, 3, "20:30", 35.0, 40)
        val funcion2 = FuncionCine.funcionEstandar(pelicula1, 1, "16:00")

        //ejercicio 9
        println(funcion1.descripcion)
        println(funcion1.puedeReservar(5))
        println(funcion1.costoReserva(5))
        val funcionReservada = funcion1.reservar(5)
        println(funcion1.asientosDisponibles)
        println(funcionReservada.asientosDisponibles)
    }
}