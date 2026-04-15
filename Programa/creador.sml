(* 
   Función de menú de creación
*)
fun limpiarStr cadena =
    if String.size cadena > 0 andalso String.sub(cadena, String.size cadena - 1) = #"\n"
    then String.substring(cadena, 0, String.size cadena - 1)
    else cadena

fun menuCreacion () =
    let
        val _ = print "=== Bienvenido al Menu de creacion ===\n";
        val _ = print "1. Agregar nueva matricula\n";
        val _ = print "2. Limpiar catalogo\n";
        val _ = print "0. Salir\n";

        val SOME opt = TextIO.inputLine(TextIO.stdIn)
        val opt = limpiarStr opt
    in 
        opt
    end;



fun nuevoRegistro() = 

    let 
        val doc = TextIO.openAppend "matricula.csv"
        val _ = print "=== Agregando matricula ===\n";

        val _ = print "Ingrese el carnet del estudiante\n";
        val SOME carnet = TextIO.inputLine(TextIO.stdIn)
        val carnet = limpiarStr carnet

        val _ = print "Ingrese el nombre del estudiante\n";
        val SOME nombre = TextIO.inputLine(TextIO.stdIn)
        val nombre = limpiarStr nombre

        val _ = print "Ingrese el código del curso\n";
        val SOME codigo = TextIO.inputLine(TextIO.stdIn)
        val codigo = limpiarStr codigo

        val _ = print "Ingrese la cantidad de creditos\n";
        val SOME creditos = TextIO.inputLine(TextIO.stdIn)
        val creditos = limpiarStr creditos

        val _ = print "Ingrese el costo por creditos\n";
        val SOME costo = TextIO.inputLine(TextIO.stdIn)
        val costo = limpiarStr costo

        val linea = carnet ^ "," ^ nombre ^ "," ^ codigo ^ "," ^ creditos ^ "," ^ costo ^ "\n"

        val _ = TextIO.output(doc, linea)

        val _ = TextIO.closeOut doc
    in 
        ()
    end;




fun main() = 
    case menuCreacion () of
         "1" => nuevoRegistro()
      |  "2" => print "Elegiste limpiar\n"
      |  "0" => print "Saliendo...\n"
      |   _  => print "Opción inválida\n"

val _ = main();



