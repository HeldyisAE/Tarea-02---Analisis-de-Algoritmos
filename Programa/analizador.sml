(* 
   Nombre: limpiarStr
   Entrada: cadena -> Corresponde una cadena de texto cualquiera, tipo String
   Salida: cadena -> La cadena de texto ingresada
   Descripción: La función tiene el proposito de eliminar el salto de linea al final
   de un string, para esto se verifica que la cadena sea válida y que tenga el salto de linea al final.
   Posteriormente se utiliza substring para eliminar este último caracter invisible y devolver la cadena
   limpia
*)
fun limpiarStr cadena =
    if String.size cadena > 0 andalso String.sub(cadena, String.size cadena - 1) = #"\n"
    then String.substring(cadena, 0, String.size cadena - 1)
    else cadena

fun menuAnalizador () =
    let
        val _ = print "=== Bienvenido al Menu del Analizador ===\n"
        val _ = print "1. Cursos con mayor ingreso\n"
        val _ = print "2. Cursos con mas de 5 estudiantes\n"
        val _ = print "3. Buscar matriculas por estudiante\n"
        val _ = print "4. Cursos por cantidad de creditos\n"
        val _ = print "5. Resumen general del sistema\n"
        val _ = print "0. Salir\n"

        val SOME opt = TextIO.inputLine TextIO.stdIn
        val opt = limpiarStr opt
    in
        opt
    end;

fun mayorIngreso () = ()

fun mainAnalizador ruta =
    case menuAnalizador () of
         "1" => print "Mayor ingreso"
      |  "2" => print "Mas de 5 estudiantes"
      |  "3" => print "Matricula por estudiante"
      |  "4" => print "Cantidad de curso por creditos"
      |  "5" => print "Resumen general"
      |  "0" => print "Saliendo...\n"
      |   _  => (print "Opcion invalida\n"; mainAnalizador ruta);

val _ = mainAnalizador("C:/Users/heldy/Desktop/TC2/Programa/matricula.csv");
