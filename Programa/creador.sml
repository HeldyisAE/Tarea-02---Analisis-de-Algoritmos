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
        val _ = print "=== Agregando matricula ===\n"

        val _ = print "Ingrese el carnet del estudiante\n"
        val SOME carnetIn = TextIO.inputLine TextIO.stdIn
        val carnet = limpiarStr carnetIn

        val _ = print "Ingrese el nombre del estudiante\n"
        val SOME nombreIn = TextIO.inputLine TextIO.stdIn
        val nombre = limpiarStr nombreIn

        val _ = print "Ingrese el codigo del curso\n"
        val SOME codigoIn = TextIO.inputLine TextIO.stdIn
        val codigo = limpiarStr codigoIn

        val _ = print "Ingrese la cantidad de creditos\n"
        val SOME creditosIn = TextIO.inputLine TextIO.stdIn
        val creditosStr = limpiarStr creditosIn

        val _ = print "Ingrese el costo por creditos\n"
        val SOME costoIn = TextIO.inputLine TextIO.stdIn
        val costoStr = limpiarStr costoIn

        val creditosOpt = Int.fromString creditosStr
        val costoOpt = Real.fromString costoStr
    in 
        case (creditosOpt, costoOpt) of
            (SOME creditos, SOME costo) =>
                let
                    val linea =
                        carnet ^ "," ^ nombre ^ "," ^ codigo ^ "," ^
                        Int.toString creditos ^ "," ^
                        Real.toString costo ^ "\n"

                    val _ = TextIO.output(doc, linea)
                    val _ = TextIO.closeOut doc
                in
                    print "Registro agregado correctamente\n"
                end
          | _ =>
                (TextIO.closeOut doc;
                 print "Error: creditos o costo inválidos\n")
    end;

fun limpiarRegistro () =
    let
        val _ = print "Esto eliminará todas las matrículas\n"
        val _ = print "¿Desea continuar? (s/n)\n"
        val SOME respIn = TextIO.inputLine TextIO.stdIn
        val resp = limpiarStr respIn
    in
        if resp = "s" orelse resp = "S" then
            let
                val doc = TextIO.openOut "matricula.csv"

                val encabezado =
                    "carnet_estudiante,nombre,curso,creditos,costo_credito\n"

                val _ = TextIO.output(doc, encabezado)
                val _ = TextIO.closeOut doc
            in
                print "Catálogo limpiado correctamente\n"
            end
        else
            print "Operación cancelada\n"
    end;


fun main() = 
    case menuCreacion () of
         "1" => nuevoRegistro()
      |  "2" => print "Elegiste limpiar\n"
      |  "0" => print "Saliendo...\n"
      |   _  => print "Opción inválida\n"

val _ = main();



