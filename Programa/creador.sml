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

(*
    Nombre: menuCreacion
    Entrada: none
    Salida: opt -> Corresponde a la opción elegida por el usuario
    Descripción: Esta función imprime la opciones del menú para el usuario
    además de retornar la opción que el usuario eligió para que main() pueda
    gestionar el flujo del programa
*)
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

(*
    Nombre: nuevoRegistro
    Entrada: none
    Salida: nada
    Descripción: Esta función solicita al usuario los datos de una matrícula
    (carnet, nombre, código de curso, créditos y costo por crédito), valida
    que los campos numéricos sean correctos y, si lo son, agrega el registro
    al archivo "matricula.csv" respetando el formato CSV. En caso de error
    en los datos numéricos, muestra un mensaje y no guarda el registro.
*)
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

(*
    Nombre: limpiarRegistro
    Entrada: none
    Salida: nada
    Descripción: Esta función solicita confirmación al usuario para eliminar
    todas las matrículas del archivo "matricula.csv". Si el usuario confirma,
    sobrescribe el archivo dejando únicamente el encabezado, eliminando así
    todos los registros existentes. En caso contrario, cancela la operación.
*)
fun limpiarRegistro () =
    let
        val _ = print "Esto eliminara todas las matriculas\n"
        val _ = print "Desea continuar? (s/n)\n"
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
                print "Catalogo limpiado correctamente\n"
            end
        else
            print "Operacion cancelada\n"
    end;

(*
    Nombre: main
    Entrada: none
    Salida: nada
    Descripción: Esta función controla el flujo principal del programa mostrando
    el menú de creación y gestionando la opción seleccionada por el usuario.
    Dependiendo de la opción, llama a las funciones correspondientes para agregar
    una matrícula o limpiar el catálogo. El menú se repite de forma recursiva
    hasta que el usuario selecciona la opción de salir.
*)
fun main() = 
    case menuCreacion () of
         "1" => (nuevoRegistro(); main())
      |  "2" => (limpiarRegistro(); main())
      |  "0" => print "Saliendo...\n"
      |   _  => (print "Opcion invalida\n"; main());

val _ = main();



