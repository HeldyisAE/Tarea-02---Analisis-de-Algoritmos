(* 
   Nombre: limpiarStr
   Entrada: cadena -> Corresponde una cadena de texto cualquiera, tipo String
   Salida: cadena -> La cadena de texto ingresada sin salto de linea al final
   Descripcion: La funcion tiene el proposito de eliminar el salto de linea al final
   de un string, para esto se verifica que la cadena sea valida y que tenga el salto de linea al final.
   Posteriormente se utiliza substring para eliminar este ultimo caracter invisible y devolver la cadena
   limpia
*)
fun limpiarStr cadena =
    if String.size cadena > 0 andalso String.sub(cadena, String.size cadena - 1) = #"\n"
    then String.substring(cadena, 0, String.size cadena - 1)
    else cadena;

(*
   Nombre: leerArchivo
   Entrada: ruta -> String con la ruta del archivo CSV
   Salida: lista de tuplas (carnet, nombre, curso, creditos, costo)
   Descripcion: Lee el archivo CSV y convierte cada linea en una tupla de datos.
   La primera linea (encabezado) es descartada antes de iniciar el loop de lectura.
   Los campos de creditos y costo se convierten a Int y Real respectivamente;
   en caso de fallo de conversion se asigna 0 o 0.0 como valor por defecto.
*)
fun leerArchivo ruta =
    let
        val ins = TextIO.openIn ruta
        val _ = TextIO.inputLine ins (* descarta el encabezado *)
        fun loop acc =
            case TextIO.inputLine ins of
                SOME linea =>
                    let
                        val linea = limpiarStr linea
                        val campos = String.tokens (fn c => c = #",") linea
                    in
                        case campos of
                            carnet::nombre::curso::creditos::costo::[] =>
                                let
                                    val cr = case Int.fromString creditos of SOME v => v | NONE => 0
                                    val co = case Real.fromString costo of SOME v => v | NONE => 0.0
                                in
                                    loop ((carnet, nombre, curso, cr, co) :: acc)
                                end
                          | _ => loop acc
                    end
              | NONE => acc
        val datos = loop []
        val _ = TextIO.closeIn ins
    in
        List.rev datos
    end;

(*
   Nombre: resumenGeneral
   Entrada: registros -> lista de tuplas (carnet, nombre, curso, creditos, costo)
   Salida: imprime el informe solicitado con 5 secciones:
           1. Cantidad de estudiantes por curso
           2. Estudiante con mayor cantidad de creditos
           3. Estudiante con menor cantidad de creditos
           4. Curso con mayor monto de ingreso total
           5. Estudiante que genera mayor ingreso total
*)
fun resumenGeneral registros =
    (
        print "\n1. Cantidad de estudiantes por curso:\n";
        let
            fun contarEstudiantesPorCurso [] _ = 0
              | contarEstudiantesPorCurso ((_, _, curso2, _, _)::xs) curso =
                    if curso2 = curso
                    then 1 + contarEstudiantesPorCurso xs curso
                    else contarEstudiantesPorCurso xs curso
            fun imprimirCursos [] = ()
              | imprimirCursos ((_, _, curso, _, _)::xs) =
                    (print ("Curso: " ^ curso ^ " | Estudiantes: " ^
                            Int.toString (contarEstudiantesPorCurso registros curso) ^ "\n");
                     imprimirCursos (List.filter (fn (_, _, c, _, _) => c <> curso) xs))
        in
            imprimirCursos registros
        end;

        print "\n2. Estudiante con mayor cantidad de creditos matriculados:\n";
        let
            fun totalCreditos carnet =
                let
                    fun aux [] acc = acc
                      | aux ((c, _, _, cr, _)::xs) acc =
                            if c = carnet then aux xs (acc + cr) else aux xs acc
                in
                    aux registros 0
                end
            fun buscarMayorCred [] mayorC mayorN mayorV = (mayorC, mayorN, mayorV)
              | buscarMayorCred ((c, n, _, _, _)::xs) mayorC mayorN mayorV =
                    let val t = totalCreditos c in
                        if t > mayorV
                        then buscarMayorCred xs c n t
                        else buscarMayorCred xs mayorC mayorN mayorV
                    end
            fun buscarMenorCred [] menorC menorN menorV = (menorC, menorN, menorV)
              | buscarMenorCred ((c, n, _, _, _)::xs) menorC menorN menorV =
                    let val t = totalCreditos c in
                        if t < menorV
                        then buscarMenorCred xs c n t
                        else buscarMenorCred xs menorC menorN menorV
                    end
            fun imprimirUnico [] _ = ()
              | imprimirUnico ((c, n, _, _, _)::xs) carnet =
                    if c = carnet
                    then print ("Carnet: " ^ c ^ " | Nombre: " ^ n ^
                                " | Creditos: " ^ Int.toString (totalCreditos c) ^ "\n")
                    else imprimirUnico xs carnet
            val (mayorC, mayorN, mayorV) = buscarMayorCred registros "" "" 0
            val (menorC, menorN, _)      = buscarMenorCred registros mayorC mayorN mayorV
        in
            imprimirUnico registros mayorC;
            print "\n3. Estudiante con menor cantidad de creditos matriculados:\n";
            imprimirUnico registros menorC
        end;

        print "\n4. Curso con mayor monto ingreso total:\n";
        let
            fun totalIngresoCurso curso =
                let
                    fun aux [] acc = acc
                      | aux ((_, _, c, cr, co)::xs) acc =
                            if c = curso then aux xs (acc + Real.fromInt cr * co) else aux xs acc
                in
                    aux registros 0.0
                end
            fun buscarMayorCurso [] mayorC mayorV = (mayorC, mayorV)
              | buscarMayorCurso ((_, _, c, _, _)::xs) mayorC mayorV =
                    let val t = totalIngresoCurso c in
                        if t > mayorV
                        then buscarMayorCurso xs c t
                        else buscarMayorCurso xs mayorC mayorV
                    end
            fun imprimirUnicoCurso [] _ = ()
              | imprimirUnicoCurso ((_, _, c, _, _)::xs) curso =
                    if c = curso
                    then print ("Curso: " ^ c ^ " | Ingreso: " ^
                                Real.toString (totalIngresoCurso c) ^ "\n")
                    else imprimirUnicoCurso xs curso
            val (mayorC, _) = buscarMayorCurso registros "" 0.0
        in
            imprimirUnicoCurso registros mayorC
        end;

        print "\n5. Estudiante que genera mayor ingreso total:\n";
        let
            fun totalIngresoEstudiante carnet =
                let
                    fun aux [] acc = acc
                      | aux ((c, _, _, cr, co)::xs) acc =
                            if c = carnet then aux xs (acc + Real.fromInt cr * co) else aux xs acc
                in
                    aux registros 0.0
                end
            fun buscarMayorEst [] mayorC mayorN mayorV = (mayorC, mayorN, mayorV)
              | buscarMayorEst ((c, n, _, _, _)::xs) mayorC mayorN mayorV =
                    let val t = totalIngresoEstudiante c in
                        if t > mayorV
                        then buscarMayorEst xs c n t
                        else buscarMayorEst xs mayorC mayorN mayorV
                    end
            fun imprimirUnicoEst [] _ = ()
              | imprimirUnicoEst ((c, n, _, _, _)::xs) carnet =
                    if c = carnet
                    then print ("Carnet: " ^ c ^ " | Nombre: " ^ n ^
                                " | Ingreso: " ^ Real.toString (totalIngresoEstudiante c) ^ "\n")
                    else imprimirUnicoEst xs carnet
            val (mayorC, _, _) = buscarMayorEst registros "" "" 0.0
        in
            imprimirUnicoEst registros mayorC
        end
    );

(*
   Nombre: cursosPorCreditos
   Entrada: registros -> lista de tuplas (carnet, nombre, curso, creditos, costo)
   Salida: imprime la lista de cursos que coincidan con la cantidad de creditos solicitada
   Descripcion: Solicita al usuario la cantidad de creditos a buscar, filtra los registros
   que coincidan con ese valor y los imprime. Si no hay coincidencias, informa al usuario.
*)
fun cursosPorCreditos registros =
    let
        val _ = print "Ingrese la cantidad de creditos:\n"
        val nCreditos =
            case TextIO.inputLine TextIO.stdIn of
                SOME cstr =>
                    (case Int.fromString (limpiarStr cstr) of
                        SOME n => n
                      | NONE => (print "Valor de creditos invalido\n"; ~1))
              | NONE => (print "No se ingreso un valor valido\n"; ~1)
    in
        if nCreditos < 0 then ()
        else
            let
                val filtrados = List.filter (fn (_, _, _, creditos, _) => creditos = nCreditos) registros
                fun imprimir [] = print "No hay cursos con esa cantidad de creditos\n"
                  | imprimir ((_, _, curso, creditos, _)::xs) =
                        (print ("Curso: " ^ curso ^ " | Creditos: " ^ Int.toString creditos ^ "\n");
                         imprimir xs)
            in
                imprimir filtrados
            end
    end;

(*
   Nombre: buscarMatriculasPorEstudiante
   Entrada: registros -> lista de tuplas (carnet, nombre, curso, creditos, costo)
   Salida: imprime las matriculas asociadas al carnet exacto o nombre que contenga la consulta
   Descripcion: Solicita al usuario un carnet exacto o parte del nombre del estudiante.
   La busqueda por nombre es insensible a mayusculas. Si no se encuentran resultados,
   informa al usuario.
*)
fun buscarMatriculasPorEstudiante registros =
    let
        val _ = print "Ingrese carnet exacto o parte del nombre:\n"
        val consulta =
            case TextIO.inputLine TextIO.stdIn of
                SOME s => limpiarStr s
              | NONE => ""
        val consultaLower = String.map Char.toLower consulta
        fun coincide (carnet, nombre, _, _, _) =
            carnet = consulta orelse
            String.isSubstring consultaLower (String.map Char.toLower nombre)
        val filtrados = List.filter coincide registros
        fun imprimir [] = print "No se encontraron matriculas para la busqueda\n"
          | imprimir ((carnet, nombre, curso, creditos, costo)::xs) =
                (print ("Carnet: " ^ carnet ^ " | Nombre: " ^ nombre ^
                        " | Curso: " ^ curso ^ " | Creditos: " ^ Int.toString creditos ^
                        " | Costo: " ^ Real.toString costo ^ "\n");
                 imprimir xs)
    in
        if consulta = "" then print "No se ingreso una busqueda valida\n"
        else imprimir filtrados
    end;

(*
   Nombre: mayorIngreso
   Entrada: registros -> lista de tuplas (carnet, nombre, curso, creditos, costo)
   Salida: imprime los cursos cuyo ingreso total se encuentre dentro del rango indicado,
   ordenados de mayor a menor ingreso
   Descripcion: Solicita al usuario un monto minimo y maximo. Agrupa los registros por curso
   sumando el ingreso de cada uno (creditos * costo por registro). Filtra los cursos cuyo
   ingreso total caiga dentro del rango y los ordena de forma descendente usando quicksort.
*)
fun mayorIngreso registros =
    let
        val cursos =
            List.map (fn (_, _, curso, creditos, costo) =>
                (curso, Real.fromInt creditos * costo)
            ) registros
 
        val agrupados =
            List.foldl (fn ((curso, ingreso), acc) =>
                let
                    fun insertar [] = [(curso, ingreso)]
                      | insertar ((c, total)::xs) =
                            if c = curso then (c, total + ingreso) :: xs
                            else (c, total) :: insertar xs
                in
                    insertar acc
                end
            ) [] cursos
 
        val _ = print "\nIngrese monto minimo:\n"
        val minOpt =
            case TextIO.inputLine TextIO.stdIn of
                SOME s => Real.fromString (limpiarStr s)
              | NONE   => NONE
 
        val _ = print "Ingrese monto maximo:\n"
        val maxOpt =
            case TextIO.inputLine TextIO.stdIn of
                SOME s => Real.fromString (limpiarStr s)
              | NONE   => NONE
    in
        case (minOpt, maxOpt) of
            (SOME minVal, SOME maxVal) =>
                let
                    val filtrados =
                        List.filter (fn (_, ingreso) =>
                            ingreso >= minVal andalso ingreso <= maxVal
                        ) agrupados
 
                    fun quicksort [] = []
                      | quicksort ((c, v)::xs) =
                            let
                                val mayores = List.filter (fn (_, v2) => v2 > v) xs
                                val menores = List.filter (fn (_, v2) => v2 <= v) xs
                            in
                                quicksort mayores @ [(c, v)] @ quicksort menores
                            end
 
                    val ordenados = quicksort filtrados
 
                    fun imprimir [] = print "No hay resultados en ese rango\n"
                      | imprimir ((curso, ingreso)::xs) =
                            (print ("Curso: " ^ curso ^ " | Ingreso: " ^
                                    Real.toString ingreso ^ "\n");
                             imprimir xs)
                in
                    print "\nResultados:\n";
                    imprimir ordenados
                end
          | _ => print "Error: los montos ingresados no son valores numericos validos\n"
    end;

(*
   Nombre: masCincoEstudiantes
   Entrada: registros -> lista de tuplas (carnet, nombre, curso, creditos, costo)
   Salida: imprime los cursos que tienen 5 o mas estudiantes distintos matriculados
   Descripcion: Agrupa los carnets unicos por curso. Un mismo carnet puede aparecer
   en multiples registros (por tener varias materias), pero solo se cuenta una vez
   por curso. Luego filtra los cursos con al menos 5 estudiantes y los imprime.
*)
fun masCincoEstudiantes registros =
    let
        val cursoEst =
            List.map (fn (carnet, _, curso, _, _) => (curso, carnet)) registros

        fun agruparPorCurso lista =
            List.foldl (fn ((curso, carnet), acc) =>
                let
                    fun insertar [] = [(curso, [carnet])]
                      | insertar ((c, cs)::xs) =
                            if c = curso then
                                if List.exists (fn x => x = carnet) cs
                                then (c, cs) :: xs
                                else (c, carnet :: cs) :: xs
                            else
                                (c, cs) :: insertar xs
                in
                    insertar acc
                end
            ) [] lista

        val agrupados = agruparPorCurso cursoEst

        val filtrados =
            List.filter (fn (_, cs) => List.length cs >= 5) agrupados

        fun imprimir [] = print "No hay cursos con 5 o mas estudiantes\n"
          | imprimir ((curso, cs)::xs) =
                (print ("Curso: " ^ curso ^ " | Estudiantes: " ^
                        Int.toString (List.length cs) ^ "\n");
                 imprimir xs)
    in
        imprimir filtrados
    end;

(*
   Nombre: menuAnalizador
   Entrada: ninguna
   Salida: String con la opcion seleccionada por el usuario
   Descripcion: Muestra el menu principal del sistema con las opciones disponibles
   y retorna la linea ingresada por el usuario, limpiando el salto de linea final.
*)
fun menuAnalizador () =
    let
        val _ = print "\n=== Bienvenido al Menu del Analizador ===\n"
        val _ = print "1. Cursos con mayor ingreso\n"
        val _ = print "2. Cursos con mas de 5 estudiantes\n"
        val _ = print "3. Buscar matriculas por estudiante\n"
        val _ = print "4. Cursos por cantidad de creditos\n"
        val _ = print "5. Resumen general del sistema\n"
        val _ = print "0. Salir\n"
        val SOME opt = TextIO.inputLine TextIO.stdIn
    in
        limpiarStr opt
    end;

(*
   Nombre: mainAnalizador
   Entrada: registros -> lista de tuplas (carnet, nombre, curso, creditos, costo)
    ya cargada desde el archivo CSV
   Salida: ejecuta la opcion seleccionada por el usuario en un loop hasta elegir salir
   Descripcion: Muestra el menu y delega la ejecucion a cada funcion especializada.
   Cada funcion es responsable de solicitar los datos adicionales que necesite.
   En caso de opcion invalida, vuelve a mostrar el menu.
*)
fun mainAnalizador registros =
    case menuAnalizador () of
        "1" => (mayorIngreso registros; mainAnalizador registros)
      | "2" => (masCincoEstudiantes registros; mainAnalizador registros)
      | "3" => (buscarMatriculasPorEstudiante registros; mainAnalizador registros)
      | "4" => (cursosPorCreditos registros; mainAnalizador registros)
      | "5" => (resumenGeneral registros; mainAnalizador registros)
      | "0" => print "Saliendo...\n"
      |  _  => (print "Opcion invalida\n"; mainAnalizador registros);

(*
   Punto de entrada del programa.
   Solicita la ruta del archivo CSV, lo lee para obtener los registros
   y lanza el menu principal.
*)
val _ =
    (
        print "Ingrese la ruta del archivo CSV a analizar:\n";
        case TextIO.inputLine TextIO.stdIn of
            SOME ruta => mainAnalizador (leerArchivo (limpiarStr ruta))
          | NONE      => print "No se ingreso una ruta valida.\n"
    );