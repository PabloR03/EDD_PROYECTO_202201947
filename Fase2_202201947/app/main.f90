program main
    use json_module
    use s_de_Split
    use global_variable
    use Mod_ListaCliente
    use Mod_ListaImagenes
    use Mod_ListaAlbumes
    use Mod_ArbolABB_p1
    use Mod_ArbolABB_p
    use Mod_ArbolAVL_p1
    use Mod_MatrizDispersa
    implicit none

    character(len=100) :: opcion_principal
    character(len=100) :: usuario
    character(len=100) :: contrasena
    character(len=100) :: dpi_cliente1, nombre_cliente1, contrasena_cliente1
    character(len=100) :: documento_capa, documento_imagen, documento_album, documento_cliente
    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: listaPunteroCapa, punteroCapa, atributoPunteroCapa, punteroPixel, atributoPixel
    logical :: capa_encontrada
    integer :: size_capa, contador_capa, size_pixel, contador_pixel
    integer ::  fila_int, columna_int, id_capa_int
    character(:), allocatable :: id_capa, fila, columna, color
    type(json_value), pointer :: listaPunteroImagen, punteroImagen, atributoPunteroImagen
    integer :: size_imagen, contador_imagen
    integer :: id_imagen, id_capas
    logical :: imagen_encontrada
    type(json_value), pointer :: listaPunteroAlbum, punteroAlbum, atributoPunteroAlbum
    character(:), allocatable :: nombre_album,imgs
    integer :: size_album, contador_album
    integer :: imgs_size, contador_a
    logical :: album_encontrado
    type(json_value), pointer :: listaPunteroCliente, punteroCiente, atributoPunteroCliente
    integer :: size_cliente, contador_cliente
    character(:), allocatable :: dpi_cliente, nombre_cliente, contrasena_cliente
    logical :: cliente_encontrado
    type(lista_cliente) :: lista_simple_cliente  

    do
        print *, "____________________________________"
        print *, "          Menu Principal - PPS      "
        print *, "1. Iniciar Sesion"
        print *, "2. Registrarse"
        print *, "3. Salir"
        print *, "____________________________________"
        print *, "Seleccione El Numero De Opcion:"
        read(*,*) opcion_principal
        select case(opcion_principal)
            case('1')
                call IniciarSesion()
                print *, "____________________________________"
            case('2')
                call Registrarse()
                print *, "____________________________________"
            case('3')
                exit
            case default
                print *, "ERROR EN SELECCION DE OPCION, INTENTE DE NUEVO."
        end select
    end do

contains

    subroutine IniciarSesion()
        print *, "____________________________________"
        print *, "           INICIAR SESION           "   
        print *, "____________________________________"
        print *, "Ingrese su Usuario - ID: "
        read(*,*) usuario
        print *, "Ingrese su contrasena: "
        read(*,*) contrasena
        if (usuario == "admin" .and. contrasena == "EDD2024") then
            print *, "____________________________________"
            print*,"BIENVENIDO ADMIN"
            call MenuAdmin()
        else if(lista_simple_cliente%iniciar_sesion_c(usuario, contrasena))then
            dpi_global = usuario
            print *, "____________________________________"
            print*,"BIENVENIDO CLIENTE: ", dpi_global
            call MenuCliente()
        else
            print *, "Usuario no registrado, si desea Registrarse seleccione la opcion 2."
        end if
    end subroutine IniciarSesion

    subroutine Registrarse()
        character(len=100) :: linea
        print *, "_________________________________"
        print *, "           REGISTRARSE           "  
        print *, "_________________________________"
        print*,"Ingrese el numero de DPI (ID): "
        read(*,'(a)') linea
        dpi_cliente1 = trim(linea)
        print*,"Ingrese su nombre y apellido: "
        read(*,'(a)') linea
        nombre_cliente1 = trim(linea)
        print*,"Ingrese su contrasena: "
        read(*,'(a)') linea
        contrasena_cliente1 = trim(linea)
        call lista_simple_cliente%insertar_cliente(dpi_cliente1, nombre_cliente1, contrasena_cliente1)
    end subroutine Registrarse

    subroutine MenuAdmin()
        character(len=100) :: opcion_admin
        do
            print *, "_____________________________________"
            print *, "          Menu Admin - PPS           "
            print *, "1. Carga Masiva Cliente"
            print *, "2. Manejo De Clientes"
            print *, "3. Visualizar Arbol Cliente"
            print *, "4. Reportes de Clientes"
            print *, "5. Regresar Al Login"
            print *, "_____________________________________"
            print *, "Seleccione El Numero De Opcion:"
            print *, "_____________________________________"
            read(*,*) opcion_admin
            select case(opcion_admin)
                case('1')
                    call clientesJSON()
                case('2')
                    call abcCLIENTE()
                case('3')
                    call lista_simple_cliente%grafica_cliente("Lista_Cliente")
                case('4')
                    call ReportesAdmin()
                case('5')
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine MenuAdmin

    subroutine abcCLIENTE()
        character(len=100) :: opcion_cliente
        character(len=100) :: linea
        do
            print *, "__________________________________________"
            print *, "         Menu Manejo Cliente - PPS        "
            print *, "1. Nuevo Cliente"
            print *, "2. Eliminar Cliente"
            print *, "3. Modificar Cliente"
            print *, "4. Regresar Al Menu Administrador"
            print *, "__________________________________________"
            print *, "Seleccione El Numero De Opcion:"
            print *, "__________________________________________"
            read(*,*) opcion_cliente
            select case(opcion_cliente)
                case('1')
                    print *, "_______________________________________"
                    print *, "            NUEVO CLIENTE              "  
                    print *, "_______________________________________"
                    print*,"Ingrese el numero de DPI: "
                    read(*,'(a)') linea
                    dpi_cliente1 = trim(linea)
                    print*,"Ingrese El Nombre y Apellido: "
                    read(*,'(a)') linea
                    nombre_cliente1 = trim(linea)
                    print*,"Ingrese La Contrasena: "
                    read(*,'(a)') linea
                    contrasena_cliente1 = trim(linea)
                    call lista_simple_cliente%insertar_cliente(dpi_cliente1, nombre_cliente1, contrasena_cliente1)
                case('2')
                    print *, "_______________________________________"
                    print *, "            ELIMINAR CLIENTE           "  
                    print *, "_______________________________________"
                    print*,"Ingrese el numero de DPI: "
                    read(*,*) dpi_cliente1
                    call lista_simple_cliente%eliminar_cliente(dpi_cliente1)
                case('3')
                    print *, "_______________________________________"
                    print *, "           MODIFICAR CLIENTE           "  
                    print *, "_______________________________________"
                    print*,"Ingrese El Numero De DPI: "
                    read(*,'(a)') linea
                    dpi_cliente1 = trim(linea)
                    print*,"Ingrese El Nuevo Nombre Y Apellido: "
                    read(*,'(a)') linea
                    nombre_cliente1 = trim(linea)
                    print*,"Ingrese La Nueva Contrasena: "
                    read(*,'(a)') linea
                    contrasena_cliente1 = trim(linea)
                    call lista_simple_cliente%modificar_cliente(dpi_cliente1, nombre_cliente1, contrasena_cliente1)
                case('4')
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine abcCLIENTE

    subroutine ReportesAdmin()
        character(len=100) :: opcion_reporte_a
        character(len=20) :: dpi_cliente_reporte
        do
            print *, "_______________________________________"
            print *, "    Menu De Reportes Admin - PPS"
            print *, "_______________________________________"
            print *, "DESEA IMPRIMIR LOS REPORTES DE: "
            print *, "  - Mostrar Informacion: Nombre, DPI Y Password."
            print *, "  - Cantidad De Albumes Y Sus Imagenes."
            print *, "  - Cantidad De Imagenes Totales."
            print *, "  - Cantidad De Capas Totales"
            print *, "  - Listar Clientes"
            print *, "1. ACEPTAR  (GENERAR REPORTE)"
            print *, "2. CANCELAR (Regresar Al Menu Administrador)"
            print *, "_______________________________________"
            print *, "Seleccione El Numero De Opcion:"
            print *, "_______________________________________"
            read(*,*) opcion_reporte_a
            select case(opcion_reporte_a)
                case('1')
                    print *, "_______________________________________"
                    print *, "           LISTAR CLIENTES             "
                    call lista_simple_cliente%reporte_listar_cliente()
                    print *, "_______________________________________"
                    print *, "Ingrese el DPI del cliente para sus reportes:"
                    print *, "_______________________________________"
                    read(*,*) dpi_cliente_reporte
                    print *, "_______________________________________"
                    print *, "      MOSTRAR INFORMACION CLIENTE      "
                    print *, "_______________________________________"
                    call lista_simple_cliente%mostrar_cliente(dpi_cliente_reporte)
                    print *, "_______________________________________"
                    print *, "  CANTIDAD DE ALBUMES Y SUS IMAGENES   "
                    print *, "_______________________________________"
                    call lista_simple_cliente%reporte_albumes_cliente(dpi_cliente_reporte)
                    print *, "_______________________________________"
                    print *, "      CANTIDAD DE IMAGENES TOTALES     "
                    print *, "_______________________________________"
                    call lista_simple_cliente%reporte_imagenes_cliente(dpi_cliente_reporte)
                    print *, "_______________________________________"
                    print *, "CANTIDAD DE CAPAS TOTALES"
                    print *, "_______________________________________"
                    call lista_simple_cliente%reporte_capas_cliente(dpi_cliente_reporte)
                case('2')
                    exit
                case default
                    print *, "OPCION INVALIDA, INTENTE DE NUEVO."
            end select
        end do
    end subroutine ReportesAdmin

    subroutine MenuCliente()
        character(len=100) :: opcion_cliente, numero_imagen
        logical :: existe_imagen
        do
            print *, "_______________________________________"
            print *, "          Menu Cliente - PPS           "
            print *, "1. Carga Masiva Informacion"
            print *, "2. Visualizar Estructuras"
            print *, "3. Navegacion Y Gestion Imagenes"
            print *, "4. Reportes"
            print *, "5. Manejo De Imagenes"
            print *, "6. Regresar Al Login"
            print *, "_______________________________________"
            print *, "Seleccione El Numero De Opcion:"
            print *, "_______________________________________"
            read(*,*) opcion_cliente
            select case(opcion_cliente)
                case('1')
                    call MenuCargaJSON()
                case('2')
                    call EstructurasCliente()
                case('3')
                    call CrearImagenes()
                case('4')
                    call ReportesCliente()
                case('5')
                    call abcIMAGEN()
                case('6')
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine MenuCliente

    subroutine EstructurasCliente()
        type(nodo_cliente), pointer :: cliente_actual
        character(len=100) :: opcion_estructura 
        integer :: numero_capa, numero_imagen
        logical :: existe_matriz, existe_imagen
        type(matriz), pointer :: matriz_auxiliar
        do
            cliente_actual => lista_simple_cliente%obtener_cliente(dpi_global)
            print *, "_______________________________________"
            print *, "Menu Visualizar Estructuras - PPS"
            print *, "1. Ver Arbol De Capas"
            print *, "2. Ver Arbol De Imagenes"
            print *, "3. Ver Listado De Albumes"
            print *, "4. Ver Capa especifica"
            print *, "5. Ver Imagen y Arbol De Capas"
            print *, "6. Regresar Al Menu Cliente"
            print *, "_______________________________________"
            print *, "Seleccione El Numero De Opcion:"
            print *, "_______________________________________"
            read(*,*) opcion_estructura
            select case(opcion_estructura)
                case('1')
                    call cliente_actual%arbol_abb_capa%graficar_arbol("ARBOL_ABB_CAPAS_"//dpi_global)
                case('2')
                    call cliente_actual%arbol_avl_imagen%graficar_arbol("ARBOL_AVL_IMAGENES_"//dpi_global)
                case('3')
                    call cliente_actual%lista_doble_album%graficar_album("LISTA_DOBLE_ALBUMES_"//dpi_global)
                case('4')
                    print *, "_______________________________________"
                    print *, "          Ver Capa Especifica          "
                    print *, "_______________________________________"
                    print *, "Escribe el ID de capa a vizualizar:"
                    read(*,*) numero_capa
                    existe_matriz = cliente_actual%arbol_abb_capa%valor_existe(numero_capa)
                    if (existe_matriz) then
                        allocate(matriz_auxiliar)
                        matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(numero_capa)
                        call matriz_auxiliar%graficar_matriz("VER_CAPA_ESPECIFICA_"//dpi_global)
                        deallocate(matriz_auxiliar)
                    else
                        print*, "Capa No Existe: ", int_to_str(numero_capa)
                    end if
                case('5')
                    print *, "_______________________________________"
                    print *, "      Ver Imagen y Arbol De Capas      "
                    print *, "_______________________________________"
                    print *, "Escribe el ID de imagen a vizualizar:"
                    read(*,*) numero_imagen
                    existe_imagen = cliente_actual%arbol_avl_imagen%valor_existe(numero_imagen)
                    if(existe_imagen)then
                        call cliente_actual%arbol_avl_imagen%graficar_arbol_imagen("ARBOL_IMAGEN_"//dpi_global,numero_imagen)
                    else
                        print*, "Imagen No Existe: ", int_to_str(numero_imagen)
                    end if
                case('6')
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine EstructurasCliente

    subroutine CrearImagenes()
        type(nodo_cliente), pointer :: cliente_actual
        type(matriz), pointer :: matriz_imagen, matriz_auxiliar
        type(arbol_abb_simple), pointer :: arbol_abb_simple
        character(len=100) :: opcion_imagen
        integer :: numero_nodo, tipo_recorrido, contador, id_capa, id_imagen, cantidad_capa,numero_capa
        logical :: existe_imagen, existe_matriz
        character(len=:), allocatable :: recorrido
        character(len=20), dimension(:), allocatable :: nodo
        do
            cliente_actual => lista_simple_cliente%obtener_cliente(dpi_global)
            print *, "_______________________________________"
            print *, "    Menu Generador Imagenes - PPS      "
            print *, "1. Por Recorrido Limitado"
            print *, "2. Por Arbol de Imagenes"
            print *, "3. Por Capas"
            print *, "4. Regresar Al Menu Cliente"
            print *, "_______________________________________"
            print *, "Seleccione El Numero De Opcion:"
            print *, "_______________________________________"
            read(*,*) opcion_imagen
            select case(opcion_imagen)
                !---------------------------------------------------------------------------!
                case('1')
                    print *, "_______________________________________"
                    print *, "           RECORRIDO LIMITADO          "
                    print *, "_______________________________________"
                    print *, "Escribe el numero de nodos:"
                    read(*,*) numero_nodo
                    print *, "Escribe el tipo de recorrido:"
                    print *,"1. Preorden"
                    print *,"2. Inorden" 
                    print *,"3. Postorden"
                    read(*,*) tipo_recorrido
                    !------------------------------------------!
                    if (tipo_recorrido==1) then
                        call cliente_actual%arbol_abb_capa%recorrido_preorden(numero_nodo, recorrido)
                        if(recorrido=="")then
                            print*, "No Existen Capas Cargadas."
                            exit
                        end if
                        allocate(matriz_imagen)
                        print*, "Preorder: "
                        call split(recorrido, '-', nodo)
                        do contador = 1, size(nodo)
                            allocate(matriz_auxiliar)
                            print *, trim(nodo(contador))
                            read(nodo(contador), *) id_capa
                            matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(id_capa)
                            call matriz_imagen%insertar_matriz(matriz_auxiliar)
                            deallocate(matriz_auxiliar)
                        end do
                        call matriz_imagen%graficar_matriz("IMAGEN_PREORDEN_"//dpi_global)
                        deallocate(matriz_imagen)
                    !------------------------------------------!
                    else if (tipo_recorrido==2) then
                        call cliente_actual%arbol_abb_capa%recorrido_inorden(numero_nodo, recorrido)
                        if(recorrido=="")then
                            print*, "No Existen Capas Cargadas."
                            exit
                        end if
                        allocate(matriz_imagen)
                        print*, "Inorder: "
                        call split(recorrido, '-', nodo)
                        do contador = 1, size(nodo)
                            allocate(matriz_auxiliar)
                            print *, trim(nodo(contador))
                            read(nodo(contador), *) id_capa
                            matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(id_capa)
                            call matriz_imagen%insertar_matriz(matriz_auxiliar)
                            deallocate(matriz_auxiliar)
                        end do
                        call matriz_imagen%graficar_matriz("IMAGEN_INORDEN_"//dpi_global)
                        deallocate(matriz_imagen)
                    !------------------------------------------!
                    else if (tipo_recorrido==3) then
                        call cliente_actual%arbol_abb_capa%recorrido_postorden(numero_nodo, recorrido)
                        if(recorrido=="")then
                            print*, "No Existen Capas Cargadas."
                            exit
                        end if
                        allocate(matriz_imagen)
                        print*, "Postorden: "
                        call split(recorrido, '-', nodo)
                        do contador = 1, size(nodo)
                            allocate(matriz_auxiliar)
                            print *, trim(nodo(contador))
                            read(nodo(contador), *) id_capa
                            matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(id_capa)
                            call matriz_imagen%insertar_matriz(matriz_auxiliar)
                            deallocate(matriz_auxiliar)
                        end do
                        call matriz_imagen%graficar_matriz("IMAGEN_POSTORDEN"//dpi_global)
                        deallocate(matriz_imagen)
                    end if
                    print *, "_______________________________________"
                !---------------------------------------------------------------------------!
                case('2')
                    print *, "_______________________________________"
                    print *, "          ARBOL DE IMAGENES            "
                    print *, "_______________________________________"
                    print *, "Escribe el ID de la imagen:"
                    read(*,*) id_imagen
                    allocate(arbol_abb_simple)
                    existe_imagen = cliente_actual%arbol_avl_imagen%valor_existe(id_imagen)
                    if (existe_imagen) then
                        arbol_abb_simple = cliente_actual%arbol_avl_imagen%buscar_valor(id_imagen)
                        call arbol_abb_simple%recorrido_amplitud(recorrido)
                        allocate(matriz_imagen)
                        print*, "Amplitud: "
                        call split(recorrido, '-', nodo)
                        do contador = 1, size(nodo)
                            print *, trim(nodo(contador))
                        end do
                        print*,"______"
                        do contador = 1, size(nodo)
                            read(nodo(contador), *) id_capa
                            existe_matriz = cliente_actual%arbol_abb_capa%valor_existe(id_capa)
                            if (existe_matriz) then
                                allocate(matriz_auxiliar)
                                matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(id_capa)
                                call matriz_imagen%insertar_matriz(matriz_auxiliar)
                                deallocate(matriz_auxiliar)
                                print*, "Capa: ",  trim(nodo(contador)), " Apilada."
                            else
                                print*, "Capa: ",  trim(nodo(contador)), " No Existe."
                            end if
                        end do
                        print*,"______"
                        call matriz_imagen%graficar_matriz("IMAGEN_AMPLITUD"//dpi_global)
                        deallocate(matriz_imagen)
                    else
                        print *, "Imagen No Existe: ", int_to_str(id_imagen)
                    end if
                    deallocate(arbol_abb_simple)
                    
                !---------------------------------------------------------------------------!
                case('3')
                    print *, "_______________________________________"
                    print *, "               POR CAPAS               "
                    print *, "_______________________________________"
                    print *, "Escribe el numero de capas a utilizar:"
                    read(*,*) cantidad_capa
                    allocate(matriz_imagen)
                    do contador = 1, cantidad_capa
                        print*,"______"
                        print *, "Ingrese El ID Capa Numero ", int_to_str(contador)
                        read(*,*) numero_capa
                        existe_matriz = cliente_actual%arbol_abb_capa%valor_existe(numero_capa)
                        if (existe_matriz) then
                            allocate(matriz_auxiliar)
                            matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(numero_capa)
                            call matriz_imagen%insertar_matriz(matriz_auxiliar)
                            deallocate(matriz_auxiliar)
                            print*, "Capa Apilada: ", int_to_str(numero_capa)
                        else
                            print*, "Capa No Existe: ", int_to_str(numero_capa)
                        end if
                    end do
                    print*,"--------"
                    call matriz_imagen%graficar_matriz("IMAGEN_POR_CAPA_"//dpi_global)
                    deallocate(matriz_imagen)
                case('4')
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine CrearImagenes

    subroutine MenuCargaJSON()
        character(len=100) :: opcion_carga
        do
            print *, "_______________________________________"
            print *, "        Menu de Carga Masiva - PPS     "
            print *, "1. Capas"
            print *, "2. Imagenes"
            print *, "3. Albumes"
            print *, "4. Regresar Al Menu Cliente"
            print *, "_______________________________________"
            print *, "Seleccione El Numero De Opcion:"
            print *, "_______________________________________"
            read(*,*) opcion_carga
            select case(opcion_carga)
                case('1')
                    call capaJSON()
                    print*,"Carga Completa"
                case('2')
                    call imgaenJSON()
                    print*,"Carga Completa"
                case('3')
                    call albumJSON()
                    print*,"Carga Completa"
                case('4')
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine MenuCargaJSON

    subroutine ReportesCliente()
        type(nodo_cliente), pointer :: cliente_actual
        character(len=100) :: opcion_reporte
        integer :: numero_nodo, contador
        character(len=:), allocatable :: recorrido
        character(len=20), dimension(:), allocatable :: nodo
        do
            cliente_actual => lista_simple_cliente%obtener_cliente(dpi_global)
            print *, "_______________________________________"
            print *, "      Menu de Carga Masiva - PPS       "
            print *, "DESEA IMPRIMIR LOS REPORTES DE: "
            print *, "  - Top 5 Imagenes Con Mas Numero De Capas"
            print *, "  - Todas Las Capas Que Son Hojas"
            print *, "  - Profundidad De Arbol De Capas"
            print *, "  - Listar Las Capas: Preorden, Inorden, Postorden"
            print *, "1. ACEPTAR  (GENERAR REPORTES)"
            print *, "2. CANCELAR (Regresar Al Menu)"
            print *, "_______________________________________"
            print *, "Seleccione El Numero De Opcion:"
            print *, "_______________________________________"
            read(*,*) opcion_reporte
            select case(opcion_reporte)
                case('1')
                    print *, "_______________________________________"
                    print *, "TOP5 IMAGENES CON MAYOR NO. DE CAPAS"
                    print *, "_______________________________________"
                    call cliente_actual%arbol_avl_imagen%top_5_imagenes()
                    print *, "_______________________________________"
                    print *, "CAPAS QUE SON HOJAS"
                    print *, "_______________________________________"
                    call cliente_actual%arbol_abb_capa%imprimir_hoja()
                    print *, "_______________________________________"
                    print *, "PROFUNDIDAD DEL ARBOL"
                    print *, "_______________________________________"
                    call cliente_actual%arbol_abb_capa%profundidad_arbol()
                    print *, "_______________________________________"
                    print *, "LISTAR LAS CAPAS"
                    print *, "_______________________________________"
                    call cliente_actual%arbol_abb_capa%recorrido_preorden(numero_nodo, recorrido)
                    print*, "Recorrido Preorden:"
                    call split(recorrido, '-', nodo)
                    do contador = 1, size(nodo)
                        print *, trim(nodo(contador))
                    end do
                    print *, "---------------------------"
                    call cliente_actual%arbol_abb_capa%recorrido_inorden(numero_nodo, recorrido)
                    print*, "Recorrido Inorder:"
                    call split(recorrido, '-', nodo)
                    do contador = 1, size(nodo)
                        print *, trim(nodo(contador))
                    end do
                    print *, "---------------------------"
                    call cliente_actual%arbol_abb_capa%recorrido_postorden(numero_nodo, recorrido)
                    print*, "Recorrido Postorden:"
                    call split(recorrido, '-', nodo)
                    do contador = 1, size(nodo)
                        print *, trim(nodo(contador))
                    end do
                case('2')
                    exit
                case default
                    print *, "OPCION INVALIDA, INTENTE DE NUEVO."
            end select
        end do
    end subroutine ReportesCliente

    subroutine abcIMAGEN()
        type(nodo_cliente), pointer :: cliente_actual
        type(arbol_abb_simple), pointer :: arbol_abb_capa_simple
        character(len=100) :: opcion_abc
        integer :: numero_imagen, numero_capa, cantidad_capa, contador
        logical :: existe_matriz, existe_imagen
        character(len=20) :: imagen
        do
            cliente_actual => lista_simple_cliente%obtener_cliente(dpi_global)
            print *, "_______________________________________"
            print *, "         Menu ABC Imagen - PPS         "
            print *, "1. Registrar Imagen"
            print *, "2. Eliminar Imagen"
            print *, "3. Regresar Al Menu Cliente"
            print *, "_______________________________________"
            print *, "Seleccione El Numero De Opcion:"
            print *, "_______________________________________"
            read(*,*) opcion_abc
            select case(opcion_abc)
                case('1')
                    print *, "_______________________________________"
                    print *, "         REGISTRAR UNA IMAGEN          "
                    print *, "_______________________________________"
                    print *, "Escribe el ID de imagen a registrar:"
                    read(*,*) numero_imagen
                    existe_imagen = cliente_actual%arbol_avl_imagen%valor_existe(numero_imagen)
                    if(existe_imagen)then
                        print*, "Imagen Ya Existe En El Sistema: ", int_to_str(numero_imagen)
                        exit
                    else
                        allocate(arbol_abb_capa_simple)
                        print *, "_______________________________________"
                        print *, "Escribe el numero de capas a Ingresar:"
                        read(*,*) cantidad_capa
                        do contador = 1, cantidad_capa
                            print*,"______"
                            print *, "Ingrese El ID Capa Numero ", int_to_str(contador)
                            read(*,*) numero_capa
                            existe_matriz = cliente_actual%arbol_abb_capa%valor_existe(numero_capa)
                            if (existe_matriz) then
                                call arbol_abb_capa_simple%insertar(numero_capa)
                                print*, "Capa Insertada: ", int_to_str(numero_capa)
                            else
                                print*, "Capa No Existe: ", int_to_str(numero_capa)
                            end if
                        end do
                        call cliente_actual%arbol_avl_imagen%insertar_nodo(numero_imagen, arbol_abb_capa_simple)
                        deallocate(arbol_abb_capa_simple)
                        print*,"Imagen: ",numero_imagen," Registrada Correctamente."
                    end if
                case('2')
                    print *, "_______________________________________"
                    print *, "ELIMINAR UNA IMAGEN"
                    print *, "_______________________________________"
                    print *, "Escribe el ID de imagen a eliminar:"
                    read(*,*) numero_imagen
                    existe_imagen = cliente_actual%arbol_avl_imagen%valor_existe(numero_imagen)
                    if(existe_imagen)then
                        write(imagen, '(I0)') numero_imagen
                        call cliente_actual%arbol_avl_imagen%eliminar_nodo(numero_imagen)
                        call cliente_actual%lista_doble_album%eliminar_imagen(imagen)
                    else
                        print*, "Imagen No Existe: ", int_to_str(numero_imagen)
                    end if
                case('3')
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine abcIMAGEN

    subroutine capaJSON()
        character(len=30) :: colorD
        type(matriz), pointer :: matriz_dispersa_capa
        type(nodo_cliente), pointer :: cliente_actual
        print *, "_______________________________________"
        print *, "          CARGA MASIVA CAPAS           "
        print *, "_______________________________________"
        print *, "Ubicacion del Json con capas:"
        print *, "_______________________________________"
        read(*,*) documento_capa
        print *, "_______________________________________"
        call json%initialize()
        call json%load(filename=documento_capa)
        call json%info('',n_children=size_capa)
        call json%get_core(jsonc)
        call json%get('', listaPunteroCapa, capa_encontrada)
        do contador_capa = 1, size_capa
            allocate(matriz_dispersa_capa)
            call jsonc%get_child(listaPunteroCapa, contador_capa, punteroCapa, capa_encontrada)
            call jsonc%get_child(punteroCapa, 'id_capa', atributoPunteroCapa, capa_encontrada)
            call jsonc%get(atributoPunteroCapa, id_capa)
            call jsonc%get_child(punteroCapa, 'pixeles', atributoPunteroCapa, capa_encontrada)
            call jsonc%info(atributoPunteroCapa,n_children=size_pixel)
            read(id_capa, *) id_capa_int
            do contador_pixel = 1, size_pixel
                call jsonc%get_child(atributoPunteroCapa, contador_pixel, punteroPixel, capa_encontrada)
                call jsonc%get_child(punteroPixel, 'fila', atributoPixel, capa_encontrada)
                call jsonc%get(atributoPixel, fila)
                call jsonc%get_child(punteroPixel, 'columna', atributoPixel, capa_encontrada)
                call jsonc%get(atributoPixel, columna)
                call jsonc%get_child(punteroPixel, 'color', atributoPixel, capa_encontrada)
                call jsonc%get(atributoPixel, color)
                read(fila, *) fila_int
                read(columna, *) columna_int
                colorD=color
                call matriz_dispersa_capa%insertar_nodo(columna_int, fila_int, colorD)
            end do
            !----------
            cliente_actual => lista_simple_cliente%cabeza
            do while (associated(cliente_actual))
                if (trim(cliente_actual%dpi) == dpi_global) then
                    call cliente_actual%arbol_abb_capa%insertar_nodo(id_capa_int, matriz_dispersa_capa)
                    exit
                end if
                cliente_actual => cliente_actual%siguiente
            end do
            !----------
            !call arbol_abb_capa%insertar_nodo(id_capa_int, matriz_dispersa_capa)
            deallocate(matriz_dispersa_capa)
        end do
        call json%destroy()
    end subroutine capaJSON

    subroutine imgaenJSON()
        type(arbol_abb_simple), pointer :: arbol_abb_capa_simple
        type(nodo_cliente), pointer :: cliente_actual
        print *, "_______________________________________"
        print *, "          CARGA MASIVA IMAGEN          "
        print *, "_______________________________________"
        print *, "Ubicacion del Json con imagen:"
        print *, "_______________________________________"
        read(*,*) documento_imagen
        print *, "_______________________________________"
        call json%initialize()
        call json%load(filename=documento_imagen)
        call json%info('',n_children=size_imagen)
        call json%get_core(jsonc)
        call json%get('', listaPunteroImagen, imagen_encontrada)
        do contador_imagen = 1, size_imagen
            call jsonc%get_child(listaPunteroImagen, contador_imagen, punteroImagen, imagen_encontrada)
            call jsonc%get_child(punteroImagen, 'id', atributoPunteroImagen, imagen_encontrada)
            call jsonc%get(atributoPunteroImagen, id_imagen)
            call jsonc%get_child(punteroImagen, 'capas', atributoPunteroImagen, imagen_encontrada)
            call jsonc%info(atributoPunteroImagen,n_children=size_capa)
            allocate(arbol_abb_capa_simple)
            do contador_capa = 1, size_capa
                call jsonc%get_child(atributoPunteroImagen, contador_capa, punteroCapa, imagen_encontrada)
                call jsonc%get(punteroCapa, id_capas)
                call arbol_abb_capa_simple%insertar(id_capas)
            end do
            !----------
            cliente_actual => lista_simple_cliente%cabeza
            do while (associated(cliente_actual))
                if (trim(cliente_actual%dpi) == dpi_global) then
                    call cliente_actual%arbol_avl_imagen%insertar_nodo(id_imagen, arbol_abb_capa_simple)
                    exit
                end if
                cliente_actual => cliente_actual%siguiente
            end do
            !----------
            !call arbol_avl_imagen%insertar_nodo(id_imagen, arbol_abb_capa_simple)
            deallocate(arbol_abb_capa_simple)
        end do
        call json%destroy()
    end subroutine imgaenJSON

    subroutine albumJSON()
        type(lista_imagen), pointer :: lista_imagen_album
        type(nodo_cliente), pointer :: cliente_actual
        print *, "_______________________________________"
        print *, "          CARGA MASIVA ALBUM           "
        print *, "_______________________________________"
        print *, "Ubicacion del Json con album:"
        print *, "_______________________________________"
        read(*,*) documento_album
        print *, "_______________________________________"
        call json%initialize()
        call json%load(filename=documento_album)
        call json%info('',n_children=size_album)
        call json%get_core(jsonc)
        call json%get('', listaPunteroAlbum, album_encontrado)
        do contador_album = 1, size_album
            call jsonc%get_child(listaPunteroAlbum, contador_Album, punteroAlbum, album_encontrado)
            call jsonc%get_child(punteroAlbum, 'nombre_album', atributoPunteroAlbum, album_encontrado)
            call jsonc%get(atributoPunteroAlbum, nombre_album)
            call jsonc%get_child(punteroAlbum, 'imgs', atributoPunteroAlbum, album_encontrado)
            call jsonc%info(atributoPunteroAlbum,n_children=imgs_size)
            allocate(lista_imagen_album)
            do contador_a = 1, imgs_size
                call jsonc%get_child(atributoPunteroAlbum, contador_a, punteroAlbum, album_encontrado)
                call jsonc%get(punteroAlbum, imgs)
                call lista_imagen_album%insertar_imagen(trim(imgs))
            end do
            !----------
            cliente_actual => lista_simple_cliente%cabeza
            do while (associated(cliente_actual))
                if (trim(cliente_actual%dpi) == dpi_global) then
                    call cliente_actual%lista_doble_album%insertar_album(nombre_album, lista_imagen_album)
                    exit
                end if
                cliente_actual => cliente_actual%siguiente
            end do
            !----------
            !call lista_doble_album%insertar_album(nombre_album, lista_imagen_album)
            deallocate(lista_imagen_album)
        end do
        call json%destroy()
    end subroutine albumJSON

    subroutine clientesJSON()
        print *, "_______________________________________"
        print *, "          CARGA MASIVA CLIENTE         "
        print *, "_______________________________________"
        print *, "Ubicacion del Json con clientes: "
        print *, "_______________________________________"
        read(*,*) documento_cliente
        print *, "_______________________________________"
        call json%initialize()
        call json%load(filename=documento_cliente)
        call json%info('',n_children=size_cliente)
        call json%get_core(jsonc)
        call json%get('', listaPunteroCliente, cliente_encontrado)
        do contador_cliente = 1, size_cliente
            call jsonc%get_child(listaPunteroCliente, contador_cliente, punteroCiente, cliente_encontrado)
            call jsonc%get_child(punteroCiente, 'dpi', atributoPunteroCliente, cliente_encontrado)
            call jsonc%get(atributoPunteroCliente, dpi_cliente)
            call jsonc%get_child(punteroCiente, 'nombre_cliente', atributoPunteroCliente, cliente_encontrado)
            call jsonc%get(atributoPunteroCliente, nombre_cliente)
            call jsonc%get_child(punteroCiente, 'password', atributoPunteroCliente, cliente_encontrado)
            call jsonc%get(atributoPunteroCliente, contrasena_cliente)
            call lista_simple_cliente%insertar_cliente(dpi_cliente, nombre_cliente, contrasena_cliente)
        end do
        call json%destroy()
    end subroutine clientesJSON
end program main