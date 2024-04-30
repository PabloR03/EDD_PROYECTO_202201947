program main
    use json_module
    use ArbolAvl
    use m_TablaHash
    !LECTURA JSON
    type(json_file) :: json
    type(json_core) :: jsonc

    !LECTURA SUCURSAL
    type(json_value), pointer :: lista_puntero_sucursal, puntero_sucursal, atributo_puntero_sucursal
    logical :: sucursal_encontrada
    integer :: size_sucursal, contador_surcusal
    character(:), allocatable :: id_sucursal, direccion_sucursal, departamento_sucursal, password_sucursal

    !LECTURA TECNICO
    type(json_value), pointer :: lista_puntero_tecnico, puntero_tecnico, atributo_puntero_tecnico
    logical :: tecnico_encontrado
    integer :: size_tecnico, contador_tecnico
    character(:), allocatable :: dpi_tec, nom_tec, ape_tec, gen_tec, dir_tec, tel_tec
    
    !LECTURA RUTAS
    type(json_value), pointer :: lista_puntero_ruta, puntero_ruta, atributo_puntero_ruta
    type(json_value), pointer :: puntero_aux, atributo_puntero_aux
    logical :: grafo_encontrado
    integer :: size_grafo, contador_grafo
    integer :: size_ruta, contador_ruta
    character(:), allocatable :: s1, s2, distancia, imp_mantenimiento

    !ESTRUCTURAS
    type(arbol_avl) :: arbol_avl_sucursal  

    !VARIABLES GLOBALES
    integer :: opcion_principal, id_s_int
    integer(8) ::  dpi_t_int, telefono_t_int
    character(len=100) :: usuario
    character(len=100) :: contrasena
    character(len=100) :: documento_sucursal, documento_grafo, documento_tecnico

    do
        call mostrar_menu()
        read(*,*) opcion_principal
        select case(opcion_principal)
            case(1)
                call iniciar_sesion()
            case(2)
                exit
            case default
              print *, "OPCION INVALIDA"
        end select
    end do

contains

subroutine mostrar_menu()
      print *, "_______________________________________"
      print *, "         Menu Principal - PPS          "
      print *, "1. Iniciar Sesion"
      print *, "2. Salir"
      print *, "_______________________________________"
      print *, "Seleccione El Numero De Opcion:"
      print *, "_______________________________________"
end subroutine mostrar_menu

subroutine iniciar_sesion()
        print *, "_______________________________________"
        print *, "            INICIAR SESION             "   
        print *, "_______________________________________"
        print *, "Ingrese su nombre de usuario:"
        read(*,*) usuario
        print *, "Ingrese su contrasena:"
        read(*,*) contrasena
        if (usuario == "EDD1S2024" .and. contrasena == "ProyectoFase3") then
            print *, "_______________________________________"
            print *, "         BIENVENIDO ADMINISTRADOR      "
            call menu_administrador()
        else
            print *, "Datos Incorrectos. Vuelva a intentarlo."
        end if
end subroutine iniciar_sesion

subroutine menu_administrador()
    integer :: opcion_admin
    do
        print *, "_______________________________________"
        print *, "       Menu Administrador - PPS        "
        print *, "1. Carga Masiva Archivos"
        print *, "2. Manejo Sucursales"
        print *, "3. Reportes Graficos"
        print *, "4. Reportes Impresos"
        print *, "5. Cerrar Sesion"
        print *, "_______________________________________"
        print *, "      Seleccione El Numero De Opcion:  "
        print *, "_______________________________________"
        read(*,*) opcion_admin
        select case(opcion_admin)
            case(1)
                call carga_masiva()
            case(2)
                call manejo_sucursal()
            case(3)
                call reportes_graficos()
            case(4)
                print *, ""
            case(5)
                exit
            case default
                print *, "OPCION INVALIDA"
        end select
    end do
end subroutine menu_administrador

subroutine carga_masiva()
    integer :: opcion_carga
    do
        print *, "_______________________________________"
        print *, "      Menu de Carga Masiva - PPS       "
        print *, "1. Sucursales"
        print *, "2. Rutas"
        print *, "3. Regresar Al Menu Principal"
        print *, "_______________________________________"
        print *, "Seleccione El Numero De Opcion:"
        print *, "_______________________________________"
        read(*,*) opcion_carga
        select case(opcion_carga)
            case(1)
                call carga_masiva_sucursal()
                print*,"Carga De Sucursales Correctamente."
            case(2)
                call carga_masiva_ruta()
                print*,"Carga De Rutas Correctamente."
            case(3)
                exit
            case default
                print *, "Opcion Invalida, Vuelva a intentarlo."
        end select
    end do
end subroutine carga_masiva

subroutine manejo_sucursal()
    integer :: opcion_carga, id_sucursal
    character(len=100) :: contrasena
    logical :: existe_matriz
    print *, "_______________________________________"
    print *, "        CREDENCIALES SUCURSALES        "
    print *, "_______________________________________"
    print *, "Escribe el ID de la sucursal:"
    read(*,*) id_sucursal
    print *, "Escribe la contrasena de la sucursal:"
    read(*,*) contrasena
    existe_matriz = arbol_avl_sucursal%valor_existe(id_sucursal, contrasena)
    if(existe_matriz)then
        do
            print *, "_______________________________________"
            print *, "Bienvenido Sucursal: ", id_sucursal
            print *, "_______________________________________"
            print *, "     Menu de Manejo Sucursal - PPS     "
            print *, "1. Carga De Tecnicos"
            print *, "2. Generar Recorrido Mas Optimo"
            print *, "3. Informacion Tecnico En Especifico"
            print *, "4. Listar Tecnicos"
            print *, "5. Regresar Al Menu Principal"
            print *, "_______________________________________"
            print *, "Seleccione El Numero De Opcion:"
            print *, "_______________________________________"
            read(*,*) opcion_carga
            select case(opcion_carga)
                case(1)
                    call carga_masiva_tecnico(id_sucursal, contrasena)
                case(2)
                    print*,"Recorrido Más Optimo"
                case(3)
                    call informacion_tecnico_especifico(id_sucursal, contrasena)
                case(4)
                    call listar_informacion_tecnico(id_sucursal, contrasena)
                case(5)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    else 
        print*, "Credenciales De Sucursal Incorrectas."
    end if
end subroutine manejo_sucursal

subroutine reportes_graficos()
    integer :: opcion_carga
    do
        print *, "_______________________________________"
        print *, "Menu de Reportes Graficos - PPS"
        print *, "1. Grafo De Sucursales"
        print *, "2. Rutas"
        print *, "3. Regresar Al Menu Principal"
        print *, "_______________________________________"
        print *, "Seleccione El Numero De Opcion:"
        print *, "_______________________________________"
        read(*,*) opcion_carga
        select case(opcion_carga)
            case(1)
                call arbol_avl_sucursal%graficar_arbol("A_SUCURSALES")
            case(2)
                print*,""
            case(3)
                exit
            case default
                print *, "OPCION INVALIDA"
        end select
    end do
end subroutine reportes_graficos

subroutine informacion_tecnico_especifico(id_sucursal, contrasena)
    type(nodo_avl), pointer :: sucursal_actual
    integer(8) :: dpi_int
    integer, intent(in) :: id_sucursal
    character(len=*), intent(in) :: contrasena
    sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
    print *, "_______________________________________"
    print *, "INFORMACION TECINCO ESPECIFICO"
    print *, "_______________________________________"
    print *, "Ingrese el DPI del tecnico:"
    print *, "_______________________________________"
    read(*,*) dpi_int
    call sucursal_actual%tabla%imprimir(dpi_int)
end subroutine informacion_tecnico_especifico

subroutine listar_informacion_tecnico(id_sucursal, contrasena)
    type(nodo_avl), pointer :: sucursal_actual
    integer, intent(in) :: id_sucursal
    character(len=*), intent(in) :: contrasena
    sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
    print *, "_______________________________________"
    print *, "      LISTAR INFORMACION TECNICOS      "
    print *, "_______________________________________"
    call sucursal_actual%tabla%listar_tecnico()
end subroutine listar_informacion_tecnico

subroutine carga_masiva_sucursal()
    print *, "____________________________________________"
    print *, "CARGA MASIVA SUCURSALES"
    print *, "____________________________________________"
    print *, "Ingrese el nombre del documento de sucursal:"
    print *, "____________________________________________"
    read(*,*) documento_sucursal
    print *, "____________________________________________"
    call json%initialize()
    call json%load(filename=documento_sucursal)
    call json%info('',n_children=size_sucursal)
    call json%get_core(jsonc)
    call json%get('', lista_puntero_sucursal, sucursal_encontrada)
    do contador_surcusal = 1, size_sucursal
        call jsonc%get_child(lista_puntero_sucursal, contador_surcusal, puntero_sucursal, sucursal_encontrada)
        call jsonc%get_child(puntero_sucursal, 'id', atributo_puntero_sucursal, sucursal_encontrada)
        call jsonc%get(atributo_puntero_sucursal, id_sucursal)
        call jsonc%get_child(puntero_sucursal, 'departamento', atributo_puntero_sucursal, sucursal_encontrada)
        call jsonc%get(atributo_puntero_sucursal, departamento_sucursal)
        call jsonc%get_child(puntero_sucursal, 'direccion', atributo_puntero_sucursal, sucursal_encontrada)
        call jsonc%get(atributo_puntero_sucursal, direccion_sucursal)
        call jsonc%get_child(puntero_sucursal, 'password', atributo_puntero_sucursal, sucursal_encontrada)
        call jsonc%get(atributo_puntero_sucursal, password_sucursal)
        read(id_sucursal, *) id_s_int
        print *, "Procesando Sucursal: ", id_sucursal
        call arbol_avl_sucursal%insertar_nodo(id_s_int, departamento_sucursal, direccion_sucursal, password_sucursal)
    end do
    call json%destroy()
end subroutine carga_masiva_sucursal

subroutine carga_masiva_tecnico(id_sucursal, contrasena)
    type(nodo_avl), pointer :: sucursal_actual
    integer, intent(in) :: id_sucursal
    integer :: i
    character(len=*), intent(in) :: contrasena
    sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
    print *, "________________________________________"
    print *, "CARGA MASIVA TECNICOS"
    print *, "________________________________________"
    print *, "Ingrese el nombre del documento de tecnico:"
    print *, "________________________________________"
    read(*,*) documento_grafo
    print *, "________________________________________"
    call json%initialize()
    call json%load(filename=documento_grafo)
    call json%info('',n_children=size_tecnico)
    call json%get_core(jsonc)
    call json%get('', lista_puntero_tecnico, tecnico_encontrado)
    do contador_tecnico = 1, size_tecnico
        call jsonc%get_child(lista_puntero_tecnico, contador_tecnico, puntero_tecnico, tecnico_encontrado)
        call jsonc%get_child(puntero_tecnico, 'dpi', atributo_puntero_tecnico, tecnico_encontrado)
        call jsonc%get(atributo_puntero_tecnico, dpi_tec)
        call jsonc%get_child(puntero_tecnico, 'nombre', atributo_puntero_tecnico, tecnico_encontrado)
        call jsonc%get(atributo_puntero_tecnico, nom_tec)
        call jsonc%get_child(puntero_tecnico, 'apellido', atributo_puntero_tecnico, tecnico_encontrado)
        call jsonc%get(atributo_puntero_tecnico, ape_tec)
        call jsonc%get_child(puntero_tecnico, 'genero', atributo_puntero_tecnico, tecnico_encontrado)
        call jsonc%get(atributo_puntero_tecnico, gen_tec)
        call jsonc%get_child(puntero_tecnico, 'direccion', atributo_puntero_tecnico, tecnico_encontrado)
        call jsonc%get(atributo_puntero_tecnico, dir_tec)
        call jsonc%get_child(puntero_tecnico, 'telefono', atributo_puntero_tecnico, tecnico_encontrado)
        call jsonc%get(atributo_puntero_tecnico, tel_tec)
        print *, "Procesando Tecnico: ", dpi_tec
        read(dpi_tec, *) dpi_t_int
        read(tel_tec, *) telefono_t_int
        call sucursal_actual%tabla%insertar(dpi_t_int, nom_tec, ape_tec, dir_tec, telefono_t_int, gen_tec)
    end do
    print*,"Tecnicos Cargados Correctamente. Sucursal: ", id_sucursal
    call json%destroy()
end subroutine carga_masiva_tecnico

subroutine carga_masiva_ruta()
    print *, "________________________________________"
    print *, "CARGA MASIVA RUTAS"
    print *, "________________________________________"
    print *, "Ingrese el nombre del documento de ruta:"
    print *, "________________________________________"
    read(*,*) documento_grafo
    print *, "________________________________________"
    call json%initialize()
    call json%load(filename=documento_grafo)
    call json%info('',n_children=size_grafo)
    call json%get_core(jsonc)
    call json%get('', lista_puntero_ruta, grafo_encontrado)
    do contador_grafo = 1, size_grafo
        call jsonc%get_child(lista_puntero_ruta, contador_grafo, puntero_ruta, grafo_encontrado)
        call jsonc%get_child(puntero_ruta, 'grafo', atributo_puntero_ruta, grafo_encontrado)
        call jsonc%info(atributo_puntero_ruta, n_children=size_ruta)
        do contador_ruta = 1, size_ruta
            call jsonc%get_child(atributo_puntero_ruta, contador_ruta, puntero_aux, grafo_encontrado)
            call jsonc%get_child(puntero_aux, 's1', atributo_puntero_aux, grafo_encontrado)
            call jsonc%get(atributo_puntero_aux, s1)
            call jsonc%get_child(puntero_aux, 's2', atributo_puntero_aux, grafo_encontrado)
            call jsonc%get(atributo_puntero_aux, s2)
            call jsonc%get_child(puntero_aux, 'distancia', atributo_puntero_aux, grafo_encontrado)
            call jsonc%get(atributo_puntero_aux, distancia)
            call jsonc%get_child(puntero_aux, 'imp_mantenimiento', atributo_puntero_aux, grafo_encontrado)
            call jsonc%get(atributo_puntero_aux, imp_mantenimiento)
            print *, "________________________________________"
            print *, "s1: ", s1
            print *, "s1: ", s2
            print *, "distancia: ", distancia
            print *, "imp_mantenimiento: ", imp_mantenimiento
        end do
    end do
    call json%destroy()
end subroutine carga_masiva_ruta

end program main