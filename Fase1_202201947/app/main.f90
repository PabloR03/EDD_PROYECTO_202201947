program FASE1_202201947
    use json_module
    use mod_queue_client
    use mod_list_windows
    use mod_client_served
    implicit none
    !READ JSON WITH FPM
    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: lista_puntero, puntero, atributo_puntero
    logical :: encontrado
    logical :: window_available
    integer, parameter :: numero_info = 4
    integer :: ejecuccion_de_paso = 0, spaces = 1
    character(len=20) :: info_cliente(numero_info)
    !LOCAL VARIABLES IN MAIN
    character(:), allocatable :: id, nombre, img_p, img_g
    character(:), allocatable :: ruta
    integer :: clientes_json, cont1, cont2, cont3, cont4
    integer :: choise, choise_param, amount_window, counter_window, opRep
    character(len=20) :: id_cliente_search
    !QUEUE
    type(Queue_Client) :: queue_client_reception
    !LIST WINDOWS 
    type(List_Windows) :: list_window_reception

    do
        call Menu_Principal()
        read(*,*) choise
        select case(choise)
            case(1)
                do spaces = 1, 10
                print *, " "
                end do
                call Option1()
            case(2)
                do spaces = 1, 10
                    print *, " "
                    end do
                call Option2()
            case(3)
                do spaces = 1, 10
                    print *, " "
                    end do
                call Option3()
            case(4)
                do spaces = 1, 10
                    print *, " "
                    end do
                call Option4()
            case(5)
                do spaces = 1, 10
                    print *, " "
                    end do
                call Option5()
            case(6)
                do spaces = 1, 10
                    print *, " "
                    end do
                print *, "********************"
                print *, "* Hasta La Proxima *"
                print *, "********************"
                exit
            case default
                print *, "Seleccion incorrecta, intente de nuevo"
        end select
    end do

    contains
    subroutine Menu_Principal()
        print *, " _________________________________________ "
        print *, "|             Menu Principal              |"
        print *, "| 1. Carga de Archivos                    |"
        print *, "| 2. Ejecutar Paso                        |"
        print *, "| 3. Estado En Memoria De Las Estructuras |"
        print *, "| 4. Reportes                             |"
        print *, "| 5. Acerca De                            |"
        print *, "| 6. Salir                                |"
        print *, "|_________________________________________|"
        print *, "Seleccione El Numero De Opcion:"
        print *, "___________________________________________"
    end subroutine

    subroutine Option1()
        print *, "_____________________________________"
        print *, "| Carga de Archivos                  |"
        print *, "|0. Carga Masiva De Clientes         |"
        print *, "|1. Cantidad De Ventanillas          |"
        print *, "_____________________________________"
        read(*,*) choise_param
        select case(choise_param)
            case(0)
                call Carga_Masiva_Clientes()
            case(1)
                call Cantidad_Ventanillas()
        end select
    end subroutine

    subroutine Option2()
        ejecuccion_de_paso = ejecuccion_de_paso + 1
        print *, "___________________________________________"
        print *, "EJECUTAR PASO"
        print *, "Cantidad de pasos: ", ejecuccion_de_paso   
        print *, "___________________________________________"
        call list_window_reception%atender_cliente_ventanilla()
        call list_window_reception%imprimir_imagenes_cliente()
        window_available = list_window_reception%ventanilla_disponible()
        if (window_available) then
            call queue_client_reception%pop_client(info_cliente)
            call list_window_reception%asignar_ventanilla(info_cliente(1), info_cliente(2), info_cliente(3), info_cliente(4))
        end if
        
    end subroutine

    subroutine Option3()
        print *, " ___________________________________________"
        print *, "|  Estado de Memoria de las Estructuras     |"
        print *, " ___________________________________________"
        print *, "___________________________________________"
        call list_window_reception%print_ventanilla()
        print *, "|                                     |"
        call list_window_reception%cola_imagen_pequena%print_img_pequena()
        print *, "|                                     |"
        call list_window_reception%cola_imagen_grande%print_img_grande()
        print *, "|                                     |"
        call list_window_reception%lista_clientes_esperando%print_lista()
        print *, "|                                     |"
        call list_window_reception%lista_clientes_esperando%graphic_waiting_client("CLIENTES_EN_ESPERA")
        print *, "|                                     |"
        call list_window_reception%lista_clientes_atendido%print_cliente_atendido()
        print *, "|                                     |"
        call queue_client_reception%print_client()
        print *, "___________________________________________"
        call  list_window_reception%print_ventanilla()
    end subroutine

    subroutine Option4()
        print *, "___________________________________________"
        print *, "                Reportes"
        print *, "Reportes descargados..." 

        print *, "1) 5 CLIENTES CON MAS IMAGENES GRANDES"
        print *, "2) 5 CLIENTE CON MAS IMAGENES PEQUENAS"
        print *, "3) CLIENTE CON MAS PASOS"
        print *, "4) BUSCAR CLIENTE POR ID"
        read(*,*) opRep

        if (opRep == 1) then
            print *, "5 CLIENTES CON MAS IMAGENES GRANDES"
            call queue_client_reception%top5_imgb()
            print *, "___________________________________________"
        else if (opRep == 2) then
            print *, "5 CLIENTE CON MAS IMAGENES PEQUENAS"
            call queue_client_reception%top5_imgs()
            print *, "___________________________________________"
        else if (opRep == 3) then
            print *, "CLIENTE CON MAS PASOS"
            call list_window_reception%lista_clientes_atendido%more_steps()
            print *, "___________________________________________"
        else if (opRep == 4) then
            print *, "BUSCANDO INFORMACION DE CLIENTE (ID)"
            print *, "Ingrese el ID del cliente a buscar:"
            read(*,*) id_cliente_search
            call list_window_reception%lista_clientes_atendido%report_by_id(id_cliente_search)
            print *, "___________________________________________"
        else
            print *, "Opcion Incorrecta"
        end if
        print *, "___________________________________________"
    end subroutine

    subroutine Option5()
        print *, " __________________________________________"
        print *, "|About:                                    |"
        Print *, "|USAC                                      |"
        Print *, "|EDD - Estructura De Datos - B             |"
        Print *, "|202201947                                 |"
        Print *, "|Pablo Andres Rodriguez Lima               |"
        Print *, "|Primer Semestre 2024 (5to Semestre)       |"
        print *, "|__________________________________________|"
    end subroutine

    subroutine Carga_Masiva_Clientes()
        print *, "___________________________________________"
        print *, "Leyendo Listado de Clientes"
        print *, "___________________________________________"
        call json%initialize()
        call json%load(filename='Listado_Clientes.json')
        call json%info('',n_children=clientes_json)
        call json%get_core(jsonc)
        call json%get('', lista_puntero, encontrado)
        do cont2 = 1, clientes_json
            call jsonc%get_child(lista_puntero, cont2, puntero, encontrado)
            call jsonc%get_child(puntero, 'id', atributo_puntero, encontrado)
            call jsonc%get(atributo_puntero, id)
            call jsonc%get_child(puntero, 'nombre', atributo_puntero, encontrado)
            call jsonc%get(atributo_puntero, nombre)
            call jsonc%get_child(puntero, 'img_p', atributo_puntero, encontrado) 
            call jsonc%get(atributo_puntero, img_p)
            call jsonc%get_child(puntero, 'img_g', atributo_puntero, encontrado) 
            call jsonc%get(atributo_puntero, img_g)
            call queue_client_reception%push_client(trim(id), trim(nombre), trim(img_g), trim(img_p))
        end do
        call json%destroy()
        print *, "Clientes En Cola."
    end subroutine

    subroutine Cantidad_Ventanillas()
        print *, "___________________________________________"
        print *, "Cantidad De Ventanillas"
        print *, "___________________________________________"
        print *, "Cuantas Ventanillas Atenderan:"
        read(*,*) amount_window
        print *, "___________________________________________"
        counter_window = 1
        do cont3 = 1, amount_window
            call  list_window_reception%agregar_ventanilla(counter_window, "VACIO", "VACIO", "0", "0")
            counter_window=counter_window + 1
        end do
        call  list_window_reception%print_ventanilla()
    end subroutine

end program FASE1_202201947