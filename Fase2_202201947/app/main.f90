program main
    use json_module
    use iso_fortran_env, only:
    use matriz_m
    use abb_m
implicit none
!cosas de prueba
integer :: values(17) = [20, 8, 3, 1, 0, 15, 30, 48, 26, 10, 7, 5, 60, 19, 11, 21, 3]
! Declaración de variables
integer :: choice
integer :: i, del
character(len=20) :: usuario, contrasena
!Para el login 
logical :: login_successfulC, login_successfulA
!Para la lectura de los Json
type(json_file) :: json
type(json_core) :: jsonc
type(json_value), pointer :: listPointer, animalPointer, attributePointer
!Para la lectura de los Json de albumes
logical :: found
integer :: size, iC
!Para la lectura de los Json de capas
character(:), allocatable :: password,nombreCliente,dpi
type(json_file) :: jsonAlbumes
type(json_core) :: jsoncAlbumes
type(json_value), pointer :: listPointerAlbumes, todoAlbumes, attributePointerAlbumes,imgsAlbumes
logical :: foundAlbumes
integer :: sizeAlbumes, iCAlbumes
character(:), allocatable :: nombreAlbum,imgs,numImagenes
character(:), allocatable :: idCapa,fila,columna,color
type(json_value), pointer :: todoCapas,attributePointerCapas
character(len=1000) :: direccion
character(len=7):: colorcito 
!Importacionnes
!Matriz Dispersa
type(mamatrix) :: matriz
type(abb) :: tree

! Menú principal
login_successfulA = .false.
login_successfulC = .false.
call login()



contains


subroutine login()
    do while (.not. login_successfulC .and. .not. login_successfulA)
        print *, "Ingrese su Usuario: "
        read(*,*) usuario
        print *, "Ingrese su Contrasena: "
        read(*,*) contrasena
        if (usuario == "admin" .and. contrasena == "12345") then
            login_successfulA = .true.
        else if (usuario == "cliente" .and. contrasena == "12345") then
            login_successfulC = .true.
        else
            print *, "Usuario o contrasena Invalidos. Intente de nuevo."
        end if
        if (login_successfulC) then
            call menuCliente()
            exit
        else if (login_successfulA) then
            call menuAdmin()
            exit
        end if
    end do
end subroutine login


subroutine menuCliente()
do
    print *, "______________________________"
    print *, "Bienvenido al menu de opciones CLIENTE"
    print *, "1. Visualizar Reportes"
    print *, "2. Carga de Archivos"
    print *, "3. Opcion 3"
    print *, "4. Opcion 4"
    print *, "5. Salir"
    print *, "Seleccione una opcion (1-5): "
    print *, "______________________________"
    read(*,*) choice
    ! Llamada a subrutina según la opcion seleccionada
    select case(choice)
        case(1)
            call reportesCliente()
        case(2)
            call cargaArchivos()
        case(5)
            login_successfulC = .false.
            call login()
        case default
            print *, "Opción no válida"
    end select
end do
end subroutine menuCliente

subroutine menuAdmin()
do
    print *, "______________________________"
    print *, "Bienvenido al menu de opciones ADMINISTRADOR"
    print *, "1. Visualizar Reportes"
    print *, "2. Manejar Clientess"
    print *, "3. Opcion 3"
    print *, "4. Opcion 4"
    print *, "5. Salir"
    print *, "Seleccione una opcion (1-5): "
    print *, "______________________________"
    read(*,*) choice
    ! Llamada a subrutina según la opcion seleccionada
    select case(choice)
        case(1)
            call reportesClientema()
        case(2)
            call manejarClientesma()
        case(5)
            login_successfulA = .false.
            call login()
        case default
            print *, "Opción no válida"
    end select
end do
end subroutine menuAdmin

subroutine manejarClientesma()
    print *, "Ha seleccionado la opcion 2 - Manejar Clientes -"
end subroutine manejarClientesma

subroutine reportesClientema()
    print *, "Ha seleccionado la opcion 1 - Visualizar Reportes del Cliente-"
end subroutine reportesClientema

! Opciones del Menú Principal
subroutine reportesCliente()
    print *, "Ha seleccionado la opcion 1 - Visualizar Reportes Cliente-"
    
end subroutine reportesCliente

subroutine cargaArchivos()
    character(len=20) :: subChoices
    do
        print *, "Ha seleccionado la opcion 2 - Carga de Archivos CLIENTE -"
        print *, "A. Carga de Capas"
        print *, "B. Carga de Imagenes"
        print *, "C. Carga de Albumes"
        print *, "D. Volver al menu principal"
        print *, "Seleccione una opcion (A-D): "
        read(*,*) subChoices
        ! Llamada a subrutina según la opcion seleccionada
        select case(subChoices)
            case('A')
                call cargaCapasmc()
            case('B')
                call cargaImagenesmc()
            case('C')
                call cargaAlbumesmc()
            case('D')
                call menuCliente()
            case default
                print *, "Opción no válida"
        end select
    end do
end subroutine CargaArchivos

subroutine cargaCapasmC()
    print *, "Ha seleccionado la opcion A - Carga de Capas Cliente -"
    !read(*,*) direccion
    !direccion="z_archivos_carga_datos/capas.json"
    !call cargaCapas(direccion)
end subroutine cargaCapasmC

subroutine cargaImagenesmc()
    print *, "Ha seleccionado la opcion B - Carga de Imagenes -"
    ! Aquí puedes colocar el código correspondiente a la opcion B
end subroutine cargaImagenesmc

subroutine cargaAlbumesmc()
    print *, "Ha seleccionado la opcion C - Carga de Albumes -"
    !call matriz%print()
    !print *, "Carga masiva de albumes"
    !direccion="z_archivos_carga_datos/albumes.json"
    !call cargaAlbumes(direccion)
end subroutine cargaAlbumesmc

subroutine cargaCapas() !subroutine cargaCapas(direccion)
    character(len=1000) :: direccionn
    !la variable direccionn es igual al valor que le es envuado por parametro
    !print *, direccionn = direccion
    !character(len=1000), intent(in) :: direccion
    !integer :: fila_int, columna_int
    !call json%initialize()
    !call json%load(filename=direccion)
    !call json%info('',n_children=size)
    !call json%get_core(jsonc)
    !call json%get('', listPointer, found) !obtengo cada trozo dentro de llaves
    !do iC = 1, size
    !    call jsonc%get_child(listPointer, iC, animalPointer, found)
    !    call jsonc%get_child(animalPointer, 'id_capa', attributePointer, found)
    !    call jsonc%get(attributePointer, nombreAlbum)
    !    call jsonc%get_child(animalPointer, 'pixeles', attributePointer, found)
    !    call jsonc%info(attributePointer,n_children=sizeAlbumes)
    !    do iCAlbumes = 1, sizeAlbumes
    !        call jsonc%get_child(attributePointer, iCAlbumes, todoAlbumes, found)
    !        call jsonc%get_child(todoAlbumes, 'fila', attributePointerCapas, found)
    !        call jsonc%get(attributePointerCapas, fila)
    !        call jsonc%get_child(todoAlbumes, 'columna', attributePointerCapas, found)
    !        call jsonc%get(attributePointerCapas, columna)
    !        call jsonc%get_child(todoAlbumes, 'color', attributePointerCapas, found)
    !        call jsonc%get(attributePointerCapas, color)
    !        print *, "----"
    !        print *, 'Id capa: ',  nombreAlbum
    !        print *, 'Pixeles '
    !        print *, 'Fila: ', fila
    !        print *, 'Columna: ', columna
    !        print *, 'Color: ', color
    !        read(fila, *) fila_int
    !        read(columna, *) columna_int
    !        call matriz%Insertar_nodo(fila_int, columna_int, .true., color)  ! Insertar nodo en la matriz
    !        call tree%insert(fila_int, columna_int, color)
    !    end do
    !            do i = 1, 5
    !                print *, ""
    !            end do
    !end do
    !call json%destroy()
end subroutine cargaCapas
end program main
