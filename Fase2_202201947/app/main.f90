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

            !Fila---Columna
            colorcito="5as5"
            call matriz%Insertar_nodo(2,2,.true.,colorcito)
            colorcito="8as5"
            call matriz%Insertar_nodo(1,1,.true.,colorcito)
            colorcito="7as5"
            call matriz%Insertar_nodo(3,3,.true.,colorcito)
            call matriz%print()
            ! Insertar valores de values
            do i = 1, size(values)
                call tree%insert(values(i))
            end do 
            call tree%graph("inserted")
            del = 30
            call tree%delete(del)
            call tree%graph("deleted1")
            del = 26
            call tree%delete(del)
            call tree%graph("deleted2")
            del = 21
            call tree%delete(del)
            call tree%graph("deleted3")
            del = 11
            call tree%delete(del)
            call tree%graph("deleted4")
            write(*, '(A)') "Escribiendo en preorden: "
            call tree%preorder()
            write(*, '(A)') "Escribiendo en inorder: "
            call tree%inorder()
            print *, "Escribiendo en posorden: "
            call tree%posorder()

! Menú principal
do
    print *, "______________________________"
    print *, "Bienvenido al menu de opciones"
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
            call opcion1()
        case(2)
            call opcion2()
        case(3)
            call opcion3()
        case(4)
            call opcion4()
        case(5)
            exit ! Salir del programa
        case default
            print *, "Opción no válida"
    end select
end do

contains
! Opciones del Menú Principal
subroutine opcion1()
    print *, "Ha seleccionado la opcion 1 - Visualizar Reportes -"
    ! Aquí puedes colocar el código correspondiente a la opcion 1
end subroutine opcion1

subroutine opcion2()
    character(len=20) :: subChoice

    ! Bucle del menu secundario
    do
        print *, "Ha seleccionado la opcion 2 - Carga de Archivos -"
        print *, "A. Carga de Capas"
        print *, "B. Carga de Imagenes"
        print *, "C. Carga de Albumes"
        print *, "D. Volver al menu principal"
        print *, "Seleccione una opcion (A-D): "
        read(*,*) subChoice
        ! Llamada a subrutina según la opcion seleccionada
        select case(subChoice)
            case('A')
                call opcionA()
            case('B')
                call opcionB()
            case('C')
                call opcionC()
            case('D')
                exit ! Volver al menu principal
            case default
                print *, "Opción no válida"
        end select
    end do
end subroutine opcion2

subroutine opcion3()
    print *, "Ha seleccionado la opcion 3"
    ! Aquí puedes colocar el código correspondiente a la opcion 3
end subroutine opcion3

subroutine opcion4()
    print *, "Ha seleccionado la opcion 4"
    ! Aquí puedes colocar el código correspondiente a la opcion 4
end subroutine opcion4


! Opciones del Menú Secundario
subroutine opcionA()
    print *, "Ha seleccionado la opcion A - Carga de Capas -"
    !direccion="z_archivos_carga_datos/capas.json"
    !call cargaCapas(direccion)
end subroutine opcionA

subroutine opcionB()
    print *, "Ha seleccionado la opcion B - Carga de Imagenes -"
    ! Aquí puedes colocar el código correspondiente a la opcion B
end subroutine opcionB

subroutine opcionC()
    print *, "Ha seleccionado la opcion C - Carga de Albumes -"
    !print *, "Carga masiva de albumes"
    !direccion="z_archivos_carga_datos/albumes.json"
    !call cargaAlbumes(direccion)
end subroutine opcionC

!Subrutinas de Funcionalidad
!subroutine cargaMasivaCliente(direccion)
!    character(len=1000), intent(in) :: direccion
!    call json%initialize()
!    call json%load(filename=direccion)
!    call json%info('',n_children=size)
!    call json%get_core(jsonc)
!    call json%get('', listPointer, found)
!    do iC = 1, size
!        call jsonc%get_child(listPointer, iC, animalPointer, found)
!        call jsonc%get_child(animalPointer, 'dpi', attributePointer, found)
!        call jsonc%get(attributePointer, dpi)
!        call jsonc%get_child(animalPointer, 'nombre_cliente', attributePointer, found)
!        call jsonc%get(attributePointer, nombreCliente)
!        call jsonc%get_child(animalPointer, 'password', attributePointer, found) 
!        call jsonc%get(attributePointer, password)
!         print *, "______________________________"
!         print *, 'DPI: ', dpi
!         print *, 'Nombre: ', nombreCliente
!         print *, 'password: ', password
!    end do
!    call json%destroy()
!end subroutine cargaMasivaCliente
!
!subroutine cargaCapas(direccion)
!    character(len=1000), intent(in) :: direccion
!    call json%initialize()
!    call json%load(filename=direccion)
!    call json%info('',n_children=size)
!    call json%get_core(jsonc)
!    call json%get('', listPointer, found) !obtengo cada trozo dentro de llaves
!    do iC = 1, size
!        call jsonc%get_child(listPointer, iC, animalPointer, found)
!        call jsonc%get_child(animalPointer, 'id_capa', attributePointer, found)
!        call jsonc%get(attributePointer, nombreAlbum)
!        call jsonc%get_child(animalPointer, 'pixeles', attributePointer, found)
!        call jsonc%info(attributePointer,n_children=sizeAlbumes)
!        do iCAlbumes = 1, sizeAlbumes
!            call jsonc%get_child(attributePointer, iCAlbumes, todoAlbumes, found)
!            call jsonc%get_child(todoAlbumes, 'fila', attributePointerCapas, found)
!            call jsonc%get(attributePointerCapas, fila)
!            call jsonc%get_child(todoAlbumes, 'columna', attributePointerCapas, found)
!            call jsonc%get(attributePointerCapas, columna)
!            call jsonc%get_child(todoAlbumes, 'color', attributePointerCapas, found)
!            call jsonc%get(attributePointerCapas, color)
!            print *, "----"
!            print *, 'Id capa: ',  nombreAlbum
!            print *, 'Pixeles '
!            print *, 'Fila: ', fila
!            print *, 'Columna: ', columna
!            print *, 'Color: ', color
!        end do
!                do i = 1, 5
!                    print *, ""
!                end do
!    end do
!    call json%destroy()
!end subroutine cargaCapas
!
!subroutine cargaAlbumes(direccion)
!    character(len=1000), intent(in) :: direccion
!    call json%initialize()
!    call json%load(filename=direccion)
!    call json%info('',n_children=size)
!    call json%get_core(jsonc)
!    call json%get('', listPointer, found) !obtengo cada trozo dentro de llaves
!    do iC = 1, size
!        call jsonc%get_child(listPointer, iC, animalPointer, found)
!        call jsonc%get_child(animalPointer, 'nombre_album', attributePointer, found)
!        call jsonc%get(attributePointer, nombreAlbum)
!        call jsonc%get_child(animalPointer, 'imgs', attributePointer, found)
!        call jsonc%info(attributePointer,n_children=sizeAlbumes)
!        do iCAlbumes = 1, sizeAlbumes
!            call jsonc%get_child(attributePointer, iCAlbumes, todoAlbumes, found)
!            call jsonc%get(todoAlbumes, imgs)
!            print *, "______________________________"
!            print *, 'Nombre Album: ',  nombreAlbum
!            print *, 'Imgs: ', imgs
!        end do
!    end do
!    call json%destroy()
!end subroutine cargaAlbumes

end program main
