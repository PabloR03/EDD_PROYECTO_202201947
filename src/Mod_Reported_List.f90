!LIST CLIENTS SERVED
module mod_client_served
    implicit none
    type :: nodo_cliente_atendido
        character(len=:), allocatable :: id_cliente
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: img_pequena
        character(len=:), allocatable :: img_grande
        integer :: cantidad_pasos
        type(nodo_cliente_atendido), pointer :: siguiente => null()
    end type nodo_cliente_atendido
    type :: lista_cliente_atendido
        type(nodo_cliente_atendido), pointer :: cabeza => null()
    contains
    
        procedure :: insertar_cliente_atendido
        procedure :: print_cliente_atendido
    end type lista_cliente_atendido

    contains
    subroutine insertar_cliente_atendido(self, id_cliente, nombre, img_pequena, img_grande, cantidad_pasos)
        class(lista_cliente_atendido), intent(inout) :: self
        character(len=*), intent(in) :: id_cliente, nombre, img_pequena, img_grande
        integer, intent(in) :: cantidad_pasos
        type(nodo_cliente_atendido), pointer :: nuevo_nodo
        allocate(nuevo_nodo)
        nuevo_nodo%id_cliente = id_cliente
        nuevo_nodo%nombre = nombre
        nuevo_nodo%img_pequena = img_pequena
        nuevo_nodo%img_grande = img_grande
        nuevo_nodo%cantidad_pasos = cantidad_pasos
        nuevo_nodo%siguiente => self%cabeza
        self%cabeza => nuevo_nodo
    end subroutine insertar_cliente_atendido
    subroutine print_cliente_atendido(self)
        class(lista_cliente_atendido), intent(in) :: self
        type(nodo_cliente_atendido), pointer :: actual
        actual => self%cabeza
        do while (associated(actual))
            print *, "___________________________________________"
            print *, "ID Cliente: ", actual%id_cliente
            print *, "Nombre: ", actual%nombre
            print *, "Imagen Pequena: ", actual%img_pequena
            print *, "Imagen Grande: ", actual%img_grande
            print *, "Cantidad de Pasos: ", actual%cantidad_pasos
            print *, "___________________________________________"
            actual => actual%siguiente
        end do
    end subroutine print_cliente_atendido
end module mod_client_served

