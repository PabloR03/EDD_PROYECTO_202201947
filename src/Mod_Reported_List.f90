!LIST CLIENTS SERVED
module mod_client_served
    implicit none
    type :: node_client_served
        character(len=:), allocatable :: id_cliente
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: img_pequena
        character(len=:), allocatable :: img_grande
        integer :: cantidad_pasos
        type(node_client_served), pointer :: next => null()
    end type node_client_served
    type :: list_client_served
        type(node_client_served), pointer :: head => null()
    contains
    
        procedure :: insertar_cliente_atendido
        procedure :: print_cliente_atendido
    end type list_client_served

    contains
    subroutine insertar_cliente_atendido(self, id_cliente, nombre, img_pequena, img_grande, cantidad_pasos)
        class(list_client_served), intent(inout) :: self
        character(len=*), intent(in) :: id_cliente, nombre, img_pequena, img_grande
        integer, intent(in) :: cantidad_pasos
        type(node_client_served), pointer :: new_node
        allocate(new_node)
        new_node%id_cliente = id_cliente
        new_node%nombre = nombre
        new_node%img_pequena = img_pequena
        new_node%img_grande = img_grande
        new_node%cantidad_pasos = cantidad_pasos
        new_node%next => self%head
        self%head => new_node
    end subroutine insertar_cliente_atendido
    subroutine print_cliente_atendido(self)
        class(list_client_served), intent(in) :: self
        type(node_client_served), pointer :: current
        current => self%head
        do while (associated(current))
            print *, "___________________________________________"
            print *, "ID Cliente: ", current%id_cliente
            print *, "Nombre: ", current%nombre
            print *, "Imagen Pequena: ", current%img_pequena
            print *, "Imagen Grande: ", current%img_grande
            print *, "Cantidad de Pasos: ", current%cantidad_pasos
            print *, "___________________________________________"
            current => current%next
        end do
    end subroutine print_cliente_atendido
end module mod_client_served

