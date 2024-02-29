
!QUEUE RECEPTION CLIENT
module mod_queue_client
implicit none
type :: Queue_Client
type(nodo_Queue_Client), pointer :: head => null()
contains
    procedure :: push_client
    procedure :: pop_client
    procedure :: print_client
end type Queue_Client
type :: nodo_Queue_Client
    character(len=:), allocatable :: id_cliente
    character(len=:), allocatable :: nombre
    character(len=:), allocatable :: img_grande
    character(len=:), allocatable :: img_pequena
    type(nodo_Queue_Client), pointer :: next
end type nodo_Queue_Client
contains
subroutine push_client(self, id_cliente, nombre, img_grande, img_pequena)
    class(Queue_Client), intent(inout) :: self
    character(len=*), intent(in) :: id_cliente, nombre, img_grande, img_pequena
    
    type(nodo_Queue_Client), pointer :: current, new_node
    allocate(new_node)
    new_node%id_cliente = id_cliente
    new_node%nombre = nombre
    new_node%img_grande = img_grande
    new_node%img_pequena = img_pequena
    new_node%next => null()

    if (.not. associated(self%head)) then
        self%head => new_node
    else
        current => self%head
        do while (associated(current%next))
            current => current%next
        end do
        current%next => new_node
    end if
end subroutine push_client
subroutine pop_client(self, info_cliente)
    class(Queue_Client), intent(inout) :: self
    type(nodo_Queue_Client), pointer :: temp
    character(len=20), dimension(:), intent(out) :: info_cliente

    if (.not. associated(self%head)) then
        info_cliente(1) = "VACIO"
        info_cliente(2) = "VACIO"
        info_cliente(3) = "0"
        info_cliente(4) = "0"
    else
        temp => self%head
        info_cliente(1) = self%head%id_cliente
        info_cliente(2) = self%head%nombre
        info_cliente(3) = self%head%img_pequena 
        info_cliente(4) = self%head%img_grande
        self%head => self%head%next
        deallocate(temp)
    end if
end subroutine pop_client
subroutine print_client(self)
    class(Queue_Client), intent(in) :: self
    type(nodo_Queue_Client), pointer :: current
    current => self%head
    do while (associated(current))
        print *, "___________________________________________"
        print *, "ID:                ",current%id_cliente
        print *, "Nombre:            ",current%nombre
        print *, "Imagenes Pequenas: ",current%img_pequena
        print *, "Imagenes Grandes:  ",current%img_grande
        print *, "___________________________________________"
        current => current%next
    end do
end subroutine print_client
end module mod_queue_client
!STACK IMG
module mod_stack_img
type :: stack_img
type(node_stack_img), pointer :: head => null()
contains
    procedure :: push_img
    procedure :: pop_img
    procedure :: print_img
    procedure :: clean_img
end type stack_img
type :: node_stack_img
    character(len=:), allocatable :: tipo_imagen
    type(node_stack_img), pointer :: next
end type node_stack_img
contains
subroutine push_img(self,tipo_imagen)
    class(stack_img), intent(inout) :: self
    character(len=*), intent(in) :: tipo_imagen
    type(node_stack_img), pointer :: new_node
    allocate(new_node)
    new_node%tipo_imagen = tipo_imagen
    new_node%next => self%head
    self%head => new_node
end subroutine push_img  
subroutine pop_img(self, tipo_imagen)
    class(stack_img), intent(inout) :: self
    character(len=20), dimension(:), intent(out) :: tipo_imagen
    type(node_stack_img), pointer :: temp
    if (.not. associated(self%head)) then
        print *, " *     LA PILA ESTA VACIA     *"
    else
        temp => self%head
        tipo_imagen(1) = self%head%tipo_imagen
        self%head => self%head%next
        deallocate(temp)
    end if
end subroutine pop_img
subroutine print_img(self)
    class(stack_img), intent(in) :: self
    type(node_stack_img), pointer :: current
    current => self%head
    print *, "*     PILA DE IMAGENES A IMPRIMIR     *"
    do while (associated(current))
        print *, current%tipo_imagen
        current => current%next
    end do
end subroutine print_img
subroutine clean_img(self)
    class(stack_img), intent(inout) :: self
    type(node_stack_img), pointer :: temp
    do while (associated(self%head))
        temp => self%head
        self%head => self%head%next
        deallocate(temp)
    end do
end subroutine clean_img
end module mod_stack_img
!QUEUE SMALL IMG
module mod_queue_sprinter
use mod_stack_img
implicit none
type :: queue_printers
type(node_printers), pointer :: head => null()
contains
    procedure :: push_img_pequena
    procedure :: pop_img_pequena
    procedure :: print_img_pequena
end type queue_printers
type :: node_printers
    character(len=:), allocatable :: tipo_imagen
    type(node_printers), pointer :: next
end type node_printers
contains
subroutine push_img_pequena(self, pila)
    class(queue_printers), intent(inout) :: self
    type(stack_img), intent(in) :: pila
    type(node_printers), pointer :: current, new_node
    type(node_stack_img), pointer :: temp
    character(len=:), allocatable :: tipo_imagen
    temp => pila%head
    do while (associated(temp))
        tipo_imagen = temp%tipo_imagen
        if (tipo_imagen == "Pequena") then
            allocate(new_node)
            new_node%tipo_imagen = tipo_imagen
            new_node%next => null()
            if (.not. associated(self%head)) then
                self%head => new_node
            else
                current => self%head
                do while (associated(current%next))
                    current => current%next
                end do
                current%next => new_node
            end if
        end if
        temp => temp%next
    end do
end subroutine push_img_pequena
subroutine pop_img_pequena(self)
    class(queue_printers), intent(inout) :: self
    type(node_printers), pointer :: temp
    if (.not. associated(self%head)) then
        print *, "*     . IMAGENES IMPRESAS .     *"
        return
    else
        temp => self%head
        self%head => self%head%next
        deallocate(temp)
    end if
end subroutine pop_img_pequena
subroutine print_img_pequena(self)
    class(queue_printers), intent(in) :: self
    type(node_printers), pointer :: current
    print*, "*  COLA IMPRESORA IMAGENES PEQUENIAS  *"
    current => self%head
    do while (associated(current))
        print *, "Imagen: ",current%tipo_imagen
        current => current%next
    end do
end subroutine print_img_pequena

end module mod_queue_sprinter
!QUEUE BIG IMG
module mod_queue_bprinter
use mod_stack_img
implicit none
type :: queue_printerb
type(node_printerb), pointer :: head => null()
contains
    procedure :: push_img_grande
    procedure :: pop_img_grande
    procedure :: print_img_grande
end type queue_printerb
type :: node_printerb
    character(len=:), allocatable :: tipo_imagen
    type(node_printerb), pointer :: next
end type node_printerb
contains
subroutine push_img_grande(self, pila)
    class(queue_printerb), intent(inout) :: self
    type(stack_img), intent(inout) :: pila
    type(node_printerb), pointer :: current, new_node
    character(len=:), allocatable :: tipo_imagen
    do while (associated(pila%head))
        tipo_imagen = pila%head%tipo_imagen
        if (tipo_imagen == "Grande") then
            allocate(new_node)
            new_node%tipo_imagen = tipo_imagen
            new_node%next => null()
            if (.not. associated(self%head)) then
                self%head => new_node
            else
                current => self%head
                do while (associated(current%next))
                    current => current%next
                end do
                current%next => new_node
            end if
        end if
        pila%head => pila%head%next
    end do
end subroutine push_img_grande
subroutine pop_img_grande(self)
    class(queue_printerb), intent(inout) :: self
    type(node_printerb), pointer :: temp
    if (.not. associated(self%head)) then
        print *, "*  COLA IMPRESORA IMAGENES GRANDES  *"
        return
    else
        temp => self%head
        self%head => self%head%next
        deallocate(temp)
    end if
end subroutine pop_img_grande
subroutine print_img_grande(self)
    class(queue_printerb), intent(in) :: self
    type(node_printerb), pointer :: current
    current => self%head
    print*, "*  COLA IMPRESORA IMAGENES GRANDES  *"
    do while (associated(current))
        print *, "Imagen: ",current%tipo_imagen
        current => current%next
    end do
end subroutine print_img_grande

end module mod_queue_bprinter
!AUXILIAR MODULE
!LIST PRINTED IMG
module mod_imgprinted
implicit none
type :: node_printed_img
    character(len=:), allocatable :: tipo_imagen
    type(node_printed_img), pointer :: next
end type node_printed_img
type :: list_img_printed
    type(node_printed_img), pointer :: head => null()
contains
    procedure :: insertar_imagen_impresa
    procedure :: print_lista_imagen_impresa
end type list_img_printed
contains
subroutine insertar_imagen_impresa(self, tipo_imagen)
    class(list_img_printed), intent(inout) :: self
    character(len=*), intent(in) :: tipo_imagen
    type(node_printed_img), pointer :: new_node
    allocate(new_node)
    new_node%tipo_imagen = tipo_imagen
    new_node%next => self%head
    self%head => new_node
end subroutine insertar_imagen_impresa
subroutine print_lista_imagen_impresa(self)
    class(list_img_printed), intent(in) :: self
    type(node_printed_img), pointer :: current
    current => self%head
    if (.not. associated(current)) then
        print *, "*     NO SE HAN IMPRESO IMAGENES TODAVIA     *"
        return
    end if
    do while (associated(current))
        print *, "Imagen: ", current%tipo_imagen
        current => current%next
    end do
end subroutine print_lista_imagen_impresa
end module mod_imgprinted
!LIST WAITING CLIENT
module mod_client_waiting
use mod_imgprinted
implicit none
type :: node_waiting_client
integer :: numero_ventanilla, cantidad_paso, pequena, grande
character(len=:), allocatable :: id_cliente
character(len=:), allocatable :: nombre
character(len=:), allocatable :: img_grande
character(len=:), allocatable :: img_pequena
type(list_img_printed) :: lista_imagen_cliente
type(node_waiting_client), pointer :: anterior, next
end type node_waiting_client
type :: list_waiting_client
    type(node_waiting_client), pointer :: head => null()
contains
    procedure :: append_client
    procedure :: delete_cliente
    procedure :: print_lista
end type list_waiting_client
contains
subroutine append_client(self, id_cliente, nombre, img_pequena, img_grande, numero_ventanilla, cantidad_paso)
    class(list_waiting_client), intent(inout) :: self
    character(len=*), intent(in) :: id_cliente, nombre, img_pequena, img_grande
    integer, intent(in) :: numero_ventanilla, cantidad_paso
    integer :: pequena, grande
    type(node_waiting_client), pointer :: new_node
    READ(img_pequena, *) pequena
    READ(img_grande, *) grande
    allocate(new_node)
    new_node%id_cliente = id_cliente
    new_node%nombre = nombre
    new_node%img_pequena = img_pequena
    new_node%img_grande = img_grande
    new_node%pequena = pequena
    new_node%grande = grande
    new_node%numero_ventanilla = numero_ventanilla
    new_node%cantidad_paso = cantidad_paso
    if (.not. associated(self%head)) then
        self%head => new_node
        new_node%anterior => new_node
        new_node%next => new_node
    else
        new_node%anterior => self%head%anterior
        new_node%next => self%head
        self%head%anterior%next => new_node
        self%head%anterior => new_node
    end if
end subroutine append_client
subroutine delete_cliente(self, id_cliente)
    class(list_waiting_client), intent(inout) :: self
    character(len=*), intent(in) :: id_cliente
    type(node_waiting_client), pointer :: current
    current => self%head
    do while (associated(current) .and. current%id_cliente /= id_cliente)
        current => current%next
    end do
    if (associated(current)) then
        current%anterior%next => current%next
        current%next%anterior => current%anterior
        if (associated(self%head, current)) then
            if (associated(current, current%next)) then
                self%head => null()
            else
                self%head => current%next
            end if
        end if
        deallocate(current)
    end if
end subroutine delete_cliente
subroutine print_lista(self)
    class(list_waiting_client), intent(in) :: self
    type(node_waiting_client), pointer :: current
    if (.not. associated(self%head)) then
        print *, "*     SALA DE ESPERA VACIA     *."
    else
        current => self%head
        do
            print *, "___________________________________________"
            print *, "ID Cliente:       ", current%id_cliente
            print *, "Nombre:           ", current%nombre
            print *, "Imagen Pequena:   ", current%img_pequena
            print *, "Imagen Grande:    ", current%img_grande
            print *, "Pequena:          ", current%pequena
            print *, "Grande:           ", current%grande
            print *, "Ventanilla:       ", current%numero_ventanilla
            print *, "Cantidad Paso:    ", current%cantidad_paso
            call current%lista_imagen_cliente%print_lista_imagen_impresa()
            print *, "___________________________________________"
            current => current%next
            if (associated(current, self%head)) exit
        end do
    end if
end subroutine print_lista
end module mod_client_waiting
!LIST WINDOWS
module mod_list_windows
use mod_stack_img
use mod_queue_sprinter
use mod_queue_bprinter
use mod_client_waiting
use mod_client_served
implicit none
type :: List_Windows
    type(node_list_client), pointer :: head => null()
    type(queue_printers) :: cola_imagen_pequena
    type(queue_printerb) :: cola_imagen_grande
    type(list_waiting_client) :: lista_clientes_esperando
    type(list_client_served) :: lista_clientes_atendido
contains
    procedure :: agregar_ventanilla
    procedure :: print_ventanilla
    procedure :: asignar_ventanilla
    procedure :: ventanilla_disponible
    procedure :: atender_cliente_ventanilla
    procedure :: imprimir_imagenes_cliente
end type List_Windows
type :: node_list_client
    integer :: numero_ventanilla, pequena, grande
    character(len=:), allocatable :: id_cliente
    character(len=:), allocatable :: nombre
    character(len=:), allocatable :: img_grande
    character(len=:), allocatable :: img_pequena
    logical :: ocupada = .false.
    type(stack_img) :: pila
    type(node_list_client), pointer :: next
end type node_list_client
integer :: cantidad_paso=1
contains
subroutine agregar_ventanilla(self, numero_ventanilla, id_cliente, nombre, img_pequena, img_grande)
    class(List_Windows), intent(inout) :: self
    integer, intent(in) :: numero_ventanilla
    character(len=*), intent(in) :: id_cliente, nombre, img_grande, img_pequena
    type(node_list_client), pointer :: current, new_node
    allocate(new_node)
    new_node%numero_ventanilla = numero_ventanilla
    new_node%id_cliente = id_cliente
    new_node%nombre = nombre
    new_node%img_pequena = img_pequena
    new_node%img_grande = img_grande
    new_node%pequena = 0
    new_node%grande = 0
    new_node%ocupada = .false.
    new_node%next => null()
    if (.not. associated(self%head)) then
        self%head => new_node
    else
        current => self%head
        do while (associated(current%next))
            current => current%next
        end do
        current%next => new_node
    end if
end subroutine agregar_ventanilla  
subroutine print_ventanilla(self)
    class(List_Windows), intent(in) :: self
    type(node_list_client), pointer :: current
    current => self%head
    do while (associated(current))
        print *, "___________________________________________"
        print *, "Ventanilla:     ", current%numero_ventanilla
        print *, "ID:             ", current%id_cliente
        print *, "Nombre:         ", current%nombre
        print *, "Img Pequena:    ", current%img_pequena
        print *, "Img Grande:     ", current%img_grande
        print *, "Pequena:        ", current%pequena
        print *, "Grande:         ", current%grande
        print *, "Ocupada:        ", current%ocupada
        print *, "___________________________________________"
        current => current%next
    end do
end subroutine print_ventanilla
logical function ventanilla_disponible(self) result (hay_ventanilla_disponible)
    class(List_Windows), intent(inout) :: self
    type(node_list_client), pointer :: current
    hay_ventanilla_disponible = .false.
    current => self%head
    do while (associated(current))
        if (.not. current%ocupada) then
            hay_ventanilla_disponible = .true.
            exit
        end if
        current => current%next
    end do
end function ventanilla_disponible
subroutine asignar_ventanilla(self, id_cliente, nombre, img_pequena, img_grande)
    class(List_Windows), intent(inout) :: self
    integer :: pequena, grande
    character(len=*), intent(in) :: id_cliente, nombre, img_pequena, img_grande
    type(node_list_client), pointer :: current
    READ(img_pequena, *) pequena
    READ(img_grande, *) grande
    current => self%head
    do while (associated(current))
        if (.not. current%ocupada) then
            current%id_cliente = id_cliente
            current%nombre = nombre
            current%img_pequena = img_pequena
            current%img_grande = img_grande
            current%pequena = pequena
            current%grande = grande
            current%ocupada = .true.
            exit
        end if
        current => current%next
    end do
end subroutine asignar_ventanilla
subroutine atender_cliente_ventanilla(self) 
    class(List_Windows), intent(inout) :: self
    type(node_list_client), pointer :: current
    integer :: num_img_grandes, num_img_pequenas
    current => self%head
    do while (associated(current))
        num_img_pequenas = current%pequena
        num_img_grandes = current%grande
        if (num_img_pequenas>0) then
            call current%pila%push_img("Pequena")
            num_img_pequenas=num_img_pequenas-1
            current%pequena = num_img_pequenas
        else if (num_img_grandes>0) then
            call current%pila%push_img("Grande")
            num_img_grandes=num_img_grandes-1
            current%grande = num_img_grandes
        else if (current%grande==0 .and. current%pequena==0) then
            call self%cola_imagen_pequena%push_img_pequena(current%pila)
            call self%cola_imagen_grande%push_img_grande(current%pila)
            if (current%id_cliente=="VACIO")then
                exit
            else
                call self%lista_clientes_esperando%append_client(current%id_cliente, &
                    current%nombre, &
                    current%img_pequena, &
                    current%img_grande, &
                    current%numero_ventanilla, &
                    cantidad_paso)
            end if
            current%id_cliente = "VACIO"
            current%nombre = "VACIO"
            current%img_pequena = "0"
            current%img_grande = "0"
            current%pequena = 0
            current%grande = 0
            current%ocupada = .false.
            call current%pila%clean_img()
        end if 
        current => current%next
    end do
    cantidad_paso=cantidad_paso+1
end subroutine atender_cliente_ventanilla
subroutine imprimir_imagenes_cliente(self)
    class(List_Windows), intent(inout) :: self
    type(node_waiting_client), pointer :: current, siguiente_nodo
    integer :: num_img_grandes, num_img_pequenas
    logical :: condicion_salida
    current => self%lista_clientes_esperando%head
    if (.not. associated(current)) then
        return
    end if
    do while (associated(current))
        num_img_pequenas = current%pequena
        num_img_grandes = current%grande
        if (num_img_pequenas>0) then
            call self%cola_imagen_pequena%pop_img_pequena()
            call current%lista_imagen_cliente%insertar_imagen_impresa("Pequena")
            num_img_pequenas=num_img_pequenas-1
            current%pequena = num_img_pequenas
            current%cantidad_paso=current%cantidad_paso+1
        end if
        if (num_img_grandes>0) then
            call self%cola_imagen_grande%pop_img_grande()
            call current%lista_imagen_cliente%insertar_imagen_impresa("Grande")
            num_img_grandes=num_img_grandes-1
            current%grande = num_img_grandes
            current%cantidad_paso=current%cantidad_paso+1
        end if
        siguiente_nodo => current%next
        if (current%grande==0 .and. current%pequena==0) then
            call self%lista_clientes_atendido%insertar_cliente_atendido( &
                current%id_cliente, current%nombre, current%img_pequena, &
                current%img_grande, current%cantidad_paso)
            call self%lista_clientes_esperando%delete_cliente(current%id_cliente)
        end if
        condicion_salida = .not. associated(self%lista_clientes_esperando%head) .or. &
                        associated(current, self%lista_clientes_esperando%head)
        current => siguiente_nodo
        if (condicion_salida) exit
    end do
end subroutine imprimir_imagenes_cliente
end module mod_list_windows





