module matriz_m
    implicit none
    private
    type :: node_val
        private
        logical :: exists = .false.
        logical :: valor
        character(len=7):: color
    end type node_val

    type :: node
        private
        integer :: i,j
        logical :: valor = .false.
        character(len=7):: color
        type(node), pointer :: arriba => null()
        type(node), pointer :: abajo => null()
        type(node), pointer :: derecha => null()
        type(node), pointer :: izquierda => null()
    end type node

    type, public :: mamatrix
        private
        type(node), pointer :: root => null()
        integer :: width = 0
        integer :: height = 0
    contains
        procedure :: Insertar_nodo
        procedure :: Insertar_cabecera_Fila
        procedure :: Insertar_cabecera_Columna
        procedure :: buscar_Fila
        procedure :: buscar_Columna
        procedure :: verificar_Nodo
        procedure :: print
        procedure :: imprimirEncabezadoColumnas
        procedure :: obtenerValor
    end type mamatrix

contains
subroutine Insertar_nodo(self, i, j, valor,color)
    class(mamatrix), intent(inout) :: self
    integer, intent(in) :: i
    integer, intent(in) :: j
    logical, intent(in) :: valor
    character(len=7), intent(in) :: color
    type(node), pointer :: nuevo
    type(node), pointer :: fila
    type(node), pointer :: columna
    allocate(nuevo)
    nuevo = node(i=i, j=j, valor=valor,color=color)
    if(.not. associated(self%root)) then
        allocate(self%root)
        self%root = node(i=-1, j=-1,color='xxxxxxx')
    end if
    fila => self%buscar_Fila(j)
    columna => self%buscar_Columna(i)
    if(i > self%width) self%width = i
    if(j > self%height) self%height = j
    if(.not. self%verificar_Nodo(nuevo)) then
        if(.not. associated(columna)) then
            columna => self%Insertar_cabecera_Columna(i)
        end if
        if(.not. associated(fila)) then
            fila => self%Insertar_cabecera_Fila(j)
        end if
        call Insertar_nodo_Columna(nuevo, fila)
        call Insertar_nodo_Fila(nuevo, columna)
    end if
end subroutine Insertar_nodo

    function Insertar_cabecera_Columna(self, i) result(nuevaCabeceraColumna)
        class(mamatrix), intent(inout) :: self
        integer, intent(in) :: i
        type(node), pointer :: nuevaCabeceraColumna
        allocate(nuevaCabeceraColumna)
        nuevaCabeceraColumna = node(i=i, j=-1,color='xxxxxxx')
        call Insertar_nodo_Columna(nuevaCabeceraColumna, self%root)
    end function Insertar_cabecera_Columna

    function Insertar_cabecera_Fila(self, j) result(nuevaCabeceraFila)
        class(mamatrix), intent(inout) :: self
        integer, intent(in) :: j
        type(node), pointer :: nuevaCabeceraFila
        allocate(nuevaCabeceraFila)
        nuevaCabeceraFila = node(i=-1, j=j,color='xxxxxxx')
        call Insertar_nodo_Fila(nuevaCabeceraFila, self%root)
    end function Insertar_cabecera_Fila

subroutine Insertar_nodo_Fila(nuevo, CabeceraFila)
    type(node), pointer :: nuevo
    type(node), pointer :: cabeceraFila !head 
    type(node), pointer :: actual
    actual => cabeceraFila
    do while(associated(actual%abajo))
        if(nuevo%j < actual%abajo%j) then
            nuevo%abajo => actual%abajo
            nuevo%arriba => actual
            actual%abajo%arriba => nuevo
            actual%abajo => nuevo
            exit
        end if
        actual => actual%abajo
    end do
    if(.not. associated(actual%abajo)) then
        actual%abajo => nuevo
        nuevo%arriba => actual
    end if
end subroutine Insertar_nodo_Fila

subroutine Insertar_nodo_Columna(nuevo, CabeceraColumna)
    type(node), pointer :: nuevo
    type(node), pointer :: CabeceraColumna !head 
    type(node), pointer :: actual
    actual => CabeceraColumna
    do while(associated(actual%derecha))
        if(nuevo%i < actual%derecha%i) then
            nuevo%derecha => actual%derecha
            nuevo%izquierda => actual
            actual%derecha%izquierda => nuevo
            actual%derecha => nuevo
            exit
        end if
        actual => actual%derecha
    end do
    if(.not. associated(actual%derecha)) then
        actual%derecha => nuevo
        nuevo%izquierda => actual
    end if
end subroutine Insertar_nodo_Columna  

    function buscar_Fila(self, j) result(actual)
        class(mamatrix), intent(in) :: self
        integer, intent(in) :: j
        type(node), pointer :: actual
        actual => self%root
        do while(associated(actual)) 
            if(actual%j == j) return
            actual => actual%abajo
        end do
    end function buscar_Fila

    function buscar_Columna(self, i) result(actual)
        class(mamatrix), intent(in) :: self
        integer, intent(in) :: i
        type(node), pointer :: actual
        actual => self%root 
        
        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%derecha
        end do
    end function buscar_Columna

    function verificar_Nodo(self, nodo) result(existe)
        class(mamatrix), intent(inout) :: self
        type(node), pointer, intent(in) :: nodo
        logical :: existe
        type(node), pointer :: encabezadoFila
        type(node), pointer :: columna
        encabezadoFila => self%root
        existe = .false.
        do while(associated(encabezadoFila))
            if(encabezadoFila%j == nodo%j) then
                columna => encabezadoFila
                do while(associated(columna)) 
                    if(columna%i == nodo%i) then
                        columna%valor = nodo%valor
                        columna%color = nodo%color
                        existe = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            encabezadoFila => encabezadoFila%abajo
        end do
        return
    end function verificar_Nodo

subroutine print(self)
    class(mamatrix), intent(in) :: self
    integer :: i
    integer :: j
    type(node), pointer :: aux
    type(node_val) :: val
    aux => self%root%abajo
    call self%imprimirEncabezadoColumnas()
    do j = 0, self%height
        print *, ""
        write(*, fmt='(I3)', advance='no') j
        do i = 0, self%width
            val = self%obtenerValor(i,j)
            if(.not. val%exists) then
                write(*, fmt='(I3)', advance='no') 0
            else
                write(*, '(A)', advance='no') val%color
            end if
        end do
    end do
    print *, ""
end subroutine print

subroutine imprimirEncabezadoColumnas(self)
    class(mamatrix), intent(in) :: self
    integer :: i
    do i=-1, self%width
        write(*, fmt='(I3)', advance='no') i
    end do
end subroutine imprimirEncabezadoColumnas

    function obtenerValor(self, i, j) result(val)
        class(mamatrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        type(node), pointer :: cabeceraFila
        type(node), pointer :: columna
        type(node_val) :: val
        cabeceraFila => self%root
        do while(associated(cabeceraFila))
            if(cabeceraFila%j == j) then
                columna => cabeceraFila
                do while(associated(columna)) 
                    if(columna%i == i) then
                        val%valor = columna%valor
                        val%color = columna%color
                        val%exists = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            cabeceraFila => cabeceraFila%abajo
        end do
        return
    end function obtenerValor


end module matriz_m