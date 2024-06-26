module s_de_Split
    implicit none
contains
    subroutine split(cadena, delimitador, resultado)
        character(len=*), intent(in) :: cadena, delimitador
        character(len=20), dimension(:), allocatable, intent(out) :: resultado
        character(len=20) :: numero
        integer :: i
        numero = ''
        allocate(character(len=20) :: resultado(0))
        do i=1, len_trim(cadena)
            if (cadena(i:i) .eq. delimitador) then
                if (len_trim(numero) > 0) then
                    call agregar(resultado, numero)
                    numero = ''
                end if
            else
                numero = trim(numero) // cadena(i:i)
            end if
        end do
        if (len_trim(numero) > 0) then
            call agregar(resultado, numero)
        end if
    end subroutine split
    subroutine agregar(arreglo, elemento)
        character(len=20), dimension(:), allocatable, intent(inout) :: arreglo
        character(len=20), intent(in) :: elemento
        character(len=20), dimension(:), allocatable :: temporal
        if (allocated(arreglo)) then
            call move_alloc(arreglo, temporal)
            allocate(arreglo(size(temporal)+1))
            arreglo(1:size(temporal)) = temporal
            arreglo(size(temporal)+1) = elemento
        else
            allocate(arreglo(1))
            arreglo(1) = elemento
        end if
    end subroutine agregar
    function int_to_str(valor)
        integer, intent(in) :: valor
        character(len=32) :: int_to_str
        write(int_to_str,'(I0)') valor
        int_to_str = trim(adjustl(int_to_str))
    end function int_to_str
end module s_de_Split

module global_variable
    implicit none
    character(len=:), allocatable :: dpi_global
end module global_variable
