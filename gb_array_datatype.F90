module gb_array_datatype
    use iso_fortran_env
    use mpi_f08
    use gb_element_datatype
    implicit none

#ifdef ENABLE_SELECT_RANK

    interface get_array_datatype
        module procedure get_array_datatype_all
    end interface

    contains

        function get_array_datatype_all(buffer) result(datatype)
            class(*), DIMENSION(..), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            select rank (buffer)
                rank(0)
                    datatype = get_element_datatype(buffer)
                rank(1)
                    datatype = get_element_datatype(buffer(1))
                rank(2)
                    datatype = get_element_datatype(buffer(1,1))
                rank(3)
                    datatype = get_element_datatype(buffer(1,1,1))
                rank(4)
                    datatype = get_element_datatype(buffer(1,1,1,1))
                rank(5)
                    datatype = get_element_datatype(buffer(1,1,1,1,1))
                rank(6)
                    datatype = get_element_datatype(buffer(1,1,1,1,1,1))
                rank(7)
                    datatype = get_element_datatype(buffer(1,1,1,1,1,1,1))
                rank(8)
                    datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1))
                rank(9)
                    datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1))
                rank(10)
                    datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1))
                rank(11)
                    datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1))
                rank(12)
                    datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1))
                rank(13)
                    datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1,1))
                rank(14)
                    datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1,1,1))
                rank(15)
                    datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
            end select
        end function

#else

    interface get_array_datatype
        module procedure get_array_datatype_d0
        module procedure get_array_datatype_d1
        module procedure get_array_datatype_d2
        module procedure get_array_datatype_d3
        module procedure get_array_datatype_d4
        module procedure get_array_datatype_d5
        module procedure get_array_datatype_d6
        module procedure get_array_datatype_d7
#ifdef ENABLE_8D_ARRAYS
        module procedure get_array_datatype_d8
        module procedure get_array_datatype_d9
        module procedure get_array_datatype_d10
        module procedure get_array_datatype_d11
        module procedure get_array_datatype_d12
        module procedure get_array_datatype_d13
        module procedure get_array_datatype_d14
#ifdef ENABLE_15D_ARRAYS
        module procedure get_array_datatype_d15
#endif
#endif
    end interface

    contains

        function get_array_datatype_d0(buffer) result(datatype)
            class(*), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer)
        end function

        function get_array_datatype_d1(buffer) result(datatype)
            class(*), dimension(:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1))
        end function

        function get_array_datatype_d2(buffer) result(datatype)
            class(*), dimension(:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1))
        end function

        function get_array_datatype_d3(buffer) result(datatype)
            class(*), dimension(:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1))
        end function

        function get_array_datatype_d4(buffer) result(datatype)
            class(*), dimension(:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1))
        end function

        function get_array_datatype_d5(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1))
        end function

        function get_array_datatype_d6(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1,1))
        end function

        function get_array_datatype_d7(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1,1,1))
        end function

#if ENABLE_FORTRAN2008_ARRAYS
        function get_array_datatype_d8(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1))
        end function

        function get_array_datatype_d9(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1))
        end function

        function get_array_datatype_d10(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1))
        end function

        function get_array_datatype_d11(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1))
        end function

        function get_array_datatype_d12(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1))
        end function

        function get_array_datatype_d13(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1,1))
        end function

        function get_array_datatype_d14(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1,1,1))
        end function

#if ENABLE_15D_ARRAYS
        ! GCC 11 does not like this
        function get_array_datatype_d15(buffer) result(datatype)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            !                   1 2 3 4 5 6 7 8 9 A B C D E F
            type(MPI_Datatype) :: datatype
            datatype = get_element_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
        end function
#endif ! ENABLE_15D_ARRAYS
#endif ! ENABLE_8D_ARRAYS

#endif ! ENABLE_SELECT_RANK

end module gb_array_datatype
