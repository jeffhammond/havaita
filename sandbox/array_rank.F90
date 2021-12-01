module gb_array_rank
    use iso_fortran_env
    implicit none

    interface get_array_rank
        module procedure get_array_d0
        module procedure get_array_d1
        module procedure get_array_d2
        module procedure get_array_d3
        module procedure get_array_d4
        module procedure get_array_d5
        module procedure get_array_d6
        module procedure get_array_d7
#if ENABLE_FORTRAN2008_ARRAYS
        module procedure get_array_d8
        module procedure get_array_d9
        module procedure get_array_d10
        module procedure get_array_d11
        module procedure get_array_d12
        module procedure get_array_d13
        module procedure get_array_d14
#if ENABLE_15D_ARRAYS
        module procedure get_array_d15
#endif
#endif
    end interface

    contains

        function get_array_d0(buffer) result(d)
            class(*), intent(in) :: buffer
            integer :: d
            d = 0
        end function

        function get_array_d1(buffer) result(d)
            class(*), dimension(:), intent(in) :: buffer
            integer :: d
            d = 1
        end function

        function get_array_d2(buffer) result(d)
            class(*), dimension(:,:), intent(in) :: buffer
            integer :: d
            d = 2
        end function

        function get_array_d3(buffer) result(d)
            class(*), dimension(:,:,:), intent(in) :: buffer
            integer :: d
            d = 3
        end function

        function get_array_d4(buffer) result(d)
            class(*), dimension(:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 4
        end function

        function get_array_d5(buffer) result(d)
            class(*), dimension(:,:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 5
        end function

        function get_array_d6(buffer) result(d)
            class(*), dimension(:,:,:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 6
        end function

        function get_array_d7(buffer) result(d)
            class(*), dimension(:,:,:,:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 7
        end function

#if ENABLE_FORTRAN2008_ARRAYS
        function get_array_d8(buffer) result(d)
            class(*), dimension(:,:,:,:,:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 8
        end function

        function get_array_d9(buffer) result(d)
            class(*), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 9
        end function

        function get_array_d10(buffer) result(d)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 10
        end function

        function get_array_d11(buffer) result(d)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 11
        end function

        function get_array_d12(buffer) result(d)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 12
        end function

        function get_array_d13(buffer) result(d)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 13
        end function

        function get_array_d14(buffer) result(d)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            integer :: d
            d = 14
        end function

#if ENABLE_15D_ARRAYS
        ! GCC 11 does not like this
        function get_array_d15(buffer) result(d)
            class(*), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: buffer
            !                   1 2 3 4 5 6 7 8 9 A B C D E F
            integer :: d
            d = 15
        end function
#endif
#endif

end module gb_array_rank
