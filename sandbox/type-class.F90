module d

    interface get
        module procedure get_type
        !module procedure get_class
    end interface

    contains

        function get_type(buffer) result(d)
            type(*), dimension(..), intent(in) :: buffer
            integer :: d
            d = 0
        end function

        function get_class(buffer) result(d)
            class(*), dimension(..), intent(in) :: buffer
            integer :: d
            d = 1
        end function

end module d

module m
    use iso_fortran_env
    implicit none

    interface gb_bcast
        module procedure gb_bcast_standard
        module procedure gb_bcast_inferred
    end interface

    contains

        subroutine gb_bcast_standard(buffer,c)
            use d
            type(*), dimension(..), intent(in) :: buffer
            integer, intent(in) :: c
            print*,get(buffer),c
        end subroutine

        subroutine gb_bcast_inferred(buffer)
            use d
            class(*), dimension(..), intent(in) :: buffer
            integer :: c
            c = size(buffer)
            print*,get(buffer),c
        end subroutine

end module m

program p


end program p
