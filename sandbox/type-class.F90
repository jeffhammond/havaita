module d

    interface get
#if defined(__PGI)
        !module procedure get_type
        module procedure get_class
#endif
#if defined(__INTEL_COMPILER)
        !module procedure get_type
        module procedure get_class
#endif
#if defined(__GNUC__)
        module procedure get_type
        !module procedure get_class
#endif
    end interface

    contains

#if defined(__GNUC__)
        function get_type(buffer) result(d)
            type(*), dimension(..), intent(in) :: buffer
            integer :: d
            d = 0
        end function
#endif

#if defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__PGI)
        function get_class(buffer) result(d)
            class(*), dimension(..), intent(in) :: buffer
            integer :: d
            d = 1
        end function
#endif

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
#if defined(__PGI) || defined(__INTEL_COMPILER)
            class(*), dimension(..), intent(in) :: buffer
#else
            type(*), dimension(..), intent(in) :: buffer
#endif
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
