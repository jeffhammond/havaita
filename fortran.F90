module ok
    contains
    subroutine gen(buffer)
        use iso_fortran_env
        implicit none
        class(*) :: buffer
        select type(buffer)
            type is (real)
                print*,'REAL'
            type is (double precision)
                print*,'DP'
        end select
    end subroutine
end module

program fortran
    use ok
    implicit none
    double precision dp
    print*,kind(dp)
    call gen(dp)
end program
