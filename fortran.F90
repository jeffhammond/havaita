subroutine gen(buffer)
    use iso_fortran_env
    implicit none
    class(*) :: buffer
    select type(buffer)
        class is (real)
            print*,'REAL'
    end select
end subroutine

program fortran
    implicit none
    real*4 :: r4
    real*8 :: r8
    double precision dp
    print*,kind(r4)
    print*,kind(r8)
    print*,kind(dp)
end program
