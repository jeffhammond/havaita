module m
  contains
    subroutine foo(buffer)
        implicit none
        class(*), dimension(..) :: buffer
        print*,'A',size(buffer)
        select rank (buffer)
            rank(1)
                print*,'B',size(buffer,1)
            rank(2)
                print*,'C',size(buffer,1)
                print*,'D',size(buffer,2)
        end select
    end subroutine

    subroutine bar(buffer)
        implicit none
        class(*), dimension(..) :: buffer
        print*,'E',shape(buffer)
    end subroutine

    subroutine pr2(buffer)
        implicit none
        integer, dimension(:,:) :: buffer
        print*,'F',buffer(1,1)
    end subroutine
end module

program p
    use m
    integer, dimension(-10:-5, -10:-5) :: a
    a = 77
    call foo(a)
    call bar(a)
    call pr2(a)
end program
