module m
  contains
    subroutine foo(buffer)
        implicit none
        class(*), dimension(..) :: buffer
        print*,'size:',size(buffer)
        select rank (buffer)
            rank(1)
                print*,'rank 1:',size(buffer,1)
            rank(2)
                print*,'rank 2:',size(buffer,1),size(buffer,2)
        end select
    end subroutine

    subroutine bar(buffer)
        implicit none
        class(*), dimension(..) :: buffer
        print*,'shape=',shape(buffer)
        print*,'contig=',is_contiguous(buffer)
    end subroutine

    subroutine pr2(buffer)
        implicit none
        integer, dimension(:,:) :: buffer
        print*,'element=',buffer(1,1)
    end subroutine
end module

program p
    use m
    integer, dimension(-10:-5, -10:-5) :: a
    a = 77

    call foo(a)
    call bar(a)
    !call pr2(a)

    call foo(a(-8:-6,-9:-7))
    call bar(a(-8:-6,-9:-7))
    !call pr2(a(-8:-6,-9:-7))

    call foo(a(:,-5:-5))
    call bar(a(:,-5:-5))
    call foo(a(-5:-5,:))
    call bar(a(-5:-5,:))

    call foo(reshape(a(:,-5:-5),[6]))
    call bar(reshape(a(:,-5:-5),[6]))
    call foo(reshape(a(-5:-5,:),[6]))
    call bar(reshape(a(-5:-5,:),[6]))
end program
