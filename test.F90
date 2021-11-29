program test
  use gb
  implicit none
  integer :: me, np
  type(MPI_Comm), parameter :: world = MPI_COMM_WORLD

  call MPI_Init()
  call MPI_Comm_rank(world,me)
  call MPI_Comm_size(world,np)
  write(*,'(a11,i3,a4,i3)') 'Hello from ',me,' of ',np
  call MPI_Barrier(world)

  block
    real(kind=REAL64) :: a(100)
    real(kind=REAL64) :: b(10,10)
    integer :: ierr

    call gb_bcast(buffer=a,root=0,comm=world)
    call gb_bcast(buffer=a,root=0)
    call gb_bcast(buffer=a,comm=world)
    call gb_bcast(buffer=a)

    call gb_bcast(buffer=b,root=0,comm=world)
    call gb_bcast(buffer=b,root=0)
    call gb_bcast(buffer=b,comm=world)
    call gb_bcast(buffer=b)

    call MPI_Barrier(world)
    call gb_bcast(b(1:5,1:5),0,world,ierr)
    write(*,*) 'gb_bcast XFAIL',ierr
  end block

  call MPI_Finalize()

end program
