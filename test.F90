program test
  use gb
  implicit none
  integer :: me, np

  call MPI_Init()
  call MPI_Comm_rank(MPI_COMM_WORLD,me)
  call MPI_Comm_size(MPI_COMM_WORLD,np)
  write(*,'(a11,i3,a4,i3)') 'Hello from ',me,' of ',np
  call MPI_Barrier(MPI_COMM_WORLD)

  block
    real(kind=REAL64) :: a(100)
    real(kind=REAL64) :: b(10,10)
    integer :: ierr
    call gb_bcast(a,0,MPI_COMM_WORLD)
    call MPI_Barrier(MPI_COMM_WORLD)
    call gb_bcast(b,0,MPI_COMM_WORLD)
    call MPI_Barrier(MPI_COMM_WORLD)
    call gb_bcast(b(1:5,1:5),0,MPI_COMM_WORLD,ierr)
    write(*,*) 'gb_bcast XFAIL',ierr
  end block

  call MPI_Finalize()

end program
