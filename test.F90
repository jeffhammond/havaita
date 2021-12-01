program test
  use gb
  implicit none
  integer :: me, np
  type(MPI_Comm) :: world
  world = MPI_COMM_WORLD

  call MPI_Init()
  call MPI_Comm_rank(world,me)
  call MPI_Comm_size(world,np)
  write(*,'(a11,i3,a4,i3)') 'Hello from ',me,' of ',np
  call MPI_Barrier(world)

  block
    real(kind=REAL64) :: a1(100)
    real(kind=REAL64) :: a2(10,10)
    real(kind=REAL64) :: a3(5,5,5)
    integer :: ierr

    ! 1D
    a1 = me
    call gb_bcast(buffer=a1,root=0,comm=world)
    if (any(a1 .ne. 0.0d0)) STOP
    a1 = me
    call gb_bcast(buffer=a1,root=0)
    if (any(a1 .ne. 0.0d0)) STOP
    a1 = me
    call gb_bcast(buffer=a1,comm=world)
    if (any(a1 .ne. 0.0d0)) STOP
    a1 = me
    call gb_bcast(buffer=a1)
    if (any(a1 .ne. 0.0d0)) STOP

    ! 2D
    a2 = me
    call gb_bcast(buffer=a2,root=0,comm=world)
    if (any(a2 .ne. 0.0d0)) STOP
    a2 = me
    call gb_bcast(buffer=a2,root=0)
    if (any(a2 .ne. 0.0d0)) STOP
    a2 = me
    call gb_bcast(buffer=a2,comm=world)
    if (any(a2 .ne. 0.0d0)) STOP
    a2 = me
    call gb_bcast(buffer=a2)
    if (any(a2 .ne. 0.0d0)) STOP

    ! 3D
    a3 = -me
    call gb_bcast(buffer=a3,root=0,comm=world)
    if (any(a3 .ne. 0.0d0)) STOP
    a3 = -me
    call gb_bcast(buffer=a3,root=0)
    if (any(a3 .ne. 0.0d0)) STOP
    a3 = -me
    call gb_bcast(buffer=a3,comm=world)
    if (any(a3 .ne. 0.0d0)) STOP
    a3 = -me
    call gb_bcast(buffer=a3)
    if (any(a3 .ne. 0.0d0)) STOP

    call MPI_Barrier(world)
    call gb_bcast(a2(1:5,1:5),0,world,ierr)
    write(*,*) 'gb_bcast XFAIL',ierr
  end block

  call MPI_Finalize()

end program
