program test
  use gb
  implicit none
  integer :: me, np, root
  type(MPI_Comm) :: world
  world = MPI_COMM_WORLD
  root = 0

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

    !call MPI_Barrier(world)
    !call gb_bcast(a2(1:5,1:5),0,world,ierr)
    !if (ierr.ne.MPI_SUCCESS) write(*,*) 'gb_bcast XFAIL',ierr
  end block

  block
    real(kind=REAL64) :: a(100)  , b(100)
    real :: ref
    integer :: ierr

    ref = (np-1.0d0) * (0.5d0*np)

    ! 1D
    a = me
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,root=0,comm=MPI_COMM_WORLD)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,comm=MPI_COMM_WORLD)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,root=0)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,root=0,comm=MPI_COMM_WORLD)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,root=0)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,comm=MPI_COMM_WORLD)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,root=0,comm=MPI_COMM_WORLD,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,comm=MPI_COMM_WORLD,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,root=0,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,root=0,comm=MPI_COMM_WORLD,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,root=0,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,comm=MPI_COMM_WORLD,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,ierror=ierr)
    if (any(b .ne. ref)) STOP

    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,root=0,comm=MPI_COMM_WORLD)
    if (me.eq.root .and. any(b .ne. ref)) print*,b,ref
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,comm=MPI_COMM_WORLD)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,root=0)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,root=0,comm=MPI_COMM_WORLD)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,root=0)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,comm=MPI_COMM_WORLD)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,root=0,comm=MPI_COMM_WORLD,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,comm=MPI_COMM_WORLD,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,root=0,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,root=0,comm=MPI_COMM_WORLD,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,root=0,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,comm=MPI_COMM_WORLD,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,ierror=ierr)
    if (any(b .ne. ref)) STOP
    write(*,*) 'gb_reduce 1D passed'
  end block

  block
    real(kind=REAL64) :: a(10,10)  , b(10,10)
    real :: ref
    integer :: ierr

    ref = (np-1.0d0) * (0.5d0*np)

    ! 1D
    a = me
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,root=0,comm=MPI_COMM_WORLD)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,comm=MPI_COMM_WORLD)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,root=0)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,root=0,comm=MPI_COMM_WORLD)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,root=0)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,comm=MPI_COMM_WORLD)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,root=0,comm=MPI_COMM_WORLD,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,comm=MPI_COMM_WORLD,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,root=0,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,root=0,comm=MPI_COMM_WORLD,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,root=0,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,comm=MPI_COMM_WORLD,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,op=MPI_SUM,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = 0
    call gb_reduce(sendbuf=a,recvbuf=b,ierror=ierr)
    if (any(b .ne. ref)) STOP

    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,root=0,comm=MPI_COMM_WORLD)
    if (me.eq.root .and. any(b .ne. ref)) print*,b,ref
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,comm=MPI_COMM_WORLD)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,root=0)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,root=0,comm=MPI_COMM_WORLD)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,root=0)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,comm=MPI_COMM_WORLD)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,root=0,comm=MPI_COMM_WORLD,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,comm=MPI_COMM_WORLD,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,root=0,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,root=0,comm=MPI_COMM_WORLD,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,root=0,ierror=ierr)
    if (me.eq.root .and. any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,comm=MPI_COMM_WORLD,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,op=MPI_SUM,ierror=ierr)
    if (any(b .ne. ref)) STOP
    b = me
    call gb_reduce(recvbuf=b,ierror=ierr)
    if (any(b .ne. ref)) STOP
    write(*,*) 'gb_reduce 2D passed'
  end block

  call MPI_Finalize()

end program
