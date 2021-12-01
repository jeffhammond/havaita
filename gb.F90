module gb
    use iso_fortran_env
    use mpi_f08
    use gb_util
    use gb_array_datatype
    implicit none

    interface gb_bcast
        module procedure gb_bcast_standard
        module procedure gb_bcast_inferred
    end interface

    interface gb_reduce
        module procedure gb_reduce_standard
        module procedure gb_reduce_inferred
    end interface

    interface gb_sendrecv
        module procedure gb_sendrecv_standard
        module procedure gb_sendrecv_inferred
    end interface

    interface gb_send
        module procedure gb_send_standard
        module procedure gb_send_inferred
    end interface

    interface gb_recv
        module procedure gb_recv_standard
        module procedure gb_recv_inferred
    end interface

    interface gb_isend
        module procedure gb_isend_standard
        module procedure gb_isend_inferred
    end interface

    interface gb_irecv
        module procedure gb_irecv_standard
        module procedure gb_irecv_inferred
    end interface

    contains

        ! MPI standard direct wrapper
        subroutine gb_bcast_standard(buffer, count, datatype, root, comm, ierror)
#ifdef __PGI
            class(*), dimension(..) :: buffer
#else
            type(*), dimension(..) :: buffer
#endif
            integer, intent(in) :: count, root
            type(MPI_Datatype), intent(in) :: datatype
            type(MPI_Comm), intent(in) :: comm
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call mpi_bcast(buffer, count, datatype, root, comm, ierror)
            else
                call mpi_bcast(buffer, count, datatype, root, comm)
            endif
        end subroutine

        subroutine gb_bcast_inferred(buffer, root, comm, ierror)
            class(*), dimension(..) :: buffer
            integer, intent(in), optional :: root
            type(MPI_Comm), intent(in), optional :: comm
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call contiguous_buffer_check(buffer,ierror)
            else
                call contiguous_buffer_check(buffer)
            endif
            block
                integer :: xroot
                type(MPI_Comm) :: xcomm
                type(MPI_Datatype) :: datatype
                if (present(comm)) then
                    xcomm = comm
                else
                    xcomm = MPI_COMM_WORLD
                endif
                if (present(root)) then
                    xroot = root
                else
                    xroot = 0
                endif
                datatype = get_array_datatype(buffer)
                if (present(ierror)) then
                    call MPI_Bcast(buffer, size(buffer), datatype, xroot, xcomm, ierror)
                else
                    call MPI_Bcast(buffer, size(buffer), datatype, xroot, xcomm)
                endif
            end block
        end subroutine

        ! MPI standard direct wrapper
        subroutine gb_reduce_standard(sendbuf, recvbuf, count, datatype, op, root, comm, ierror)
#ifdef __PGI
            class(*), dimension(..), intent(in) :: sendbuf
            class(*), dimension(..)             :: recvbuf
#else
            type(*), dimension(..), intent(in) :: sendbuf
            type(*), dimension(..)             :: recvbuf
#endif
            integer, intent(in) :: count
            integer, intent(in), optional :: root
            type(MPI_Datatype), intent(in) :: datatype
            type(MPI_Op), intent(in) :: op
            type(MPI_Comm), intent(in) :: comm
            integer, optional, intent(out) :: ierror
            if (present(root)) then
                if (present(ierror)) then
                    call mpi_reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierror)
                else
                    call mpi_reduce(sendbuf, recvbuf, count, datatype, op, root, comm)
                endif
            else
                if (present(ierror)) then
                    call mpi_allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierror)
                else
                    call mpi_allreduce(sendbuf, recvbuf, count, datatype, op, comm)
                endif
            endif
        end subroutine

        subroutine gb_reduce_inferred(sendbuf, recvbuf, op, root, comm, ierror)
            class(*), dimension(..), intent(in), optional :: sendbuf
            class(*), dimension(..)                       :: recvbuf
            type(MPI_Op), intent(in), optional :: op
            integer, intent(in), optional :: root
            type(MPI_Comm), intent(in), optional :: comm
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                if (present(sendbuf)) then
                    call contiguous_buffer_check(sendbuf,ierror)
                endif
                call contiguous_buffer_check(recvbuf,ierror)
            else
                if (present(sendbuf)) then
                    call contiguous_buffer_check(sendbuf)
                endif
                call contiguous_buffer_check(recvbuf)
            endif
            block
                type(MPI_Op) :: xop
                type(MPI_Comm) :: xcomm
                type(MPI_Status) :: xstatus
                integer :: count
                type(MPI_Datatype) :: datatype
                count = size(recvbuf)
                datatype = get_array_datatype(recvbuf)
                if (present(op)) then
                    xop = op
                else
                    if (datatype.eq.MPI_LOGICAL) then
                        xop = MPI_LAND
                    else
                        xop = MPI_SUM
                    endif
                endif
                if (present(comm)) then
                    xcomm = comm
                else
                    xcomm = MPI_COMM_WORLD
                endif
                if (present(root)) then
                    if (present(ierror)) then
                        if (present(sendbuf)) then
                            call mpi_reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierror)
                        else
                            call mpi_reduce(MPI_IN_PLACE, recvbuf, count, datatype, op, root, comm, ierror)
                        endif
                    else
                        if (present(sendbuf)) then
                            call mpi_reduce(sendbuf, recvbuf, count, datatype, op, root, comm)
                        else
                            call mpi_reduce(MPI_IN_PLACE, recvbuf, count, datatype, op, root, comm)
                        endif
                    endif
                else ! no root => allreduce
                    if (present(ierror)) then
                        if (present(sendbuf)) then
                            call mpi_allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierror)
                        else
                            call mpi_allreduce(MPI_IN_PLACE, recvbuf, count, datatype, op, comm, ierror)
                        endif
                    else
                        if (present(sendbuf)) then
                            call mpi_allreduce(sendbuf, recvbuf, count, datatype, op, comm)
                        else
                            call mpi_allreduce(MPI_IN_PLACE, recvbuf, count, datatype, op, comm)
                        endif
                    endif
                endif
            end block
        end subroutine

        ! MPI standard direct wrapper
        subroutine gb_sendrecv_standard(sendbuf, sendcount, sendtype, dest, sendtag,        &
                                        recvbuf, recvcount, recvtype, source, recvtag,      &
                                        comm, status, ierror)
#ifdef __PGI
            class(*), dimension(..), intent(in) :: sendbuf
            class(*), dimension(..)             :: recvbuf
#else
            type(*), dimension(..), intent(in) :: sendbuf
            type(*), dimension(..)             :: recvbuf
#endif
            integer, intent(in) :: sendcount, dest, sendtag, recvcount, source, recvtag
            type(MPI_Datatype), intent(in) :: sendtype, recvtype
            type(MPI_Comm), intent(in) :: comm
            type(MPI_Status) :: status
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call mpi_sendrecv(sendbuf, sendcount, sendtype, dest, sendtag,      &
                                  recvbuf, recvcount, recvtype, source, recvtag,    &
                                  comm, status, ierror)
            else
                call mpi_sendrecv(sendbuf, sendcount, sendtype, dest, sendtag,      &
                                  recvbuf, recvcount, recvtype, source, recvtag,    &
                                  comm, status)
            endif
        end subroutine

        subroutine gb_sendrecv_inferred(sendbuf, dest, sendtag,     &
                                        recvbuf, source, recvtag,   &
                                        comm, status, ierror)
            class(*), dimension(..), intent(in) :: sendbuf
            class(*), dimension(..)             :: recvbuf
            integer, intent(in) :: dest, sendtag, source, recvtag
            type(MPI_Comm), intent(in), optional :: comm
            type(MPI_Status), optional :: status
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call contiguous_buffer_check(sendbuf,ierror)
                call contiguous_buffer_check(recvbuf,ierror)
            else
                call contiguous_buffer_check(sendbuf)
                call contiguous_buffer_check(recvbuf)
            endif
            block
                type(MPI_Comm) :: xcomm
                type(MPI_Status) :: xstatus
                type(MPI_Datatype) :: sendtype, recvtype
                if (present(comm)) then
                    xcomm = comm
                else
                    xcomm = MPI_COMM_WORLD
                endif
                if (present(status)) then
                    xstatus = status
                else
                    xstatus = MPI_STATUS_IGNORE
                endif
                sendtype = get_array_datatype(sendbuf)
                recvtype = get_array_datatype(recvbuf)
                if (present(ierror)) then
                    call mpi_sendrecv(sendbuf, size(sendbuf), sendtype, dest, sendtag,      &
                                      recvbuf, size(recvbuf), recvtype, source, recvtag,    &
                                      xcomm, xstatus, ierror)
                else
                    call mpi_sendrecv(sendbuf, size(sendbuf), sendtype, dest, sendtag,      &
                                      recvbuf, size(recvbuf), recvtype, source, recvtag,    &
                                      xcomm, xstatus)
                endif
            end block
        end subroutine

        ! MPI standard direct wrapper
        subroutine gb_send_standard(buf, count, datatype, dest, tag, comm, ierror)
#ifdef __PGI
            class(*), dimension(..), intent(in) :: buf
#else
            type(*), dimension(..), intent(in) :: buf
#endif
            integer, intent(in) :: count, dest, tag
            type(MPI_Datatype), intent(in) :: datatype
            type(MPI_Comm), intent(in) :: comm
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call mpi_send(buf, count, datatype, dest, tag, comm, ierror)
            else
                call mpi_send(buf, count, datatype, dest, tag, comm)
            endif
        end subroutine

        subroutine gb_send_inferred(buf, dest, tag, comm, ierror)
            class(*), dimension(..), intent(in) :: buf
            integer, intent(in) :: dest, tag
            type(MPI_Comm), intent(in), optional :: comm
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call contiguous_buffer_check(buf,ierror)
            else
                call contiguous_buffer_check(buf)
            endif
            block
                type(MPI_Comm) :: xcomm
                type(MPI_Datatype) :: datatype
                if (present(comm)) then
                    xcomm = comm
                else
                    xcomm = MPI_COMM_WORLD
                endif
                datatype = get_array_datatype(buf)
                if (present(ierror)) then
                    call mpi_send(buf, size(buf), datatype, dest, tag, xcomm, ierror)
                else
                    call mpi_send(buf, size(buf), datatype, dest, tag, xcomm)
                endif
            end block
        end subroutine

        ! MPI standard direct wrapper
        subroutine gb_recv_standard(buf, count, datatype, source, tag, comm, status, ierror)
#ifdef __PGI
            class(*), dimension(..), intent(in) :: buf
#else
            type(*), dimension(..), intent(in) :: buf
#endif
            integer, intent(in) :: count, source, tag
            type(MPI_Datatype), intent(in) :: datatype
            type(MPI_Comm), intent(in) :: comm
            type(MPI_Status) :: status
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call mpi_recv(buf, count, datatype, source, tag, comm, status, ierror)
            else
                call mpi_recv(buf, count, datatype, source, tag, comm, status)
            endif
        end subroutine

        subroutine gb_recv_inferred(buf, source, tag, comm, status, ierror)
            class(*), dimension(..) :: buf
            integer, intent(in) :: source, tag
            type(MPI_Comm), intent(in), optional :: comm
            type(MPI_Status), optional :: status
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call contiguous_buffer_check(buf,ierror)
            else
                call contiguous_buffer_check(buf)
            endif
            block
                type(MPI_Comm) :: xcomm
                type(MPI_Status) :: xstatus
                type(MPI_Datatype) :: datatype
                if (present(comm)) then
                    xcomm = comm
                else
                    xcomm = MPI_COMM_WORLD
                endif
                if (present(status)) then
                    xstatus = status
                else
                    xstatus = MPI_STATUS_IGNORE
                endif
                datatype = get_array_datatype(buf)
                if (present(ierror)) then
                    call mpi_recv(buf, size(buf), datatype, source, tag, xcomm, xstatus, ierror)
                else
                    call mpi_recv(buf, size(buf), datatype, source, tag, xcomm, xstatus)
                endif
            end block
        end subroutine

        ! MPI standard direct wrapper
        subroutine gb_isend_standard(buf, count, datatype, dest, tag, comm, request, ierror)
#ifdef __PGI
            class(*), dimension(..), asynchronous, intent(in) :: buf
#else
            type(*), dimension(..), asynchronous, intent(in) :: buf
#endif
            integer, intent(in) :: count, dest, tag
            type(MPI_Datatype), intent(in) :: datatype
            type(MPI_Comm), intent(in) :: comm
            type(MPI_Request), intent(out) :: request
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call mpi_isend(buf, count, datatype, dest, tag, comm, request, ierror)
            else
                call mpi_isend(buf, count, datatype, dest, tag, comm, request)
            endif
        end subroutine

        subroutine gb_isend_inferred(buf, dest, tag, comm, request, ierror)
            class(*), dimension(..), asynchronous, intent(in) :: buf
            integer, intent(in) :: dest, tag
            type(MPI_Comm), intent(in), optional :: comm
            type(MPI_Request), intent(out) :: request
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call contiguous_buffer_check(buf,ierror)
            else
                call contiguous_buffer_check(buf)
            endif
            block
                type(MPI_Comm) :: xcomm
                type(MPI_Datatype) :: datatype
                if (present(comm)) then
                    xcomm = comm
                else
                    xcomm = MPI_COMM_WORLD
                endif
                datatype = get_array_datatype(buf)
                if (present(ierror)) then
                    call mpi_isend(buf, size(buf), datatype, dest, tag, xcomm, request, ierror)
                else
                    call mpi_isend(buf, size(buf), datatype, dest, tag, xcomm, request)
                endif
            end block
        end subroutine

        ! MPI standard direct wrapper
        subroutine gb_irecv_standard(buf, count, datatype, source, tag, comm, request, ierror)
#ifdef __PGI
            class(*), dimension(..), asynchronous, intent(in) :: buf
#else
            type(*), dimension(..), asynchronous, intent(in) :: buf
#endif
            integer, intent(in) :: count, source, tag
            type(MPI_Datatype), intent(in) :: datatype
            type(MPI_Comm), intent(in) :: comm
            type(MPI_Request), intent(out) :: request
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call mpi_irecv(buf, count, datatype, source, tag, comm, request, ierror)
            else
                call mpi_irecv(buf, count, datatype, source, tag, comm, request)
            endif
        end subroutine

        subroutine gb_irecv_inferred(buf, source, tag, comm, request, ierror)
            class(*), dimension(..), asynchronous :: buf
            integer, intent(in) :: source, tag
            type(MPI_Comm), intent(in), optional :: comm
            type(MPI_Request), intent(out) :: request
            integer, optional, intent(out) :: ierror
            if (present(ierror)) then
                call contiguous_buffer_check(buf,ierror)
            else
                call contiguous_buffer_check(buf)
            endif
            block
                type(MPI_Comm) :: xcomm
                type(MPI_Datatype) :: datatype
                if (present(comm)) then
                    xcomm = comm
                else
                    xcomm = MPI_COMM_WORLD
                endif
                datatype = get_array_datatype(buf)
                if (present(ierror)) then
                    call mpi_irecv(buf, size(buf), datatype, source, tag, xcomm, request, ierror)
                else
                    call mpi_irecv(buf, size(buf), datatype, source, tag, xcomm, request)
                endif
            end block
        end subroutine

end module gb
