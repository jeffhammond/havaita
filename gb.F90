module gb
    use iso_fortran_env
    use mpi_f08
    implicit none

    interface gb_bcast
        module procedure gb_bcast_standard
        module procedure gb_bcast_inferred
    end interface

    contains

        function get_mpi_datatype(element) result(datatype)
            use mpi_f08
            class(*) :: element
            type(MPI_Datatype) :: datatype
            select type(element)
                !    datatype = MPI_REAL
                type is ( real(kind=REAL32) )
                    datatype = MPI_REAL4
                type is ( real(kind=REAL64) )
                    datatype = MPI_REAL8
#ifdef ENABLE_128B_TYPES
                type is ( real(kind=REAL128) )
                    datatype = MPI_REAL16
#endif
                ! COMPLEX types
                type is ( complex(kind=REAL32) )
                    datatype = MPI_COMPLEX8
                type is ( complex(kind=REAL64) )
                    datatype = MPI_COMPLEX16
#ifdef ENABLE_128B_TYPES
                type is ( complex(kind=REAL128) )
                    datatype = MPI_COMPLEX32
#endif
                ! INTEGER types
                type is ( integer(kind=INT8) )
                    datatype = MPI_INTEGER1
                type is ( integer(kind=INT16) )
                    datatype = MPI_INTEGER2
                type is ( integer(kind=INT32) )
                    datatype = MPI_INTEGER4
                type is ( integer(kind=INT64) )
                    datatype = MPI_INTEGER8
#ifdef ENABLE_128B_TYPES
                type is ( integer(kind=INT128) )
                    datatype = MPI_INTEGER16
#endif
                ! OTHER types
                type is ( logical )
                    datatype = MPI_LOGICAL
                ! MPI types - overlaps with kind=INT32
                !type is ( integer(kind=MPI_ADDRESS_KIND) )
                !    datatype = MPI_AINT
                !type is ( integer(kind=MPI_OFFSET_KIND) )
                !    datatype = MPI_OFFSET
                !type is ( integer(kind=MPI_COUNT_KIND) )
                !    datatype = MPI_COUNT
            end select
        end function

        function get_mpi_datatype_array(buffer) result(datatype)
            use mpi_f08
            class(*), DIMENSION(..) :: buffer
            type(MPI_Datatype) :: datatype
            ! what happens here if the array bounds are e.g. [-10:-5]?
            select rank (buffer)
                rank(0)
                    datatype = get_mpi_datatype(buffer)
                rank(1)
                    datatype = get_mpi_datatype(buffer(1))
                rank(2)
                    datatype = get_mpi_datatype(buffer(1,1))
                rank(3)
                    datatype = get_mpi_datatype(buffer(1,1,1))
                rank(4)
                    datatype = get_mpi_datatype(buffer(1,1,1,1))
                rank(5)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1))
                rank(6)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1,1))
                rank(7)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1,1,1))
                rank(8)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1,1,1,1))
                rank(9)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1,1,1,1,1))
                rank(10)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1,1,1,1,1,1))
                rank(11)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1))
                rank(12)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1))
                rank(13)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1,1))
                rank(14)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1,1,1))
                rank(15)
                    datatype = get_mpi_datatype(buffer(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
            end select
        end function

        ! MPI standard direct wrapper
        subroutine gb_bcast_standard(buffer, count, datatype, root, comm, ierror)
            use mpi_f08
            type(*), dimension(..) :: buffer
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
            use mpi_f08
            class(*), dimension(..) :: buffer
            integer, intent(in), optional :: root
            type(MPI_Comm), intent(in), optional :: comm
            integer, optional, intent(out) :: ierror
            if (.not.MPI_SUBARRAYS_SUPPORTED) then
                if (.not.is_contiguous(buffer)) then
                    write(ERROR_UNIT,'(a59)') 'Galaxy Brain Failed: only contigous buffers are supported!'
                    if (present(ierror)) then
                        ierror = MPI_ERR_BUFFER
                        return
                    else
                        call MPI_Abort(MPI_COMM_WORLD,size(buffer))
                        STOP
                    endif
                endif
            endif
            block
                integer :: xroot
                type(MPI_Comm) :: xcomm
                type(MPI_Datatype) :: dt
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
                dt = get_mpi_datatype_array(buffer)
                if (present(ierror)) then
                    call MPI_Bcast(buffer, size(buffer), dt, xroot, xcomm, ierror)
                else
                    call MPI_Bcast(buffer, size(buffer), dt, xroot, xcomm)
                endif
            end block
        end subroutine

    end module gb
