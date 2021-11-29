module gb
    use iso_fortran_env
    use mpi_f08
    implicit none

    interface gb_bcast
        module procedure gb_bcast_standard
        module procedure gb_bcast_r64_inferred
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
            select rank (buffer)
                rank(0)
                    datatype = get_mpi_datatype(buffer)
                rank(1)
                    datatype = get_mpi_datatype(buffer(1))
            end select
        end function

        ! MPI standard direct wrapper
        subroutine gb_bcast_standard(buffer, count, datatype, root, comm, ierror)
            use mpi_f08
            TYPE(*), DIMENSION(..) :: buffer
            INTEGER, INTENT(IN) :: count, root
            TYPE(MPI_Datatype), INTENT(IN) :: datatype
            TYPE(MPI_Comm), INTENT(IN) :: comm
            INTEGER, OPTIONAL, INTENT(OUT) :: ierror
            if (present(ierror)) then
                call mpi_bcast(buffer, count, datatype, root, comm, ierror)
            else
                call mpi_bcast(buffer, count, datatype, root, comm)
            endif
        end subroutine

        subroutine gb_bcast_r64_inferred(buffer, root, comm, ierror)
            use mpi_f08
            TYPE(*), DIMENSION(..) :: buffer
            INTEGER, INTENT(IN) :: root
            TYPE(MPI_Comm), INTENT(IN) :: comm
            INTEGER, OPTIONAL, INTENT(OUT) :: ierror
            TYPE(MPI_Datatype) :: dt
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
            if (present(ierror)) then
                call MPI_Bcast(buffer, size(buffer), MPI_DOUBLE_PRECISION, root, comm, ierror)
            else
                call MPI_Bcast(buffer, size(buffer), MPI_DOUBLE_PRECISION, root, comm)
            endif
        end subroutine

end module gb
