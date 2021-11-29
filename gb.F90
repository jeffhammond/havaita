module gb
    use iso_fortran_env
    use mpi_f08
    implicit none

    interface gb_bcast
        module procedure gb_bcast_standard
        module procedure gb_bcast_r64_inferred
    end interface

    contains

        !function get_mpi_datatype(element) result(datatype)
        !    use mpi_f08
        !    TYPE(*) :: element
        !    TYPE(MPI_Datatype) :: datatype
        !    !if (same_type_as(element,real(0,kind=REAL64))) then
        !    !if (extends_type_of(element,real(0,kind=REAL64))) then
        !    !    datatype = MPI_DOUBLE_PRECISION
        !    !    return
        !    !endif
        !    select type(element)
        !      type is ( real(kind=REAL64) )
        !    end select
        !end function

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
            real(kind=REAL64), DIMENSION(..) :: buffer
            INTEGER, INTENT(IN) :: root
            TYPE(MPI_Comm), INTENT(IN) :: comm
            INTEGER, OPTIONAL, INTENT(OUT) :: ierror
            if (.not.is_contiguous(buffer)) then
                write(ERROR_UNIT,'(a59)') 'Galaxy Brain Failed: only contigous buffers are supported!'
                if (present(ierror)) then
                    ierror = MPI_ERR_BUFFER
                    return
                else
                    call MPI_Abort(MPI_COMM_WORLD,size(shape(buffer)))
                endif
            endif
            if (present(ierror)) then
                call MPI_Bcast(buffer, size(buffer), MPI_DOUBLE_PRECISION, root, comm, ierror)
            else
                call MPI_Bcast(buffer, size(buffer), MPI_DOUBLE_PRECISION, root, comm)
            endif
        end subroutine


end module gb
