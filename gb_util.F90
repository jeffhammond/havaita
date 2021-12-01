module gb_util
    use iso_fortran_env
    use mpi_f08
    implicit none

    contains

        subroutine contiguous_buffer_check(buffer,ierror)
            class(*), dimension(..), intent(in) :: buffer
            integer, optional, intent(out) :: ierror
            if (.not.MPI_SUBARRAYS_SUPPORTED) then
                if (.not.is_contiguous(buffer)) then
                    write(ERROR_UNIT,'(a59)') 'Galaxy Brain Failed: only contigous buffers are supported!'
                    if (present(ierror)) then
                        ierror = MPI_ERR_BUFFER
                        return
                    else
                        call MPI_Abort(MPI_COMM_WORLD,size(buffer))
                        stop
                    endif
                endif
            endif
        end subroutine

end module gb_util
