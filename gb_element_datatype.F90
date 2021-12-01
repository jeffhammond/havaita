module gb_element_datatype
    use iso_fortran_env
    use mpi_f08
    implicit none

    contains

        function get_element_datatype(element) result(datatype)
            class(*), intent(in) :: element
            type(MPI_Datatype) :: datatype
            select type(element)
                ! REAL types
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
                ! character is omitted because i cannot make it work
                ! MPI types - these overlap with integer types above
                !type is ( integer(kind=MPI_ADDRESS_KIND) )
                !    datatype = MPI_AINT
                !type is ( integer(kind=MPI_OFFSET_KIND) )
                !    datatype = MPI_OFFSET
                !type is ( integer(kind=MPI_COUNT_KIND) )
                !    datatype = MPI_COUNT
            end select
        end function

end module gb_element_datatype
