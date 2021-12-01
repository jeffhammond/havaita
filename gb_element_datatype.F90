module gb_element_datatype
    use iso_fortran_env
    use mpi_f08
    implicit none

    contains

        function get_element_datatype(element) result(datatype)
            class(*), intent(in) :: element
            type(MPI_Datatype) :: datatype
            select type(element)
                !    datatype = MPI_REAL
                class is ( real(kind=REAL32) )
                    datatype = MPI_REAL4
                class is ( real(kind=REAL64) )
                    datatype = MPI_REAL8
#ifdef ENABLE_128B_TYPES
                class is ( real(kind=REAL128) )
                    datatype = MPI_REAL16
#endif
                ! COMPLEX types
                class is ( complex(kind=REAL32) )
                    datatype = MPI_COMPLEX8
                class is ( complex(kind=REAL64) )
                    datatype = MPI_COMPLEX16
#ifdef ENABLE_128B_TYPES
                class is ( complex(kind=REAL128) )
                    datatype = MPI_COMPLEX32
#endif
                ! INTEGER types
                class is ( integer(kind=INT8) )
                    datatype = MPI_INTEGER1
                class is ( integer(kind=INT16) )
                    datatype = MPI_INTEGER2
                class is ( integer(kind=INT32) )
                    datatype = MPI_INTEGER4
                class is ( integer(kind=INT64) )
                    datatype = MPI_INTEGER8
#ifdef ENABLE_128B_TYPES
                class is ( integer(kind=INT128) )
                    datatype = MPI_INTEGER16
#endif
                ! OTHER types
                class is ( logical )
                    datatype = MPI_LOGICAL
                ! character is omitted because i cannot make it work
                ! MPI types - these overlap with integer types above
                !class is ( integer(kind=MPI_ADDRESS_KIND) )
                !    datatype = MPI_AINT
                !class is ( integer(kind=MPI_OFFSET_KIND) )
                !    datatype = MPI_OFFSET
                !class is ( integer(kind=MPI_COUNT_KIND) )
                !    datatype = MPI_COUNT
            end select
        end function

end module gb_element_datatype
