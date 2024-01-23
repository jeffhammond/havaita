# MPI Fortran type inference

This code shows how to write a modern Fortran interface to MPI which infers arguments that are already
contained in the Fortran type itself, such as element type and count, and renders optional arguments when
context is sufficient.  For example, a root-less reduction maps to `MPI_Allreduce` 
and omitting the communicator argument causes `MPI_COMM_WORLD` to be used.

Formerly referred to as "galaxy brain", it has been renamed to havaita, which means detect or perceive in Finnish.


![5vwsez](https://user-images.githubusercontent.com/406118/143855974-32af3b31-49a2-4dc9-93b3-d14dabde4c8a.jpg)
