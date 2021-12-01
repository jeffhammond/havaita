FC:=ifort
FFLAGS:=

all: array.x pgi.o fortran.x type-class.x type.o

%.o: %.F90
	$(FC) $(FFLAGS) -c $^ -o $@

%.x: %.o
	$(FC) $(FFLAGS) $^ -o $@

clean:
	-rm -f *.o
	-rm -f *.x
	-rm -f *.mod
	-rm -rf *.dSYM
