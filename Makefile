FC=mpifort
FFLAGS=-std=f2018 -Wall

all: test

#gb.mod: gb.F90
#	$(FC) $(FFLAGS) -c $<

gb.o: gb.F90
	$(FC) $(FFLAGS) -c $<

pgi: pgi.F90
	$(FC) $(FFLAGS) $^ -o $@

fortran: fortran.F90
	$(FC) $(FFLAGS) $^ -o $@

test: test.F90 gb.o
	$(FC) $(FFLAGS) $^ -o $@

clean:
	-rm -f gb.o
	-rm -f gb.mod
	-rm -f test
