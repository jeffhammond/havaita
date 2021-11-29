FC=mpifort
FFLAGS=-std=f2018 -Wall

all: test

#gb.mod: gb.F90
#	$(FC) $(FFLAGS) -c $<

gb.o: gb.F90
	$(FC) $(FFLAGS) -c $<

test: test.F90 gb.o
	$(FC) $(FFLAGS) $^ -o test

clean:
	-rm -f gb.o
	-rm -f gb.mod
	-rm -f test
