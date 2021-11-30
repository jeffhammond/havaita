FC=mpifort
FFLAGS=-g -Os -std=f2018

all: test

%.o: %.F90
	$(FC) $(FFLAGS) -c $^ -o $@

%.x: %.o
	$(FC) $(FFLAGS) $^ -o $@

#gb.mod: gb.F90
#	$(FC) $(FFLAGS) -c $<

gb.o: gb.F90
	$(FC) $(FFLAGS) -c $<

test: test.F90 gb.o
	$(FC) $(FFLAGS) $^ -o $@

clean:
	-rm -f *.o
	-rm -f *.x
	-rm -f *.mod
	-rm -f test
	-rm -rf *.dSYM
