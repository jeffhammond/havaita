FC=mpifort
FFLAGS=-g -Os -std=f2018

all: test

%.o: %.F90
	$(FC) $(FFLAGS) -c $^ -o $@

%.x: %.o
	$(FC) $(FFLAGS) $^ -o $@

#gb.mod: gb.F90
#	$(FC) $(FFLAGS) -c $<

gb_array_rank.o: gb_array_rank.F90
	$(FC) $(FFLAGS) -c $<

gb_util.o: gb_util.F90 gb_array_rank.o
	$(FC) $(FFLAGS) -c $<

gb.o: gb.F90 gb_util.o
	$(FC) $(FFLAGS) -c $<

test: test.F90 gb.o gb_util.o gb_array_rank.o
	$(FC) $(FFLAGS) $^ -o $@

clean:
	-rm -f *.o
	-rm -f *.x
	-rm -f *.mod
	-rm -f test
	-rm -rf *.dSYM
