FC=mpifort
FFLAGS=

all: test

%.o: %.F90
	$(FC) $(FFLAGS) -c $^ -o $@

%.x: %.o
	$(FC) $(FFLAGS) $^ -o $@

gb_element_datatype.o: gb_element_datatype.F90
	$(FC) $(FFLAGS) -c $<

gb_array_datatype.o: gb_array_datatype.F90 gb_element_datatype.o
	$(FC) $(FFLAGS) -c $<

gb_util.o: gb_util.F90 gb_array_datatype.o
	$(FC) $(FFLAGS) -c $<

gb.o: gb.F90 gb_util.o
	$(FC) $(FFLAGS) -c $<

test: test.F90 gb.o gb_util.o gb_array_datatype.o gb_element_datatype.o
	$(FC) $(FFLAGS) $^ -o $@

clean:
	-rm -f *.o
	-rm -f *.x
	-rm -f *.mod
	-rm -f test
	-rm -rf *.dSYM
