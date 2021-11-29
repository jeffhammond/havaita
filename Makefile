FC=mpifort
FFLAGS=-std=f2008 -Wall

all: gb.mod

gb.mod: gb.F90
	$(FC) $(FFLAGS) -c $<

clean:
	-rm -f gb.o
	-rm -f gb.mod
