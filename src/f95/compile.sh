rm -f GetRingCurrent.exe
rm -f *.mod
rm -f *.o
rm -f *~
gfortran -c definitions.f95  #; rm definitions.mod
gfortran -c datatypes.f95    #; rm datatypes.mod
gfortran -c xyzfun.f95       #; rm xyzfun.mod
gfortran -c ringcurrents.f95 #; rm ringcurrents.mod

gfortran RingCurrentsP.f95 definitions.o datatypes.o xyzfun.o ringcurrents.o -o GetRingCurrent.exe
rm -f *.mod
rm -f *.o
rm -f *~