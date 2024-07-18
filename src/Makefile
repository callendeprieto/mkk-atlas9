#FC = /opt/intel/fc/10.1.015/bin/ifort
#FC= /opt/intel/2016.0.1/compilers_and_libraries_2016.1.150/linux/bin/intel64/ifort
FC=ifort
FFLAGS = -c -save -1 -tpp7 -w -V -WB 
LDFLAGS = -Vaxlib -static -zero
SIMPLE_FLAGS = -w95 -save -V -tpp7 -static -zero

all: atlas9 atlas9old atlas12 synthe width molec odf zero install

zero: atlas9mem_zero.for
	$(FC) $(FFLAGS) -o atlas9mem_zero.o atlas9mem_zero.for
	$(FC) $(LDFLAGS) -o atlas9mem_zero.exe   atlas9mem_zero.o

atlas9:	atlas9mem_newodf.for
	$(FC) $(FFLAGS) -o atlas9mem_newodf.o atlas9mem_newodf.for
	$(FC) $(LDFLAGS) -o atlas9mem_newodf.exe   atlas9mem_newodf.o 

atlas9old:	atlas9mem_oldodf.for
	$(FC) $(FFLAGS) -o atlas9mem_oldodf.o atlas9mem_oldodf.for
	$(FC) $(LDFLAGS) -o atlas9mem_oldodf.exe   atlas9mem_oldodf.o 
	
atlas12: atlas12.for
	$(FC) $(FFLAGS) -o atlas12.o atlas12.for
	$(FC) $(LDFLAGS) -o atlas12.exe atlas12.o

synthe:	atlas7lib.for xnfpelsyn.for synbeg.for  rline2.for synthe.for  spectrv.for syntoascanga.for broaden.for rotate.for
	$(FC) $(FFLAGS)  -o atlas7lib.o  atlas7lib.for
	$(FC) $(FFLAGS) -o xnfpelsyn.o xnfpelsyn.for
	$(FC) $(LDFLAGS) -o xnfpelsyn.exe xnfpelsyn.o atlas7lib.o
	$(FC) $(FFLAGS)  -o synbeg.o synbeg.for
	$(FC) $(LDFLAGS)  -o synbeg.exe synbeg.o
	$(FC) $(FFLAGS)   -o rline2.o rline2.for
	$(FC) $(LDFLAGS)  -o rline2.exe rline2.o
	$(FC) $(FFLAGS)  -o synthe.o synthe.for
	$(FC) $(LDFLAGS) -o synthe.exe synthe.o
	$(FC) $(FFLAGS) -o spectrv.o spectrv.for
	$(FC) $(LDFLAGS) -o spectrv.exe spectrv.o atlas7lib.o
	$(FC) $(FFLAGS) -o syntoascanga.o syntoascanga.for
	$(FC) $(LDFLAGS)  -o syntoascanga.exe syntoascanga.o
	$(FC) $(FFLAGS) -o readoh.o readoh.for
	$(FC) $(LDFLAGS)  -o readoh.exe readoh.o
	$(FC) $(FFLAGS) -o rmolecasc.o rmolecasc.for
	$(FC) $(LDFLAGS)  -o rmolecasc.exe rmolecasc.o
	$(FC) $(FFLAGS) -o broaden.o broaden.for
	$(FC) $(LDFLAGS) -o broaden.exe broaden.o
	$(FC) $(FFLAGS) -o rotate.o rotate.for
	$(FC) $(LDFLAGS) -o rotate.exe rotate.o

width:	width9.for atlas9lib_newodf.for
	$(FC) $(FFLAGS) -o atlas9lib_newodf.o atlas9lib_newodf.for
	$(FC) $(FFLAGS) -o width9.o width9.for
	$(FC) $(LDFLAGS) -o width9.exe width9.o atlas9lib_newodf.o 

molec:	readoh.for rmolecasc.for
	$(FC) $(FFLAGS) -o readoh.o readoh.for
	$(FC) $(LDFLAGS)  -o readoh.exe readoh.o
	$(FC) $(FFLAGS) -o rmolecasc.o rmolecasc.for
	$(FC) $(LDFLAGS)  -o rmolecasc.exe rmolecasc.o
	
odf: bigodfasctobin.for
	$(FC) $(SIMPLE_FLAGS) -o bigodfasctobin.exe bigodfasctobin.for
	


install:	
	/bin/mv *.exe ../bin
	/bin/rm -f *.o

