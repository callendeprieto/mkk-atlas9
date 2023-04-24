FC = . /opt/intel_fc_80/bin/ifort
FFLAGS = -c -save -1 -tpp7 -w -V
LDFLAGS = -Vaxlib -static -zero
SIMPLE_FLAGS = -w95 -save -V -tpp7 -static -zero

all: prep_width abucalc testcv install

prep_width: prep_width_mod.for
	$(FC) $(SIMPLE_FLAGS) -o prep_width_mod.exe prep_width_mod.for
	
abucalc: abucalc.f
	$(FC) $(SIMPLE_FLAGS) -o abucalc.exe abucalc.f
	
testcv: testcv.for
	$(FC) $(SIMPLE_FLAGS) -o testcv.exe testcv.for
	
install:	
	mv *.exe ../bin
	rm -f *.o

