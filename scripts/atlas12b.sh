#!/bin/csh -fv
set MOD_DIR = ./
set ODF = /home/bonifacio/kurucz/odf/
set LIN = ../lines/
set EXE = ../bin/
set SEL = sellin.bin
set OUT = am10t5000g300k2at12b.mod  


ln -s ${ODF}molecules.dat fort.2
ln -s ${MOD_DIR}inmod.mod fort.3
ln -s ${LIN}nltelines.bin  fort.19
ln -s ${SEL} fort.12
date
${EXE}atlas12.exe<<EOF>a12b.out
MOLECULES ON
READ MOLECULES
READ PUNCH
TITLE ATLAS12 
OPACITY ON LINES
OPACITY ON XLINES
CONVECTION OVER 1.25 0 30
ITERATIONS 15
PRINT 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1  
PUNCH 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
SCALE MODEL 72 -6.875 0.125 5000. 3.0
BEGIN 
PRINT 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1  
PUNCH 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
SCALE MODEL 72 -6.875 0.125 5000. 3.0
BEGIN 
END
EOF
mv fort.7  ${OUT}
date
rm fort.*
