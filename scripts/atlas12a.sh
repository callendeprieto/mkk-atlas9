#!/bin/csh -fv
set MOD_DIR = ./
set ODF = /home/bonifacio/kurucz/odf/
set LIN = ../lines/
set EXE = ../bin/
set SEL = sellin.bin
ln -s ${ODF}molecules.dat fort.2
ln -s ${MOD_DIR}inmod.mod fort.3
ln -s ${LIN}/lowlines.bin fort.11
ln -s ${LIN}diatomicsiwl.bin fort.31
ln -s ${LIN}schwenke.bin fort.41 
ln -s ${LIN}h2ofast.bin fort.51
date
${EXE}atlas12.exe<<EOF>a12a.out
MOLECULES ON
READ MOLECULES
READ PUNCH
READ LINES
CONVECTION OVER 1.25 0 30
ITERATIONS 1 PRINT 1 PUNCH 0
BEGIN
END
EOF
date
mv fort.12 sellin.bin
rm fort.*

