#!/usr/bin/tcsh
#$ASSIGN SYS$INPUT FOR005
ln -s p00big2.asc fort.1

./bigodfasctobin.exe

mv fort.2 p00big2.bdf
#$ASSIGN P00BIG2.asc FOR001
#$ASSIGN P00BIG2.bdf FOR002
#$RUN bigodfasctobin
