rm -f for00*
rm -f testcv.out
ln -s t4892g230m05a0vt20_l.dat for001.dat
cat <<EOF > for008.dat
15,4
EOF
/usr/local/kurucz/bin/testcv.exe
more testcv.out
