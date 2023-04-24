cp sgr927_abucalc.in fort.001
abucalc.exe
cp fort.003 sgr927_abucalc.out
less sgr927_abucalc.out
