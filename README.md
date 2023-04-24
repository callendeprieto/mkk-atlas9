# mkk-atlas9
Compute Kurucz ATLAS9 model atmospheres using Luca Sbordone's version of ATLAS9

This repository includes the ATLAS9 code by Kurucz, adapted by Luca Sbordone (see Sbordone, Bonifacio & Castelli 2006), with minor modifications to compile with ifort version 2001.9.0 (part of the Intel oneAPI), and a couple of Perl scripts that facilitate creating new models.

1. Install

First download/clone the complete repo. The ATLAS9 sourcde code is in the src folder. The ODF folder contains pre-caclulated Opacity Distribution Functions. There is a documentation folder, and others with scripts and utilities. The wrapper for ATLAS9 (mkk) and an auxiliary code (kget) are in the perl folder. 

The ATLAS9 executables are in the bin folder. These have been compiled for a 64-bit Intel processor. If using a different architecture you will need to recompile ATLAS9 with ifort, which is now free as part of oneAPI. To recompile just enter the src folder and run 'make'. Here's an example on how to install ifort in ubuntu
https://gist.github.com/SomajitDey/aeb6eb4c8083185e06800e1ece4be1bd

You will need to copy mkk and kget from the perl folder to your home directory (e.g. /home/callende/perl), or anywhere you want in your path, and make them executable. Make sure this folder is in your path. Set an environmental variable KURUCZ to the path where you downloaded the repository. For example, using bash, if you downloaded the repo to /work/<yourusername>/kurucz and copied mkk and kget to your home then include in your .bashrc:
 
export KURUCZ=/work/<yourusername>/kurucz

export PATH=$PATH:/home/<yourusername>/perl

2. Test
 
Try building a solar model by running
 
mkk 5777. 4.437

 and you should get something like this

Best match for kap file is /work/callende/kurucz/ODF/NEW//kapp00.ros with a distance 0
 
Best match for big file is /work/callende/kurucz/ODF/NEW//p00big2.bdf with a distance 0
 
Best match for the starting model file is /home/callende/idl/idl_database/kurucz_models//ap00k2odfnew.dat with a distance 0
 
Best match for the starting model is 5750. 4.50000 with a distance 27.063
 
Died at /home/callende/perl/kget line 50, <INFILE> line 10573.
 
solar abundances are 0.92040 0.07834 -10.94 -10.64 -9.49 -3.52 -4.12 -3.21 -7.48 -3.96 -5.71 -4.46 -5.57 -4.49 -6.59 -4.71 -6.54 -5.64 -6.92 -5.68 -8.87 -7.02 -8.04 -6.37 -6.65 -4.54 -7.12 -5.79 -7.83 -7.44 -9.16 -8.63 -9.67 -8.63 -9.41 -8.73 -9.44 -9.07 -9.80 -9.44 -10.62 -10.12 -20.00 -10.20 -10.92 -10.35 -11.10 -10.27 -10.38 -10.04 -11.04 -9.80 -10.53 -9.87 -10.91 -9.91 -10.87 -10.46 -11.33 -10.54 -20.00 -11.03 -11.53 -10.92 -11.69 -10.90 -11.78 -11.11 -12.04 -10.96 -11.98 -11.16 -12.17 -10.93 -11.76 -10.59 -10.69 -10.24 -11.03 -10.91 -11.14 -10.09 -11.33 -20.00 -20.00 -20.00 -20.00 -20.00 -20.00 -11.95 -20.00 -12.54 -20.00 -20.00 -20.00 -20.00 -20.00 -20.00 -20.00
 
Chemical composition NH/Ntot=  0.92040 NHe/Ntot=  0.07834 Nz/Ntot= 0.00123
 
Chemical composition NH + NHe + Nz =  0.99997
 
Chemical composition --- NH adjusted to force (NH + NHe + Nz)/Ntot=1
 
Chemical composition NH/Ntot=  0.92043 NHe/Ntot=  0.07834 Nz/Ntot= 0.00123
 
Chemical composition NH + NHe + Nz =  1.00000
 
Computing a Kurucz model for Teff=5777. and logg=4.437

Running Atlas9 -- block 1 -- 15 iterations ...
 
 --- Model has not yet converged: max(errors)=8.304 1.663

 Running Atlas9 -- block 2 -- 15 iterations ...
 
 --- Model has not yet converged: max(errors)=1.948 1.285

 Running Atlas9 -- block 3 -- 15 iterations ...

 --- Model has not yet converged: max(errors)=2.09 1.157

 Running Atlas9 -- block 4 -- 15 iterations ...

 --- Model has not yet converged: max(errors)=1.934 1.15

 Running Atlas9 -- block 5 -- 15 iterations ...

 --- Model has not yet converged: max(errors)=1.773 1.145

 Running Atlas9 -- block 6 -- 15 iterations ...

 --- Model has not yet converged: max(errors)=1.554 1.143

 Running Atlas9 -- block 7 -- 15 iterations ...

 --- Model has not yet converged: max(errors)=1.36 1.147

 Running Atlas9 -- block 8 -- 15 iterations ...

 --- Model has not yet converged: max(errors)=1.199 1.146

 Running Atlas9 -- block 9 -- 15 iterations ...

 --- Model has not yet converged: max(errors)=1.073 1.146

 Running Atlas9 -- block 10 -- 15 iterations ...

 --- Convergence: max changes are 0.976 and 1.146 at depth 72

 Output to file k5777._4.437.5

 Output to file k5777._4.437.7

 Output to file k5777._4.437.13

 done

and three files:
 
 k5777._4.437.13  -> the model SED)

 k5777._4.437.7   -> the model atmosphere)

 k5777._4.437.5   -> the input file for ATLAS9
 
