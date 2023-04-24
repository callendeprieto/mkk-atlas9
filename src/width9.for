      PROGRAM WIDTH9
C ************************************************************************
C
C     Linux port by L. Sbordone, P. Bonifacio and F. Castelli
C
C     -------------------------------------------------------
C
C     - March 2004: Initial Linux port by L.S. and P.B.
C
C     -------------------------------------------------------
C
C     Please aknowledge the use of this code by citing:
C
C     * Kurucz, R. 1993, ATLAS9 Stellar Atmosphere Programs and 2 km/s
C       grid. Kurucz CD-ROM No. 13. Cambridge, Mass.: Smithsonian Astrophysical
C       Observatory, 1993., 13
C
C     * Sbordone, L., Bonifacio, P., Castelli, F., & Kurucz, R. L. 2004a, Memorie
C       della Societa Astronomica Italiana Supplement, 5, 93
C
C     --------------------------------------------------------
C
C     For updates, documentation, utilities and needed files please refer to:
C     www.******.it
C
C ************************************************************************ 

c     revise 9mar93
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (kw=99)
      COMMON /ABROSS/ABROSS(kw),TAUROS(kw)
      COMMON /ABTOT/ABTOT(kw),ALPHA(kw)
      COMMON /CONT/ABTOTC(kw),ALPHAC(kw),TAUNUC(kw),SNUC(kw),HNUC(kw),
     1             JNUC(kw),JMINSC(kw),RESIDC(kw)
      REAL*8 JNUC,JMINSC
      COMMON /CONV/DLTDLP(kw),HEATCP(kw),DLRDLT(kw),VELSND(kw),
     1             GRDADB(kw),HSCALE(kw),FLXCNV(kw),VCONV(kw),MIXLTH,
     2             OVERWT,FLXCNV0(kw),FLXCNV1(kw),IFCONV
      REAL*8 MIXLTH
      COMMON /DEPART/BHYD(kw,6),BMIN(kw),NLTEON
      COMMON /ELEM/ABUND(99),ATMASS(99),ELEM(99)
      COMMON /EDENS/EDENS(kw),IFEDNS
      COMMON /FILENAME/FILENAME
      CHARACTER*60 FILENAME
      COMMON /FLUX/ FLUX,FLXERR(kw),FLXDRV(kw),FLXRAD(kw)
      COMMON /FREQ/FREQ,FREQLG,EHVKT(kw),STIM(kw),BNU(kw)
      COMMON /FRESET/FRESET(1563),RCOSET(1563),NULO,NUHI,NUMNU
      COMMON /HEIGHT/HEIGHT(kw)
      COMMON /IF/IFCORR,IFPRES,IFSURF,IFSCAT,TAUSCAT,IFMOL
      COMMON /IFOP/IFOP(20)
      COMMON /IONS/XNFPH(kw,2),XNFPHE(kw,3),XNFH(kw,2),XNFHE(kw,3)
      COMMON /ITER/ ITER,IFPRNT(15),IFPNCH(15),NUMITS
      COMMON /JUNK/TITLE(74),FREQID(6),WLTE,XSCALE
      COMMON /MUS/ANGLE(20),SURFI(20),NMU
      COMMON /OPS/AHYD(kw),AH2P(kw),AHMIN(kw),SIGH(kw),AHE1(kw),
     1       AHE2(kw),AHEMIN(kw),SIGHE(kw),ACOOL(kw),ALUKE(kw),AHOT(kw),
     2            SIGEL(kw),SIGH2(kw),AHLINE(kw),ALINES(kw),SIGLIN(kw),
     3            AXLINE(kw),SIGXL(kw),AXCONT(kw),SIGX(kw),SHYD(kw),
     4            SHMIN(kw),SHLINE(kw),SXLINE(kw),SXCONT(kw)
      COMMON /OPTOT/ACONT(kw),SCONT(kw),ALINE(kw),SLINE(kw),SIGMAC(kw),
     1              SIGMAL(kw)
      COMMON /PTOTAL/PTOTAL(kw)
      COMMON /PUT/PUT,IPUT
      COMMON /PZERO/PZERO,PCON,PRADK0,PTURB0,KNU(kw),PRADK(kw),RADEN(kw)
      REAL*8 KNU
      COMMON /RAD/ ACCRAD(kw),PRAD(kw)
      COMMON /RHOX/RHOX(kw),NRHOX
      COMMON /STATE/P(kw),XNE(kw),XNATOM(kw),RHO(kw)
      COMMON /TAUSHJ/TAUNU(kw),SNU(kw),HNU(kw),JNU(kw),JMINS(kw)
      REAL*8 JNU,JMINS
      COMMON /STEPLG/STEPLG,TAU1LG,KRHOX
      COMMON /TEFF/TEFF,GRAV,GLOG
      COMMON /TEMP/T(kw),TKEV(kw),TK(kw),HKT(kw),TLOG(kw),HCKT(kw),ITEMP
      COMMON /TURBPR/VTURB(kw),PTURB(kw),TRBFDG,TRBCON,TRBPOW,TRBSND,
     1               IFTURB
      COMMON /WAVEY/WBEGIN,DELTAW,IFWAVE
      COMMON /XABUND/XABUND(99),WTMOLE
      COMMON /XNF/XNFC(kw,6),XNFN(kw,6),XNFO(kw,6),XNFNE(kw,6),
     1            XNFMG(kw,6),XNFSI(kw,6),XNFS(kw,6),XNFFE(kw,5)
      COMMON /XNFP/XNFPC(kw,4),XNFPN(kw,5),XNFPO(kw,6),XNFPNE(kw,6),
     1             XNFPAL(kw,1),XNFPMG(kw,2),XNFPSI(kw,2),XNFPCA(kw,2),
     2             XNFPFE(kw,1),XNFPCH(kw),XNFPOH(kw)
      DIMENSION PART(kw,6)
C                                                                               
      COMMON /CONTIN/CONTIN                                                     
      COMMON /CURVE/MINLOG,DABLOG,NABLOG                                        
      REAL*8 MINLOG                                                               
      COMMON /GAM/GLOGR,GLOGS,GLOGW                                             
      COMMON /IFPROF/IFPROF                                                     
      COMMON/LINDAT/WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(3),LABELP(3),       
     1               GAMMAR,GAMMAS,GAMMAW,REF,X1,           
     2               X2,OTHER1(3),OTHER2(3),ELO,GF,ISO1,ISO2,
     3               NBLO,NBUP,NELION                              
      REAL*8 WL,E,EP                                                            
      COMMON /LINEY/LINE0(3,kw),DOPWAV(3,kw),ADAMP(3,kw)                        
      REAL*8 LINE0                                                                
      COMMON /OBS/EW,WLOBS,STAR(19)                                             
c      COMMON /RESIDU/RESIDU(150),TAUONE(150),DELWAV(150)              
      COMMON /RESIDU/RESIDU(1000),TAUONE(1000),DELWAV(1000)              
      COMMON /VTS/VTS(3),NVT                                                    
      DIMENSION POTION(594)                                                     
      DIMENSION POTA(114),POTB(114),POTC(114),POTD(114),POTE(114)               
      DIMENSION POTF(24)                                                        
      EQUIVALENCE (POTION(1),POTA(1)),(POTION(115),POTB(1))                     
      EQUIVALENCE (POTION(229),POTC(1)),(POTION(343),POTD(1))                   
      EQUIVALENCE (POTION(457),POTE(1)),(POTION(571),POTF(1))                   
      REAL*4 CARD(20),QSTOP,QEND,QCURV,QAVER,QVTUR,QLINE,QPROF,XLABEL                                                        
      DATA QSTOP,QEND,QCURV,QAVER,QVTUR,QLINE,QPROF/4HSTOP,4HEND ,              
     1 4HCURV,4HAVER,4HVTUR,4HLINE,4HPROF/                                      
      DATA POTA/                                                                
     1   13.598,   0.   ,   0.   ,   0.   ,   0.   ,   0.   ,                  1
     2   24.587,  54.416,   0.   ,   0.   ,   0.   ,   0.   ,                  2
     3    5.392,  75.638, 122.451,   0.   ,   0.   ,   0.   ,                  3
     4    9.322,  18.211, 153.893, 217.713,   0.   ,   0.   ,                  4
     5    8.298,  25.154,  37.930, 259.368, 340.217,   0.   ,                  5
     6   11.260,  24.383,  47.887,  64.492, 392.077, 489.981,                  6
     7   14.534,  29.601,  47.448,  77.472,  97.888, 552.057,                  7
     8   13.618,  35.116,  54.934,  77.412, 113.896, 138.116,                  8
     9   17.422,  34.970,  62.707,  87.138, 114.240, 157.161,                  9
     T   21.564,  40.962,  63.45 ,  97.11 , 126.21 , 157.93 ,                 10
     1    5.139,  47.286,  71.64 ,  98.91 , 138.39 , 172.15 ,                 11
     2    7.646,  15.035,  80.143, 109.24 , 141.26 , 186.50 ,                 12
     3    5.986,  18.828,  28.447, 119.99 , 153.71 , 190.47 ,                 13
     4    8.151,  16.345,  33.492,  45.141, 166.77 , 205.05 ,                 14
     5   10.486,  19.725,  30.18 ,  51.37 ,  65.023, 220.43 ,                 15
     6   10.36 ,  23.33 ,  34.83 ,  47.30 ,  72.68 ,  88.049,                 16
     7   12.967,  23.81 ,  39.61 ,  53.46 ,  67.80 ,  97.03 ,                 17
     8   15.759,  27.629,  40.74 ,  59.81 ,  75.02 ,  91.007,                 18
     9    4.341,  31.625,  45.72 ,  60.91 ,  82.66 , 100.0  /                 19
      DATA POTB/                                                                
     1    6.113,  11.871,  50.908,  67.10 ,  84.41 , 108.78 ,                 20
     2    6.54 ,  12.80 ,  24.76 ,  73.47 ,  91.66 , 111.1  ,                 21
     3    6.82 ,  13.58 ,  27.491,  43.266,  99.22 , 119.36 ,                 22
     4    6.74 ,  14.65 ,  29.310,  46.707,  65.23 , 128.12 ,                 23
     5    6.766,  16.50 ,  30.96 ,  49.1  ,  69.3  ,  90.56 ,                 24
     6    7.435,  15.640,  33.667,  51.2  ,  72.4  ,  95.   ,                 25
     7    7.870,  16.18 ,  30.651,  54.8  ,  75.0  ,  99.0  ,                 26
     8    7.86 ,  17.06 ,  33.50 ,  51.3  ,  79.5  , 102.0  ,                 27
     9    7.635,  18.168,  35.17 ,  54.9  ,  75.5  , 108.0  ,                 28
     T    7.726,  20.292,  36.83 ,  55.2  ,  79.9  , 103.0  ,                 29
     1    9.394,  17.964,  39.722,  59.4  ,  82.6  , 108.0  ,                 30
     2    5.999,  20.51 ,  30.71 ,  64.0  ,   0.   ,   0.   ,                 31
     3    7.899,  15.934,  34.22 ,  45.71 ,  93.5  ,   0.   ,                 32
     4    9.81 ,  18.633,  28.351,  50.13 ,  62.63 , 127.6  ,                 33
     5    9.752,  21.19 ,  30.820,  42.944,  68.3  ,  81.70 ,                 34
     6   11.814,  21.8  ,  36.   ,  47.3  ,  59.7  ,  88.6  ,                 35
     7   13.999,  24.359,  36.95 ,  52.5  ,  64.7  ,  78.5  ,                 36
     8    4.177,  27.28 ,  40.0  ,  52.6  ,  71.0  ,  84.4  ,                 37
     9    5.695,  11.030,  43.6  ,  57.0  ,  71.6  ,  90.8  /                 38
      DATA POTC/                                                                
     1    6.38 ,  12.24 ,  20.52 ,  61.8  ,  77.0  ,  93.0  ,                 39
     2    6.84 ,  13.13 ,  22.99 ,  34.34 ,  81.50 ,   0.   ,                 40
     3    6.88 ,  14.32 ,  25.04 ,  38.3  ,  50.55 , 102.6  ,                 41
     4    7.099,  16.15 ,  27.16 ,  46.4  ,  61.2  ,  68.0  ,                 42
     5    7.28 ,  15.26 ,  29.54 ,   0.   ,   0.   ,   0.   ,                 43
     6    7.37 ,  16.76 ,  28.47 ,   0.   ,   0.   ,   0.   ,                 44
     7    7.46 ,  18.08 ,  31.06 ,   0.   ,   0.   ,   0.   ,                 45
     8    8.34 ,  19.43 ,  32.93 ,   0.   ,   0.   ,   0.   ,                 46
     9    7.576,  21.49 ,  34.83 ,   0.   ,   0.   ,   0.   ,                 47
     T    8.993,  16.908,  34.48 ,   0.   ,   0.   ,   0.   ,                 48
     1    5.786,  18.869,  28.03 ,  54.0  ,   0.   ,   0.   ,                 49
     2    7.344,  14.632,  30.502,  40.734,  72.28 ,   0.   ,                 50
     3    8.641,  16.53 ,  25.3  ,  44.2  ,  56.0  , 108.0  ,                 51
     4    9.009,  18.6  ,  27.96 ,  37.41 ,  58.75 ,  70.7  ,                 52
     5   10.451,  19.131,  33.0  ,   0.   ,   0.   ,   0.   ,                 53
     6   12.130,  21.21 ,  32.1  ,   0.   ,   0.   ,   0.   ,                 54
     7    3.894,  25.1  ,  35.0  ,   0.   ,   0.   ,   0.   ,                 55
     8    5.212,  10.004,  37.000,   0.   ,   0.   ,   0.   ,                 56
     9    5.577,  11.06 ,  19.177,  49.954,   0.   ,   0.   /                 57
      DATA POTD/                                                                
     1    5.47 ,  10.85 ,  20.197,  36.758,   0.   ,   0.   ,                 58
     2    5.42 ,  10.55 ,  21.624,  38.981,   0.   ,   0.   ,                 59
     3    5.49 ,  10.72 ,  22.14 ,  40.42 ,   0.   ,   0.   ,                 60
     4    5.55 ,  10.90 ,  22.42 ,  41.09 ,   0.   ,   0.   ,                 61
     5    5.63 ,  11.07 ,  23.45 ,  41.47 ,   0.   ,   0.   ,                 62
     6    5.67 ,  11.25 ,  24.71 ,  42.65 ,   0.   ,   0.   ,                 63
     7    6.14 ,  12.1  ,  20.38 ,  44.03 ,   0.   ,   0.   ,                 64
     8    5.85 ,  11.52 ,  21.98 ,  39.84 ,   0.   ,   0.   ,                 65
     9    5.93 ,  11.67 ,  22.83 ,  41.56 ,   0.   ,   0.   ,                 66
     T    6.02 ,  11.80 ,  22.84 ,  42.51 ,   0.   ,   0.   ,                 67
     1    6.10 ,  11.93 ,  22.74 ,  42.66 ,   0.   ,   0.   ,                 68
     2    6.18 ,  12.05 ,  23.68 ,  42.69 ,   0.   ,   0.   ,                 69
     3    6.254,  12.17 ,  25.03 ,  43.74 ,   0.   ,   0.   ,                 70
     4    5.426,  13.9  ,  20.960,  45.193,   0.   ,   0.   ,                 71
     5    7.0  ,  14.9  ,  23.3  ,  33.319,   0.   ,   0.   ,                 72
     6    7.89 ,  16.200,  24.0  ,   0.   ,   0.   ,   0.   ,                 73
     7    7.98 ,  17.70 ,  25.0  ,   0.   ,   0.   ,   0.   ,                 74
     8    7.88 ,  16.6  ,  26.0  ,   0.   ,   0.   ,   0.   ,                 75
     9    8.7  ,  17.0  ,  27.0  ,   0.   ,   0.   ,   0.   /                 76
      DATA POTE/                                                                
     1    9.1  ,  20.0  ,  28.0  ,   0.   ,   0.   ,   0.   ,                 77
     2    9.0  ,  18.563,  29.0  ,   0.   ,   0.   ,   0.   ,                 78
     3    9.225,  20.5  ,  30.0  ,   0.   ,   0.   ,   0.   ,                 79
     4   10.437,  18.756,  34.2  ,   0.   ,   0.   ,   0.   ,                 80
     5    6.108,  20.428,  29.83 ,   0.   ,   0.   ,   0.   ,                 81
     6    7.416,  15.032,  31.937,  42.32 ,  68.8  ,   0.   ,                 82
     7    7.289,  16.69 ,  25.56 ,  45.3  ,  56.0  ,  88.3  ,                 83
     8    8.42 ,  19.0  ,  27.0  ,   0.   ,   0.   ,   0.   ,                 84
     9    9.3  ,  20.0  ,  30.   ,   0.   ,   0.   ,   0.   ,                 85
     T   10.748,  20.0  ,  30.0  ,   0.   ,   0.   ,   0.   ,                 86
     1    4.0  ,  22.0  ,  33.0  ,   0.   ,   0.   ,   0.   ,                 87
     2    5.279,  10.147,  34.0  ,   0.   ,   0.   ,   0.   ,                 88
     3    6.9  ,  12.1  ,  20.0  ,   0.   ,   0.   ,   0.   ,                 89
     4    6.0  ,  11.5  ,  20.0  ,  28.8  ,   0.   ,   0.   ,                 90
     5    6.0  ,  12.0  ,  20.0  ,   0.   ,   0.   ,   0.   ,                 91
     6    6.0  ,  12.0  ,  20.0  ,   0.   ,   0.   ,   0.   ,                 92
     7    6.0  ,  12.0  ,  20.0  ,   0.   ,   0.   ,   0.   ,                 93
     8    5.800,  12.0  ,  20.0  ,   0.   ,   0.   ,   0.   ,                 94
     9    6.0  ,  12.0  ,  20.0  ,   0.   ,   0.   ,   0.   /                 95
      DATA POTF/                                                                
     1    6.0  ,  12.0  ,  20.0  ,   0.   ,   0.   ,   0.   ,                 96
     2    6.0  ,  12.0  ,  20.0  ,   0.   ,   0.   ,   0.   ,                 97
     3    6.0  ,  12.0  ,  20.0  ,   0.   ,   0.   ,   0.   ,                 98
     4    6.0  ,  12.0  ,  20.0  ,   0.   ,   0.   ,   0.   /                 99
C      EXP10(X)=EXP(X*2.30258509299405E0)                                       
c      interface
c        subroutine exit(status)
c          integer(4),optional,intent(in)::status
c        end subroutine
c        subroutine abort(string)
c          !ms$attributes alias:'abort_'::abort
c          character(len=*),optional,intent(in)::string
c        end subroutine
c      end interface
      IFPROF=0                                                                  
      ITEMP=0                                                                   
      NABLOG=0                                                                  
 2222 REWIND 13                                                                 
      VTS(1)=0.                                                                 
      VTS(2)=0.                                                                 
      VTS(3)=0.                                                                 
      NVT=1                                                                     
      IFSURF=1                                                                  
      MODEAV=1                                                                  
    1 READ(5,2)CARD                                                             
      IF(CARD(1).EQ.QSTOP)CALL EXIT                                             
      WRITE(13,2)CARD                                                           
    2 FORMAT(20A4)                                                              
      IF(CARD(1).NE.QEND)GO TO 1                                                
      REWIND 13                                                                 
    3 CALL READIN(20)                                                           
      IF(NRHOX.EQ.0)GO TO 2222                                                  
      WRITE(6,333)                                                              
  333 FORMAT(1H1)                                                               
      ITEMP=ITEMP+1                                                             
      IFOP(15)=0                                                                
      IF(IFPRES.NE.0)CALL POPS(0.D0,1,XNE)                                                                  
      CALL POPS(1.01D0,11,XNFPH)
      CALL POPS(2.02D0,11,XNFPHE)
      CALL POPS(6.03D0,11,XNFPC)
      CALL POPS(7.04D0,11,XNFPN)
      CALL POPS(8.05D0,11,XNFPO)
      CALL POPS(10.05D0,11,XNFPNE)
      CALL POPS(12.01D0,11,XNFPMG)
      CALL POPS(13.00D0,11,XNFPAL)
      CALL POPS(14.01D0,11,XNFPSI)
      CALL POPS(20.01D0,11,XNFPCA)
      CALL POPS(26.00D0,11,XNFPFE)
c	do 1000 j=1,nrhox
c	write(6,786),j,xnfpmg(j,2)
c786	format(1x,i5,1pe12.4)
c1000	continue
      IF(IFMOL.EQ.0)THEN
      CALL POPS(1.01D0,12,XNFH)
      CALL POPS(2.02D0,12,XNFHE)
      CALL POPS(6.05D0,12,XNFC)
      CALL POPS(7.05D0,12,XNFN)
      CALL POPS(8.05D0,12,XNFO)
      CALL POPS(10.05D0,12,XNFNE)
      CALL POPS(12.05D0,12,XNFMG)
      CALL POPS(14.05D0,12,XNFSI)
      CALL POPS(16.05D0,12,XNFS)
      CALL POPS(26.04D0,12,XNFFE)
      ENDIF
      IF(IFMOL.EQ.1)THEN
      CALL POPS(106.00D0,11,XNFPCH)
      CALL POPS(108.00D0,11,XNFPOH)
      CALL W(6HXNFPCH,XNFPCH,NRHOX)
      CALL W(6HXNFPOH,XNFPOH,NRHOX)
C     THE POPS WILL NOT RETURN NUMBER DENSITIES WHEN MOLECULES ARE ON
C     SO WE COMPUTE NUMBER DENSITIES/PART FUNCTIONS  AND PART FUNCTIONS
      CALL POPS(6.05D0,11,XNFC)
      CALL POPS(7.05D0,11,XNFN)
      CALL POPS(8.05D0,11,XNFO)
      CALL POPS(10.05D0,11,XNFNE)
      CALL POPS(12.05D0,11,XNFMG)
      CALL POPS(14.05D0,11,XNFSI)
      CALL POPS(16.05D0,11,XNFS)
      CALL POPS(26.04D0,11,XNFFE)
      DO 444 J=1,NRHOX
      CALL PFSAHA(J,1,1,3,PART)
      XNFH(J,1)=XNFPH(J,1)*PART(J,1)
      XNFH(J,2)=XNFPH(J,2)
      CALL PFSAHA(J,2,2,13,PART)
      XNFHE(J,1)=XNFPHE(J,1)*PART(J,1)
      XNFHE(J,2)=XNFPHE(J,2)*PART(J,2)
      XNFHE(J,3)=XNFPHE(J,3)
      CALL PFSAHA(J,6,6,13,PART)
      XNFC(J,1)=XNFC(J,1)*PART(J,1)
      XNFC(J,2)=XNFC(J,2)*PART(J,2)
      XNFC(J,3)=XNFC(J,3)*PART(J,3)
      XNFC(J,4)=XNFC(J,4)*PART(J,4)
      XNFC(J,5)=XNFC(J,5)*PART(J,5)
      XNFC(J,6)=XNFC(J,6)*PART(J,6)
      CALL PFSAHA(J,7,6,13,PART)
      XNFN(J,1)=XNFN(J,1)*PART(J,1)
      XNFN(J,2)=XNFN(J,2)*PART(J,2)
      XNFN(J,3)=XNFN(J,3)*PART(J,3)
      XNFN(J,4)=XNFN(J,4)*PART(J,4)
      XNFN(J,5)=XNFN(J,5)*PART(J,5)
      XNFN(J,6)=XNFN(J,6)*PART(J,6)
      CALL PFSAHA(J,8,6,13,PART)
      XNFO(J,1)=XNFO(J,1)*PART(J,1)
      XNFO(J,2)=XNFO(J,2)*PART(J,2)
      XNFO(J,3)=XNFO(J,3)*PART(J,3)
      XNFO(J,4)=XNFO(J,4)*PART(J,4)
      XNFO(J,5)=XNFO(J,5)*PART(J,5)
      XNFO(J,6)=XNFO(J,6)*PART(J,6)
      CALL PFSAHA(J,10,6,13,PART)
      XNFNE(J,1)=XNFNE(J,1)*PART(J,1)
      XNFNE(J,2)=XNFNE(J,2)*PART(J,2)
      XNFNE(J,3)=XNFNE(J,3)*PART(J,3)
      XNFNE(J,4)=XNFNE(J,4)*PART(J,4)
      XNFNE(J,5)=XNFNE(J,5)*PART(J,5)
      XNFNE(J,6)=XNFNE(J,6)*PART(J,6)
      CALL PFSAHA(J,12,6,13,PART)
      XNFMG(J,1)=XNFMG(J,1)*PART(J,1)
      XNFMG(J,2)=XNFMG(J,2)*PART(J,2)
      XNFMG(J,3)=XNFMG(J,3)*PART(J,3)
      XNFMG(J,4)=XNFMG(J,4)*PART(J,4)
      XNFMG(J,5)=XNFMG(J,5)*PART(J,5)
      XNFMG(J,6)=XNFMG(J,6)*PART(J,6)
      CALL PFSAHA(J,14,6,13,PART)
      XNFSI(J,1)=XNFSI(J,1)*PART(J,1)
      XNFSI(J,2)=XNFSI(J,2)*PART(J,2)
      XNFSI(J,3)=XNFSI(J,3)*PART(J,3)
      XNFSI(J,4)=XNFSI(J,4)*PART(J,4)
      XNFSI(J,5)=XNFSI(J,5)*PART(J,5)
      XNFSI(J,6)=XNFSI(J,6)*PART(J,6)
      CALL PFSAHA(J,16,6,13,PART)
      XNFS(J,1)=XNFS(J,1)*PART(J,1)
      XNFS(J,2)=XNFS(J,2)*PART(J,2)
      XNFS(J,3)=XNFS(J,3)*PART(J,3)
      XNFS(J,4)=XNFS(J,4)*PART(J,4)
      XNFS(J,5)=XNFS(J,5)*PART(J,5)
      XNFS(J,6)=XNFS(J,6)*PART(J,6)
      CALL PFSAHA(J,26,5,13,PART)
      XNFFE(J,1)=XNFFE(J,1)*PART(J,1)
      XNFFE(J,2)=XNFFE(J,2)*PART(J,2)
      XNFFE(J,3)=XNFFE(J,3)*PART(J,3)
      XNFFE(J,4)=XNFFE(J,4)*PART(J,4)
      XNFFE(J,5)=XNFFE(J,5)*PART(J,5)
  444 CONTINUE
      ENDIF
C      IF(TRBSND.EQ.0.)GO TO 6                                                   
C      WRITE(6,7)                                                                
C    7 FORMAT(40HREWRITE WIDTH5 TO INCLUDE SOUND VELOCITY)                       
C      CALL EXIT                                                                 
C    6 DO 5 J=1,NRHOX                                                            
C      VELSND(J)=0.                                                              
C    5 VTURB(J)=(TRBFDG*RHO(J)**TRBPOW+TRBSND*VELSND(J)/1.E5+TRBCON)*1.E5        
    8 READ(13,9)XLABEL,EW,WLOBS,STAR                                            
    9 FORMAT(A4,F10.2,F10.4,19A4)                                               
C     CURV(E OF GROWTH)                                                         
      IF(XLABEL.EQ.QCURV)GO TO 170                                              
C     AVER(AGE)                                                                 
      IF(XLABEL.EQ.QAVER)GO TO 20                                               
C     VTUR(B)                                                                   
      IF(XLABEL.EQ.QVTUR)GO TO 40                                               
C     END                                                                       
      IF(XLABEL.EQ.QEND)GO TO 50                                                
C     LINE                                                                      
      IF(XLABEL.EQ.QLINE)GO TO 60                                               
C     PROF(ILE)                                                                 
      IF(XLABEL.EQ.QPROF)GO TO 80                                               
      WRITE(6,11)XLABEL                                                         
      CALL EXIT                                                                 
   11 FORMAT(9H WHAT IS A4)                                                     
   80 IFPROF=1                                                                  
      GO TO 8                                                                   
   20 IF(MODEAV.EQ.2)GO TO 30                                                   
      MODEAV=2                                                                  
      GO TO 8                                                                   
   30 MODEAV=3                                                                  
      CALL AVERAG(MODEAV,IVT,ABLG,TAULG)                                        
      GO TO 8                                                                   
   40 READ(13,41)NVT,(VTS(IVT),IVT=1,NVT)                                       
   41 FORMAT(I5,3F5.2)                                                          
      GO TO 8                                                                   
  170 READ(13,171)NABLOG,MINLOG,DABLOG                                          
  171 FORMAT(I5,2F8.2)                                                          
      GO TO 8                                                                   
   50 REWIND 13                                                                 
      GO TO 3                                                                   
C  60 READ(13,140)WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL,LABELP,WL,NELION,             
   60 READ(13,4443)WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL,LABELP                        
      READ(13,445)                                       WL,NELION,             
     1GAMMAR,GAMMAS,GAMMAW,REF,NBLO,NBUP,ISO1,X1,ISO2,X2,OTHER1,OTHER2          
 4443 FORMAT(F10.4,F7.3,F5.1,F12.3,F5.1,F12.3,F9.2,2A4,A2,2A4,A2)               
  445 FORMAT(                                                                   
     1F10.4,I4,F6.2,F6.2,F6.2,A4,I2,I2,I3,F7.3,I3,F7.3,2A4,A2,2A4,A2)           
  140 FORMAT(F10.4,F7.3,F5.1,F12.3,F5.1,F12.3,F9.2,2A4,A2,2A4,A2/               
     1F10.4,I4,F6.2,F6.2,F6.2,A4,I2,I2,I3,F7.3,I3,F7.3,2A4,A2,2A4,A2)           
      WRITE(6,1400)                                                             
 1400 FORMAT(//)                                                                
      WRITE(6,140)WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL,LABELP,WL,NELION,             
     1GAMMAR,GAMMAS,GAMMAW,REF,NBLO,NBUP,ISO1,X1,ISO2,X2,OTHER1,OTHER2          
      GFLOG=GFLOG+X1+X2                                                         
      GF=EXP(GFLOG*2.30258509299405E0)                                          
      ELO= MIN (E,EP)                                                           
      EXPOT=ELO*1.23981E-4                                                      
      GAMMAR=EXP(GAMMAR*2.30258509299405E0)                                     
      GAMMAS=EXP(GAMMAS*2.30258509299405E0)                                     
      GAMMAW=EXP(GAMMAW*2.30258509299405E0)                                     
C     CLASSICAL DAMPING CONSTANT                                                
      IF(GAMMAR.EQ.1.)GAMMAR=2.223E13/WL**2                                     
      IF(GAMMAS.NE.1..AND.GAMMAW.NE.1.)GO TO 141                                
      IF(GAMMAS.NE.1.)GO TO 138                                                 
      IF(CODE.GE.100.)GO TO 137                                                 
      EPUP= MAX (E,EP)*1.23981E-4                                               
      ZEFF=(CODE-FLOAT( INT(CODE)))*100.+1.                                     
      EFFNSQ=25.                                                                
      DEPUP=POTION(NELION)-EPUP                                                 
      IF(DEPUP.GT.0.)EFFNSQ=13.595*ZEFF**2/DEPUP                                
      GAMMAS=1.0E-8*EFFNSQ**2*SQRT(EFFNSQ)                                      
      GO TO 138                                                                 
  137 GAMMAS=1.0E-5                                                             
  138 IF(GAMMAW.NE.1.)GO TO 141                                                 
      IF(CODE.GE.100.)GO TO 139                                                 
      EPUP= MAX (E,EP)*1.23981E-4                                               
      ZEFF=(CODE-FLOAT( INT(CODE)))*100.+1.                                     
      EFFNSQ=25.                                                                
      DEPUP=POTION(NELION)-EPUP                                                 
      IF(DEPUP.GT.0.)EFFNSQ=13.595*ZEFF**2/DEPUP                                
      RSQ=2.5*(EFFNSQ/ZEFF)**2                                                  
      NSEQ=CODE-ZEFF+1.                                                         
      IF(NSEQ.GT.20.AND.NSEQ.LT.29)RSQ=(45.-FLOAT(NSEQ))/ZEFF                   
      GAMMAW=4.5E-9*RSQ**.4                                                     
      GO TO 141                                                                 
  139 GAMMAW=1.E-7/ZEFF                                                         
  141 GLOGR= LOG10(GAMMAR)                                                      
      GLOGS= LOG10(GAMMAS)                                                      
      GLOGW= LOG10(GAMMAW)                                                      
      FREQ=2.997925E17/WL                                                       
      FREQLG= LOG(FREQ)                                                         
      DO 61 J=1,NRHOX                                                           
      EHVKT(J)=EXP(-FREQ*HKT(J))                                                
      STIM(J)=1.-EHVKT(J)                                                       
      BNU(J)=1.47439E-2*(FREQ/1.E15)**3*EHVKT(J)/STIM(J)                        
   61 CONTINUE                                                                  
C     CALCULATES CONTINUOUS OPACITIES                                           
      CALL KAPP(1,NSTEPS,STEPWT)                                                
C     CALCULATES THE SOURCE FUNCTION AND SURFACE FLUX OR SURFACE INTENSITY      
      CALL JOSH(IFSCAT,IFSURF)                                                  
      IF(IFSURF.LT.2)CONTIN=HNU(1)                                              
      IF(IFSURF.EQ.2)CONTIN=SURFI(1)                                            
C     TREAT HYDROGEN LINES AS CONTINUUM                                         
      IF(IFSCAT.EQ.0.)GO TO 162                                                 
      DO 63 J=1,NRHOX                                                           
      SCONT(J)=(ACONT(J)*SCONT(J)+ALINE(J)*SLINE(J))/(ACONT(J)+ALINE(J))        
      ACONT(J)=ACONT(J)+ALINE(J)                                                
      SLINE(J)=BNU(J)                                                           
   63 CONTINUE                                                                  
      GO TO 164                                                                 
C     SOURCE FUNCTION WILL BE WEIGHTED MEAN OF CONTINUUM SOURCE                 
C        FUNCTION (INCLUDING JNU) AND BNU                                       
  162 DO 163 J=1,NRHOX                                                          
      SCONT(J)=SNU(J)                                                           
      SLINE(J)=BNU(J)                                                           
  163 ACONT(J)=ACONT(J)+ALINE(J)                                                
  164 CONTINUE                                                                  
C     CALCULATES THE LINE CENTER MASS ABSORPTION COEFFICIENT FOR UNIT ABUNDANCE 
      if(wl.eq.448.12)CALL LINCENmg(ABUND1,VTS,NVT)                                               
      if(wl.ne.448.12)CALL LINCEN(ABUND1,VTS,NVT)                                               
      DO 65 IVT=1,NVT                                                           
C     CALCULATES THE CURVE OF GROWTH AND THE FINAL ABUNDANCE                    
C     FIRST GUESS FOR ABLG                                                      
      ABLG= LOG10(ABUND1)                                                       
      IF(IFPROF.EQ.1)WRITE(6,64)                                                
   64 FORMAT(///23H PROFILES FOR NEXT LINE)                                     
      if(wl.eq.448.12)CALL COGmg(IVT,ABLG,TAULG)                                                  
      if(wl.ne.448.12)CALL COG(IVT,ABLG,TAULG)                                                  
C     CALCULATES THE AVERAGE ABUNDANCE                                          
      CALL AVERAG(MODEAV,IVT,ABLG,TAULG)                                        
   65 CONTINUE                                                                  
      GO TO 8                                                                   
      END                                                                       
      SUBROUTINE COG(IVT,ABLG,TAULG)                                            
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /CONTIN/CONTIN                                                     
      COMMON /CURVE/MINLOG,DABLOG,NABLOG                                        
      REAL*8 MINLOG                                                               
      COMMON /GAM/GLOGR,GLOGS,GLOGW                                             
      COMMON/LINDAT/WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(3),LABELP(3),       
     1               GAMMAR,GAMMAS,GAMMAW,REF,X1,           
     2               X2,OTHER1(3),OTHER2(3),ELO,GF,ISO1,ISO2,                              
     3               NBLO,NBUP,NELION
      REAL*8 WL,E,EP                                                            
      COMMON /OBS/EW,WLOBS,STAR(19)                                             
c      COMMON /RESIDU/RESIDU(150),TAUONE(150),DELWAV(150)              
      COMMON /RESIDU/RESIDU(1000),TAUONE(1000),DELWAV(1000)              
      COMMON /TAUBAR/TAUBAR                                                     
      COMMON /VTS/VTS(3),NVT                                                    
      DIMENSION ABLOG(9),EWLOG(9),TAULOG(9)                                     
      DIMENSION RESID0(9)                                                       
C                                                                               
      EXP10(X)=EXP(X*2.30258509299405E0)                                        
      IF(NABLOG.EQ.0)GO TO 63                                                   
      ABLOG(1)=MINLOG+ABLG                                                      
      EWLOG(1)= LOG10(WID(EXP10(ABLOG(1)),IVT))                                 
      RESID0(1)=RESIDU(1)                                                       
      TAULOG(1)= LOG10(TAUBAR)                                                  
      DO 162 I=2,NABLOG                                                         
      ABLOG(I)=ABLOG(I-1)+DABLOG                                                
      EWLOG(I)= LOG10(WID(EXP10(ABLOG(I)),IVT))                                 
      RESID0(I)=RESIDU(1)                                                       
  162 TAULOG(I)= LOG10(TAUBAR)                                                  
      NAB=NABLOG                                                                
      OBEWLG=0.                                                                 
      GO TO 76                                                                  
   63 OBEWLG= LOG10(EW)                                                         
      ABLOG(1)=ABLG                                                             
      EWLOG(1)= LOG10(WID(EXP10(ABLOG(1)),IVT))                                 
      TAULOG(1)= LOG10(TAUBAR)                                                  
      IF(EWLOG(1).GT.OBEWLG)GO TO 71                                            
      ABLOG(2)=ABLOG(1)+1.                                                      
      EWLOG(2)= LOG10(WID(EXP10(ABLOG(2)),IVT))                                 
      TAULOG(2)= LOG10(TAUBAR)                                                  
      ABLOG(3)=ABLOG(2)+1.                                                      
      IF(EWLOG(2).GT.OBEWLG)ABLOG(3)=ABLOG(3)-1.5                               
      GO TO 72                                                                  
   71 ABLOG(2)=ABLOG(1)-1.                                                      
      EWLOG(2)= LOG10(WID(EXP10(ABLOG(2)),IVT))                                 
      TAULOG(2)= LOG10(TAUBAR)                                                  
      ABLOG(3)=ABLOG(2)+.5                                                      
      IF(EWLOG(2).GT.OBEWLG)ABLOG(3)=ABLOG(3)-1.5                               
   72 EWLOG(3)= LOG10(WID(EXP10(ABLOG(3)),IVT))                                 
      TAULOG(3)= LOG10(TAUBAR)                                                  
      DO 73 IAB=3,8                                                             
      NAB=IAB                                                                   
c      IF(ABS(OBEWLG-EWLOG(IAB)).LT..005)GO TO 76                                
      IF(ABS(OBEWLG-EWLOG(IAB)).LT..0005)GO TO 76                                
      NAB1=NAB-1                                                                
      DO 79 KK=1,NAB1                                                           
      DO 79 II=1,NAB1                                                           
      IF(ABLOG(II+1).GE.ABLOG(II))GO TO 79                                      
      DUMMY=ABLOG(II+1)                                                         
      ABLOG(II+1)=ABLOG(II)                                                     
      ABLOG(II)=DUMMY                                                           
      DUMMY=EWLOG(II+1)                                                         
      EWLOG(II+1)=EWLOG(II)                                                     
      EWLOG(II)=DUMMY                                                           
      DUMMY=TAULOG(II+1)                                                        
      TAULOG(II+1)=TAULOG(II)                                                   
      TAULOG(II)=DUMMY                                                          
   79 CONTINUE                                                                  
      DO 80 I=2,NAB                                                             
      K=I                                                                       
      IF(OBEWLG.LT.EWLOG(I))GO TO 81                                            
   80 CONTINUE                                                                  
   81 ABLOG(NAB+1)=ABLOG(K-1)+(ABLOG(K)-ABLOG(K-1))/(EWLOG(K)-EWLOG(K-1)        
     1)*(OBEWLG-EWLOG(K-1))                                                     
      EWLOG(IAB+1)= LOG10(WID(EXP10(ABLOG(IAB+1)),IVT))                         
   73 TAULOG(IAB+1)= LOG10(TAUBAR)                                              
      WRITE(6,75)                                                               
   75 FORMAT(//14H NOT CONVERGED)                                               
      NAB=NAB+1                                                                 
   76 WRITE(6,3333)STAR,WLOBS,GLOGR,GLOGS,GLOGW,TAULOG(NAB),EW,                 
     1ABLOG(NAB)                                                                
 3333 FORMAT(1H0,19A4,F10.4,3F6.2,F6.2,F10.2,F10.3)                             
C     WRITE(7,765)WLOBS,CODE,EW,GFLOG,E,VTS(IVT),ABLOG(NAB)                     
C 765 FORMAT(F10.4,F6.2,F6.2,F6.2,F12.3,F6.2,F6.2)                              
      WRITE(6,77)(ABLOG(IAB),IAB=1,NAB)                                         
   77 FORMAT(9X5HVTURB,2X5HABUND9F9.2)                                          
      WRITE(6,78)VTS(IVT),(EWLOG(IAB),IAB=1,NAB)                                
   78 FORMAT(9XF5.2,5X2HEW9F9.3)                                                
      DO 113 IAB=1,NAB                                                          
  113 EWLOG(IAB)=EXP10(EWLOG(IAB))                                              
      WRITE(6,114)(EWLOG(IAB),IAB=1,NAB)                                        
  114 FORMAT(21X9F9.2)                                                          
      WRITE(6,177)(TAULOG(IAB),IAB=1,NAB)                                       
  177 FORMAT(16X5HDEPTH,9F9.2)                                                  
      IF(NABLOG.GT.0)WRITE(6,178)(RESID0(IAB),IAB=1,NAB)                        
  178 FORMAT(16X5HRESID,9F9.2)                                                  
      ABLG=ABLOG(NAB)                                                           
      TAULG=TAULOG(NAB)                                                         
      RETURN                                                                    
      END                                                                       
      SUBROUTINE COGmg(IVT,ABLG,TAULG)                                            
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /CONTIN/CONTIN                                                     
      COMMON /CURVE/MINLOG,DABLOG,NABLOG                                        
      REAL*8 MINLOG                                                               
      COMMON /GAM/GLOGR,GLOGS,GLOGW                                             
      COMMON/LINDAT/WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(3),LABELP(3),       
     1               GAMMAR,GAMMAS,GAMMAW,REF,X1,           
     2               X2,OTHER1(3),OTHER2(3),ELO,GF,ISO1,ISO2,                              
     3               NBLO,NBUP,NELION
      REAL*8 WL,E,EP                                                            
      COMMON /OBS/EW,WLOBS,STAR(19)                                             
c      COMMON /RESIDU/RESIDU(150),TAUONE(150),DELWAV(150)              
      COMMON /RESIDU/RESIDU(1000),TAUONE(1000),DELWAV(1000)              
      COMMON /TAUBAR/TAUBAR                                                     
      COMMON /VTS/VTS(3),NVT                                                    
      DIMENSION ABLOG(9),EWLOG(9),TAULOG(9)                                     
      DIMENSION RESID0(9)                                                       
C                                                                               
      EXP10(X)=EXP(X*2.30258509299405E0)                                        
      IF(NABLOG.EQ.0)GO TO 63                                                   
      ABLOG(1)=MINLOG+ABLG                                                      
      EWLOG(1)= LOG10(WIDmg(EXP10(ABLOG(1)),IVT))                              
      RESID0(1)=RESIDU(1)                                                       
      TAULOG(1)= LOG10(TAUBAR)                                                  
      DO 162 I=2,NABLOG                                                         
      ABLOG(I)=ABLOG(I-1)+DABLOG                                                
      EWLOG(I)= LOG10(WIDmg(EXP10(ABLOG(I)),IVT))                              
      RESID0(I)=RESIDU(1)                                                       
  162 TAULOG(I)= LOG10(TAUBAR)                                                  
      NAB=NABLOG                                                                
      OBEWLG=0.                                                                 
      GO TO 76                                                                  
   63 OBEWLG= LOG10(EW)                                                         
      ABLOG(1)=ABLG                                                             
      EWLOG(1)= LOG10(WIDmg(EXP10(ABLOG(1)),IVT))                              
      TAULOG(1)= LOG10(TAUBAR)                                                  
      IF(EWLOG(1).GT.OBEWLG)GO TO 71                                            
      ABLOG(2)=ABLOG(1)+1.                                                      
      EWLOG(2)= LOG10(WIDmg(EXP10(ABLOG(2)),IVT))                              
      TAULOG(2)= LOG10(TAUBAR)                                                  
      ABLOG(3)=ABLOG(2)+1.                                                      
      IF(EWLOG(2).GT.OBEWLG)ABLOG(3)=ABLOG(3)-1.5                               
      GO TO 72                                                                  
   71 ABLOG(2)=ABLOG(1)-1.                                                      
      EWLOG(2)= LOG10(WIDmg(EXP10(ABLOG(2)),IVT))                               
      TAULOG(2)= LOG10(TAUBAR)                                                  
      ABLOG(3)=ABLOG(2)+.5                                                      
      IF(EWLOG(2).GT.OBEWLG)ABLOG(3)=ABLOG(3)-1.5                            
   72 EWLOG(3)= LOG10(WIDmg(EXP10(ABLOG(3)),IVT))                              
      TAULOG(3)= LOG10(TAUBAR)                                                  
      DO 73 IAB=3,8                                                             
      NAB=IAB                                                                   
c      IF(ABS(OBEWLG-EWLOG(IAB)).LT..005)GO TO 76                             
      IF(ABS(OBEWLG-EWLOG(IAB)).LT..0005)GO TO 76                              
      NAB1=NAB-1                                                                
      DO 79 KK=1,NAB1                                                           
      DO 79 II=1,NAB1                                                           
      IF(ABLOG(II+1).GE.ABLOG(II))GO TO 79                                      
      DUMMY=ABLOG(II+1)                                                         
      ABLOG(II+1)=ABLOG(II)                                                     
      ABLOG(II)=DUMMY                                                           
      DUMMY=EWLOG(II+1)                                                         
      EWLOG(II+1)=EWLOG(II)                                                     
      EWLOG(II)=DUMMY                                                           
      DUMMY=TAULOG(II+1)                                                        
      TAULOG(II+1)=TAULOG(II)                                                   
      TAULOG(II)=DUMMY                                                          
   79 CONTINUE                                                                  
      DO 80 I=2,NAB                                                             
      K=I                                                                       
      IF(OBEWLG.LT.EWLOG(I))GO TO 81                                            
   80 CONTINUE                                                                  
   81 ABLOG(NAB+1)=ABLOG(K-1)+(ABLOG(K)-ABLOG(K-1))/(EWLOG(K)-EWLOG(K-1)        
     1)*(OBEWLG-EWLOG(K-1))                                                     
      EWLOG(IAB+1)= LOG10(WIDmg(EXP10(ABLOG(IAB+1)),IVT))                       
   73 TAULOG(IAB+1)= LOG10(TAUBAR)                                              
      WRITE(6,75)                                                               
   75 FORMAT(//14H NOT CONVERGED)                                               
      NAB=NAB+1                                                                 
   76 WRITE(6,3333)STAR,WLOBS,GLOGR,GLOGS,GLOGW,TAULOG(NAB),EW,                 
     1ABLOG(NAB)                                                                
 3333 FORMAT(1H0,19A4,F10.4,3F6.2,F6.2,F10.2,F10.3)                             
C     WRITE(7,765)WLOBS,CODE,EW,GFLOG,E,VTS(IVT),ABLOG(NAB)                     
C 765 FORMAT(F10.4,F6.2,F6.2,F6.2,F12.3,F6.2,F6.2)                              
      WRITE(6,77)(ABLOG(IAB),IAB=1,NAB)                                         
   77 FORMAT(9X5HVTURB,2X5HABUND9F9.2)                                          
      WRITE(6,78)VTS(IVT),(EWLOG(IAB),IAB=1,NAB)                                
   78 FORMAT(9XF5.2,5X2HEW9F9.3)                                                
      DO 113 IAB=1,NAB                                                          
  113 EWLOG(IAB)=EXP10(EWLOG(IAB))                                              
      WRITE(6,114)(EWLOG(IAB),IAB=1,NAB)                                        
  114 FORMAT(21X9F9.2)                                                          
      WRITE(6,177)(TAULOG(IAB),IAB=1,NAB)                                       
  177 FORMAT(16X5HDEPTH,9F9.2)                                                  
      IF(NABLOG.GT.0)WRITE(6,178)(RESID0(IAB),IAB=1,NAB)                        
  178 FORMAT(16X5HRESID,9F9.2)                                                  
      ABLG=ABLOG(NAB)                                                           
      TAULG=TAULOG(NAB)                                                         
      RETURN                                                                    
      END                                                                       
      FUNCTION WID(ABUND,IVT)                                                   
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (kw=99)
      COMMON /IF/IFCORR,IFPRES,IFSURF,IFSCAT,TAUSCAT,IFMOL                              
      COMMON /MUS/ANGLE(20),SURFI(20),NMU                                       
      COMMON /OPTOT/ACONT(kw),SCONT(kw),ALINE(kw),SLINE(kw),SIGMAC(kw),         
     1              SIGMAL(kw)                                                  
      COMMON /RHOX/RHOX(kw),NRHOX                                               
      COMMON /TEMP/T(kw),TKEV(kw),TK(kw),HKT(kw),TLOG(kw),HCKT(kw),ITEMP
      COMMON /TAUSHJ/TAUNU(kw),SNU(kw),HNU(kw),JNU(kw),JMINS(kw)                
      REAL*8 JNU,JMINS                                                            
      COMMON /TURBPR/VTURB(kw),PTURB(kw),TRBFDG,TRBCON,TRBPOW,TRBSND,           
     1               IFTURB                                                     
C                                                                               
      COMMON /CONTIN/CONTIN                                                     
      COMMON /IFPROF/IFPROF                                                     
      COMMON/LINDAT/WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(3),LABELP(3),       
     1               GAMMAR,GAMMAS,GAMMAW,REF,X1,           
     2               X2,OTHER1(3),OTHER2(3),ELO,GF,ISO1,ISO2,                              
     3               NBLO,NBUP,NELION
      REAL*8 WL,E,EP                                                            
      COMMON /LINEY/LINE0(3,kw),DOPWAV(3,kw),ADAMP(3,kw)                        
      REAL*8 LINE0                                                                
c      COMMON /RESIDU/RESIDU(150),TAUONE(150),DELWAV(150)              
      COMMON /RESIDU/RESIDU(1000),TAUONE(1000),DELWAV(1000)              
      COMMON /TAUBAR/TAUBAR                                                     
      COMMON /VTS/VTS(3),NVT                                                    
      DIMENSION DEL(15),WEIGHT(15),ALINE0(kw)                                   
C     IN PM                                                                     
      DATA DEL/0.,.5,1.,1.5,2.,2.5,3.,3.5,4.,4.5,5.,6.25,7.5,8.75,10./          
      DATA WEIGHT/.166667,.666667,.333333,.666667,.333333,.666667,              
     1.333333,.666667,.333333,.666667,.583333,1.666667,.833333,1.666667,        
     2 10.416667/                                                               
      EXP10(X)=EXP(X*2.30258509299405E0)                                        
      DO 1 J=1,NRHOX                                                            
    1 ALINE0(J)=LINE0(IVT,J)*ABUND                                              
      ASSIGN 73 TO ISWTCH                                                       
C     MICROTURBULENCE IN PM                                                     
      DOPVT=(VTS(IVT)*(1.E5/2.997925E10))*(WL*1000.)                            
      SCALE=SQRT(2.5**2+DOPVT**2)/2.5                                           
      DO 70 MDUMMY=1,10                                                         
      CCC=5.*SCALE                                                              
      DO 71 J=1,NRHOX                                                           
   71 ALINE(J)=ALINE0(J)*VOIGT(CCC/DOPWAV(IVT,J),ADAMP(IVT,J))                  
      CALL JOSH(IFSCAT,IFSURF)                                                  
      IF(IFSURF.LT.2)RESID=HNU(1)/CONTIN                                        
      IF(IFSURF.EQ.2)RESID=SURFI(1)/CONTIN                                      
      IF(MDUMMY.EQ.1)GO TO 75                                                   
      IF(RESID.GT..93)GO TO 74                                                  
      GO TO ISWTCH,(73,76)                                                      
   75 IF(RESID.GE..91)GO TO 80                                                  
   73 SCALE1=SCALE                                                              
      SCALE=SCALE*2.                                                            
      ASSIGN 73 TO ISWTCH                                                       
      GO TO 70                                                                  
   74 SCALE2=SCALE                                                              
      SCALE=(SCALE1+SCALE2)/2.                                                  
      ASSIGN 76 TO ISWTCH                                                       
      GO TO 70                                                                  
   76 SCALE1=SCALE                                                              
      SCALE=(SCALE1+SCALE2)/2.                                                  
   70 CONTINUE
   80 WIDTH=0.                                                                  
      TAUSUM=0.                                                                 
      DO 81 I=1,15                                                              
      CCC=DEL(I)*SCALE                                                          
      DELWAV(I)=CCC                                                             
      DO 82 J=1,NRHOX                                                           
   82 ALINE(J)=ALINE0(J)*VOIGT(CCC/DOPWAV(IVT,J),ADAMP(IVT,J))                  
      CALL JOSH(IFSCAT,IFSURF)                                                  
      IF(IFSURF.LT.2)RESIDU(I)=HNU(1)/CONTIN                                    
      IF(IFSURF.EQ.2)RESIDU(I)=SURFI(1)/CONTIN                                  
      MAX=MAP1(TAUNU,RHOX,NRHOX,1.D0,TAUONE(I),1)                                 
      TAUONE(I)= LOG10(TAUONE(I))                                               
      TAUSUM=TAUSUM+WEIGHT(I)*TAUONE(I)*(1.-RESIDU(I))                          
   81 WIDTH=WIDTH+WEIGHT(I)*(1.-RESIDU(I))                                      
      TAUBAR=EXP10(TAUSUM/WIDTH)                                                
      WID=WIDTH*SCALE*2.
      IF(IFPROF.EQ.0)RETURN                                                     
      WRITE(6,87)(DELWAV(i),i=1,15),(TAUONE(i),i=1,15),
	1 (RESIDU(i),i=1,15)                                           
   87 FORMAT(1H017X3HDEL15F7.3/16X5HDEPTH15F7.3/16X5HRESID15F7.3)               
      RETURN                                                                    
      END                                                                       
      FUNCTION WIDmg(ABUND,IVT)                                                 
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (kw=99)
      COMMON /IF/IFCORR,IFPRES,IFSURF,IFSCAT,TAUSCAT,IFMOL                     
      COMMON /MUS/ANGLE(20),SURFI(20),NMU                                       
      COMMON /OPTOT/ACONT(kw),SCONT(kw),ALINE(kw),SLINE(kw),SIGMAC(kw),         
     1              SIGMAL(kw)                                                  
      COMMON /RHOX/RHOX(kw),NRHOX                                               
      COMMON /TEMP/T(kw),TKEV(kw),TK(kw),HKT(kw),TLOG(kw),HCKT(kw),ITEMP
      COMMON /TAUSHJ/TAUNU(kw),SNU(kw),HNU(kw),JNU(kw),JMINS(kw)                
      REAL*8 JNU,JMINS                                                         
      COMMON /TURBPR/VTURB(kw),PTURB(kw),TRBFDG,TRBCON,TRBPOW,TRBSND,           
     1               IFTURB                                                     
C                                                                               
      COMMON /CONTIN/CONTIN                                                     
      COMMON /IFPROF/IFPROF                                                     
      COMMON/LINDAT/WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(3),LABELP(3),       
     1               GAMMAR,GAMMAS,GAMMAW,REF,X1,           
     2               X2,OTHER1(3),OTHER2(3),ELO,GF,ISO1,ISO2,                  
     3               NBLO,NBUP,NELION
      REAL*8 WL,E,EP                                                            
      COMMON /LINEY/LINE0(3,kw),DOPWAV(3,kw),ADAMP(3,kw)                        
      COMMON /LINEY1/LINE01(3,kw),DOPWAV1(3,kw),ADAMPa(3,kw)                   
      COMMON /LINEY2/LINE02(3,kw),DOPWAV2(3,kw),ADAMPb(3,kw)                    
      COMMON /LINEY3/LINE03(3,kw),DOPWAV3(3,kw),ADAMPc(3,kw)                    
      REAL*8 LINE0,line01,line02,line03                                        
c      COMMON /RESIDU/RESIDU(150),TAUONE(150),DELWAV(150)              
      COMMON /RESIDU/RESIDU(1000),TAUONE(1000),DELWAV(1000)              
      COMMON /TAUBAR/TAUBAR                                                     
      COMMON /VTS/VTS(3),NVT                                                    
      DIMENSION DEL(2000),WEIGHT(2000),ALINE0(kw)                                  
      DIMENSION ALINE01(kw),aline02(kw),aline03(kw)                            
C     IN PM                                                                     
c      DATA DEL/0.,.5,1.,1.5,2.,2.5,3.,3.5,4.,4.5,5.,5.5,6.0,6.25,7.5,8.75,10./
c      DATA WEIGHT/.166667,.666667,.333333,.666667,.333333,.666667,             
c     1.333333,.666667,.333333,.666667,.583333,1.666667,.833333,1.666667,       
c     2 10.416667,135*0./
      EXP10(X)=EXP(X*2.30258509299405E0)                                        
	del(1)=-30.
c	do 111 i=2,120
c	del(i)=del(i-1)+0.5
	do 111 i=2,600
	del(i)=del(i-1)+0.1
111	continue
c	weight(1)=.166667
c	do 112 i=2,120,2
c	weight(i)=.666667
c	weight(i+1)=.333333
	weight(1)=.03333334
	do 112 i=2,600,2
	weight(i)=.13333334
	weight(i+1)=.06666667
112	continue
c	weight(120)=weight(1)                                                       
	weight(600)=weight(1)                                                       
      DO 1 J=1,NRHOX                                                            
      ALINE0(J)=LINE0(IVT,J)*ABUND                                              
	aline01(j)=line01(ivt,j)*abund
	aline02(j)=line02(ivt,j)*abund
	aline03(j)=line03(ivt,j)*abund
1	continue
      ASSIGN 73 TO ISWTCH                                                       
C     MICROTURBULENCE IN PM                                                     
      DOPVT=(VTS(IVT)*(1.E5/2.997925E10))*(WL*1000.)                            
      SCALE=SQRT(2.5**2+DOPVT**2)/2.5                                           
      DO 70 MDUMMY=1,10                                                         
      CCC=5.*SCALE                                                              
      DO 71 J=1,NRHOX                                                           
   71 ALINE(J)=ALINE0(J)*VOIGT(CCC/DOPWAV(IVT,J),ADAMP(IVT,J))                  
	if(wl.eq.448.12)then
	do 711 j=1,nrhox
	aline1=aline01(j)*voigt((abs(ccc+7.4))/dopwav1(ivt,j),adampa(ivt,j))
	aline2=aline02(j)*voigt((abs(ccc+5.0))/dopwav2(ivt,j),adampb(ivt,j))
	aline3=aline03(j)*voigt((abs(ccc-12.5))/dopwav3(ivt,j),adampc(ivt,j))
	aline(j)=aline1+aline2+aline3
711	continue
	endif
      CALL JOSH(IFSCAT,IFSURF)                                                  
      IF(IFSURF.LT.2)RESID=HNU(1)/CONTIN                                        
      IF(IFSURF.EQ.2)RESID=SURFI(1)/CONTIN                                      
      IF(MDUMMY.EQ.1)GO TO 75                                                   
      IF(RESID.GT..93)GO TO 74                                                  
      GO TO ISWTCH,(73,76)                                                      
   75 IF(RESID.GE..91)GO TO 80                                                  
   73 SCALE1=SCALE                                                              
      SCALE=SCALE*2.                                                            
      ASSIGN 73 TO ISWTCH                                                       
      GO TO 70                                                                  
   74 SCALE2=SCALE                                                              
      SCALE=(SCALE1+SCALE2)/2.                                                  
      ASSIGN 76 TO ISWTCH                                                       
      GO TO 70                                                                  
   76 SCALE1=SCALE                                                              
      SCALE=(SCALE1+SCALE2)/2.                                                  
   70 CONTINUE
   80 WIDTH=0.                                                                  
      TAUSUM=0.
c      DO 81 I=1,120                                                              
      DO 81 I=1,600                                                              
      CCC=DEL(I)*SCALE                                                         
c	type*,i,del(i),ccc,scale
      DELWAV(I)=CCC                                                             
c      DO 82 J=1,NRHOX                                                           
c   82 ALINE(J)=ALINE0(J)*VOIGT(CCC/DOPWAV(IVT,J),ADAMP(IVT,J))                  
	do 822 j=1,nrhox
	aline1=aline01(j)*voigt((abs(ccc+7.4))/dopwav1(ivt,j),adampa(ivt,j))
	aline2=aline02(j)*voigt((abs(ccc+5.0))/dopwav2(ivt,j),adampb(ivt,j))
	aline3=aline03(j)*voigt((abs(ccc-12.5))/dopwav3(ivt,j),adampc(ivt,j))
	aline(j)=aline1+aline2+aline3
822	continue
      CALL JOSH(IFSCAT,IFSURF)                                                  
      IF(IFSURF.LT.2)RESIDU(I)=HNU(1)/CONTIN                                    
      IF(IFSURF.EQ.2)RESIDU(I)=SURFI(1)/CONTIN                                  
c	type*,i,delwav(i),residu(i)
      MAX=MAP1(TAUNU,RHOX,NRHOX,1.D0,TAUONE(I),1)                              
      TAUONE(I)= LOG10(TAUONE(I))                                               
      TAUSUM=TAUSUM+WEIGHT(I)*TAUONE(I)*(1.-RESIDU(I))                          
      WIDTH=WIDTH+WEIGHT(I)*(1.-RESIDU(I))                                      
81	continue
      TAUBAR=EXP10(TAUSUM/WIDTH)                                                
c      WID=WIDTH*SCALE*2.
	widmg=width*scale                                                        
      IF(IFPROF.EQ.0)RETURN
	k1=1
998	k2=14+k1
       WRITE(6,87)(DELWAV(k),TAUONE(k),RESIDU(k),k=k1,k2)                       
c     WRITE(6,87)((DELWAV(k),k=k1,k2),(tauone(k),k=k1,k2),
c1 (residu(k),k=k1,k2))     
   87 FORMAT(17X3HDEL15F7.3/16X5HDEPTH15F7.3/16X5HRESID15F7.3/)               
	k1=1+k2
c	if(k1.le.105)go to 998
	if(k1.le.585)go to 998
	open(unit=8,file='mg.out',status='new')
	do 2000 i=1,600
	write(8,877)delwav(i),residu(i)
877	format(1x,2f15.4)
2000	continue
	close (unit=8)
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LINCEN(ABUND1,VTS,NVT)                                         
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (kw=99)
      COMMON /ELEM/ABUND(99),ATMASS(99),ELEM(99)                                
      COMMON /FREQ/FREQ,FREQLG,EHVKT(kw),STIM(kw),BNU(kw)                       
      COMMON /IONS/XNFPH(kw,2),XNFPHE(kw,3),XNFH(kw,2),XNFHE(kw,3)
      COMMON /IF/IFCORR,IFPRES,IFSURF,IFSCAT,TAUSCAT,IFMOL                              
      COMMON /RHOX/RHOX(kw),NRHOX                                               
      COMMON /STATE/P(kw),XNE(kw),XNATOM(kw),RHO(kw)                            
      COMMON /TEMP/T(kw),TKEV(kw),TK(kw),HKT(kw),TLOG(kw),HCKT(kw),ITEMP
      COMMON /TURBPR/VTURB(kw),PTURB(kw),TRBFDG,TRBCON,TRBPOW,TRBSND,           
     1               IFTURB                                                     
      COMMON /XABUND/XABUND(99),WTMOLE                                          
C                                                                               
      COMMON/LINDAT/WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(3),LABELP(3),       
     1               GAMMAR,GAMMAS,GAMMAW,REF,X1,           
     2               X2,OTHER1(3),OTHER2(3),ELO,GF,ISO1,ISO2,                              
     3               NBLO,NBUP,NELION
      REAL*8 WL,E,EP                                                            
      COMMON /LINEY/LINE0(3,kw),DOPWAV(3,kw),ADAMP(3,kw)                        
      REAL*8 LINE0                                                                
c      COMMON /RESIDU/RESIDU(150),TAUONE(150),DELWAV(150)              
      COMMON /RESIDU/RESIDU(1000),TAUONE(1000),DELWAV(1000)              
      DIMENSION VTS(3)                                                          
      DIMENSION TDOP(kw),ADAMP1(kw),LINE(kw),XNFPRC(kw),TXNXN(kw)               
      REAL*8 LINE                                                                 
      DIMENSION XCODE(8)                                                        
      DATA XCODE/1.E14,1.E12,1.E10,1.E8,1.E6,1.E4,1.E2,1.E0/                    
      DATA ITEMP1/0/                                                            
      DIMENSION PART(kw,6)
c      interface
c        subroutine exit(status)
c          integer(4),optional,intent(in)::status
c        end subroutine
c        subroutine abort(string)
c          !ms$attributes alias:'abort_'::abort
c          character(len=*),optional,intent(in)::string
c        end subroutine
c      end interface
      IF(ITEMP.EQ.ITEMP1)GO TO 2
      XNFH2=0.                                                                  
      DO 1 J=1,NRHOX                                                            
C     NUMBER DENSITIES FOR VAN DER WAALS BROADENING                             
    1 TXNXN(J)=(XNFPH(J,1)*2.+.42*XNFPHE(J,1)*1.+.85*XNFH2)*                    
     1(T(J)/10000.)**.3                                                         
      ITEMP1=ITEMP                                                              
      SAVE=0.                                                                   
    2 IF(CODE.EQ.SAVE)GO TO 18                                                  
      AMASS=0.                                                                  
      ABUND1=1.                                                                 
      C=CODE                                                                    
      DO 11 II=1,8                                                              
      IF(C.GE.XCODE(II))GO TO 12                                                
   11 CONTINUE                                                                  
      CALL EXIT                                                                 
   12 DO 13 I=II,8                                                              
      ID=C/XCODE(I)                                                             
      IF(ID.LT.100)ABUND1=ABUND1*XABUND(ID)                                     
      AMASS=AMASS+ATMASS(ID)                                                    
   13 C=C-FLOAT(ID)*XCODE(I)                                                    
      SAVE=CODE                                                                 
      CALL POPS(CODE,1,XNFPRC)                                                  
      TWOMAS=2./AMASS/1.660E-24                                                 
      DO 17 J=1,NRHOX                                                           
      XNFPRC(J)=XNFPRC(J)*(.026538/1.77245)/ABUND1/RHO(J)                       
   17 TDOP(J)=TK(J)*TWOMAS                                                      
   18 ELOC=ELO*2.997925E10                                                      
      DO 5 J=1,NRHOX                                                            
      ADAMP1(J)=(GAMMAR+GAMMAS*XNE(J)+GAMMAW*TXNXN(J))/12.5664                  
    5 LINE(J)=GF*EXP(-ELOC*HKT(J))*STIM(J)*XNFPRC(J)                            
      FREQC=FREQ/2.997925E10                                                    
      WAVENU=1.E10/FREQC/FREQ                                                   
      IF(VTS(1).EQ.0..AND.NVT.EQ.1)GO TO 7                                      
      DO 6 IVT=1,NVT                                                            
      VT2=VTS(IVT)**2*1.E10                                                     
      DO 6 J=1,NRHOX                                                            
      DOPNU=SQRT(TDOP(J)+VT2)*FREQC                                             
C     DOPWAV IS DOPLER WIDTH IN PM                                              
      DOPWAV(IVT,J)=DOPNU*WAVENU                                                
C     LINE ABSORPTION COEFFICIENT PER UNIT ABUNDANCE                            
      LINE0(IVT,J)=LINE(J)/DOPNU                                                
    6 ADAMP(IVT,J)=ADAMP1(J)/DOPNU                                              
      RETURN                                                                    
    7 DO 8 J=1,NRHOX                                                            
      DOPNU=SQRT(TDOP(J)+VTURB(J)**2)*FREQC                                     
      DOPWAV(1,J)=DOPNU*WAVENU                                                  
      LINE0(1,J)=LINE(J)/DOPNU                                                  
    8 ADAMP(1,J)=ADAMP1(J)/DOPNU                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LINCENmg(ABUND1,VTS,NVT)                                      
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (kw=99)
      COMMON /ELEM/ABUND(99),ATMASS(99),ELEM(99)                                
      COMMON /FREQ/FREQ,FREQLG,EHVKT(kw),STIM(kw),BNU(kw)                       
      COMMON /IONS/XNFPH(kw,2),XNFPHE(kw,3),XNFH(kw,2),XNFHE(kw,3)
      COMMON /IF/IFCORR,IFPRES,IFSURF,IFSCAT,TAUSCAT,IFMOL                     
      COMMON /RHOX/RHOX(kw),NRHOX                                               
      COMMON /STATE/P(kw),XNE(kw),XNATOM(kw),RHO(kw)                            
      COMMON /TEMP/T(kw),TKEV(kw),TK(kw),HKT(kw),TLOG(kw),HCKT(kw),ITEMP
      COMMON /TURBPR/VTURB(kw),PTURB(kw),TRBFDG,TRBCON,TRBPOW,TRBSND,           
     1               IFTURB                                                     
      COMMON /XABUND/XABUND(99),WTMOLE                                          
C                                                                               
      COMMON/LINDAT/WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(3),LABELP(3),       
     1               GAMMAR,GAMMAS,GAMMAW,REF,X1,           
     2               X2,OTHER1(3),OTHER2(3),ELO,GF,ISO1,ISO2,                  
     3               NBLO,NBUP,NELION
      REAL*8 WL,E,EP                                                            
      COMMON /LINEY/LINE0(3,kw),DOPWAV(3,kw),ADAMP(3,kw)                        
      COMMON /LINEY1/LINE01(3,kw),DOPWAV1(3,kw),ADAMPa(3,kw)                   
      COMMON /LINEY2/LINE02(3,kw),DOPWAV2(3,kw),ADAMPb(3,kw)                    
      COMMON /LINEY3/LINE03(3,kw),DOPWAV3(3,kw),ADAMPc(3,kw)                    
      REAL*8 LINE0,line01,line02,line03                                        
c      COMMON /RESIDU/RESIDU(150),TAUONE(150),DELWAV(150)              
      COMMON /RESIDU/RESIDU(1000),TAUONE(1000),DELWAV(1000)              
      DIMENSION VTS(3)                                                          
      DIMENSION TDOP(kw),ADAMP1(kw),LINE(kw),XNFPRC(kw),TXNXN(kw)               
	dimension line1(kw),line2(kw),line3(kw)
      REAL*8 LINE,line1,line2,line3                                                               
      DIMENSION XCODE(8)                                                        
      DATA XCODE/1.E14,1.E12,1.E10,1.E8,1.E6,1.E4,1.E2,1.E0/                    
      DATA ITEMP1/0/                                                            
      DIMENSION PART(kw,6)
c      interface
c        subroutine exit(status)
c          integer(4),optional,intent(in)::status
c        end subroutine
c        subroutine abort(string)
c          !ms$attributes alias:'abort_'::abort
c          character(len=*),optional,intent(in)::string
c        end subroutine
c      end interface
      IF(ITEMP.EQ.ITEMP1)GO TO 2
      XNFH2=0.                                                                  
      DO 1 J=1,NRHOX                                                            
C     NUMBER DENSITIES FOR VAN DER WAALS BROADENING                             
    1 TXNXN(J)=(XNFPH(J,1)*2.+.42*XNFPHE(J,1)*1.+.85*XNFH2)*                    
     1(T(J)/10000.)**.3                                                         
      ITEMP1=ITEMP                                                              
      SAVE=0.                                                                   
    2 IF(CODE.EQ.SAVE)GO TO 18                                                  
      AMASS=0.                                                                  
      ABUND1=1.                                                                 
      C=CODE                                                                    
      DO 11 II=1,8                                                              
      IF(C.GE.XCODE(II))GO TO 12                                                
   11 CONTINUE                                                                  
      CALL EXIT                                                                 
   12 DO 13 I=II,8                                                              
      ID=C/XCODE(I)                                                             
      IF(ID.LT.100)ABUND1=ABUND1*XABUND(ID)                                     
      AMASS=AMASS+ATMASS(ID)                                                    
   13 C=C-FLOAT(ID)*XCODE(I)                                                    
      SAVE=CODE                                                                 
      CALL POPS(CODE,1,XNFPRC)                                                  
      TWOMAS=2./AMASS/1.660E-24                                                 
      DO 17 J=1,NRHOX                                                           
      XNFPRC(J)=XNFPRC(J)*(.026538/1.77245)/ABUND1/RHO(J)                       
   17 TDOP(J)=TK(J)*TWOMAS                                                      
   18 ELOC=ELO*2.997925E10
	elo1c=71490.190*2.997925E10
	elo2c=elo1c
	elo3c=71491.063*2.997925e10                                                      
      DO 5 J=1,NRHOX                                                            
      ADAMP1(J)=(GAMMAR+GAMMAS*XNE(J)+GAMMAW*TXNXN(J))/12.5664                  
      LINE(J)=GF*EXP(-ELOC*HKT(J))*STIM(J)*XNFPRC(J)                            
	gf1=exp(0.738*2.30258509299405E0)
	gf2=exp(-0.562*2.30258509299405E0)
	gf3=exp(0.578*2.30258509299405E0)
	freq1=2.997925e17/448.1126
	freq2=2.997925e17/448.1150
	freq3=2.997925e17/448.1325
	ehvkt1=exp(-freq1*hkt(j))
	ehvkt2=exp(-freq2*hkt(j))
	ehvkt3=exp(-freq3*hkt(j))
	stim1=1.-ehvkt1
	stim2=1.-ehvkt2
	stim3=1.-ehvkt3
	line1(j)=gf1*exp(-elo1c*HKT(J))*stim1*xnfprc(j)
	line2(j)=gf2*exp(-elo2c*HKT(J))*stim2*xnfprc(j)
	line3(j)=gf3*exp(-elo3c*HKT(J))*stim3*xnfprc(j)
5	continue
      FREQC=FREQ/2.997925E10                                                    
	freq1c=freq1/2.997925E10
	freq2c=freq2/2.997925E10
	freq3c=freq3/2.997925E10
      WAVENU=1.E10/FREQC/FREQ                                                   
      WAVENU1=1.E10/FREQ1C/FREQ1                                                   
      WAVENU2=1.E10/FREQ2C/FREQ2                                                   
      WAVENU3=1.E10/FREQ3C/FREQ3                                                   
      IF(VTS(1).EQ.0..AND.NVT.EQ.1)GO TO 7                                      
      DO 6 IVT=1,NVT                                                            
      VT2=VTS(IVT)**2*1.E10                                                     
      DO 6 J=1,NRHOX                                                            
      DOPNU=SQRT(TDOP(J)+VT2)*FREQC                                             
      DOPNU1=SQRT(TDOP(J)+VT2)*FREQ1C                                          
      DOPNU2=SQRT(TDOP(J)+VT2)*FREQ2C                                          
      DOPNU3=SQRT(TDOP(J)+VT2)*FREQ3C                                          
C     DOPWAV IS DOPLER WIDTH IN PM                                              
      DOPWAV(IVT,J)=DOPNU*WAVENU                                                
      DOPWAV1(IVT,J)=DOPNU1*WAVENU1                                             
      DOPWAV2(IVT,J)=DOPNU2*WAVENU2                                             
      DOPWAV3(IVT,J)=DOPNU3*WAVENU3                                             
C     LINE ABSORPTION COEFFICIENT PER UNIT ABUNDANCE                            
      LINE0(IVT,J)=LINE(J)/DOPNU                                                
      LINE01(IVT,J)=LINE1(J)/DOPNU1                                                
      LINE02(IVT,J)=LINE2(J)/DOPNU2                                                
      LINE03(IVT,J)=LINE3(J)/DOPNU3                                                
      ADAMP(IVT,J)=ADAMP1(J)/DOPNU                                              
	adampa(ivt,j)=adamp1(j)/dopnu1
	adampb(ivt,j)=adamp1(j)/dopnu2
	adampc(ivt,j)=adamp1(j)/dopnu3
6	continue
      RETURN                                                                    
    7 DO 8 J=1,NRHOX                                                            
      DOPNU=SQRT(TDOP(J)+VTURB(J)**2)*FREQC                                     
      DOPNU1=SQRT(TDOP(J)+VTURB(J)**2)*FREQ1C                                   
      DOPNU2=SQRT(TDOP(J)+VTURB(J)**2)*FREQ2C                                   
      DOPNU3=SQRT(TDOP(J)+VTURB(J)**2)*FREQ3C                                   
      DOPWAV(1,J)=DOPNU*WAVENU                                                  
      DOPWAV1(1,J)=DOPNU1*WAVENU1                                              
      DOPWAV2(1,J)=DOPNU2*WAVENU2                                              
      DOPWAV3(1,J)=DOPNU3*WAVENU3                                               
      LINE0(1,J)=LINE(J)/DOPNU                                                  
      LINE01(1,J)=LINE1(J)/DOPNU1                                               
      LINE02(1,J)=LINE2(J)/DOPNU2                                               
      LINE03(1,J)=LINE3(J)/DOPNU2                                               
      ADAMP(1,J)=ADAMP1(J)/DOPNU                                                
      ADAMPa(1,J)=ADAMP1(J)/DOPNU1                                                
      ADAMPb(1,J)=ADAMP1(J)/DOPNU2                                                
      ADAMPc(1,J)=ADAMP1(J)/DOPNU3                                                
8	continue
      RETURN                                                                    
      END                                                                       
      FUNCTION VOIGT(V,A)                                                       
CC     INTERPOLATES FROM TABLES OF HARRIS AP.J. 1948                             
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION H0(41),H1(81),H2(41)                                            
      DATA H0 /                                                                 
     1 1.0000000, 0.9900500, 0.9607890, 0.9139310, 0.8521440, 0.7788010,        
     2 0.6976760, 0.6126260, 0.5272920, 0.4448580, 0.3678790, 0.2981970,        
     3 0.2369280, 0.1845200, 0.1408580, 0.1053990, 0.0773050, 0.0555760,        
     4 0.0391640, 0.0270520, 0.0183156, 0.0121552, 0.0079071, 0.0050418,        
     5 0.0031511, 0.0019305, 0.0011592, 0.0006823, 0.0003937, 0.0002226,        
     6 0.0001234, 0.0000671, 0.0000357, 0.0000186, 0.0000095, 0.0000048,        
     7 0.0000024, 0.0000011, 0.0000005, 0.0000002, 0.0000001/                   
      DATA H1/                                                                  
     1-1.1283800,-1.1059600,-1.0404800,-0.9370300,-0.8034600,-0.6494500,        
     2-0.4855200,-0.3219200,-0.1677200,-0.0301200, 0.0859400, 0.1778900,        
     3 0.2453700, 0.2898100, 0.3139400, 0.3213000, 0.3157300, 0.3009400,        
     4 0.2802700, 0.2564800, 0.2317260, 0.207528 , 0.1848820, 0.1643410,        
     5 0.1461280, 0.1302360, 0.1165150, 0.1047390, 0.0946530, 0.0860050,        
     6 0.0785650, 0.0721290, 0.0665260, 0.0616150, 0.0572810, 0.0534300,        
     7 0.0499880, 0.0468940, 0.0440980, 0.0415610, 0.0392500, 0.0351950,        
     8 0.0317620, 0.0288240, 0.0262880, 0.0240810, 0.0221460, 0.0204410,        
     9 0.0189290, 0.0175820, 0.0163750, 0.0152910, 0.0143120, 0.0134260,        
     A 0.0126200, 0.0118860, 0.0112145, 0.0105990, 0.0100332, 0.0095119,        
     B 0.0090306, 0.0085852, 0.0081722, 0.0077885, 0.0074314, 0.0070985,        
     C 0.0067875, 0.0064967, 0.0062243, 0.0059688, 0.0057287, 0.0055030,        
     D 0.0052903, 0.0050898, 0.0049006, 0.0047217, 0.0045526, 0.0043924,        
     E 0.0042405, 0.0040964, 0.0039595/                                         
      DATA H2 /                                                                 
     1 1.0000000, 0.9702000, 0.8839000, 0.7494000, 0.5795000, 0.3894000,        
     2 0.1953000, 0.0123000,-0.1476000,-0.2758000,-0.3679000,-0.4234000,        
     3-0.4454000,-0.4392000,-0.4113000,-0.3689000,-0.3185000,-0.2657000,        
     4-0.2146000,-0.1683000,-0.1282100,-0.0950500,-0.0686300,-0.0483000,        
     5-0.0331500,-0.0222000,-0.0145100,-0.0092700,-0.0057800,-0.0035200,        
     6-0.0021000,-0.0012200,-0.0007000,-0.0003900,-0.0002100,-0.0001100,        
     7-0.0000600,-0.0000300,-0.0000100,-0.0000100, 0.0000000/                   
      V0=V*10.                                                                  
      N=V0                                                                      
      IF(N.LT.40)GO TO 1                                                        
      IF(N.LT.120)GO TO 2                                                       
      VOIGT=(.56419+.846/(V**2))/(V**2)*A                                       
c      PRINT 7777,V,A,VOIGT
 7777 FORMAT(1P3E12.3)
      RETURN                                                                    
    1 V1=N                                                                      
      N=N+1                                                                     
      V2=V0-V1                                                                  
      N1=N+1                                                                    
      VOIGT=V2*(H0(N1)-H0(N)+A*(H1(N1)-H1(N)+A*(H2(N1)-H2(N))))+                
     1H0(N)+A*(H1(N)+A*H2(N))                                                   
C      PRINT 7777,V,A,VOIGT
      RETURN                                                                    
    2 N=N/2+20                                                                  
      V1=(N-20)*2                                                               
      N=N+1                                                                     
      V2=(V0-V1)/2.                                                             
      N1=N+1                                                                    
      VOIGT=A*((H1(N1)-H1(N))*V2+H1(N))                                         
C      PRINT 7777,V,A,VOIGT
      RETURN                                                                    
      END                                                                       
      SUBROUTINE AVERAG(MODEAV,IVT,ABLG,TAULG)                                  
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/LINDAT/WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(3),LABELP(3),       
     1               GAMMAR,GAMMAS,GAMMAW,REF,X1,           
     2               X2,OTHER1(3),OTHER2(3),ELO,GF,ISO1,ISO2,                              
     3               NBLO,NBUP,NELION
      REAL*8 WL,E,EP                                                            
      COMMON /OBS/EW,WLOBS,STAR(19)                                             
      COMMON /VTS/VTS(3),NVT                                                    
      COMMON ABUNDI(250,3),TAUBAR(250,3)                                        
      DIMENSION KABUND(250,3)                                                   
C      EQUIVALENCE (KABUND(1),ABUNDI(1))                                        
      DIMENSION EPOT(250),WDTH(250)                                             
      DIMENSION STAR1(20)                                                       
      DATA NLINES/0/                                                            
C      EXP10(X)=EXP(X*2.30258509299405E0)                                       
c      interface
c        subroutine exit(status)
c          integer(4),optional,intent(in)::status
c        end subroutine
c        subroutine abort(string)
c          !ms$attributes alias:'abort_'::abort
c          character(len=*),optional,intent(in)::string
c        end subroutine
c      end interface
      IF(MODEAV.EQ.1)RETURN
      IF(MODEAV.EQ.2)GO TO 64                                                   
      IF(MODEAV.EQ.3)GO TO 30                                                   
      CALL EXIT                                                                 
   64 IF(IVT.EQ.1)NLINES=NLINES+1                                               
      ABUNDI(NLINES,IVT)=ABLG                                                   
      TAUBAR(NLINES,IVT)=TAULG                                                  
      EPOT(NLINES)=ELO*1.23981E-4                                               
      WDTH(NLINES)=EW                                                           
      DO 25 I=1,19                                                              
   25 STAR1(I)=STAR(I)                                                          
      SAVE=CODE                                                                 
      RETURN                                                                    
   30 IF(NLINES.NE.1) GO TO 31                                                  
      DEV=0.                                                                    
      CODE=SAVE                                                                 
      WRITE(6,33)NLINES,CODE,ABLG,DEV                                           
      GO TO 35                                                                  
   31 DO 34 IVT=1,NVT                                                           
      SUMM=0.                                                                   
      CSUMXY=0.                                                                 
      CSUMX=0.                                                                  
      CSUMXX=0.                                                                 
      WSUMXY=0.                                                                 
      WSUMX=0.                                                                  
      WSUMXX=0.                                                                 
      DO 6005 I=1,NLINES                                                        
      SUMM=SUMM+ABUNDI(I,IVT)                                                   
      CSUMXY=CSUMXY+ABUNDI(I,IVT)*EPOT(I)                                       
      CSUMX=CSUMX+EPOT(I)                                                       
      CSUMXX=CSUMXX+EPOT(I)**2                                                  
      WSUMXY=WSUMXY+ABUNDI(I,IVT)*WDTH(I)                                       
      WSUMX=WSUMX+WDTH(I)                                                       
      WSUMXX=WSUMXX+WDTH(I)**2                                                  
 6005 CONTINUE                                                                  
      X=NLINES                                                                  
      WB=( X*WSUMXY-WSUMX*SUMM)/(X*WSUMXX-WSUMX**2)                             
      CB=(X*CSUMXY-CSUMX*SUMM)/(X*CSUMXX-CSUMX**2)                              
      AVABN=SUMM/X                                                              
      SUM=0.                                                                    
      DO 32 I=1,NLINES                                                          
   32 SUM=SUM+(AVABN-ABUNDI(I,IVT))**2                                          
      DEV=SQRT(SUM/X)                                                           
      DO 26 I=1,19                                                              
   26 STAR(I)=STAR1(I)                                                          
      CODE=SAVE                                                                 
      WRITE(6,1111)VTS(IVT),STAR                                                
 1111 FORMAT(6H1VTURB,F6.3,3X,19A4)                                             
      WRITE(6,33)NLINES,CODE,AVABN,DEV                                          
   33 FORMAT(///26H ***** THE ABUNDANCE FROM I3,2XF11.2,9H LINES ISF7.2,        
     1 3H+/-,F5.2)                                                              
      WRITE(6,6006)                                                             
 6006 FORMAT(//5X,17HLOG ABUND VS. CHI)                                         
      DO 50 I=1,NLINES                                                          
   50 KABUND(I,IVT)=(AVABN-ABUNDI(I,IVT))*50.+51.5                              
      WRITE(6,6009)CB,NLINES                                                    
 6009 FORMAT(5X,7H SLOPE=,E10.3,14H PER EV USING I4,6H LINES)                   
      CALL PLOTIT(AVABN,KABUND(1,IVT),EPOT,NLINES)                              
      WRITE(6,1111)VTS(IVT),STAR                                                
      WRITE(6,6007)                                                             
 6007 FORMAT(//5X,23HLOG ABUND VS. EQ. WIDTH)                                   
      WRITE(6,6008)WB,NLINES                                                    
 6008 FORMAT(5X,7H SLOPE=,E10.3,14H PER PM USING I4,6H LINES)                   
      CALL PLOTIT(AVABN,KABUND(1,IVT),WDTH,NLINES)                              
      WRITE(6,1111)VTS(IVT),STAR                                                
      WRITE(6,1112)                                                             
 1112 FORMAT(//5X,24HLOG ABUND VS. LOG HEIGHT)                                  
      CALL PLOTIT(AVABN,KABUND(1,IVT),TAUBAR(1,IVT),NLINES)                     
   34 CONTINUE                                                                  
   35 NLINES=0                                                                  
      MODEAV=1                                                                  
      WRITE(6,3333)                                                             
 3333 FORMAT(1H1)                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE PLOTIT(AVX,KX,Y,N)                                             
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION KX(1),Y(1)                                                      
      DIMENSION IPLOT(101),AXIS(11)                                             
      DATA IBLANK,IDOT,IMINUS,II,IX,I2/1H ,1H.,1H-,1HI,1HX,1H2/                 
      DO 6  I=1,11                                                              
    6 AXIS(I)=AVX+.2*FLOAT(6-I)                                                 
      YMAX=-1.E30                                                               
      YMIN=1.E30                                                                
      DO 1 I=1,N                                                                
      YMIN= MIN (YMIN,Y(I))                                                     
    1 YMAX= MAX (YMAX,Y(I))                                                     
      IF(YMIN.GT.0.)YMIN=0.                                                     
      SLOPE=50./(YMAX-YMIN)                                                     
      DO 3  J=1,51                                                              
      ISYMB=IBLANK                                                              
      IF(MOD(J,10).EQ.1)ISYMB=IMINUS                                            
      DO 4  K=1,101                                                             
    4 IPLOT(K)=ISYMB                                                            
      DO 44 K=2,100,2                                                           
   44 IPLOT(K)=IBLANK                                                           
      DO 40 K=1,101,10                                                          
   40 IPLOT(K)=IDOT                                                             
      IPLOT(51)=II                                                              
      DO 5  I=1,N                                                               
      IY=51.5-SLOPE*(Y(I)-YMIN)                                                 
      IF(IY.NE.J)GO TO 5                                                        
      L=KX(I)                                                                   
      IF(L.LT.1) GO TO 5                                                        
      IF(L.GT.101)  GO TO 5                                                     
      IF(IPLOT(L).EQ.IX) GO TO 7                                                
      IPLOT(L)=IX                                                               
      GO TO 5                                                                   
    7 IPLOT(L)=I2                                                               
    5 CONTINUE                                                                  
      YY=FLOAT(51-J)/SLOPE+YMIN                                                 
      WRITE(6,10)YY,IPLOT                                                       
   10 FORMAT(F12.2,1X101A1)                                                     
    3 CONTINUE                                                                  
      WRITE(6,20) AXIS                                                          
   20 FORMAT(5X,11F10.2)                                                        
      RETURN                                                                    
      END                                                                       
c      FUNCTION VOIGT(V,A)                                                       
c      IMPLICIT REAL*8 (A-H,O-Z)
cC     FUNCTION H(A,V)                                                           
Cc     FROM ERIC PEYTREMANN                                                      
c      LOGICAL Q                                                                 
c      VV=V*V                                                                    
c      Q=A.LT.0.2                                                                
c      IF(Q.AND.V.GT.5.)GOTO 1                                                   
c      IF(.NOT.Q.AND.(A.GT.1.4.OR.A+V.GT.3.2))GOTO 2                             
c      HO=EXP(-VV)                                                               
c      H2=(1.-2.*VV)*HO                                                          
c      IF(V.GT.2.4)GOTO 3                                                        
c      IF(V.GT.1.3)GOTO 4                                                        
c      H1=(.42139*VV-2.34358*V+3.28868)*VV-.15517*V-1.1247                       
c    5 IF(Q)GOTO 6                                                               
c      HH1=H1+HO*1.12838                                                         
c      HH2=H2+HH1*1.12838-HO                                                     
c      HH3=(1.-H2)*.37613-HH1*.66667*VV+HH2*1.12838                              
c      HH4=(3.*HH3-HH1)*.37613+HO*.66667*VV*VV                                   
Cc     H=((((HH4*A+HH3)*A+HH2)*A+HH1)*A+HO)*(((-.122727278*A+.532770573)*        
c      VOIGT=                                                                    
c     A  ((((HH4*A+HH3)*A+HH2)*A+HH1)*A+HO)*(((-.122727278*A+.532770573)*        
c     1A-.96284325)*A+.979895032)                                                
c      RETURN                                                                    
c    1 VOIGT=((2.12/VV+.8463)/VV+.5642)*A/VV                                     
Cc   1 H=((2.12/VV+.8463)/VV+.5642)*A/VV                                         
c      RETURN                                                                    
c    2 AA=A*A                                                                    
c      U=(AA+VV)*1.4142                                                          
c      UU=U*U                                                                    
Cc     H=((((AA-10.*VV)*AA*3.+15.*VV*VV)/UU+3.*VV-AA)/UU+1.)*A*.79788/U          
c      VOIGT=                                                                    
c     A  ((((AA-10.*VV)*AA*3.+15.*VV*VV)/UU+3.*VV-AA)/UU+1.)*A*.79788/U          
c      RETURN                                                                    
c    3 H1=((-.0032783*VV+.0429913*V-.188326)*VV+.278712*V+.55415)/(VV-1.5        
c     1)                                                                         
c      GOTO 5                                                                    
c    4 H1=(-.220416*VV+1.989196*V-6.61487)*VV+9.39456*V-4.4848                   
c      GOTO 5                                                                    
c    6 VOIGT=(H2*A+H1)*A+HO                                                      
Cc   6 H=(H2*A+H1)*A+HO                                                          
c      RETURN                                                                    
c      END                                                                       
