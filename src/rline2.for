      PROGRAM RGFALL
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
c     revised 25may97
c     this program is a quick and dirty demonstration of replacing programs
c     RNLTE and RLINE while keeping all the other SYNTHE programs the same.
C      READS LINES FROM UNIT 11 AND WRITES THEM ON UNIT 19 IF THE LINE
C      WAS ORIGINALLY FROM THE FILE NLTELINES.DAT OR TO UNIT 12 IF NOT,
C      IF IFNLTE=0 UNIT 19 IS READ BY SYNTHE AND THE LINES ARE
C      TREATED IN LTE.  IF IFNLTE=1 UNIT 19 IS READ BY SPECTR AND THE
C      LINES ARE TREATED IN NLTE IF THE MODEL IS NLTE.
C        THESE LINES ARE TREATED WITH EXACT VOIGT OR FANO PROFILES
C     WL IS THE AIR WAVELENGTH IF WL .GT. 200 NM
C        IF THE SWITCH IFVAC=1 THE WAVELENGTH USED BY THE PROGRAM WILL
C        BE THE VACUUM WAVELENGTH OBTAINED FROM THE DIFFERENCE OF
C        THE ENERGY LEVELS
C     A SUFFIX P STANDS FOR PRIME INDICATING THE SECOND CONFIGURATION
C     J IS ANGULAR MOMENTUM
C     E IS ENERGY IN WAVENUMBERS
C     LABEL IS A LABEL FOR THE CONFIGURATION
C          THE GF TAPE DOES NOT KEEP LABEL AND LABELP DISTINCT
C     CODE FOR ATOM OR MOLECULE
C     NELION IS THE STORAGE LOCATION OF ELEM IN ARRAYS XNFPEL AND DOPPLE
C     GAMMAR IS THE RADIATIVE DAMPING CONSTANT
C     GAMMAW IS THE DAMPING CONSTANT PER HYDROGEN ATOM FOR VAN DER WAALS
C            BROADENING BY HYDROGEN AT T=10000K.
C            FOR HELIUM MULTIPLY BY .42
C            FOR H2 MULTIPLY BY .85
C     GAMMAS IS THE STARK DAMPING CONSTANT PER ELECTRON ASSUMED TO BE
C            TEMPERATURE INDEPENDENT
C     TO CONVERT GRIEM"S HALF WIDTH TO GAMMAS  FOR DLAM AND LAM IN A
C     GAMMAS=3767.*DLAM/LAM**2
C     LOG(GAMMA) IS READ IN
C     IF NOT READ IN GAMMAR IS CLASSICAL, GAMMAW IS FROM ALLER, AND
C            GAMMAS IS FROM PEYTREMANN
C     REF ARE A REFERENCE OR REFERENCES FOR GF AND DAMPING CONSTANTS
C     NBLO AND NBUP REFER TO DEPARTURE COEFFICIENT ARRAYS FOR THE LOWER
C        AND UPPER LEVELS (NOT FIRST AND SECOND)
C     ISO1 AND ISO2 ARE ISOTOPE NUMBERS FOR UP TO 2 COMPONENTS
C     X1 AND X2 ARE LOG FRACTIONAL ISOTOPIC ABUNDANCES THAT ARE ADDED TO
C        LOG GF TO OBTAIN AN ISOTOPIC ABUNDANCE
C     OTHER1 AND 2 ARE ADDITIONAL LABEL FIELDS OR QUANTUM NUMBERS OR
C        WHATEVER
C        OTHER1 IS NOW USED TO STORE LANDE G VALUES AS 2 I5 INTEGERS IN UNITS
C        OF .001 .  EXAMPLE  GLANDE=-.007 GLANDEP=2.499   OTHER1=   -7 2499
C     DWL  CORRECTION TO WL
C     DLOGGF  CORRECTION TO LOGGF
C     DGAMMAR  LOG CORRECTION TO GAMMAR
C     DGAMMAS  LOG CORRECTION TO GAMMAS
C     DGAMMAW  LOG CORRECTION TO GAMMAW
C     ISOSHIFT  IS ISOTOPE SHIFT OF WAVELENGTH IN MK = 0.001 CM-1
CC     SAMPLE CARDS
C 396.8470 -0.162  0.5       0.000  1.5   25191.541    20.01 4S        4P
C 396.8470 116  8.24 -4.44 -7.80 REF
C********
C revision for IFC 8.0, 23012004 LS
c      use ifport
C *******
      PARAMETER (kw=99)
      COMMON /LINDAT/WL,E,EP,LABEL(2),LABELP(2),OTHER1(2),OTHER2(2),
     1        WLVAC,CENTER,CONCEN, NELION,GAMMAR,GAMMAS,GAMMAW,REF,
     2      NBLO,NBUP,ISO1,X1,ISO2,X2,GFLOG,XJ,XJP,CODE,ELO,GF,GS,GR,GW,
     3        DWL,DGFLOG,DGAMMAR,DGAMMAS,DGAMMAW,DWLISO,ISOSHIFT,EXTRA3
      REAL*8 LINDAT8(14)
      REAL*4 LINDAT4(28)
      EQUIVALENCE (LINDAT8(1),WL),(LINDAT4(1),NELION)
      REAL*8 RESOLU,RATIO,RATIOLG,SIGMA2,WLBEG,WLEND
      REAL*8 WL,E,EP,WLVAC,CENTER,CONCEN
      REAL*8 LABEL,LABELP,OTHER1,OTHER2
      CHARACTER*20 NOTES
      CHARACTER*10 COTHER1,COTHER2
      EQUIVALENCE (COTHER1,OTHER1(1)),(COTHER2,OTHER2(1))
      CHARACTER*3 AUTO
      CHARACTER*6 IXFIXFP
      DIMENSION DECKJ(7,kw)
      INTEGER TYPE
      EQUIVALENCE (GAMMAS,ASHORE),(GAMMAW,BSHORE)
      EQUIVALENCE (GF,G,CGF),(TYPE,NLAST),(GAMMAR,XSECT,GAUNT)
      COMMON /POTION/POTION(594)
      DIMENSION CODEX(17)
      DIMENSION DELLIM(7)
      DIMENSION NTENS(10)
      DATA NTENS/1,10,100,1000,10000,100000,1000000,10000000,
     1 100000000,1000000000/
      DATA CODEX/1.,2.,2.01,6.,6.01,12.,12.01,13.,13.01,14.,14.01,
     1 20.,20.01,8.,11.,5.,19./
      DATA DELLIM/100.,30.,10.,3.,1.,.3,.1/
C     CALL BEGTIME
c      interface
c        subroutine exit(status)
c          integer(4),optional,intent(in)::status
c        end subroutine
c        subroutine abort(string)
c          !ms$attributes alias:'abort_'::abort
c          character(len=*),optional,intent(in)::string
c        end subroutine
c      end interface
c
      READ(93)NLINES,LENGTH,IFVAC,IFNLTE,N19,TURBV,DECKJ,IFPRED,
     1WLBEG,WLEND,RESOLU,RATIO,RATIOLG,CUTOFF,LINOUT
      IXWLBEG=DLOG(WLBEG)/RATIOLG
      IF(DEXP(IXWLBEG*RATIOLG).LT.WLBEG)IXWLBEG=IXWLBEG+1
      DELFACTOR=1.
      IF(WLBEG.GT.500.)DELFACTOR=WLBEG/500.
      N14=0
      OPEN(UNIT=11,file='fort.11',STATUS='OLD',READONLY,SHARED,RECL=160)
      OPEN(UNIT=12,file='fort.12',STATUS='OLD',FORM='UNFORMATTED'
     &,ACCESS='APPEND')
      OPEN(UNIT=14,file='fort.14',STATUS='OLD',FORM='UNFORMATTED'
     &,ACCESS='APPEND')
      OPEN(UNIT=19,file='fort.19',STATUS='OLD',FORM='UNFORMATTED'
     &,ACCESS='APPEND')
      OPEN(UNIT=20,file='fort.20',STATUS='OLD',FORM='UNFORMATTED'
     &,ACCESS='APPEND')
      OTHER1(2)=(8H        )
      OTHER2(1)=(8H        )
      OTHER2(2)=(8H        )
      DWL=0.
      DLOGGF=0.
      DGAMMAR=0.
      DGAMMAS=0.
      DGAMMAW=0.
      DWLISO=0.
      DO 900 ILINE=1,50000000
      READ(11,140,END=145)WL,GFLOG,CODE,E,XJ,LABEL,EP,XJP,LABELP,
     1 GAMMAR,GAMMAS,GAMMAW,REF,NBLO,NBUP,ISO1,X1,ISO2,X2,OTHER1,
     2 OTHER2,LANDE,LANDEG,ISOSHIFT
C           1234567      123456789012     1
C12345678901       123456            12345 1234567890                  1234567890
C   234.0154 -3.888  3.00       0.000  0.5 2s  2S       42719.141  0.5 12p  2P   more
C
C continuing                                   12345678901234567890
C  5.21  0.00  0.00LN   0 0  0 0.000  0 0.000                     1234 5678    -7
  140 FORMAT(F11.4,F7.3,F6.2,F12.3,F5.1,1X,A8,A2,F12.3,F5.1,1X,A8,A2,
     1 F6.2,F6.2,F6.2,A4,I2,I2,I3,F6.3,I3,F6.3,A8,A2,A8,A2,2I5,I6)
c	if(wl.gt.190.) type*,wl,gflog,code
C     OTHER1 IS HYPERFINE SHIFTS
C     IXFIXFP IS HYPERFINE NOTATION
c      READ(COTHER1,'(2I5)')ISHIFT,ISHIFTP
c      READ(COTHER1,'(1x,I1,1x,I1,6x)')ISHIFT,ISHIFTP
      READ(COTHER2,'(A6,I1,A3)')IXFIXFP,LINESIZE,AUTO
	ishift=0
	ishiftp=0
      ESHIFT=ISHIFT*.001
      ESHIFTP=ISHIFTP*.001
      DWLISO=-ISOSHIFT*.001*ABS(WL)**2/1.D7
      WLVAC=ABS(WL)+DWL+DWLISO
      IF(IFVAC.EQ.1.OR.LABELP(1).EQ.8HCONTINUU)WLVAC=
     1 1.D7/DABS(DABS(EP)+ESHIFTP-DABS(E)+ESHIFT)+DWL+DWLISO
      IF(WLVAC.GT.WLEND+DELLIM(1))GO TO 145
      IXWL=DLOG(WLVAC)/RATIOLG+.5D0
      NBUFF=IXWL-IXWLBEG+1
      LIM=MIN(8-LINESIZE,7)
      IF(CODE.EQ.1.)LIM=1
      IF(WLVAC.LT.WLBEG-DELLIM(LIM)*DELFACTOR)GO TO 900
C      IF(WLVAC.GT.WLEND+DELLIM(LIM)*DELFACTOR)GO TO 900
      IF(WLVAC.GT.WLEND+DELLIM(LIM)*DELFACTOR)GO TO 145
C     CORONAL APPROXIMATION LINE
      IF(AUTO.EQ.'COR')GO TO 900
C     WRITE(6,140)WL,GFLOG,CODE,E,XJ,LABEL,EP,XJP,LABELP
      GR=GAMMAR
      GS=GAMMAS
      GW=GAMMAW
      GF=EXP((GFLOG+DGFLOG+X1+X2)*2.30258509299405E0)
      ELO=DMIN1(DABS(E),DABS(EP))
      GAMMAR=EXP((GAMMAR+DGAMMAR)*2.30258509299405E0)
      GAMMAS=EXP((GAMMAS+DGAMMAS)*2.30258509299405E0)
      GAMMAW=EXP((GAMMAW+DGAMMAW)*2.30258509299405E0)
      IF(GAMMAR.EQ.1)THEN
      GAMMAR=2.223E13/WLVAC**2
      GR=ALOG10(GAMMAR)
      ENDIF
      NELEM=CODE
      ICHARGE=(CODE-FLOAT(NELEM))*100.+.1
      ZEFF=ICHARGE+1
      NELION=NELEM*6-6+IFIX(ZEFF)
      IF(NELEM.GT.19.AND.NELEM.LT.29.AND.ICHARGE.GT.5)NELION=
     1 6*(NELEM+ICHARGE*10-30)-1
      IF(GAMMAS.NE.1..AND.GAMMAW.NE.1.)GO TO 141
      IF(GAMMAS.NE.1.)GO TO 138
      IF(CODE.GE.100.)GO TO 137
      EUP=DMAX1(DABS(E),DABS(EP))
      EFFNSQ=25.
      DELEUP=POTION(NELION)-EUP
      IF(DELEUP.GT.0.)EFFNSQ=109737.31*ZEFF**2/DELEUP
      GAMMAS=1.0E-8*EFFNSQ**2*SQRT(EFFNSQ)
      GS=ALOG10(GAMMAS)
      GO TO 138
  137 GAMMAS=1.0E-5
      GS=-5.
  138 IF(GAMMAW.NE.1.)GO TO 141
      IF(CODE.GE.100.)GO TO 139
      EUP=DMAX1(DABS(E),DABS(EP))
      EFFNSQ=25.
      DELEUP=POTION(NELION)-EUP
      IF(DELEUP.GT.0.)EFFNSQ=109737.31*ZEFF**2/DELEUP
      EFFNSQ=AMIN1(EFFNSQ,1000.)
      RSQUP=2.5*(EFFNSQ/ZEFF)**2
      DELELO=POTION(NELION)-ELO
      EFFNSQ=109737.31*ZEFF**2/DELELO
      EFFNSQ=AMIN1(EFFNSQ,1000.)
      RSQLO=2.5*(EFFNSQ/ZEFF)**2
      NSEQ=CODE-ZEFF+1.
      IF(NSEQ.GT.20.AND.NSEQ.LT.29)THEN
      RSQUP=(45.-FLOAT(NSEQ))/ZEFF
      RSQLO=0.
      ENDIF
      IF(LABELP(1).EQ.8HCONTINUU)RSQLO=0.
      IF(RSQUP.LT.RSQLO)RSQUP=2.*RSQLO
      GAMMAW=4.5E-9*(RSQUP-RSQLO)**.4
      GW=ALOG10(GAMMAW)
      GO TO 141
  139 GAMMAW=1.E-7/ZEFF
      GW=ALOG10(GAMMAW)
  141 CONTINUE
C      WRITE(6,144)WL,GFLOG,CODE,E,XJ,LABEL,EP,XJP,LABELP,
C     1GAMMAR,GAMMAS,GAMMAW,REF,NBLO,NBUP,ISO1,X1,ISO2,X2,OTHER1,OTHER2
  144 FORMAT(F11.4,F7.3,F6.2,F12.3,F5.1,1X,A8,A2,F12.3,F5.1,1X,A8,A2,
     1 F6.2,F6.2,F6.2,A4,I2,I2,I3,F7.3,I3,F7.3,A8,A2,A8,A2)
C     TYPE=-6  3HE II LINE
C     TYPE=-5  4HE I LINE
C     TYPE=-4  3HE I LINE
C     TYPE=-3  4HE I LINE
C     TYPE=-2  DEUTERIUM LINE
C     TYPE=-1  HYDROGEN LINE
C     TYPE=0  NORMAL LINE
C     TYPE=1  AUTOIONIZING LINE
C     TYPE=2  CORONAL APPROXIMATION LINE
C     TYPE=3  PRD LINE
C     TYPE.GT.3 = NLAST  CONTINUUM
      TYPE=0
      IF(CODE.EQ.1.00)TYPE=-1
      IF(CODE.EQ.1.00.AND.ISO1.EQ.2)TYPE=-2      
      IF(CODE.EQ.2.00)TYPE=-3
      IF(CODE.EQ.2.00.AND.ISO1.EQ.3)TYPE=-4      
      IF(CODE.EQ.2.01)TYPE=-6
      IF(CODE.EQ.2.01.AND.ISO1.EQ.3)TYPE=-6      
      IF(AUTO.EQ.'COR')TYPE=2
      IF(AUTO.EQ.'AUT')TYPE=1
      IF(AUTO.EQ.'PRD') TYPE=3
      IF(LABELP(1).EQ.8HCONTINUU)NLAST=XJP
      IF(LABELP(1).EQ.8HCONTINUU)GF=GF*(XJ+XJ+1.)
      NCON=0
      IF(ISO1.EQ.0.AND.ISO2.GT.0)NCON=ISO2
      IF(TYPE.EQ.1)GO TO 17
      IF(TYPE.GT.3)GO TO 17
      FRELIN=2.99792458E17/WLVAC
      CGF=.026538/1.77245*GF/FRELIN
C     GR IS GAUNT FACTOR FOR CORONAL LINES
      IF(TYPE.EQ.2)GAMMAR=GR
      IF(TYPE.EQ.2)GO TO 1253
      GAMMAR=GAMMAR/12.5664/FRELIN
      GAMMAS=GAMMAS/12.5664/FRELIN
      GAMMAW=GAMMAW/12.5664/FRELIN
   17 NBUP=IABS(NBUP)
      NBLO=IABS(NBLO)
      IF(NBLO+NBUP.EQ.0)GO TO 1260
      DO 1250 I=1,17
      IF(CODE.EQ.CODEX(I))GO TO 1252
 1250 CONTINUE
      WRITE(6,1251)CODE
 1251 FORMAT(9H BAD CODE,F10.2)
      CALL EXIT
 1252 NELIONX=I
 1253 WRITE(19)WLVAC,ELO,GF,NBLO,NBUP,NELION,TYPE,NCON,NELIONX,
     1GAMMAR,GAMMAS,GAMMAW,NBUFF,LIM
      IF(LINOUT.GE.0)WRITE(20)LINDAT8,LINDAT4
      N19=N19+1
C     WRITE(6,5555)WLVAC,ILINE
 5555 FORMAT(112X,F10.4,I10)
      GO TO 900
C     PLAIN LINE
 1260 WRITE(12)NBUFF,CGF,NELION,ELO,GAMMAR,GAMMAS,GAMMAW
      IF(LINOUT.GE.0)WRITE(14)LINDAT8,LINDAT4
      N14=N14+1
      NLINES=NLINES+1
  900 CONTINUE
  145 WRITE(6,1118)N14
 1118 FORMAT(I10,' LINES ADDED TO TAPE 12')
      WRITE(6,1120)NLINES
 1120 FORMAT(I10,' LINES TOTAL ON TAPE 12')
      WRITE(6,1119)N19
 1119 FORMAT(I10,' LINES TOTAL ON TAPE 19')
C      IF(LINOUT.LT.0.)GO TO 1125
C      IF(N19.GT.0)THEN
C      REWIND 20
C      DO 1121 I=1,N19
C      READ(20)LINDAT8,LINDAT
C 1121 WRITE(13)LINDAT8,LINDAT
C      ENDIF
C      IF(NLINES.GT.0)THEN
C      REWIND 14
C      DO 1122 I=1,NLINES
C      READ(14)LINDAT8,LINDAT
C 1122 WRITE(13)LINDAT8,LINDAT
C      ENDIF
C 1125 CONTINUE
C      IF(IFNLTE.EQ.1)N19=0
      REWIND 93
      WRITE(93)NLINES,LENGTH,IFVAC,IFNLTE,N19,TURBV,DECKJ,IFPRED,
     1WLBEG,WLEND,RESOLU,RATIO,RATIOLG,CUTOFF,LINOUT
C     CALL ENDTIME
      CALL EXIT
      END
      SUBROUTINE IONPOTS
C
C     Moore, C.E., NSRDS-NBS 34,1970,22pp.
C     AEL, rare-earth elements, 1978,NSRDS-NBS 60,411pp.
C     Kelley,R.L. and Harrison, D.E, Atomic Data 3,177-193,1971.  Ga V,VI,Ge VI
C     Martin,W.C. et al. ,JPCRDa,3,771-780,1974.  Ac I-Es I
C     Guess 55.02 56.02 73.01 73.02 74.01 74.02 75.01 75.02 76.01 76.01
C     Guess 77.01 77.02 78.02 79.02 84.01 84.02 85.00 85.01 85.02 86.01 86.02
C     Guess 87.00 87.01 87.02 88.02 89.02 91.01 91.02 92.01 92.02 93.01 93.02
C     Guess 94.01 94.02 95.01 95.02 96.01 96.02 97.01 97.02 98.01 98.02
C     Guess 99.01 99.02
C
      COMMON /POTION/POTION(594)
      DIMENSION POTA(114),POTB(114),POTC(114),POTD(114),POTE(114)
      DIMENSION POTF(24)
      EQUIVALENCE (POTION(1),POTA(1)),(POTION(115),POTB(1))
      EQUIVALENCE (POTION(229),POTC(1)),(POTION(343),POTD(1))
      EQUIVALENCE (POTION(457),POTE(1)),(POTION(571),POTF(1))
      DATA POTA/
     1  109678.764,      0.   ,      0.  ,      0. ,      0. ,      0. ,       1
     2  198310.76 , 438908.85 ,      0.  ,      0. ,      0. ,      0. ,       2
     3   43487.150, 610079.0  , 987660.1 ,      0. ,      0. ,      0. ,       3
     4   75192.07 , 146882.86 ,1241259.4 ,1756018.7,      0. ,      0. ,       4
     5   66928.10 , 202887.4  , 305931.1 ,2092001.4,2744105.1,      0. ,       5
     6   90820.42 , 196664.7  , 386241.0 , 520178.4,3162395. ,3952061.4,       6
     7  117225.4  , 238750.5  , 382704.  , 624866. , 789537.2,4452758. ,       7
     8  109837.02 , 283240.   , 443086.  , 624383.8, 918657. ,1114008. ,       8
     9  140524.5  , 282058.6  , 505777.  , 702830. , 921430. ,1267622. ,       9
     T  173929.70 , 330391.0  , 511800.  , 783300. ,1018000. ,1273800. ,      10
     1   41449.44 , 381395.   , 577800.  , 797800. ,1116200. ,1388500. ,      11
     2   61671.02 , 121267.61 , 646410.  , 881100. ,1139400. ,1504300. ,      12
     3   48278.37 , 151860.4  , 229445.71, 967800. ,1239800. ,1536300. ,      13
     4   65747.5  , 131838.4  , 270139.3 , 364093.1,1345100. ,1653900. ,      14
     5   84580.   , 159100.   , 243400.  , 414312.4, 524460. ,1777900. ,      15
     6   83558.0  , 188200.   , 280900.  , 381541.4, 586200. , 710184. ,      16
     7  104591.0  , 192070.   , 319500.  , 431226. , 547000. , 782600. ,      17
     8  127109.9  , 222848.2  , 328600.  , 482400. , 605100. , 734040. ,      18
     9   35009.77 , 255076.   , 368800.  , 491300. , 666700. , 806600. /      19
      DATA POTB/
     1   49305.72 ,  95751.87 , 410614.1 , 541200. , 680800. , 877400. ,      20
     2   52750.   , 103240.   , 199700.  , 592600. , 739300. , 896000. ,      21
     3   55010.   , 109506.   , 221735.  , 348973. , 800300. , 962700. ,      22
     4   54400.   , 118200.   , 236400.  , 376730. , 526100. ,1033400. ,      23
     5   54570.   , 133060.   , 249700.  , 396000. , 559000. , 730400. ,      24
     6   59970.   , 126145.   , 271550.  , 413000. , 584000. , 766000. ,      25
     7   63480.   , 130524.   , 247221.  , 442000. , 604900. , 798500. ,      26
     8   63430.   , 137572.   , 270200.  , 413800. , 641200. , 823000. ,      27
     9   61579.   , 146541.56 , 283700.  , 442800. , 609000. , 871000. ,      28
     T   62317.2  , 163669.2  , 297100.  , 445124. , 644500. , 831000. ,      29
     1   75768.10 , 144892.6  , 320390.  , 479100. , 666000. , 871000. ,      30
     2   48387.63 , 165458.   , 247700.  , 517600. , 726000. , 944000. ,      31
     3   63715.   , 128521.3  , 276036.  , 368701. , 753800. , 968000. ,      32
     4   79165.   , 150290.   , 228670.  , 404369. , 505136. ,1028800. ,      33
     5   78658.22 , 170900.   , 248583.  , 346375. , 551000. , 658994. ,      34
     6   95284.8  , 175870.   , 289529.  , 381600. , 481600. , 714800. ,      35
     7  112914.5  , 196474.8  , 298020.  , 423600. , 522000. , 633300. ,      36
     8   33690.81 , 220048.   , 320000.  , 424400. , 572800. , 680900. ,      37
     9   45932.0  ,  88964.0  , 351800.  , 460000. , 577700. , 732600. /      38
      DATA POTC/
     1   51447.   ,  98690.   , 165500.  , 498600. , 621200. , 750300. ,      39
     2   55145.   , 105900.   , 185400.  , 276970. , 657600. ,      0. ,      40
     3   55511.   , 115500.   , 202000.  , 308600. , 407700. , 827300. ,      41
     4   57260.   , 130300.   , 219100.  , 374180. , 493360. , 549000. ,      42
     5   58700.   , 123100.   , 238300.  ,      0. ,      0. ,      0. ,      43
     6   59410.   , 135200.   , 229600.  ,      0. ,      0. ,      0. ,      44
     7   60197.   , 145800.   , 250500.  ,      0. ,      0. ,      0. ,      45
     8   67236.   , 156700.   , 265600.  ,      0. ,      0. ,      0. ,      46
     9   61106.50 , 173300.   , 280900.  ,      0. ,      0. ,      0. ,      47
     T   72538.8  , 136374.74 , 302300.  ,      0. ,      0. ,      0. ,      48
     1   46670.11 , 152195.   , 226100.  , 439000. ,      0. ,      0. ,      49
     2   59231.8  , 118017.0  , 246020.0 , 328550. , 583000. ,      0. ,      50
     3   69700.   , 133327.5  , 204248.  , 356156. , 449300. , 868000. ,      51
     4   72667.   , 150000.   , 225500.  , 301776. , 473900. , 570000. ,      52
     5   84295.1  , 154304.   , 266000.  ,      0. ,      0. ,      0. ,      53
     6   97834.0  , 171068.4  , 259089.  ,      0. ,      0. ,      0. ,      54
     7   31406.432, 202263.   , 280000.  ,      0. ,      0. ,      0. ,      55
     8   42035.14 ,  80686.87 , 300000.  ,      0. ,      0. ,      0. ,      56
     9   44981.   ,  89200.   , 154675.  , 402900. , 497000. ,      0. /      57
      DATA POTD/
     1   44672.   ,  87500.   , 162903.  , 296470. , 528700. , 626000. ,      58
     2   44070.   ,  85100.   , 174407.  , 314400. , 464000. ,      0. ,      59
     3   44562.   ,  86500.   , 178600.  , 326000. ,      0. ,      0. ,      60
     4   44800.   ,  87900.   , 180000.  , 331000. ,      0. ,      0. ,      61
     5   45519.   ,  89300.   , 189000.  , 334000. ,      0. ,      0. ,      62
     6   45734.9  ,  90665.   , 201000.  , 344000. ,      0. ,      0. ,      63
     7   49603.   ,  97500.   , 166400.  , 355000. ,      0. ,      0. ,      64
     8   47295.   ,  92900.   , 176700.  , 317500. ,      0. ,      0. ,      65
     9   47900.   ,  94100.   , 183800.  , 334000. ,      0. ,      0. ,      66
     T   48567.   ,  95200.   , 184200.  , 343000. ,      0. ,      0. ,      67
     1   49262.   ,  96200.   , 183400.  , 344000. ,      0. ,      0. ,      68
     2   49879.8  ,  97200.   , 191000.  , 344000. ,      0. ,      0. ,      69
     3   50441.0  ,  98269.   , 202070.  , 351300. ,      0. ,      0. ,      70
     4   43762.39 , 112000.   , 169049.  , 364960. , 538700. ,      0. ,      71
     5   56600.   , 120000.   , 187800.  , 268500. ,      0. ,      0. ,      72
     6   63600.   , 131000.   , 194000.  ,      0. ,      0. ,      0. ,      73
     7   64400.   , 143000.   , 202000.  ,      0. ,      0. ,      0. ,      74
     8   63530.   , 134000.   , 210000.  ,      0. ,      0. ,      0. ,      75
     9   70450.   , 137000.   , 218000.  ,      0. ,      0. ,      0. /      76
      DATA POTE/
     1   73000.   , 161000.   , 226000.  ,      0. ,      0. ,      0. ,      77
     2   72300.   , 149723.   , 234000.  ,      0. ,      0. ,      0. ,      78
     3   74410.   , 165000.   , 242000.  ,      0. ,      0. ,      0. ,      79
     4   84184.1  , 151280.   , 276000.  ,      0. ,      0. ,      0. ,      80
     5   49266.7  , 164765.   , 240600.  ,      0. ,      0. ,      0. ,      81
     6   59819.4  , 121243.   , 257592.  , 341350. , 555000. ,      0. ,      82
     7   58790.   , 134600.   , 206180.  , 365500. , 451700. , 712000. ,      83
     8   67885.3  , 153000.   , 218000.  ,      0. ,      0. ,      0. ,      84
     9   75000.   , 161000.   , 242000.  ,      0. ,      0. ,      0. ,      85
     T   86692.5  , 161000.   , 242000.  ,      0. ,      0. ,      0. ,      86
     1   32000.   , 177000.   , 266000.  ,      0. ,      0. ,      0. ,      87
     2   42577.35 , 81842.31  , 274000.  ,      0. ,      0. ,      0. ,      88
     3   41700.   , 97300.    , 161000.  ,      0. ,      0. ,      0. ,      89
     4   49000.   , 93000.    , 161000.  , 231900. ,      0. ,      0. ,      90
     5   47500.   , 95000.    , 161000.  ,      0. ,      0. ,      0. ,      91
     6   48800.   , 95000.    , 161000.  ,      0. ,      0. ,      0. ,      92
     7   49900.   , 95000.    , 161000.  ,      0. ,      0. ,      0. ,      93
     8   48900.   , 95000.    , 161000.  ,      0. ,      0. ,      0. ,      94
     9   48300.   , 95000.    , 161000.  ,      0. ,      0. ,      0. /      95
      DATA POTF/
     1   48600.   , 95000.    , 161000.  ,      0. ,      0. ,      0. ,      96
     2   50200.   , 95000.    , 161000.  ,      0. ,      0. ,      0. ,      97
     3   50800.   , 95000.    , 161000.  ,      0. ,      0. ,      0. ,      98
     4   51800.   , 95000.    , 161000.  ,      0. ,      0. ,      0. /      99
      RETURN
      END
