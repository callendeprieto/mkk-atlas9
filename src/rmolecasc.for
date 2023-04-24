      PROGRAM RMOLEC
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
c     revised 25jun00
c     IMPLICIT REAL*4 (A-H,O-Z)
      PARAMETER (kw=99)
      COMMON /LINDAT/WL,E,EP,LABEL(2),LABELP(2),OTHER1(2),OTHER2(2),
     1        WLVAC,CENTER,CONCEN, NELION,GAMMAR,GAMMAS,GAMMAW,REF,
     2      NBLO,NBUP,ISO1,X1,ISO2,X2,GFLOG,XJ,XJP,CODE,ELO,GF,GS,GR,GW,
     3        DWL,DGFLOG,DGAMMAR,DGAMMAS,DGAMMAW,EXTRA1,EXTRA2,EXTRA3
      REAL*8 LINDAT8(14)
      REAL*4 LINDAT4(28)
      EQUIVALENCE (LINDAT8(1),WL),(LINDAT4(1),NELION)
      REAL*8 RESOLU,RATIO,RATIOLG,SIGMA2,WLBEG,WLEND
      REAL*8 WL,E,EP,WLVAC,CENTER,CONCEN
      REAL*8 LABEL,LABELP,OTHER1,OTHER2
      CHARACTER*8 CLABELP,clabel
      character*74 string
      EQUIVALENCE (CLABELP,LABELP(1)),(clabel,label(1))
      CHARACTER*4 CREF
      EQUIVALENCE (CREF,REF)
      DIMENSION DECKJ(7,kw)
      REAL*8 START,STOP
      REAL*8 ISOLAB(33)
      DATA ISOLAB/2H 1,2H 2,2H 3,2H 4,2H 5,2H 6,2H 7,2H 8,2H 9,2H10,
     1            2H11,2H12,2H13,2H14,2H15,2H16,2H17,2H18,2H19,2H20,
     2            2H21,2H22,2H23,2H24,2H25,2H26,2H27,2H28,2H29,2H30,
     3            2H31,2H32,2H33/
c     interface
c       subroutine exit(status)
c         integer(4),optional,intent(in)::status
c       end subroutine
c       subroutine abort(string)
c         !ms$attributes alias:'abort_'::abort
c         character(len=*),optional,intent(in)::string
c       end subroutine
c     end interface
      OPEN(UNIT=11,file='fort.11',
     &status='OLD',FORM='FORMATTED',
     1ACCESS='DIRECT',RECL=75,READONLY,SHARED)
      OPEN(UNIT=12,file='fort.12',
     &STATUS='OLD',FORM='UNFORMATTED',ACCESS='APPEND')
      OPEN(UNIT=14,file='fort.14',
     &STATUS='OLD',FORM='UNFORMATTED',ACCESS='APPEND')
      READ(93)NLINES,LENGTH,IFVAC,IFNLTE,N19,TURBV,DECKJ,IFPRED,
     1WLBEG,WLEND,RESOLU,RATIO,RATIOLG,CUTOFF,LINOUT
      IXWLBEG=DLOG(WLBEG)/RATIOLG
      IF(DEXP(IXWLBEG*RATIOLG).LT.WLBEG)IXWLBEG=IXWLBEG+1
      NBLO=0
      NBUP=0
      OTHER1(1)=(8H        )
      OTHER1(2)=(2H  )
      OTHER2(1)=(8H        )
      OTHER2(2)=(2H  )
      LABEL(2)=(2H  )
      REF=(2HK )
      ION=1
      ZEFF=ION
      START=WLBEG-.1
      STOP=WLEND+1.
      STOP1=STOP+1.
      N=0
      READ(11,5001,REC=1)WL
5001  format(f10.4)
c     write(6,*)'first line is',wl
c     read(11,5003,rec=1)string
c003  format(a74)
c      write(6,*)'x ',string
      READ(11,5002,REC=1)WL,GFLOG,XJ,E,XJP,EP,ICODE,clabel,clabelp
     1,ISO,LOGGR
c5002  FORMAT(F10.4,F7.3,F5.1,F10.3,F5.1,F11.3,I4,a8,a8)
 5002  FORMAT(F10.4,F7.3,F5.1,F10.3,F5.1,F11.3,I4,A8,A8,I2,I4)
c     write(6,8891)wl,icode,clabel,clabelp
c8891  format(1x,'line is',f10.4,2x,i4,2x,a8,'secondo ',a8)
      IF(ABS(WL).GT.STOP1)GO TO 21
C     FIND NUMBER OF LINES
      LIMITBLUE=1
      LIMITRED=10000000
    8 NEWLIMIT=(LIMITRED+LIMITBLUE)/2
      READ(11,5001,REC=NEWLIMIT,ERR=9)WL
      LIMITBLUE=NEWLIMIT
      IF(LIMITRED-LIMITBLUE.EQ.1)GO TO 11
      GO TO 8
    9 LIMITRED=NEWLIMIT
      IF(LIMITRED-LIMITBLUE.EQ.1)GO TO 11
      GO TO 8
   11 LENGTHFILE=LIMITBLUE
      READ(11,5001,REC=1)WL
      PRINT 3334,WL
 3334 FORMAT(' FIRST LINE IS      1','  WL',F11.4)
      READ(11,5001,REC=LENGTHFILE)WL
      PRINT 3335,LENGTHFILE,WL
 3335 FORMAT(' LAST LINE IS ',I7,'  WL',F11.4)
      IF(ABS(WL).LT.START)GO TO 21
C     FIND THE FIRST LINE AFTER START
      LIMITBLUE=1
      LIMITRED=LENGTHFILE
   12 NEWLIMIT=(LIMITRED+LIMITBLUE)/2
      PRINT 3333,LIMITBLUE,NEWLIMIT,LIMITRED
 3333 FORMAT(3I10)
      READ(11,5001,REC=NEWLIMIT)WL
      IF(ABS(WL).LT.START)GO TO 13
      LIMITRED=NEWLIMIT
      IF(LIMITRED-LIMITBLUE.LE.1)GO TO 14
      GO TO 12
   13 LIMITBLUE=NEWLIMIT
      IF(LIMITRED-LIMITBLUE.LE.1)GO TO 14
      GO TO 12
   14 ISTART=NEWLIMIT
      PRINT 3333,LIMITBLUE,LIMITRED,NEWLIMIT
      DO 20 ILINE=ISTART,LENGTHFILE
      READ(11,5002,REC=ILINE)WL,GFLOG,XJ,E,XJP,EP,ICODE,clabel,clabelp
     1,ISO,LOGGR
      WL=ABS(WL)
      CODE=ICODE   
      IF(ABS(WL).GT.STOP1)GO TO 21
      IF(IFPRED.EQ.0.AND.E.LT.0.)GO TO 20
      IF(IFPRED.EQ.0.AND.EP.LT.0.)GO TO 20
      WLVAC=ABS(WL)
      IF(IFVAC.EQ.1)WLVAC=1.E7/ABS(ABS(EP)-ABS(E))
      IF(WLVAC.LT.START)GO TO 20
      IF(N.EQ.0)THEN
      WRITE(6,6)ILINE
    6 FORMAT(I10,19H IS FIRST LINE READ)
      PRINT 3,ILINE,WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(1),LABELP(1),ISO
      ENDIF
      IF(WLVAC.GT.STOP)GO TO 21
      GO TO (10,99,99,99,99,99,99,99,99,99,99,120,130,140,150,160,170,
     1 180,99,99,99,99,99,240,250,260,99,280,290,300,99,99,330),ISO
C     H2
   10 NELION=240
      IS01=1
      IS02=1
      X1=0.
      X2=0.
      GO TO 5000
  120 IF(CODE.EQ.606.)GO TO 1200
      IF(CODE.EQ.608.)GO TO 1210
      IF(CODE.EQ.106.)GO TO 1220
C     CN
      NELION=270
      ISO1=12
      ISO2=14
      X1=-.005
      X2=-.002
      GO TO 5000
  130 IF(CODE.EQ.606.)GO TO 1300
      IF(CODE.EQ.608.)GO TO 1310
      IF(CODE.EQ.106.)GO TO 1320
C     CN
      NELION=270
      ISO1=13
      ISO2=14
      X1=-1.955
      X2=-.002
      GO TO 5000
C     NH
  140 NELION=252
      ISO1=1
      ISO2=14
      X1=0.
      X2=-.002
      GO TO 5000
C     NH
  150 IF(CODE.EQ.607.)GO TO 1500
      NELION=252
      ISO1=1
      ISO2=15
      X1=0.
      X2=-2.444
      GO TO 5000
C     OH
  160 NELION=258
      ISO1=1
      ISO2=16
      X1=0.
      X2=-.001
      GO TO 5000
C     CO
  170 NELION=276
      ISO1=12
      ISO2=17
      X1=-.005
      X2=-3.398
      GO TO 5000
  180 IF(CODE.EQ.814.)GO TO 1800
      IF(CODE.EQ.608.)GO TO 1810
C     OH
      NELION=258
      ISO1=1
      ISO2=18
      X1=0.
      X2=-2.690
      GO TO 5000
C     MgH
  240 NELION=300
      ISO1=1
      ISO2=24
      X1=0.
      X2=-.105
      GO TO 5000
C     MgH
  250 NELION=300
      ISO1=1
      ISO2=25
      X1=0.
      X2=-.996
      GO TO 5000
C     MgH
  260 NELION=300
      ISO1=1
      ISO2=26
      X1=0.
      X2=-.947
      GO TO 5000
  280 IF(CODE.EQ.814.)GO TO 2800
C     SiH
      ISO1=1
      ISO2=28
      NELION=312
      X1=0.
      X2=-.035
      GO TO 5000
  290 IF(CODE.EQ.814.)GO TO 2900
C     SiH
      NELION=312
      ISO1=1
      ISO2=29
      X1=0.
      X2=-1.331
      GO TO 5000
  300 IF(CODE.EQ.814.)GO TO 3000
C     SiH
      NELION=312
      ISO1=1
      ISO2=30
      X1=0.
      X2=-1.516
      GO TO 5000
C     C2
  330 NELION=264
      ISO1=13
      ISO2=13
      X1=-1.955
      X2=-1.955
      GO TO 5000
C     C2
 1200 NELION=264
      ISO1=12
      ISO2=12
      X1=-.005
      X2=-.005
      GO TO 5000
C     CO
 1210 NELION=276
      ISO1=12
      ISO2=16
      X1=-.005
      X2=-.001
      GO TO 5000
C     CH
 1220 NELION=246
      ISO1=1
      ISO2=12
      X1=0.
      X2=-.005
      GO TO 5000
C     C2
 1300 NELION=264
      ISO1=12
      ISO2=13
      X1=-.005
      X2=-1.955
      GO TO 5000
C     CO
 1310 NELION=276
      ISO1=13
      ISO2=16
      X1=-1.955
      X2=-.001
      GO TO 5000
C     CH
 1320 NELION=246
      ISO1=1
      ISO2=13
      X1=0.
      X2=-1.955
      GO TO 5000
C     CN
 1500 NELION=270
      ISO1=12
      ISO2=15
      X1=-.005
      X2=-2.444
      GO TO 5000
C     SiO
 1800 NELION=330
      ISO1=28
      ISO2=18
      X1=-.035
      X2=-2.690
      GO TO 5000
C     CO
 1810 NELION=276
      ISO1=12
      ISO2=18
      X1=-.005
      X2=-2.690
      GO TO 5000
C     SiO
 2800 NELION=330
      ISO1=28
      ISO2=16
      X1=-.035
      X2=-.001
      GO TO 5000
C     SiO
 2900 NELION=330
      ISO1=29
      ISO2=16
      X1=-1.328
      X2=-.001
      GO TO 5000
C     SiO
 3000 NELION=330
      ISO1=30
      ISO2=16
      X1=-1.510
      X2=-.001
      GO TO 5000
 5000 GF=EXP((GFLOG+X1+X2)*2.30258509299405E0)
      ELO=DMIN1(ABS(E),ABS(EP))
      IXWL=DLOG(WLVAC)/RATIOLG+.5D0
      NBUFF=IXWL-IXWLBEG+1
      FREQ=2.99792458E17/WLVAC
      CONGF=.01502*GF/FREQ
      FRQ4PI=FREQ*12.5664
      GAMMAR=10.**(LOGGR*.01)
C     GUESSES
C     ELECTRON
      GAMMAS=3.E-5
      GAMMAW=1.E-7
C     VIBRATION-ROTATIONAL
      IF(CLABELP(1:1).EQ.'X')THEN
      GAMMAS=3.E-8
      GAMMAW=1.E-8
      ENDIF
      GR=LOG10(GAMMAR)
      GS=LOG10(GAMMAS)
      GW=LOG10(GAMMAW)
      GAMRF=GAMMAR/FRQ4PI
      GAMSF=GAMMAS/FRQ4PI
      GAMWF=GAMMAW/FRQ4PI
      WRITE(12)NBUFF,CONGF,NELION,ELO,GAMRF,GAMSF,GAMWF
   17 FORMAT(I10)
      IF(NELION.EQ.270)THEN
C     FIX REFERENCE
      CREF='K'//CLABELP(7:8)
      CLABELP=CLABELP(1:6)
      ENDIF
      LABELP(2)=ISOLAB(ISO)
      IF(LINOUT.GE.0)WRITE(14)LINDAT8,LINDAT4
      N=N+1
      NLINES=NLINES+1
   20 CONTINUE
   21 WRITE(6,22)ILINE
   22 FORMAT(I10,18H IS LAST LINE READ)
   25 WRITE(6,26)N
   26 FORMAT(I10,' LINES ADDED TO TAPE 12')
      WRITE(6,27)NLINES
   27 FORMAT(I10,' LINES TOTAL ON TAPE 12')
      REWIND 93
      WRITE(93)NLINES,LENGTH,IFVAC,IFNLTE,N19,TURBV,DECKJ,IFPRED,
     1WLBEG,WLEND,RESOLU,RATIO,RATIOLG,CUTOFF,LINOUT
   99 PRINT 3,ILINE,WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(1),LABELP(1),ISO
    3 FORMAT(I10,1X,F10.4,F7.3,F5.1,F12.3,F5.1,F12.3,F9.2,A8,2X,A8,I2)
      CALL EXIT
      END
