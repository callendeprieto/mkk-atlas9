      PROGRAM BROADEN
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
c     revised 27feb97
C     TAPE5=INPUT
C     TAPE6=OUTPUT
C     TAPE21=SPECTRUM INPUT
C     TAPE22=SPECTRUM OUTPUT
C     THE MINIMUM DIMENSION OF H IS (NWL+19999+19999)*NMU
C     FOR FLUX SPECTRA NMU IS 1
C      DIMENSION H(4000000)
      DIMENSION H(4100000)
      DIMENSION RED(20000),BLUE(20000)
      DIMENSION COMMENT(13),A(3),B(3)
      REAL*8 LINDAT8(14)
      REAL*4 LINDAT(28)
      DIMENSION XMU(20),QMU(40),WLEDGE(377),TITLE(74)
      REAL*8 TEFF,GLOG,TITLE,WBEGIN,RESOLU,XMU,WLEDGE,RATIO
      REAL*8 QMU
      DIMENSION APLOT(101)
      DATA APLOT/101*1H  /
C      interface
C        subroutine exit(status)
C          integer(4),optional,intent(in)::status
C        end subroutine
C        subroutine abort(string)
C          !ms$attributes alias:'abort_'::abort
C          character(len=*),optional,intent(in)::string
C        end subroutine
C      end interface
      LINOUT=300
      REWIND 21
      READ(21)TEFF,GLOG,TITLE,WBEGIN,RESOLU,NWL,IFSURF,NMU,XMU,NEDGE,
     1WLEDGE
      WRITE(6,1010)TEFF,GLOG,TITLE
 1010 FORMAT(  5H TEFF,F7.0,7H   GRAV,F7.3/7H TITLE ,74A1)
      WRITE(6,1007)NMU,(XMU(IMU),IMU=1,NMU)
 1007 FORMAT(I4,20F6.3)
C     IFSURF=3 FOR ROTATED SPECTRUM
      IF(IFSURF.EQ.3) NMU=1
      RATIO=1.+1./RESOLU
      WEND=WBEGIN*RATIO**(NWL-1)
      WCEN=(WBEGIN+WEND)*.5
      VSTEP=2.99792458E5/RESOLU
      WRITE(6,1005)WBEGIN,WEND,RESOLU,VSTEP,NWL
 1005 FORMAT(2F14.5,F12.1,F12.5,I10)
      NMU1=NMU+1
      NMU2=NMU+NMU
C
C     SAMPLE CARDS RIGHT SHIFTED BY 1
CGAUSSIAN  3.5       KM        COMMENT FIELD
CGAUSSIAN  100000.   RESOLUTIONCOMMENT FIELD
CGAUSSIAN  7.        PM        COMMENT FIELD
CSINX/X    3.5       KM        COMMENT FIELD
CSINX/X    100000.   RESOLUTIONCOMMENT FIELD
CSINX/X    7.        PM        COMMENT FIELD
CRECT      7.        PM        COMMENT FIELD
CRECT      3.5       KM        COMMENT FIELD
CRECT      100000.   RESOLUTIONCOMMENT FIELD
CMACRO     2.0       KM        COMMENT FIELD
CPROFILE   5.        POINTS    COMMENT FIELD
CRED       .3        .1        .1        .1        .05
CBLUE      .3        .1        .1        .1        .05
C
C     POINTS ARE TABULATED AT THE SPACING OF THE COMPUTED SPECTRUM
C     THE CENTER IS THE FIRST POINT FOR EACH WING
C     TAKING THE CENTER ONLY ONCE, THE PROFILE SUMS TO 1.
C
      READ(5,1)A,X,B,COMMENT
    1 FORMAT(A4,A4,A2,F10.2,A4,A4,A2,12A4,A2)
      WRITE(6,2)A,X,B,COMMENT
    2 FORMAT(1X,A4,A4,A2,F10.2,A4,A4,A2,12A4,A2)
      IF(A(1).EQ.4HMACR)GO TO 10
      IF(A(1).EQ.4HGAUS)GO TO 20
      IF(A(1).EQ.4HSINX)GO TO 25
      IF(A(1).EQ.4HRECT)GO TO 30
      IF(A(1).EQ.4HPROF)GO TO 40
      WRITE(6,3)
    3 FORMAT(10H0BAD INPUT)
      CALL EXIT
C
C     MACROTURBULENT VELOCITY IN KM
   10 VMAC=X
      DO 11 I=1,20000
      RED(I)=EXP(-(FLOAT(I-1)*VSTEP/VMAC)**2)
      IF(RED(I).LT.1.E-5)GO TO 12
   11 CONTINUE
   12 NPROF=I
      RED(1)=RED(1)/2.
      SUM=0.
      DO 13 I=1,NPROF
   13 SUM=SUM+RED(I)
      SUM=SUM*2.
      DO 14 I=1,NPROF
      RED(I)=RED(I)/SUM
   14 BLUE(I)=RED(I)
      GO TO 50
C
C     GAUSSIAN INSTRUMENTAL PROFILE HALF WIDTH IN KM  FWHM
   20 FWHM=0.
      IF(B(1).EQ.4HPM  )FWHM=X/WCEN/1000.*299792.5
      IF(B(1).EQ.4HKM  )FWHM=X
      IF(B(1).EQ.4HRESO)FWHM=299792.5/X
      DO 21 I=1,20000
      RED(I)=EXP(-(FLOAT(I-1)*VSTEP/FWHM  *.8325546*2.)**2)
      IF(RED(I).LT.1.E-5)GO TO 22
   21 CONTINUE
   22 NPROF=I
      RED(1)=RED(1)/2.
      SUM=0.
      DO 23 I=1,NPROF
   23 SUM=SUM+RED(I)
      SUM=SUM*2.
      DO 24 I=1,NPROF
      RED(I)=RED(I)/SUM
   24 BLUE(I)=RED(I)
      GO TO 50
C
C     SINX/X INSTRUMENTAL PROFILE HALF WIDTH IN KM  FWHM
C     APODIZED BY EXP(-0.06*X**2)
   25 FWHM=0.
      IF(B(1).EQ.4HPM  )FWHM=X/WCEN/1000.*299792.5
      IF(B(1).EQ.4HKM  )FWHM=X
      IF(B(1).EQ.4HRESO)FWHM=299792.5/X
      RED(1)=0.5
      DO 26 I=2,20000
      X=(FLOAT(I-1)*VSTEP/FWHM*2.*1.8954942)
      RED(I)=SIN(X)/X*EXP(-0.06*X**2)
      IF(ABS(RED(I))+ABS(RED(I-1)).LT.1.E-5)GO TO 27
   26 CONTINUE
   27 NPROF=I
      SUM=0.
      DO 28 I=1,NPROF
   28 SUM=SUM+RED(I)
      SUM=SUM*2.
      DO 29 I=1,NPROF
      RED(I)=RED(I)/SUM
   29 BLUE(I)=RED(I)
      GO TO 50
C
C     RECTANGULAR INSTRUMENTAL PROFILE HALF WIDTH IN KM  FWHM
   30 FWHM=0.
      IF(B(1).EQ.4HPM  )FWHM=X/WCEN/1000.*299792.5
      IF(B(1).EQ.4HKM  )FWHM=X
      IF(B(1).EQ.4HRESO)FWHM=299792.5/X
      XRECT=FWHM/2./VSTEP
      NRECT=XRECT+1.5
      NPROF=NRECT
      DO 31 I=1,NPROF
   31 RED(I)=1.
      RED(NPROF)=XRECT+1.5-FLOAT(NRECT)
      RED(1)=RED(1)/2.
      SUM=0.
      DO 33 I=1,NPROF
   33 SUM=SUM+RED(I)
      SUM=SUM*2.
      DO 34 I=1,NPROF
      RED(I)=RED(I)/SUM
   34 BLUE(I)=RED(I)
      GO TO 50
C
C     INSTRUMENTAL PROFILE TABULATED AT SPECTRUM POINT SPACING.
C        RED AND BLUE WINGS BOTH START AT CENTRAL POINT.
C        THE PROFILE SHOULD SUM TO 1.
   40 NPROF=X
      READ(5,41)(RED(I),I=1,NPROF)
      READ(5,41)(BLUE(I),I=1,NPROF)
   41 FORMAT(10X,7F10.6)
      RED(1)=RED(1)/2.
      BLUE(1)=BLUE(1)/2.
   50 WRITE(6,51)(I,RED(I),BLUE(I),I=1,NPROF)
   51 FORMAT(I5,2F10.6)
      WRITE(22)TEFF,GLOG,TITLE,WBEGIN,RESOLU,NWL,IFSURF,NMU,XMU,NEDGE,
     1WLEDGE
      NH=(NWL+19999+19999)*NMU
      DO 52 I=1,NH
   52 H(I)=0.
      WRITE(6,1117)
 1117 FORMAT(1H1)
      IF(NMU.EQ.1)GO TO 150
      DO 57 IWL=1,NWL
      READ(21)(QMU(I),I=1,NMU)
      DO 56 IMU=1,NMU
      Q=QMU(IMU)
      IWL1000=(IWL+20000)*NMU+IMU
      DO 53 I=1,NPROF
   53 H(IWL1000-I*NMU)=H(IWL1000-I*NMU)+BLUE(I)*Q
      IWL998=(IWL+19998)*NMU+IMU
      DO 54 I=1,NPROF
   54 H(IWL998+I*NMU)=H(IWL998+I*NMU)+RED(I)*Q
   56 CONTINUE
   57 CONTINUE
      GO TO 160
  150 DO 157 IWL=1,NWL
      READ(21)QMU(1)
      Q=QMU(1)
      IWL1001=IWL+20001
      DO 153 I=1,NPROF
  153 H(IWL1001-I)=H(IWL1001-I)+BLUE(I)*Q
      IWL999=IWL+19999
      DO 154 I=1,NPROF
  154 H(IWL999+I)=H(IWL999+I)+RED(I)*Q
  157 CONTINUE
  160 REWIND 21
      READ(21)
      DO 70 IWL=1,NWL
      READ(21)(QMU(IMU),IMU=1,NMU2)
      IWLNMU=(IWL+19999)*NMU
      DO 58 IMU=1,NMU
   58 QMU(IMU)=H(IWLNMU+IMU)
      WRITE(22)(QMU(I),I=1,NMU2)
      IF(IWL.GT.LINOUT)GO TO 63
      WAVE=WBEGIN*RATIO**(IWL-1)
      RESID=QMU(1)/QMU(NMU1)
      IRESID=RESID*1000.+.5
      IPLOT=RESID*100.+1.5
      IPLOT=MAX0(1,MIN0(101,IPLOT))
      APLOT(IPLOT)=1HX
      WRITE(6,2300)IWL,WAVE,IRESID,APLOT
 2300 FORMAT(1H ,I5,F11.4,I7,101A1)
      APLOT(IPLOT)=(1H )
   63 CONTINUE
   68 CONTINUE
   70 CONTINUE
      READ(21)NLINES
      WRITE(22)NLINES
      DO 200 I=1,NLINES
      READ(21)LINDAT8,LINDAT
      WRITE(22)LINDAT8,LINDAT
  200 CONTINUE
      CALL EXIT
      END
