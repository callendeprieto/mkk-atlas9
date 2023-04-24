      PROGRAM Readoh

c
c  reads data for OH molecule wavelengths from Stark et al 1994
c  einstein coefficients from Goldman&Gillis 1981
c
c  input file is OHUV_gf.DAT  gf computed with readoh_raw from
c  the file ohuv.dat prepared by Tullio Valente@OAT
c
c  P. Bonifacio@oat giugno 1998
c
c   revised dicembre 1999
c
C     THIS IS WRITTEN AS A GENERIC PROGRAM
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
      character*10 label10,label10p
      character*2  b
      CHARACTER*8 CLABELP
      EQUIVALENCE (CLABELP,LABELP(1))
      CHARACTER*4 CREF
      EQUIVALENCE (CREF,REF)
      EQUIVALENCE (CLABELP,LABELP(1))
      equivalence(label10,label(1))
      equivalence(label10p,labelp(1))
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

      OPEN(UNIT=11,file='fort.11',TYPE='OLD',READONLY,SHARED)
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
      EFFNSQ=25.                                                                
      GAMMAS=1.0E-8*EFFNSQ**2*SQRT(EFFNSQ)                                      
      RSQ=2.5*(EFFNSQ/ZEFF)**2                                                  
      GAMMAW=1.E-7/ZEFF                                                         
      START=WLBEG-.1                                                            
      STOP=WLEND+1.                                                             
      STOP1=STOP+1.
      N=0                                                                   
      read(11,376)wl,xj,xjp,elo,gflog,lab
      wl=wl/10.
376   format(1x,f9.4,1x,f4.1,1x,f4.1,1x,f9.3,1x,f10.3,a15)
      IF(ABS(WL).GT.STOP1)GO TO 21
      rewind(11)
      DO 20 ILINE=1,100000
      READ(11,376,end=964)wl,xj,xjp,elo,gflog,lab
c      READ(11,REC=ILINE)WL,E,EP,LABEL(1),LABELP(1),GFLOG,XJ,XJP,CODE,ISO
      wl=wl/10.
      if (wl.lt.start) go to 20
      CODE=108.00
      label10='2A00     '
      label10p='2X00       '
      CREF='GG  '
      ISO=16
      IF(ABS(WL).GT.STOP1)GO TO 21
      IF(IFPRED.EQ.0.AND.WL.LT.0.)GO TO 20                                                      
      WLVAC=ABS(WL)                                                             
      IF(IFVAC.EQ.1)WLVAC=1.E7/ABS(ABS(EP)-ABS(E))                              
      IF(WLVAC.LT.START)GO TO 20                                                
      IF(N.EQ.0)THEN
      WRITE(6,6)ILINE                                                 
    6 FORMAT(I10,19H IS FIRST LINE READ)                                        
      PRINT 3,ILINE,WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL(1),LABELP(1),ISO
      ENDIF
      IF(WLVAC.GT.STOP)GO TO 21                                                 
160   NELION=258
      ISO1=1
      ISO2=16
      X1=0.
      X2=-.001      
 5000 GF=EXP((GFLOG+X1+X2)*2.30258509299405E0)                                          
c      ELO=DMIN1(ABS(E),ABS(EP))                                                 
      IXWL=DLOG(WLVAC)/RATIOLG+.5D0
      NBUFF=IXWL-IXWLBEG+1
      FREQ=2.99792458E17/WLVAC                                                    
      CONGF=.01502*GF/FREQ                                                      
      FRQ4PI=FREQ*12.5664                                                       
      GAMMAR=2.223E13/WLVAC**2                                                  
      IF(CLABELP(1:1).EQ.'X')THEN
      GAMMAR=GAMMAR*.001
      GAMMAS=3.E-8
C     GAMMAW=3.E-9
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
      LABELP(2)=ISOLAB(ISO)                                                     
      IF(LINOUT.GE.0)WRITE(14)LINDAT8,LINDAT4                                                           
      N=N+1
      NLINES=NLINES+1                                                           
   20 CONTINUE                                                                  
964   continue
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
