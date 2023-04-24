      program widthsgr
c     *********************
c     modified by LS 2004
c     this version to be used inside crea_width script
c     *********************
      double precision w,check,eps
      REAL*8 WL,E,EP,WLVAC,CENTER,CONCEN
      REAL*8 LABEL,LABELP,OTHER1,OTHER2
      dimension label(2),labelp(2),other1(2),other2(2)
      real xlabel,star(19)
      DIMENSION CODEX(17)
      logical loop,inloop
      EQUIVALENCE (GAMMAS,ASHORE),(GAMMAW,BSHORE)
      EQUIVALENCE (GF,G,CGF),(TYPE,NLAST),(GAMMAR,XSECT,GAUNT)
      COMMON /POTION/POTION(594)
      DATA CODEX/1.,2.,2.01,6.,6.01,12.,12.01,13.,13.01,14.,14.01,
     1 20.,20.01,8.,11.,5.,19./
      eps=5.d-4
      loop=.true.
      XLABEL='LINE'
      STAR(1)='    '
      STAR(2)='STAR'
      STAR(3)='NAME'
      do kk=4,19
       star(kk)='    '
      enddo
      DWL=0.
      DGFLOG=0.
      DGAMMAR=0.
      DGAMMAS=0.
      DGAMMAW=0.
      DWLISO=0.
      write(6,61)xlabel,star
61    format(1x,a4,2x,19a4)
      n=0
c      write(6,671)
c671   format(1x,'NLINES, element ? ',$)
      read(5,*)codeg
      OPEN(UNIT=11,file='fort.11',
     &STATUS='OLD',READONLY,SHARED,RECL=160)
c ******* modif to open other files explicitly
      open(1,file='fort.01',status='old')
      open(7,file='fort.07',status='unknown',access='append')
c *******
      do while (loop)
      read(11,11,end=600)WL,GFLOG,CODE,E,XJ,LABEL,EP,XJP,LABELP,
     1 GAMMAR,GAMMAS,GAMMAW,REF,NBLO,NBUP,ISO1,X1,ISO2,X2,OTHER1,
     2 OTHER2,LANDE,LANDEG,ISOSHIFT
      iz=code
      ICHARGE=(code-FLOAT(iz))*100+.1
      ZEFF=ICHARGE+1
      NELION=(IZ-1)*6+IFIX(ZEFF)
c11    format(F11.4,F7.3,F6.2,F12.3,F5.1,1X,A8,A2,F12.3,F5.1,1X,A8,A2,
c     1 F6.2,F6.2,F6.2,A4,I2,I2,I3,F7.3,I3,F7.3,A8,A2,A8,A2,2I5,I6)
  11  FORMAT(F11.4,F7.3,F6.2,F12.3,F5.1,1X,A8,A2,F12.3,F5.1,1X,A8,A2,
     1 F6.2,F6.2,F6.2,A4,I2,I2,I3,F6.3,I3,F6.3,A8,A2,A8,A2,2I5,I6)

31      format(A4,F10.2,F10.4,19A4,/,
     &F10.4,F7.3,F5.1,F12.3,F5.1,F12.3,F9.2,A8,A2,A8,A2,/,
     &F10.4,I4,F6.2,F6.2,F6.2,A4,I2,I2,I3,F7.3,I3,F7.3,A8,A2,A8,A2)
      WLVAC=ABS(WL)
      rewind(1)
      do j=1,1000
      read(1,*,end=888)w,wobs,eww
c ************* Modif to use Angstrom input file....
c      w=w/10.
c      wobs=wobs/10.
c      eww=eww/10.
c *************
21    format(f8.3,1x,f5.2,1x,f8.3,22x,f8.5)
22    format(1x,f8.3,1x,f6.2,1x,f8.3,1x,f9.7,1x,i10)
      check=abs(w-wl)
      if(check.le.eps.and.code.eq.codeg)then
        n=n+1
      write(6,22)w,codeg,wl,eww,nelion
c        ew=eww*1000.
	 ew=eww
        write(7,31)XLABEL,EW,Wobs,STAR,
     &WL,GFLOG,XJ,E,XJP,EP,CODE,LABEL,LABELP,
     &WL,NELION,GAMMAR,GAMMAS,GAMMAW,REF,NBLO,NBUP,ISO1,X1,ISO2,
     &X2,OTHER1,OTHER2
      endif
      enddo
888   ntot=j-1
333   continue
      enddo
600   continue
      close(11)
      write(6,610)n
      write(66,610)n
      write(66,611)ntot
610   format(1x,'lines written  ',i4)
611   format(1x,'of total',i4)
      stop'files WIDTH creati'
      end
      
