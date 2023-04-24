	program testcv
	character*132 riga
	character*2 num
	logical loop
	open(unit=1,file='for001.dat',status='old')
        open(unit=8,file='for008.dat',status='old')
	read(8,*)niter,ntot
	encode(2,10,num(1:2))niter
10	format(i2)
	loop=.true.
	k=0
	do while(loop)
	read(1,1,end=100)riga
1       format(a132)
	if(riga(117:125).eq.'ITERATION'.and.riga(127:128).eq.num)then
		k=k+1
		if(k.eq.ntot)then
		 open(unit=2,file='for002.dat',status='new')
	         write(2,1)riga
	         do j=1,74
		  read(1,1)riga
		  write(2,1)riga
	         enddo
		 loop=.false.
		endif
	endif
	enddo
	close(1)
	rewind(2)
	read(2,1)riga
	read(2,1)riga
	read(2,1)riga
	s=0
	sd=0
	k=0
	n=0
	do j=1,72
		read(2,90)mdepth,rhox,t,p,xne,rho,abross,height,tauros,flxcnv,
     &          accrad,flxerr,flxdrv
	  if(abs(flxerr).ge.1.)k=k+1
	  if(abs(flxdrv).ge.10.)n=n+1
	  s=s+flxerr
	  sd=sd+flxdrv
	enddo
	s=s/72
	sd=sd/72
	open(unit=3,file='testcv.out',status='new')
	if(k.eq.0.and.n.eq.0)then 
	 write(3,92)s,sd
         open(unit=4,file='CONVERGED',status='new')
	else
	 write(3,91)k,n,s,sd
	endif
	close(2)
	close(3)
	stop'finito ####'
91	format(1x, 'problems !!!',/,1x,i4,' depths have large flux errors',
     & /,1x,i4,' depths have large flux derivative errors',/
     & ,1x,' mean flux error ',f16.4,/
     & ,1x,' mean flux derivative error ',f16.4)
92	format(1x,'the model is converging',/
     & ,1x,' mean flux error ',f16.4,/
     & ,1x,' mean flux derivative error ',f16.4)
90	format(I4,1PE10.3,0PF9.1,1P8E11.3,0PF12.3,F9.3)
100	continue
	close(1)
	stop'reached EOF, check you fed me the right file !'
	end
