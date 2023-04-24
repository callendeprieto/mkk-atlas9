      program abucalc
c
c ssssssssssssssssssssssssssssssssss
c     calculates abundance ratios
c     from ATLAS style abundances
c     LS 2003
c ssssssssssssssssssssssssssssssssss
c
      implicit none
      integer :: i,conta,starname
      integer,dimension(276) :: el_ob,ion_ob
      real :: FeI,FeII,err_FeI,err_FeII
      real,dimension(92) :: sun_abu,sun_err
      real,dimension(276) :: ab_ob,compa,toH,toFeI,toFeII,dump,errs,
     1compa_errs,errs_toFeI,errs_toFeII
      character(len=2) :: names(92),name_use(276)
      data el_ob,ion_ob,ab_ob /276*0,276*0,276*0./
      data sun_err / 92*0./
C      data names / 92*'--' /
C initializes solar abundances.
C revised table G&S (2001)
C Note: As,Se,Br,Kr,Te,I,Xe,Cs,Ta,Re,Hg,Bi,Th,U from meteorites!
C       Tc,Pm elements between Po and Ac, and Pa not found in nature
C       so put to 0 in the table.
C------------
C Changes from G&S 01:
C -20050301 S Changed to 7.21 (Lodders 2003)
C                  1H    2He   3Li  4Be  5B   6C   7N   8O   9F   10N
      sun_abu = (/ 12.00,10.93,1.10,1.40,2.70,8.52,7.95,8.72,4.56,8.06,
C     11Na 12Mg 13Al 14Si 15P  16S  17Cl 18Ar 19K  20Ca 21Sc 22Ti 23V 
     26.33,7.58,6.47,7.55,5.43,7.21,5.50,6.40,5.12,6.36,3.17,5.02,4.00,
C     24Cr 25Mn 26Fe 27Co 28Ni 29Cu 30Zn 31Ga 32Ge 33As 34Se 35Br 36Kr
     35.67,5.39,7.50,4.92,6.25,4.21,4.60,2.88,3.41,2.37,3.41,2.63,3.31,
C     37Rb 38Sr 39Y  40Zr 41Nb 42Mo 43Tc 44Ru 45Rh 46Pd 47Ag 48Cd 49In
     42.60,2.92,2.24,2.60,1.42,1.92,0.00,1.84,1.12,1.69,0.94,1.77,1.66,
C     50Sn 51Sb 52Te 53I  54Xe 55Cs 56Ba 57La 58Ce 59Pr 60Nd 61Pm 62Sm
     52.00,1.00,2.24,1.51,2.17,1.13,2.13,1.13,1.58,0.71,1.50,0.00,1.01,
C     63Eu 64Gd 65Tb 66Dy 67Ho 68Er 69Tm 70Yb 71Lu 72Hf 73Ta  74W  75Re
     60.51,1.12,0.00,1.14,0.26,0.93,0.00,1.08,0.06,0.88,-0.13,1.11,0.28,
C     76Os 77Ir 78Pt 79Au 80Hg 81Tl 82Pb 83Bi 84Po 85At 86Rn 87Fr 88Ra
     71.45,1.35,1.80,1.01,1.13,0.90,1.95,0.71,0.00,0.00,0.00,0.00,0.00,
C     89Ac 90Th 91Pa 92U
     80.00,0.09,0.00,0.50 /)
C
C elements names... useless but sooo cool...
      names = (/'H ','He','Li','Be','B ','C ','N ','O ','F ','N ','Na',
     1'Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca','Sc','Ti','V ','Cr',
     2'Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr','Rb',
     3'Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn',
     4'Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu',
     5'Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W ','Re','Os',
     6'Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac',
     7'Th','Pa','U '/)
C
      open(1,file='fort.001',status='old')
      open(2,file='fort.003',status='unknown')
C Reads in and counts elements
      read(1,*)starname
      do i=1,276
      read(1,*,end=200)el_ob(i),ion_ob(i),ab_ob(i),errs(i)
      enddo
      where(el_ob.eq.0)el_ob=1
200   conta=i-1
      write(*,*)"Read. ",conta," abundances in the file"
      close(1)
C passes to G&S scale and builds ratios
C ----note that the conversion factor depends on the 
C     H/He ratio. This is for H=0.920 He=0.0783
      ab_ob=ab_ob+12.0354
      compa=sun_abu(el_ob)
      name_use=names(el_ob)
      dump=compa
      toH=ab_ob-compa
      compa=compa*0.
C ----extracting Fe I abundance and error
      where((el_ob.eq.26).and.(ion_ob.eq.0))compa=toH
      where((el_ob.eq.26).and.(ion_ob.eq.0))compa_errs=errs
      FeI=sum(compa)
      err_FeI=sum(compa_errs)
      compa_errs=compa_errs*0.
      compa=compa*0.
C ----extracting Fe II abundance and error
      where((el_ob.eq.26).and.(ion_ob.eq.1))compa=toH
      where((el_ob.eq.26).and.(ion_ob.eq.1))compa_errs=errs
      FeII=sum(compa)
      err_FeII=sum(compa_errs)
      compa=compa*0.+FeI
      compa_errs=compa_errs*0.+err_FeI
C ----actually calculates ratios and errors
      toFeI=toH-compa
      errs_toFeI=sqrt(compa_errs**2+errs**2)
      compa=compa*0.+FeII
      compa_errs=compa_errs*0.+err_FeII 
      toFeII=toH-compa
      errs_toFeII=sqrt(compa_errs**2+errs**2)
C ----verifies for values without error...
      where(errs.eq.0.)
           errs_toFeI=0.
           errs_toFeII=0.
      end where
C printout
      write(2,*)"#        elem     ion   abu     sun      [X/H]   +/-   
     1[X/FeI] +/-  [X/FeII]"
      write(2,*)"#------------------------------------------------------
     1----------------------"
      do i=1,conta
      write(2,222)starname,name_use(i),el_ob(i),ion_ob(i),ab_ob(i),dump(
     1i),toH(i),errs(i),toFeI(i),errs_toFeI(i),toFeII(i),errs_toFeII(i)
      enddo
      close(2)
      write(*,*)"Finished"
C formats
222   format(I6,A4,2X,2(I5),2(F8.2),'  *',6(F7.2))
      stop
      end
C Under development...
C table for errors in solar ab. 


      

      
      
      
      
      
