      PROGRAM RDFBIGASC
      integer*2 kap(25,57,12)
      DIMENSION ISTEPS(12)
      open(unit=2, status='new',form='unformatted')
      DO 7 IREC=1,328
c	TYPE*,IREC
      DO 5 IT=1,57
      DO 5 IP=1,25
      READ(1,4)(KAP(IP,IT,ISTEP),Istep=1,12)
4     FORMAT(1X,12(I6,1X))
5     CONTINUE      
	do 55 itt=1,57
55	write(2)((kap(ip,itt,istep),istep=1,12),ip=1,25)
7	continue
	stop
      END
