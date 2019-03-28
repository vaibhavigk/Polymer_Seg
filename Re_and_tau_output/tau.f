 	program comd

	implicit real*4(a-f,o-z) 

	character(23)::Restring1, Restring2
	character(25)::xyzstring1, xyzstring2
		
	parameter (ndata=1000,nskip=500,dt=0.001) 
	      
        dimension :: x(ndata)  

	!!!! calculating average end to end distance




open(100,file='Filenames2')

do g=1,9	!!for different radii from 2.25 to 4.25

do h=1,50
   	read(100,*)Restring1
	read(100,*)Restring2
	read(100,*)xyzstring1
	read(100,*)xyzstring2
	read(100,*)
	
	
	 Ree1=0.0; Ree2=0.0 

	open(12,file=trim(Restring1),status='old')

	do i = 1, nskip
	   read(12,*)
	enddo	

	do i=1, ndata
	   read(12,*)step, a, b, c, x(i), e, f
	   Re1= Re1+x(i)
	enddo
	   Ree1 = Re1/float(ndata) 
	   write(*,*)'Re2-chain1:',Ree1, sqrt(Ree1) 
	close(12)
	
	open (13,file=trim(Restring2),status='old')
	
	do i=1, nskip
	   read(13,*)
	enddo	

	do i=1, ndata
	   read(13,*)step, a, b, c, x(i), e, f
	   Re2= Re2+x(i)
	enddo
	   Ree2 = Re2/float(ndata)
	   write(*,*)'Re2-chain2:',Ree2, sqrt(Ree2)
	close(13)
	  Re2avg=(Ree1+Ree2)/2.
	  write(*,*)'Re2-avg', Re2avg, sqrt(Re2avg)  

!!**************************

	open (10,file=trim(xyzstring1),status='old')
	open (11,file=trim(xyzstring2),status='old')

        do i = 1, nskip
	   read(10,*)
	   read(11,*)
	enddo	

	do i=1, ndata
	   read(10,*)step, x1, b, c
	   read(11,*)step, x2, b, c
	   dx = abs(x2-x1)
	   dx2 = dx*dx
	   if(i.eq.1)t0=step 
	   if(dx2.ge.Re2avg)then 
	     tau = step-t0 
	     write(*,*)'tau (LJ-unit):',tau*dt 
	     exit 
	   endif
	enddo
	close(10)
	close(11)
enddo		!!for config
enddo 		!!for radii
	endprogram comd
