program Re_Rg_tau

implicit real*4(a-f,o-z) 
character(26)::Restring1, Restring2
character(28)::xyzstring1, xyzstring2
parameter (ndata=1500,nskip=501,dt=0.001,nsize=7) 	      
dimension :: x(ndata), EnsRee1(nsize), EnsRee2(nsize), Err1(nsize), Err2(nsize), Ree1sq(nsize), Ree2sq(nsize)

!!intialising 
do ii=1,7
EnsRee1(ii)=0.0
EnsRee2(ii)=0.0
Err1(ii)=0.0
Err2(ii)=0.0
Ree1sq(ii)=0.0
Ree2sq(ii)=0.0
enddo

!!!! calculating average end to end distance

open(14,file='output',status='replace')
write(14,*) '#<Re1> ; <Re2> ; tau'

!Ree1sq=0.0;   Ree2sq=0.0; 

open(100,file='Filenames2')
open(30,file='Ensemble_avg',status='replace')
write(30,*) '#EnsRee1/50,		Err1,		EnsRee2/50,		Err2'

do m=1,7			!!for different radii from 2.25 to 4.25

!EnsRee1=0.0; EnsRee2=0.0; 

	do n=1,50			!!for different config 

 	
		read(100,*)	Restring1
		read(100,*)	Restring2
		read(100,*)	xyzstring1
		read(100,*)	xyzstring2
		read(100,*)
				
		open(12,file=trim(Restring1),status='old')
					
		do i = 1, nskip
			   		
			read(12,*)

		enddo	

		Re1=0.0; Ree1=0.0

		do i=1, ndata
			   
			read(12,*)step, a, b, c, x(i), e, f
			Re1= Re1+x(i)
			
		enddo
			   
		Ree1 = Re1/float(ndata) 
		EnsRee1(m)=EnsRee1(m)+sqrt(Ree1)
		Ree1sq(m) = Ree1sq(m) + Ree1
			 
		write(*,*)'Re2-chain1:',Ree1, sqrt(Ree1) 
		close(12)
		open (13,file=trim(Restring2),status='old')
		
		do i=1, nskip
			   	
			read(13,*)
				
		enddo	

		step=0.0; Re2=0.0;  Ree2=0.0

		do i=1, ndata
			   	
			read(13,*)step, a, b, c, x(i), e, f
			Re2= Re2+x(i)
			
		enddo
			   
		Ree2 = Re2/float(ndata)
		EnsRee2(m)=EnsRee2(m)+sqrt(Ree2)
		Ree2sq(m) = Ree2sq(m) + Ree2
		write(*,*)'Re2-chain2:',Ree2, sqrt(Ree2)
		close(13)
		Re2avg=(Ree1+Ree2)/2
		write(*,*)'Re2-avg', Re2avg, sqrt(Re2avg)  

		!***************************************

		open (10,file=trim(xyzstring1),status='old')
		read (10,*)
		open (11,file=trim(xyzstring2),status='old')
		read (11,*)
		step=0.0
				
		do j=1, ndata
				   
			read(10,*)step, x1, b, c
			read(11,*)step, x2, b, c
			dx = abs(x2-x1)
			dx2 = dx*dx
			   		
			if(j.eq.1)then 
			     	
				t0=step

			elseif(dx2.ge.Re2avg)then 
			     		
				tau = step-t0
				write(*,*) 
			    write(*,*)'tau (LJ-unit):',tau*dt 
			    exit 
			endif
		enddo
		write(14,*) sqrt(Ree1),sqrt(Ree2),tau*dt
		close(10)
		close(11)

	enddo !! second endo for config

	Err1(m)=Ree1sq(m)/50 - (EnsRee1(m)/50)**2
	Err2(m)=Ree2sq(m)/50 - (EnsRee2(m)/50)**2
	write(30,*) EnsRee1(m)/50, Err1(m), EnsRee2(m)/50, Err2(m)
	
					
enddo 	! first endo for radii
	close(14)
	close(100)
	close(30)

end program Re_Rg_tau	
