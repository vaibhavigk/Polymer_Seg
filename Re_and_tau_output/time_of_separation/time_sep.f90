program tau_avg

    implicit real*10(t-v)

    integer:: line_count
    integer:: Reason
    integer:: incre
    real:: buffer_1=0
    real:: buffer_2=0
    real:: tau_read=0
    !real:: tau_value=0
    !real:: tau_average 
    character(len=150):: File_Name
    dimension::tau_square(7), tau_square_avg(7), tau_Error(7), tau_average(7), tau_value(7)

    do i=1,7
        tau_average(i)=0
        tau_square(i)=0
        tau_square_avg(i)=0
        tau_Error(i)=0
        tau_value(i)=0
    enddo

    open(14,file="names.txt",status='old', action='read')
    open(15, file="Tau_avg_Err",status='replace')

    do line_count=1,7

        !READ(14,*,IOSTAT=Reason)
        !IF (Reason > 0)  THEN
           ! exit
        !ELSE IF (Reason < 0) THEN
            !exit
        !Else

            read(14,*) File_Name
            open(11,file=File_Name,status='old', action='read')
            read(11,*);
         
            do incre=1,50
            tau_read=0
                READ(11,*,IOSTAT=Reason)
                IF (Reason > 0)  THEN
                    exit
                ELSE IF (Reason < 0) THEN
                    exit
                ELSE
                    read(11,*) buffer_1,buffer_2,tau_read
                    tau_value(line_count)=tau_value(line_count)+tau_read
                    tau_square(line_count)=tau_square(line_count)+(tau_read**2)
                    
                END IF
            end do

            tau_average(line_count)=tau_value(line_count)/50
            tau_square_avg(line_count)=tau_square(line_count)/50
            tau_Error(line_count)=tau_square_avg(line_count)-(tau_average(line_count)**2)
            close(11)
            write(15,*) tau_average(line_count),tau_Error(line_count)
        !END IF
    end do
    
    close(14)
    close(15)


end program tau_avg

