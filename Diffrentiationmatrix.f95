! Author: Kuldeep Singh
! Institute: IIT Bombay
! Last edit: June 18, 2014, 7:38 p.m.
program diffmatrixfind
implicit none
 
real, allocatable, dimension(:)::eigenvalue 
real, allocatable, dimension (:,:) :: diffmatrix
real :: multi, add ,denomnetor,numretor
integer :: n,row,col,i,j,k,l

logical :: add_multi_flag
integer :: count

!user input will be n only 

print*, "give the no of eigenvalue  n"
read *, n

allocate (eigenvalue(n+1))
!defining the eigenvalue  matrix
!eigenvalue(n+1)= 1 because we want to include intial point (1) in calculation of Li

do row =1,n
print *,"give",row,"eigenvalue"
read *,eigenvalue(row+1)
end do

eigenvalue(1)= -1

allocate (diffmatrix(n+1,n+1))
!defining the jacobi matrix

do col=1,n+1
! denominetor calculation for a paticular diffrentiated  lagrang function 
	denomnetor=1.0
 	do i=1,n+1
  	if (col/=i) then 
	   	denomnetor=denomnetor*(eigenvalue(col)-eigenvalue(i))
 	end if
	end do

!numretor calculation for  a paticular diffrentiated  lagrang function 

	do row=1,n+1
	numretor=0.0
		do l=1,n+1
		multi=1.0
		count = 0
			do j=1,n+1
			
				if(j/=col .and. j/=l) then
				multi = multi*(eigenvalue(row)- eigenvalue(j))
				count = count + 1
				!flag = flag +1 ! this is checking whether we are entring in if loop or not 
				end if 
	        end do
	        	if(.not. count == n) then
			 		numretor= numretor+multi
		 		end if

	   	end do
	   	!print*, "numretor(", col,row ,")",numretor
! now we have numretor and dennomntor 
	diffmatrix(col,row)= (numretor/denomnetor)
	end do
end do
!printing the jacobi matrix 

do k =2,n+1
  do row =1,n+1
	    !print *,"your diffmatrix (", row,k,")is ",diffmatrix(row,k)
	    !print *, diffmatrix(row,k), " "
	    write(*,"(F14.7)",advance="no") diffmatrix(row,k)
  end do
  print *
end do


!Now finding the eiganvalue of matrix 

deallocate (eigenvalue)
deallocate (diffmatrix)

end program diffmatrixfind
    
   
     
