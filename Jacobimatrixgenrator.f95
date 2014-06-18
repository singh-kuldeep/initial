program jacobimat
implicit none
 
real ::  rand
real, allocatable, dimension (:,:) :: jacobi

integer :: n,row,col

!user input will be n only 

print*, "give the value of n"
read *, n

allocate (jacobi(n,n))

!defining the jacobi matrix

do row =1,n
  rand=0.0 
  do col =1,n
    if (row == col+1) then
       rand=sqrt(real((4*col*col)-1))
    	jacobi(row,col)=(col/(rand))
    elseif (row == col-1) then
     rand = sqrt(real((4*row*row)-1))
    	jacobi(row,col)=(row/(rand))
    else 
        jacobi(row,col)= 0
   	endif
  end do
end do


!printing the jacobi matrix 

do row =1,n
  do col =1,n
    print *,"your jacobi (", row,col,"is "
    print *, jacobi(row,col)
  end do
end do
deallocate (jacobi)
!Now finding the eiganvalue of matrix 

call ssteqr('N',n)




end program jacobimat 
    
   
     
