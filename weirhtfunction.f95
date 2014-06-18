program weightfunction

implicit none
 
real, allocatable, dimension(:)::eigenvalue 
real, allocatable, dimension(:)::diffrentiation
real, allocatable, dimension(:)::weight

integer :: n,i

print *, "give the no of root "
read *, n

allocate (eigenvalue(n))
!defining the eigenvalue  matrix

do i=1,n
print *,"give",i,"eigenvalue"
read *,eigenvalue(i)
end do
!for now only we are taking valu of diff from user and wrighting the code
allocate (diffrentiation(n))

do i=1,n
print*,"give",i,"diffrentiation"
read *,diffrentiation(i)
end do

allocate (weight(n)) !defining the weight matrix 
!for that we need only diffrentiation of Ln+1(We cane prove it easily using lagrang interpolation rule)
! because value of polynomial at the root will be zero so no contribution of Li's up to Ln only Ln+1 requaired 

do i=1,n
weight(i)= 2/((1-(eigenvalue(i))*eigenvalue(i))*(diffrentiation(i)*diffrentiation(i)))
end do

! finally printing the weight function

do i=1,n
print *, "weight",i,"=",weight(i)
end do

deallocate(eigenvalue)
deallocate(diffrentiation)
deallocate(weight)

end program weightfunction















