Program Main

Implicit none

integer	::u,v,i,j,n
real(4), dimension(:,:), allocatable::a,b,c

u=50
v=60

open(unit=u, file='/work/ese-ouycc/fortran_2/M.dat', status='old')
open(unit=v, file='/work/ese-ouycc/fortran_2/N.dat', status='old')
n=3

allocate( a(n,n),b(n,n),c(n,n) )
j=1
do i=1,3
read(u,*)a(i,j),a(i,j+1),a(i,j+2)
read(v,*)b(i,j),b(i,j+1),b(i,J+2)
enddo

close(u)
close(v)

call Matrix_multip(a,b,c,n)
open(unit=u,file='MN.dat',status='replace')

do i=1,n
write(u,'(f8.1,f8.1,f8.1)') c(i,j),c(i,j+1),c(i,j+2)
enddo
close(u)

do i=1,n

write(*,*)"the answer is:"
write(*,*)"line",i,":",c(i,j),c(i,j+1),c(i,j+2)
enddo

deallocate(a,b,c)

end program Main


