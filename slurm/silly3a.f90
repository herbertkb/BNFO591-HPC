	program silly3a
!$	use omp_lib
        implicit none
	real :: A(1:5)
        integer :: num_threads
	integer :: thread_idnum,i
	print *,'entering parallel block ...'
!!      call omp_set_num_threads(num_threads)
	print *,'you set the number of threads = ',num_threads
!$omp parallel
		thread_idnum=omp_get_thread_num()
		print*,'using thread number ',thread_idnum
		call fill_vector(thread_idnum,A)
		write(6,1)thread_idnum,(A(i),i=1,5)
1		format(t2,'thread num= ',I2,1x,'array values=',&
& 5(1x,f4.1))
!$omp end parallel
		print *,'exiting parallel block ...'
                write(6,1)thread_idnum,(A(i),i=1,5)
	end program silly3a
	subroutine fill_vector(thread,B)
	real :: B(1:5)
	integer :: j
	do j=1,5,1
	B(j)=j
	end do
	return
	end
