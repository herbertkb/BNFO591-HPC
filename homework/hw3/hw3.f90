program hw3
    implicit none
    
    interface 
        subroutine CollectDataPairs(filename, pairs)
            character(len=*), intent(in) :: filename
            character(len=4), intent(inout) :: pairs(:,:)
        end subroutine
    end interface
    
    character(len=*),parameter :: file1="testdata.dat"
    character(len=4) :: labelA, labelB
    character*4, allocatable :: labels(:), uniqueLabels(:)
    integer :: iostatus, i, j
    integer :: countOfEdges
    logical :: flagFoundInArray
    integer :: indexForLabels, indexForUniqueLabels
    integer :: CountLinesInFile, CollectEdgesFromFile
    character*4, dimension(:, :), allocatable :: edges

    countOfEdges = CountLinesInFile(file1)
    allocate( edges(countOfEdges, 2))

    print '("count of edges: ", I5)', countOfEdges
    
    
    call CollectDataPairs(file1, edges)
    
    
    do i=1,countOfEdges
        print *, edges(i,1), edges(i,2)
    end do
    
    
!     allocate(labels(       countOfEdges * 2) )
!     allocate(uniqueLabels( countOfEdges * 2) )
!     indexForLabels          = 0
!     indexForUniqueLabels    = 0
!        
!     open(unit=11, file=file1, status='old')
!         do
!             read(11, *, iostat=iostatus) labelA, labelB
!             print *, labelA, labelB
!             
!             if (iostatus .EQ. 0) then
! !!------------------------------------------------------------------------
!                 indexForLabels         = indexForLabels + 1
!                 labels(indexForLabels) = labelA
!                 
!                 flagFoundInArray = .FALSE.
!                 do i=1,countOfEdges*2
!                     if (labels(i) .EQ. labelA ) then
!                         flagFoundInArray = .TRUE.
!                         exit
!                     end if
!                 end do
!                 print *, flagFoundInArray
!                 if(flagFoundInArray .EQV. .FALSE.) then
!                     indexForUniqueLabels = indexForUniqueLabels + 1
!                     uniqueLabels(indexForUniqueLabels) = labelA
!                 end if
! !!--------------------------------------------------------------------------                
!                 
!             else
!                 exit
!             end if
!        end do
!     close(unit=11)
!     
!     
!     
!     print '("count of unique labels: ", I5)', indexForUniqueLabels  
!     print *, uniqueLabels  
    
end program hw3

!!--------------------------------------------------------------------------                
integer function CountLinesInFile(filename) result (lineCount)
    implicit none
    character(len=*), intent(in) :: filename
    integer :: iostatus
    character(len=4) :: a, b

    lineCount = 0
    open(unit=11, file=filename, status='old')
        do
            read(11, *, iostat=iostatus) a,b  
            if (iostatus .EQ. 0) then
                lineCount = lineCount + 1
                print *, a,b
            else
                exit
            end if
       end do
    close(unit=11)
    
    return
    
end function CountLinesInFile


!!--------------------------------------------------------------------------                

subroutine CollectDataPairs(filename, pairs)
    implicit none
    character(len=*), intent(in) :: filename
    character*4, intent(inout) :: pairs(:,:)
    character(len=4) :: a, b
    integer :: i, n, iostatus 
    n = size(pairs,1) 

    
    open(unit=11, file=filename, status='old')
        do i=1,n
            read(11, *, iostat=iostatus) pairs(i,1), pairs(i, 2)   ! a, b
       end do
    close(unit=11)
    
end subroutine CollectDataPairs




! subroutine f(r)
! real(dp), intent(in) :: r(:)
! integer :: n, i
! n = size(r)
! do i = 1, n
!     r(i) = 1.0_dp / i**2
! enddo
! end subroutine
! 
! 2D arrays:
! 
! subroutine g(A)
! real(dp), intent(in) :: A(:, :)
! ...
! end subroutine
! 
! and call it like this:
! 
! real(dp) :: r(5)
! call f(r)

