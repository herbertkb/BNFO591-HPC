program hw3
    implicit none
    
    interface 
        subroutine CollectDataPairs(filename, pairs)
            character(len=*), intent(in) :: filename
            character(len=4), intent(inout) :: pairs(:,:)
        end subroutine
        
        subroutine GetUniqueValues(matrix, list)
            character(len=4), intent(in) :: matrix(:,:)
            character(len=4), intent(inout) :: list(:)
        end subroutine
        
        subroutine SelectionSort(array)
            character(len=4), intent(inout) :: array(:)
        end subroutine
        
        integer function StringIndex(array, string)
            character(len=4), intent(in) :: array(:)
            character(len=4), intent(in) :: string
        end function
    end interface
    
    character(len=*),parameter :: file1="testdata.dat"
    character(len=4) :: labelA, labelB
    character*4, allocatable :: labels(:), uniqueLabels(:)
    integer :: iostatus, i, j, n
    integer :: countOfEdges, countUniqueLabels
    integer :: CountLinesInFile, CollectEdgesFromFile
    character*4, dimension(:, :), allocatable :: edges
    integer, dimension(:, :), allocatable :: adjMatrix
    integer :: index_a, index_b
    character(len=4) :: a, b
    
    
    countOfEdges = CountLinesInFile(file1)
    allocate( edges(countOfEdges, 2))

    print '("count of edges: ", I5)', countOfEdges
    
    call CollectDataPairs(file1, edges)
    
    
    do i=1,countOfEdges
        print *, edges(i,1), edges(i,2)
    end do
    
    ! worst case: every node is unique so #nodes is twice #edges 
    allocate(uniqueLabels( 2 * countOfEdges))
    
    call GetUniqueValues(edges, uniqueLabels)
    
    countUniqueLabels = size(uniqueLabels)
    print '("count of unique nodes:", I5)', countUniqueLabels
    
    call SelectionSort(uniqueLabels)
    
    !print *, uniqueLabels
    
    allocate( adjMatrix( countUniqueLabels, countUniqueLabels) )
    
    do i=1,countUniqueLabels
        do j=1,countUniqueLabels
            adjMatrix(i,j) = 0
        end do
    end do
        
    
    do i=1,countOfEdges
        !print *, edges(i,1), edges(i,2)
        a = edges(i,1)
        b = edges(i,2)
        
        index_a = StringIndex(uniqueLabels, a)
        index_b = StringIndex(uniqueLabels, b)
        
        adjMatrix(index_a, index_b) = 1
        adjMatrix(index_b, index_a) = 1
    end do
    
    n = size(adjMatrix, 1)
    print '(A7 *(A4))', "", (uniqueLabels(i), i=1,n)
    do i=1,n
        print '(A4 *(I4))', uniqueLabels(i), ( adjMatrix(i, j), j=1,n)
    end do

    
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
    integer :: i, n, iostatus 
    n = size(pairs,1) 

    
    open(unit=11, file=filename, status='old')
        do i=1,n
            read(11, *, iostat=iostatus) pairs(i,1), pairs(i, 2)   ! a, b
       end do
    close(unit=11)
    
end subroutine CollectDataPairs


!!--------------------------------------------------------------------------

subroutine GetUniqueValues(matrix, list)
    implicit none
    character(len=4), intent(in) :: matrix(:,:)
    character(len=4), allocatable, intent(inout) :: list(:)
    character(len=4), allocatable :: tempList(:)
    integer :: m, n
    integer :: i, j, k
    logical :: isUnique = .FALSE.
    
    m = size(matrix, 1)
    n = size(matrix, 2)
    
    k = 0
    do i=1, m
        do j=1, n
            if (.NOT. ANY( list == matrix(i,j) ) ) then
                k = k+1
                list(k) = matrix(i,j)
            end if
        end do
    end do
    
    !! resize list to remove empty cells.
    !! TODO: make this its own subroutine
    allocate(tempList(k))
    do i=1,k
        tempList(i) = list(i)
    end do
    deallocate(list)
    list = tempList
    
end subroutine


!! Adapted from RosettaCode.org, C implementation of Selection Sort
subroutine SelectionSort(array)
    implicit none
    character(len=4), intent(inout) :: array(:)
    integer :: n
    integer :: i, j
    integer :: smallest

    character(len=4) :: temp
    
    n = size(array, 1)
    
    !! for each element in the array
    do i=1,n
    
        !! find the ith smallest
        smallest = i
        do j=i, n
            if (array(j) < array(smallest)) then
                 smallest = j
            end if
        end do
        
        !! and swap array(i) with the ith smallest
        temp = array(i)
        array(i) = array(smallest)
        array(smallest) = temp
        
    end do 
    
end subroutine



integer function StringIndex(array, string)
    character(len=4), intent(in) :: array(:)
    character(len=4), intent(in) :: string
    integer :: i
    
    do i=1,size(array)
        if(string == array(i)) then
            StringIndex = i
            exit
        end if
    end do
    
    return
    
end function