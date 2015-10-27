!! Keith Herbert 
!! Homework Assignment FORTRAN 3
!! Computes the network diameter of a network from none-node connectivity data 

program hw3
    implicit none
    
    interface
        integer function CountLinesInFile(filename) result (lineCount)
            character(len=*), intent(in) :: filename
        end function 
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
        
        subroutine BuildAdjacencyMatrix(matrix, edges, labels)
            integer, allocatable, intent(inout) :: matrix(:,:)
            character(len=4), intent(in) :: edges(:,:)
            character(len=4), intent(in) :: labels(:)
        end subroutine
        
        subroutine PrintAdjacencyMatrix(matrix, labels)
            integer, intent(in) :: matrix(:,:)
            character(len=4), intent(in) :: labels(:)
        end subroutine
        
        subroutine WriteAdjacencyMatrix(matrix, labels, unit_num)
            integer, intent(in) :: matrix(:,:)
            character(len=4), intent(in) :: labels(:)
            integer :: unit_num
        end subroutine
        
        subroutine PrintLogicalMatrix(matrix, labels)
            logical, intent(in) :: matrix(:,:)
            character(len=4), intent(in) :: labels(:)
        end subroutine
        
        integer function NetworkDiameter(matrix)
            integer, intent(in) :: matrix(:,:)
        end function
        
    end interface
    
    character(len=*),parameter  ::  file1="experimental_data.dat"
    character(len=*),parameter  ::  fileout="testoutput_herbertkb.dat"    
    integer :: iostatus, i, j,k, n
    integer :: countOfEdges, countUniqueLabels
    character*4, allocatable :: edges(:,:)
    character*4, allocatable :: uniqueLabels(:)
    integer, dimension(:, :), allocatable :: adjMatrix, tempMatrix
    logical, allocatable :: nonZeroMatrix(:,:)
    logical :: flagAllTrue
    integer :: diameter
    
    !! open output file for writing
    open(unit=10, file=fileout, status='replace')
    
    !! Print and Write a header
    print *, "Keith Herbert"
    print *, "27 Oct 2015"
    print *, "Homework Assignment FORTRAN 3"
    write (10,*) "Keith Herbert"
    write (10,*) "27 Oct 2015"
    write (10,*) "Homework Assignment FORTRAN 3"    

    !! First, count #edges described in file
    countOfEdges = CountLinesInFile(file1)
    allocate( edges(countOfEdges, 2))

    print '("count of edges: ", I5)', countOfEdges
    write(10, '("count of edges: ", I5)') countOfEdges
   
    !! Next, collect the edges into a two column array
    print *, "collecting edges" 
    call CollectDataPairs(file1, edges)
    
    !! And find the unique nodes in the edge list.
    ! worst case: every node is unique so #nodes is twice #edges 
    print *, "finding unique nodes from the edge collection"
    allocate(uniqueLabels( 2 * countOfEdges))
    call GetUniqueValues(edges, uniqueLabels)
    
    print *, "finding count of unique nodes"
    countUniqueLabels = size(uniqueLabels)
    
    print '("count of unique nodes:", I5)', countUniqueLabels
    write(10, '("count of unique nodes:", I5)') countUniqueLabels
    
    !! Sort the label list so the Adjancency Matrix is readable
    print *, "sorting the unique nodes by label"
    call SelectionSort(uniqueLabels)
    
    !! Build Adjancency Matrix    
    print *, "build the adjancency matrix"
    call BuildAdjacencyMatrix(adjMatrix, edges, uniqueLabels)
    
    !! print Adjancency Matrix
!    call PrintAdjacencyMatrix(adjMatrix, uniqueLabels)
!    call WriteAdjacencyMatrix(adjMatrix, uniqueLabels, 10)
    

    print *, "finding network diameter"
    diameter = NetworkDiameter(adjMatrix)
    
    print *, diameter
    write(10, '("diameter of network is:", I5)') diameter
    
    close(10)
    
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

    
    open(unit=12, file=filename, status='old')
        do i=1,n
            read(12, *, iostat=iostatus) pairs(i,1), pairs(i, 2)   ! a, b
       end do
    close(unit=12)
    
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
    
    print *, list(:k)
         
    !! resize list to remove empty cells.
    allocate(tempList(k))
    tempList = list(:k)
    print *, size(tempList)
    
    deallocate(list)
    allocate(list(k))
    
    list = tempList
    print *, size(list)
    
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

subroutine BuildAdjacencyMatrix(matrix, edges, labels)
    implicit none
    
    interface
        integer function StringIndex(array, string)
            character(len=4), intent(in) :: array(:)
            character(len=4), intent(in) :: string
        end function
        
    end interface
    
    
    integer, allocatable, intent(inout) :: matrix(:,:)
    character(len=4), intent(in) :: edges(:,:)
    character(len=4), intent(in) :: labels(:)
    character(len=4) :: a, b
    integer :: n, i, j
    integer :: index_a, index_b
    
    n = size(labels)
 
    !! initialize the matrix and zero-fill it
    allocate( matrix( n, n) )
    do i=1,n
        do j=1,n
            matrix(i,j) = 0
        end do
    end do
        
    
    do i=1,size(edges, 1)
        a = edges(i,1)
        b = edges(i,2)
        
        index_a = StringIndex(labels, a)
        index_b = StringIndex(labels, b)
        
        matrix(index_a, index_b) = 1
        matrix(index_b, index_a) = 1
    end do   

end subroutine


subroutine PrintAdjacencyMatrix(matrix, labels)
    integer, intent(in) :: matrix(:,:)
    character(len=4), intent(in) :: labels(:)
    integer :: n
    
    n = size(matrix, 1)
    
    print '(A7 *(A4))', "", (labels(i), i=1,n)
    do i=1,n
        print '(A4 *(I4))', labels(i), ( matrix(i, j), j=1,n)
    end do
    
end subroutine

subroutine WriteAdjacencyMatrix(matrix, labels, unit_num)
    integer, intent(in) :: matrix(:,:)
    character(len=4), intent(in) :: labels(:)
    integer, intent(in) :: unit_num 
    integer :: n
    
    n = size(matrix, 1)
    
    write(unit_num, '(A7 *(A4))') (labels(i), i=1,n)
    do i=1,n
        write(unit_num, '(A4 *(I4))') labels(i), ( matrix(i, j), j=1,n)
    end do
    
end subroutine



subroutine PrintLogicalMatrix(matrix, labels)
    logical, intent(in) :: matrix(:,:)
    character(len=4), intent(in) :: labels(:)
    integer :: n
    
    n = size(matrix, 1)
    
    print '(A7 *(A4))', "", (labels(i), i=1,n)
    do i=1,n
        print '(A4 *(L4))', labels(i), ( matrix(i, j), j=1,n)
    end do
    
end subroutine

integer function NetworkDiameter( matrix )
    integer, intent(in) :: matrix(:,:)      ! network to calculate diameter for
    integer, allocatable :: adjMatrix(:,:)
    logical, allocatable :: nonZeroMatrix(:,:)  ! tests if all nodes visited
    integer :: n                            ! nodes in network, n x n matrix
    integer :: i,j                          ! index variables
    logical :: flagAllTrue                  ! test condition for diameter
    
!!Find the diameter of the graph for the Adjancency Matrix
!!===============================================================
!! do i=1,N-1
!!      multiply matrix by itself
!!      if all elements take a value > 0
!!          d = i
!!          break    
!! end loop

    n = size(matrix, 1)
    adjMatrix = matrix      ! Fortran won't allow modifying dummy argument

!! Create a matrix of booleans to indicate when each cell is nonzero atleast once
    allocate(nonZeroMatrix (n, n) )
    do i=1, n
        do j=1, n
            if(adjMatrix(i,j) > 0 ) then
                nonZeroMatrix(i,j) = .TRUE.
            else
                nonZeroMatrix(i,j) = .FALSE.
            end if
        end do
    end do
!    call PrintLogicalMatrix(nonZeroMatrix, uniqueLabels)


    do i=1, n-1
        
        !! Multply the matrix by itself
        adjMatrix = MATMUL(adjMatrix, adjMatrix)
        
        !print *, "=============================================="
        !call PrintAdjacencyMatrix(adjMatrix, uniqueLabels)
       
        !! set all the corresponding cells in the truth matrix to true
        do j=1, n
            do k=1, n
                if(adjMatrix(j,k) > 0 ) then
                    nonZeroMatrix(j,k) = .TRUE.
                end if
            end do
        end do
        
       !call PrintLogicalMatrix(nonZeroMatrix, uniqueLabels)
       !print *, "=============================================="
               
        !! Check if the entire truth matrix is true
        flagAllTrue = .TRUE.
        do j=1, n
            do k=1, n
                if( nonZeroMatrix(j,k) .EQV. .FALSE. ) then
                    flagAllTrue = .FALSE.
                end if
            end do
        end do
        
        !! Update the diameter and stop looping if whole matrix is true
        NetworkDiameter = i
        if(flagAllTrue .EQV. .TRUE.) then
            exit
        end if

    end do

    return
    
end function


