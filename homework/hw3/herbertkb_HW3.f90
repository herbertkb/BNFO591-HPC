! herbertkb_H3.f90
! Keith Herbert
! BNFO 591, 04 Oct 2015

! requirements
! * read a graph .dat file into a 2D adjanceny matrix
! * write output to file
!   * # of data pairs (# of edges?)
!   * # of unique nodes (the dimension of the matrix?)
! * store names of nodes
! * construct the adjanceny matrix (if not already done)
! * compute diameter of network (compare to right side of Table1)
!   * output the original adjanceny matrix and its "diameterized" form
!------------------------------------------------------------------------------


module AdjacencyMatrix
    implicit none
    type adjMatrix
        character, allocatable :: labelsNodes(:)
        integer :: countEdges, countNodes
        real, allocatable :: matrix (:,:)
    end type adjMatrix   
    
contains

    type( adjMatrix ) function readMatrixFile (filename)
        
        character(len=*), intent(in)  :: filename
        type( adjMatrix ) :: matrix
        integer, parameter :: MAXNODES = 50
        integer :: ios                      ! End of Line for read() IOSTAT
        character(MAXNODES) :: currentLine
        
        open(unit = 10, file=filename, status='old')
            do 
                !! read whole line into currentLine
                read (10, '(a)', IOSTAT=ios) currentLine
                currentLine = trim(currentLine)
                
                if (ios .EQ. 0 ) then
                    !! skip the line if it contains a comment char #
                    if ( scan(currentLine, '#') > 0) then
                        cycle
                    endif
                    
                    !! process the line
                    print *, currentLine
                    
                else
                    exit
                endif
            end do
        close(11)
        
        matrix%labelsNodes = (/'A', 'B'/)
        matrix%countEdges = 1
        matrix%countNodes = 2
        
        allocate ( matrix%matrix(2,2) )
        
        matrix%matrix = reshape((/1,0,0,1/), shape (matrix%matrix))
        

    end function readMatrixFile

end module AdjacencyMatrix

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

program herbertkb_H3
    use AdjacencyMatrix
    implicit none
    type( adjMatrix ) :: testMatrix

    testMatrix = readMatrixFile("testdata.dat")


end program herbertkb_H3

!------------------------------------------------------------------------------
