program read_my_datafile
      integer, parameter :: nrecords = 100 ! how many records are in the file to be read
      integer, parameter :: nvars = 37 ! how many variables are in the line
      integer :: i,j ! loop variables
      integer dimension(1,nrecords:1,nvars) ! stores the data into the array vars
      open(101,file='all_possible_subsets.dat',status='old') ! open the hw datafile
      for i=1,nrecords,1 ! loop through all of the records in the file
      read(101,*)(vars(i,j) j=1,nvars,1) ! read each line
      end for
end program read_my_datafile
