!!  sample fortran from lecture slides
program arithmetic_example
    integer:: a=2, b=7, c=4,z
    real:: x=2, y=3, w,v=2.5

    i = 2
    j = 3
    k = j/i
    w = y/x
    print *, a+b
    print *, k,w
    print *, v**a
end program
