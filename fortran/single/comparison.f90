! comparison_example from lecture slides
program comparison_example
    integer:: a=2, b=7, c=4
    real:: x=2, y=3, w,v=2.5
    i = 2
    j = 3
    k = j/i
    w = y/x
    print *, a<b, b<c
    print *, a==b, b/=a
    print *, x /= a
    print *, (x /= y).and.(y<v)
end program


