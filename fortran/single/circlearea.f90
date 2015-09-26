! Compute the radius of a circle
! demo for reading from console and calculation with variables

program areaofcircle
    real R, PI, AREA
    character NAME
    print *, 'Enter circle label: '
    read *, NAME
    print *, 'Enter circle radius: '
    read *, R
    PI = 3.141592
    AREA = PI * R**2

    print *, 'Area of cicrle ', NAME, ' is: ', AREA

end
