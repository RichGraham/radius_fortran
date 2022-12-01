implicit none
double precision :: my_area = 2.0
double precision :: calculate_radius

print*,"A circle of area", my_area, "has a radius of", calculate_radius( my_area )
end program

function calculate_radius( area)
  implicit none
  double precision :: area, calculate_radius
  double precision :: pi = 4 * atan (1.0_16)
  double precision :: x, x_2, x_3, x_4, x_5

  ! Calculate the square root by a Taylor series in powers of x- 1 (a bad choice of method!)
  x = area / pi -1.0
  x_2 = x*x
  x_3 = x_2*x
  x_4 = x_2*x_2
  x_5 = x_3*x_2

  calculate_radius = 1.0 + 1.0/2.0*x - 1.0/8.0*x_2 + 1.0/16.0*x_3 - 5.0/128.0*x_4 + 7.0/256.0*x_5

end function calculate_radius
