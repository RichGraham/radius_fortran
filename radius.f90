implicit none
double precision :: my_area = 2.0
double precision :: calculate_radius

print*,"A circle of area", my_area, "m^2 has a radiys of", calculate_radius( my_area ),'m'
end program

function calculate_radius( area)
  implicit none
  double precision :: area, calculate_radius
  double precision :: pi = 3.14159265

  calculate_radius = sqrt(area/pi)

end function calculate_radius
