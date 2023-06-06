module mod_simpson

use mod_prec
use funciones

implicit none

contains

	subroutine simpson (a, b, n, f, int)
	
  !Declaracion de variables
  	real(wp), intent(in)	 :: a, b
  	real(wp)				 :: f
  	integer(il), intent (in) :: n
  	real (wp), intent(out)	 :: int
  !Declaracion de variables auxiliares
  	real(wp)				 :: h, x
  	integer(il)				 :: i
  	
  !Inicialización de variables
  
  	h = (b-a)/(n+2._wp)
  	
  	
  	int = (f(a) - f(b))/3._wp
  	
  	do i = 1 , (n/2)-1, 1
  	
  		x = a + 2*i*h
  		
  		int = int + (2._wp/3)*f(x)
  	
  	end do 	
  	
  	int = int * h
  	
	end subroutine simpson


end module mod_simpson
