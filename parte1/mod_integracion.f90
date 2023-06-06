module trapecio

use mod_prec 

implicit none

contains
		 !(a, b, f1, n, int, x, y, cant_datos)
	subroutine trapecionoeq (x, y, n, int)
		
		use mod_prec
		
		implicit none
	
	!Declaracion de variables
		real(wp), dimension(0:n), intent(in)    :: x, y
		!real(wp)			                  :: f
		real(wp), intent(out)		          :: int
		integer(il), intent(in)		          :: n
		 
	!Declaracion de variables aux
		real(wp)  		        :: aux, h, xi
		integer(il)			    :: i
		
	!Incializacion de variables
		
		int = 0.0_wp	
		
	!Bloque de procesamiento
	
		do i = 0 , n-1
			
			!h = x(i+1)-x(i)
			!aux = f(x(i))
			!aux = (f(x(i)) + f(x(i+1)))*2._wp
			
			
			h = x(i+1)-x(i)
			aux = y(i) + y(i+1)/2
			int = int + aux * h
		    !write(*,*) int
		end do 
		
		print *, '~~~~~~~~~~~~~~~~~~~~~~~~~~~'
		!print *, 'la integral es', int
		print *, '~~~~~~~~~~~~~~~~~~~~~~~~~~~'
		
		!int = int * h	eb este caso tenemos distintos h por c/intervalo
		
	end subroutine trapecionoeq
	

end module trapecio
