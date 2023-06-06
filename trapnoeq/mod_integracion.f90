module mod_integracion

use mod_prec 
use mod_funciones
implicit none

!las subr deben incluir entre sus arg. a los limites de integracion 
!y el nº de puntos o intervalos empleados. 

contains
		 !(a, b, f1, n, int, x, y, cant_datos)
	subroutine trapecio (a, b, f, n, int, x, dim)
		
		use mod_prec
		use mod_funciones
		implicit none
	
	!Declaracion de variables
		real(wp), intent(in)  		:: a, b
		real(wp)			:: f
		real(wp), intent(out)		:: int
		integer(il), intent(in)		:: n, dim
		real(wp), dimension(dim), intent(in) :: x
	!Declaracion de variables aux
		real(wp)  		       :: aux, h, xi
		integer(il)			:: i
		
	!Incializacion de variables
		
		int = 0
		
		write(*,*) "···································"
		write(*,'(8X, F22.14, 8X, F22.14)') x
		write(*,*) "···································"
		
	!Bloque de procesamiento
	
		do i = 1 , dim-1
			
			h = x(i+1)-x(1)
			aux = (f(x(i)) + f(x(i+1)))*2._wp
			
			int = int + aux*h	
		
		end do 
		
		print *, '~~~~~~~~~~~~~~~~~~~~~~~~~~~'
		print *, 'la integral es', int
		print *, '~~~~~~~~~~~~~~~~~~~~~~~~~~~'
		
		!int = int * h	eb este caso tenemos distintos h por c/intervalo
		
	end subroutine trapecio
	

end module mod_integracion
