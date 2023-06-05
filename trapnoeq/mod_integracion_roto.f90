module mod_integracion

use mod_prec 
use mod_funciones
implicit none


!las subr deben incluir entre sus arg. a los limites de integracion 
!y el nº de puntos o intervalos empleados. 

contains
					   !(a, b, f1, n, int, x, y, cant_datos)
	subroutine trapecio (a, b, f, n, int, x, y, dim)
		
		use mod_prec
		use mod_funciones
		implicit none
	
	!Declaracion de variables
		real(wp), intent(in)  				:: a, b
		real(wp)							:: f
		real(wp), intent(out)				:: int
		integer(il), intent(in)				:: n, dim
		real(wp), allocatable, dimension(:) :: x, y 
	!Declaracion de variables aux
		real(wp) 							:: aux, h, xi
		integer(il)							:: i
		
	!Incializacion de variables
		
		allocate (x(dim), y(dim))
		
		h = (b - a)/ n
		int = (f(a) + f(b)) / 2.0_wp
		
		!Bloque de procesamiento
		do i = 1 , dim-1
			
			xi = a + i*h
			int = int + f(xi)
					
		end do 
		
		print *, '~~~~~~~~~~~~~~~~~~~~~~~~~~~'
		print *, 'la integral es', int
		print *, '~~~~~~~~~~~~~~~~~~~~~~~~~~~'
		
		!int = int * h	eb este caso tenemos distintos h por c/intervalo
		
		deallocate (x, y)
		
	end subroutine trapecio
	
		

end module mod_integracion
