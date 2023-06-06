module mod_integracion_nofunca

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
		real(wp), intent(in)			:: a, b
		real(wp)				:: f
		real(wp), intent(out)			:: int
		integer(il), intent(in)			:: n, dim
		real(wp), dimension(dim), intent (in)	:: x, y 
	!Declaracion de variables aux
		real(wp) 				:: aux, h, xi
		integer(il)				:: i
		
	!Incializacion de variables
				
		!int = y(0) + y(1) / 2.0_wp
		int = 0._wp
		
	!Bloque de procesamiento
	
		do i = 0 , dim-1
			
			h = x(i+1) - x(i)
			aux = (y(i) + y(i+1))*2._wp*h
			
			int = int + aux
					
		end do 
		
		print *, '~~~~~~~~~~~~~~~~~~~~~~~~~~~'
		write(*,'(A16, F22.14)') 'la integral es', int
		print *, '~~~~~~~~~~~~~~~~~~~~~~~~~~~'
		
		!int = int * h	eb este caso tenemos distintos h por c/intervalo
		
		
	end subroutine trapecio
	
end module mod_integracion_nofunca
