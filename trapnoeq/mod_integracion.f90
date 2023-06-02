module mod_integracion

use mod_prec 
use mod_funciones
implicit none


!las subr deben incluir entre sus arg. a los limites de integracion 
!y el nº de puntos o intervalos empleados. 

contains
	
	subroutine trapecio (a, b, n, f, int)
		use mod_prec
		use mod_funciones
		implicit none
		!Declaracion de variables
		real(wp), intent(in)  	:: a, b
		real(wp)		:: f
		real(wp), intent(out)	:: int
		integer(il), intent(in)	:: n
		
		!Declaracion de variables aux
		real(wp) 		:: xi, h
		integer(il)		:: i
		
		!Incializacion de variables
		h = (b - a)/ n
		int = (f(a) + f(b)) / 2.0_wp
		
		!Bloque de procesamiento
		do i = 1 , n-1
			xi = a + i*h
			int = int + f(xi)
			
		end do 
		int = int * h
		
	end subroutine trapecio
	
		

end module mod_integracion
