program main_test
use mod_prec
use mod_funciones
use mod_integracion
implicit none
		
	!Declaracion de variables
	real(wp)		:: int
    real(wp), dimension(22)   :: x = (/0.0,0.5,0.75, 0.81, 1.0, 1.1, 1.3, 1.5, 1.7,&
                     2.0, 2.2, 2.3, 2.7, 2.95, 3.0, 3.12, 3.25, 3.67, 3.79, 3.91, 3.98, 4.0/)
	integer(il)		:: n
	!Inicializacion de variables

	
	n = size(x)
	  
	!Bloque de procesamiento	
	call trapecionoeq (x, f1, n, int)
	
	!Salida de datos
	write(*,*) "El valor de la integral de integral es:", int

end program main_test
