program main_test
use mod_prec
use mod_funciones
use mod_integracion
implicit none
		
	!Declaracion de variables
	real(wp)		:: a, b, int
	integer(il)		:: n
	!Inicializacion de variables
	a = 0
	b = 4
	
	write(*,*) "DAME ENEEEE" 
	read(*,*) n
	
	!Bloque de procesamiento	
	call trapecio(a, b, n, f1,int)
	
	!Salida de datos
	write(*,*) "El valor de la integral de integral es:", int

end program main_test
