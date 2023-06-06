program problema2

use mod_prec
use funciones
use derivadas

implicit none
	
  !Declaracion de variables
	real(wp)			:: c, h, dfc
	
	c = 0._wp
	h = 2._wp

  !Llammamos a las subrutinas
  
	call derivada3centrada (c, h, f3, dfc)	 	
	 	print *,'derivada tres puntos:', dfc
	
	call derivada5centrada (c, h, f3, dfc)	 	
	 	print *,'derivada xinxo puntos:', dfc
	 	
	call derivada2puntos (c, h, f3, dfc)	 	
	 	print *,'derivada dos puntos:', dfc

end program problema2
