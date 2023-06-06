module derivadas

use mod_prec
use funciones
implicit none 

contains

! 3 PUNTOS CENTRADO
!-------------------------------------------------------------------------------------	
	
	subroutine derivada3centrada(c, h, f, dfc)
	
	!Declaracion de Dummy varaibles
		real(wp), intent(in)	:: c
        real(wp), intent(in)	:: h
        real(wp)   				:: f
        real(wp), intent(out)   :: dfc 	
	    
	    dfc = 1._wp/2_wp*h * (f(c - h) - f(c + h))
	
	end subroutine derivada3centrada

!5 PUNTOS CENTRADO
!-------------------------------------------------------------------------------------	
	
	subroutine derivada5centrada(c, h ,f, dfc)

    !Declaracion de Dummy variables
        real(wp), intent(in)	:: c
        real(wp), intent(in)    :: h
        real(wp)         		:: f
        real(wp), intent(out)   :: dfc 	
	    
	    !Declaracion de variables auxiliares
	    
	    dfc = 1._wp/(12._wp*h)*(f(c - 2._wp*h) - 8._wp*(c-h) + 8._wp*(c + h) - f(c + 2._wp*h))	
	    
	end subroutine derivada5centrada

!2 PUNTOS
!-------------------------------------------------------------------------------------

	subroutine derivada2puntos (c, h, f, dfc)
	!Declaracion de Dummy variables
        real(wp), intent(in)    :: c
        real(wp), intent(in)    :: h
        real(wp)            	:: f
        real(wp), intent(out)   :: dfc 	
	    
	    dfc = (f(c+h)-f(c))/h
	    
	end subroutine derivada2puntos

end module derivadas
