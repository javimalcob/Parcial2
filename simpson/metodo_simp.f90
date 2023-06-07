module sinso
use mod_prec
use funciones
implicit none

contains
!###########################################################################################
!---------------------------METODO DE SIMPSON COMPUESTO-------------------------------------
!------------------------------------------------------------------------------------------- 

	subroutine simpson (a, b, n, f, int)
	    
      !Declaracion de variables
      	real(wp), intent(in)	 :: a, b    !intervalo 
      	real(wp)				 :: f       !funcion
      	integer(il), intent (in) :: n       !cant puntos (par)
      	real (wp), intent(out)	 :: int     !integrak
      !Declaracion de variables auxiliares
      	real(wp)				 :: h, x_par, x_impar    !intervalo, puntos
      	integer(il)				 :: i       !los pasitos
      	
      !Inicialización de variables
      
      	h = (b-a)/n
      	int = (f(a) + f(b))/3._wp
      	
      	par: do i = 1 , (n/2)-1, 1
      	    x_par = a + 2._wp*i*h + h
      		int = int + (2._wp/3)*f(x_par)
      	end do par
      	
      	impar: do i = 1 , (n/2), 1
      	    x_impar = a + 2._wp*i*h
      		int = int + (4._wp/3)*f(x_impar)
      	end do impar
      	
      	int = int * h
      	
	end subroutine simpson
	
end module sinso
