module funciones
use mod_prec
implicit none

!ESTA ESTA EN SIMPSON

contains
    
    real(wp) function cuad(x)
    real(wp), intent(in)    :: x
    cuad = exp(x)
    end function cuad
    
    !cuando terminamos de escribir todo se nos ocurrió que podríamos haber definido unas variables
    !alfa y  g con los valores en vez de escribirlo cada vez. :(


end module funciones
