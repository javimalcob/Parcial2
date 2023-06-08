program prueba_simpson

use mod_prec
use funciones
use sinso

implicit none

    real(wp)    :: a, b, int
    integer(il) :: n, i
    
    a = 0._wp
    b = 4._wp
    n = 4   

    call simpson (a, b, n, cuad, int)
       
            print *, int


    
end program prueba_simpson
