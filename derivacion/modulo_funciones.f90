module funciones

use mod_prec
implicit none

contains
    function f1(x)
        real(wp)                   :: f1
        real(wp), intent(in)       :: x
        f1 = cos(x)
    end function f1
    
    function f2(x)
        real(wp)                   :: f2
        real(wp), intent(in)       :: x
        f2 = exp(x)
    end function f2
    
        function f3(x)
        real(wp)                   :: f3
        real(wp), intent(in)       :: x
        f3 = x**2._wp
    end function f3
    
end module funciones
