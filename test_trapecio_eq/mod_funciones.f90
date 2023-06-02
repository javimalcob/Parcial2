module mod_funciones
use mod_prec

contains
	real(wp) function f1(x)
		real(wp), intent(in)  :: x
		f1 = exp(x)
		end function f1

end module mod_funciones
