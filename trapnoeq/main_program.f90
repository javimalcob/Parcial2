program main_test
use mod_prec
use mod_funciones
use mod_integracion

!hay que abrir un archivo de datos, leer las filas, guardarlas en un 
!vector dos pro dos y darselo de comera ala subrutina.

implicit none
		
	!Declaracion de variables
	real(wp)				:: a, b, int
	real(wp), allocatable, dimension(:)	:: x, y
	integer(il)				:: i, n, cant_datos, fu, coso
	character(20)				:: archivo, line, col1, col2
	
	!Inicializacion de variables
	
	archivo = 'datos.dat'
	cant_datos = 0
	
	!Abrimos el .dat
    !#### FUNCIONA DESDE ACAAAAAA :D ################################## 
	
	open(newunit=fu, file=archivo, status='old', action ='read',iostat=coso)
			
		du: do
			read (fu,*, iostat=coso) line
			if (coso == iostat_end) then
                write(*,*) "caca final"
				exit du
			end if
			
			cant_datos = cant_datos + 1
	    	
	    	!write(*,*) "a"  
		
		end do du

	close (fu)
	
	print *, 'Cantidad de datos:', cant_datos

	allocate (x(cant_datos), y(cant_datos))
	
	open(newunit=fu, file=archivo, status='old', action = 'read')
		
		do i = 1, cant_datos
		
			read(fu,*) x(i), y(i)
            write(*,*) "LOQUE TIENE, x , y iteracion #",i, x(i), y(i)


 
		
		end do 
	
	close (fu)
	
	write(*,*) "Los vectores 	    x		   e   	 	    y"
	write(*,'(8X, F22.14, 8X, F22.14)') x,  y
	
	
!################FUNCIONAAAAAA HASTA ACA :D ####################
	
	write(*,*) "DAME ENEEEE" 
	read(*,*) n
	
  !Bloque de procesamiento	
	call trapecio (a, b, f1, n, int, x, cant_datos)
	
	deallocate (x,y)
  !Salida de datos
	write(*,*) "El valor de la integral de integral es:", int

end program main_test
