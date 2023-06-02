program main_test
use mod_prec
!use mod_funciones
!use mod_integracion

!hay que abrir un archivo de datos, leer las filas, guardarlas en un 
!vector dos pro dos y darselo de comera ala subrutina.

implicit none
		
	!Declaracion de variables
	real(wp)								:: a, b, int
	real(wp), allocatable, dimension(:)		:: x, y
	integer(il)								:: n, i, cant_datos, fu, coso
	character(20)							:: archivo, line
	
	!Inicializacion de variables
	
	archivo = 'datos.dat'
	cant_datos = 0
	
	!Abrimos el .dat
	open(newunit=fu, file=archivo, status='old', action ='read')
			
		du: do
			read (fu, iostat=coso) line
			if (coso == iostat_end) exit du
			
			cant_datos = cant_datos + 1
		write(*,*) "a"
		end do du

	close (fu)
	
	print *, 'Cantidad de datos:', cant_datos

	allocate (x(cant_datos), y(cant_datos))
	
	open(newunit=fu, file=archivo, status='old', action = 'read')
		
		do i = 1, cant_datos
			read(fu,*) line
			read(line,*) x(i), y(i)
		end do 
	close (fu)
	
	print *, 'lo bectoreh', x, y
	
	deallocate (x, y)
	
!-----------------------------------	
!	write(*,*) "DAME ENEEEE" 
!	read(*,*) n
!	
!	!Bloque de procesamiento	
!	call trapecio(a, b, n, f1,int)
!	
!	!Salida de datos
!	write(*,*) "El valor de la integral de integral es:", int

end program main_test