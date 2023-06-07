program parte2
use mod_prec
use metodos
use funciones
implicit none


    !########################### ENTRADA DE DATOS################################
    !----------------------------------------------------------------------------
    !Seccion de Declaracion de variables
    real(wp)                                       :: c, h, dfc, int, a , b
    real(wp), dimension(:), allocatable            :: t, z, v
    real(wp), dimension(:), allocatable            :: x, y ! vectores auxiliares
    integer(il)                                    :: fu, i, std, nlines, n
    character(80)                                  :: archivo_out , archivo_in, line
    !----------------------------------------------------------------------------
    !Seccion para inicializar las variables del programa
    archivo_in = 'datos.dat'
    archivo_out = 'salida.dat'
    nlines = 0.0_wp

    !-----------------------------------------------------------------------------
    !Abrimos el archivo de datos.dat para sacar los vectores t , z 
        !Primero averiguamos la cantidad de filas del archivo
    open(newunit=fu, file=archivo_in, status='old', action='read')
        do
            read(fu, *, iostat=std) line
            if (std /= 0) exit
            nlines = nlines + 1
        end do
    close(fu)    
    !------------------------------------------------------------------------------
    !Conocida la dimension en nlines le asignamos la cantidad de memoria incluido v
    allocate(t(0:nlines-1), z(0:nlines-1), v(0:nlines-1)) 
   
    !Cargamos los datos.dat al los vectores t , z
    open(newunit=fu, file=archivo_in, status='old', action='read')
        write(*,*) "Primer y segunda columna del archivo datos.dat corresponde a t y z(t)" !!BORRAR ESTO DEPUES
        do i = 0, nlines-1
            read(fu,*) t(i), z(i)
            write(*,*) t(i), z(i) !Solo para ver que se cargo bien
        end do
    close(fu)


    !###########################PROCESAMIENTO DE DATOS ##############################
    !-------------------------------------------------------------------------------- 
    !Para calcular la velocidad usamos formulas de derivacion numerica 2, 3 y 5 puntos modificadas
    !y guardamos los resultados en salida.dat con 3 columnas   
    open(newunit=fu, file=archivo_out)
        write(*,*) "Tercer Columna del archivo salida.dat corresponde a v(t)" !!BORRAR ESTO DESPUES
        write(fu,*)  "  t(i)                ", "        z(i)         ", "             v(i)"
        do i = 0, nlines-1
            h = 1.0_wp  !Longitud del intervalo tiempo equiespaciados 
            ! Primer punto uso formula de 2 puntos hacia adelante
            if (i == 0) then
                n = size(t(i:i+1)) - 1
                allocate(x(0:n), y(0:n))
                x = t(i:i+1) 
                y = z(i:i+1)
                call derivada2adelante(x, h, y, n, dfc)
                v(i) = dfc
                
                deallocate(x,y)

            ! Segundo punto uso formula de 3 puntos centrada  
            else if (i == 1) then
                n = size(t(i-1:i+1)) - 1
                allocate(x(0:n), y(0:n))
                x = t(i-1:i+1)
                y = z(i-1:i+1)
                call derivada3centrada(x, h, y, n, dfc)
                v(i) = dfc
                
                deallocate(x,y)
            ! Anteultimo punto uso formula de 3 puntos centrada 
            else if (i == nlines-2) then
                n = size(t(i-1:i+1)) - 1
                allocate(x(0:n), y(0:n))
                x = t(i-1:i+1)
                y = z(i-1:i+1)
                call derivada3centrada(x, h, y, n, dfc)
                v(i) = dfc
                
                deallocate(x,y)
            ! Ultimo punto uso formula de 2 puntos hacia atras
            else if (i == nlines-1) then
                n = size(t(i-1:i)) - 1
                allocate(x(0:n), y(0:n))
                x = t(i-1:i)
                y = z(i-1:i)
                call derivada2atras(x, h, y, n, dfc)
                v(i) = dfc

                deallocate(x,y)
            !Puntos intermedios uso formula de 5 puntos centrada
            else
                n = size(t(i-2:i+2)) - 1
                allocate(x(0:n), y(0:n))
                x = t(i-2:i+2)
                y = z(i-2:i+2)
                call derivada5centrada(x, h, y, n, dfc)
                v(i) = dfc

                deallocate(x,y)


            end if
            
            write(*,*) 'Iteracion', i ,v(i) !!BORRAR ESTO DESPUES
            write(fu,*) t(i), z(i), v(i)
        end do
           write(*,*) "Procesamiento de datos exitoso lo datos se guardaron en >salida.dat< abrelo!  :D"
    
    close(fu)

     !########################### CALCULO DEL TRABAJO##############################
    !--------------------------------------------------------------------------------
    a = 0._wp
    b = 16._wp
    n = 100
    
    call simpson (a, b, n, Fv, int)
    print*, "El trabajo es W =", int 
    
    
      !########################### CALCULO DEL TRABAJO################################
    !--------------------------------------------------------------------------------
    
     !subroutine lagrange(n, x, fx, c, pc)
     
     do i = 0 , 100
        c = 0 + (16/100) * i
        call lagrange(16, z, v, c , pc)
     end do
     deallocate(t, z, v)
end program parte2
