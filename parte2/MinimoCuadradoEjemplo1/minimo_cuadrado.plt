set term pos col

set out 'pendiente.ps'
    set title "mejor recta"
    set grid
    set style line 1 ps 0.5 pt 7 lc 'red' #ESTO LO COPIE
    set style line 2 lw 1.5 lc 'blue' # NI IDEA
    set key bottom center box height 1.4 # SI MODIFICO ALGO SE ROMPE XD
   
    #------------------------------------------------------------------------
    # JAZZZ IGNORA TODO LO DEMAS SON DETALLES ACA LO IMPORTANTE
    # ESTA PARTECITA SE PUEDE RECORTAR Y HACER LOS SCRIPT DE GNU PLOT COMO SIEMPRE
    # EL SCRIT FUNCION EJECUTAR CON GNUPLOT PARA VER COMO QUEDARIA Y GENERA EL .log
    # LO DEJE CON EL ARCHIVO .dat que usa 
    
     f(t) =  -g*a * t + g*a**2 *(1 - exp(-t/a))   # Aca defino el modelo de la funcion para ajustiar(un recta)

    # USO LA FUNCION FIT Y EL ARCHIVO DE DATOS usando m y q como los parametros a determinar
    fit f(x) 'datos.dat' using 1:2 via g, a 
    
    #-------------------------------------------------------------------------
    # ESTO ES UNA COSA RARA SOLO PARA QUE SE VEA BONITO(AVANZADO) LO COPIE DE UN CHABON xD
    # CREO QUE ERA EL CUADRITO ESE PARA TIRAR FACHA EN LA GRAFICA FINAL 
    #mq_value = sprintf("m = %f k$/m^2\nq = %f k$", m, q)
    #set object 1 rect from 90,725 to 200, 650 fc rgb "white" 
    #set label 1 at 100,700 mq_value

    # ACA SE GRAFICA LOS DATOS Y CREERIA QUE LA RECTA TMB 
    #p 'datos .dat' ls 1 t 'pts ({m},{T²})', f(x) ls 2 t 'Recta de Ajuste'
    set out


exit
