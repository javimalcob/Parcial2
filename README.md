# Parcial2

Segundo Parcial -  Métodos Numéricos 2023

plapla
## Lista de Tareas
### PARTE I
+ Implementar el metodo del trapecio integracion numerica equidistante
+ Probar el metodo del punto anterior
+ Implementar el metodo del trapecio no equidistante
+ Probar el metodo con una integral analitica
+ Manejo de Archivos, leer un archivos y detectar el numero de filas del archivo

### PARTE II
+ LatEx
+ Implementar el metodo centrado de 5 puntos
+ Implementar el metodo centrado de 3 puntos
+ Implementar el metodo de 2 puntos hacia adelante y hacia atras
+ Escribir en archivos
+ leer de la funcion Fit de GnuPlot 

+ Implementar el metodo de Simpson compuesto
### Adicionales
+ Arrays , Matrices , ALLOCATABLE


Archivo .dat
#t [s]	z [m]
0.0	 0.000
1.0	-4.412
2.0	-15.971
3.0	-32.690
4.0	-53.134
5.0	-76.300
6.0	-101.397
7.0	-127.914
8.0	-155.443
9.0	-183.709
10.0	-212.492
11.0	-241.716
12.0	-271.065
13.0	-301.263
14.0	-331.608
15.0	-358.733
16.0	-394.825

# notas ecucaciones diferenciales 

+ Problema de encontrar el parametro inicial de una ecuacion diferencial
+ Metodo de Euler
+ dy/dt = f(t,y)
  a < t < b
  h = (b-a)/ n
 y(t0) = alfa
 ```fortran
  t = a
  w = alfa
  !write(I2, 100) t, w
  i = 1
  do while (t <= b)
    t = a + i*h
    w = w + h * f(t,w)
    write ('I2', 100) t, w
    i = i + 1
  end do
 ```
