"
Considere una bencinera, la cual tiene una única bomba de bencina. El tiempo entre llegada de los
automóviles a la bencinera sigue una distribución exponencial con una tasa de llegada de 1 vehículo cada 3
minutos. Por otro lado, el tempo que el operador se demora en cargar la bencina en el automóvil sigue una
distribución normal, con una media de 2 minutos y una desviación estándar de 0,5 minutos.

Se pide, simule el proceso de compra de bencina durante 5 horas y responda las siguientes preguntas.
1.- Cuál es el tiempo promedio de espera de un vehículo para ser atendido.
2.- Cuál es el tiempo máximo que espera un vehículo para ser atendido.
3.- Cuál es el tiempo ocioso del operador.
"

tiempos_promedio_espera <- c()
tiempos_maximos_espera <- c()
tiempos_ososios <- c()

for (i in 1:1000){ # ciclo de las mil simulaciones 
                  
  lambda <- 1/3 # Tasa de llegada de clientes
  tiempo <- 300 # Tiempo total del problema en minutos
  tiempo_acumulado <- 0
  
  tiempo_llegada <- c() # Donde almaceno los tiempos de llegada
  
  while (tiempo_acumulado <= tiempo){
    x <- rexp(1,lambda) # Funcion distribucion exponencial 
    tiempo_acumulado = tiempo_acumulado + x
    if (tiempo_acumulado <= tiempo)
      tiempo_llegada = c(tiempo_llegada, tiempo_acumulado)  # agrego el tiempo acumulado al sistema
  }
  
  
  numero_autos <- length(tiempo_llegada) # Numero de autos en el sistema
  tiempo_atencion <- rnorm(numero_autos,2,0.5) # Distribucion normal con la cantidad de autos en el sistema
  
  tiempo_inicio_atencion <- c()
  tiempo_termino_atencion <- c()
  
  tiempo_inicio_atencion <- c(tiempo_inicio_atencion,tiempo_llegada[1]) # si o si cuando llegue debe atenderlo
  tiempo_termino_atencion<- c(tiempo_termino_atencion,tiempo_inicio_atencion[1] + tiempo_atencion[1]) # se suma el tiempo de inicio de la atencion y el vakor almacenado del tiempo de atencion de la distribucion normal
  
  for (i in 2:numero_autos){
    tiempo_atencion <- rnorm(numero_autos,2,0.5) # Asigno otro valor a la distribucion normal 
    tiempo_inicio_atencion <- c(tiempo_inicio_atencion, max(tiempo_llegada[i], tiempo_termino_atencion[i-1])) # El tiempo de atencion empieza cuando termine el cliente anterior inmediatamente si hay alguien en fila, si no hay el tiempo sigue corriendo 
    tiempo_termino_atencion<- c(tiempo_termino_atencion,(tiempo_inicio_atencion[i] + tiempo_atencion[i]))
  }
  
  " 1. Cuál es el tiempo promedio de espera de un vehículo para ser atendido.
  "
 
  tiempo_espera <- c() # Asigno cadena para los tiempos de espera
  
  for (i in 1:numero_autos){
    tiempo <- tiempo_inicio_atencion[i] - tiempo_llegada[i]
    tiempo_espera = c(tiempo_espera, tiempo)
  }
  
  mean(tiempo_espera) # Media de los tiempos de espera
  
  "
  2.- Cuál es el tiempo máximo que espera un vehículo para ser atendido
  "

  max(tiempo_espera) # Maximo de los tiempos de espera 
  
  "
  3.- Cuál es el tiempo ocioso del operador.
  "
  
  tiempo_ocioso <- c() # Creo una cadena para los tiempos ociosos del operador
  
  tiempo_ocioso <- c(tiempo_ocioso, tiempo_inicio_atencion[1]) # Agrego el tiempo hasta que se empezo a atender el primer cliente
  
  for (i in 1:numero_autos - 1){
    tiempo = tiempo_inicio_atencion[i+1] - tiempo_termino_atencion[i] # Tiempo de inicio de atencion del segundo cliente menos el termino de atencion del primer cliente 
    tiempo_ocioso = c(tiempo_ocioso, tiempo) # agrego el tiempo a la cadena en cada usuario 
  }
  
  sum(tiempo_ocioso) # sumo los tiempos de ocio 
  mean(tiempo_ocioso) # media de los tiempos de ocio 
"
------------------------------------------------------------------------------------------------------------------------------------------

1000 SIMULACIONES
"
  tiempos_promedio_espera <- c(tiempos_promedio_espera, mean(tiempo_espera))  # promedio de los tiempos de espera
  tiempos_maximos_espera <- c(tiempos_maximos_espera, max(tiempo_espera))   # maximos tiempos de espera en cada simulasion
  tiempos_ososios <- c(tiempos_ososios, sum(tiempo_ocioso)) # suma de todo el tiempo de ocio en los 300 minutos para cada una de las mil simulaciomes
}

tiempos_ososios

"
HISTOGRAMAS
"
length(tiempo_llegada)

hist(tiempos_promedio_espera)
hist(tiempos_maximos_espera)
hist(tiempos_ososios)

tiempo_ocioso



