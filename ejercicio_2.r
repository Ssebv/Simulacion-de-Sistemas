"
Considere una bencinera, la cual tiene una única bomba de bencina. El tempo entre llegada de los
automóviles a la bencinera sigue una distribución exponencial con una tasa de llegada de 2 vehículo cada 5
minutos. Además, se sabe que, por diversos motivos, el 5% de los vehículos que ingresan a la bencinera
salen de esta antes de realizar la fila. Por otro lado, el tempo que el operador se demora en cargar la
bencina en el automóvil sigue una distribución normal, con una media de 2.5 minutos y una desviación
estándar de 1,0 minuto.

1. Replique 1000 veces usando el proceso de compra de bencina durante 8 horas de
operación.
Además, responda las siguientes preguntas
2. Cuál es el tempo promedio y desviación estándar de espera de un vehículo para ser
atendido.
3.Entre que rangos se encuentra el tempo máximo de espera de un vehículo.
4.Entre que rangos se encuentra el tempo ocioso del operador.
5.Entre que rangos se encuentra el número máximo de vehículos en el sistema
"

tiempos_promedio_espera_media <- c()
tiempos_promedio_espera_d <- c()
tiempos_maximos_espera <- c()
tiempos_ososios <- c()
numero_maximo_vehiculos <- c()

for (i in 1:1000){
    
    lambda <- 2/5
    tiempo <- 480
    tiempo_acumulado <- 0

    tiempo_llegada <- c()

    while (tiempo_acumulado <= tiempo) {

        if (runif(1) > 0.05) {
            x <- rexp(1, lambda)
            tiempo_acumulado = tiempo_acumulado + x
            if (tiempo_acumulado <= tiempo)
                tiempo_llegada = c(tiempo_llegada, tiempo_acumulado)
        }
    }

    tiempo_llegada

    numero_autos <- length(tiempo_llegada)
    tiempo_atencion <- rnorm(numero_autos ,2.5 , 1)

    tiempo_inicio_atencion <- c()
    tiempo_fin_atencion <- c()

    tiempo_inicio_atencion[1] <- tiempo_llegada[1]
    tiempo_fin_atencion[1] <- tiempo_inicio_atencion[1] + tiempo_atencion[1]

    for (i in 2:numero_autos) {
    if (tiempo_llegada[i] > tiempo_fin_atencion[i-1]) {
        tiempo_inicio_atencion[i] <- tiempo_llegada[i]
        tiempo_fin_atencion[i] <- tiempo_inicio_atencion[i] + tiempo_atencion[i]
    } else {
        tiempo_inicio_atencion[i] <- tiempo_fin_atencion[i-1]
        tiempo_fin_atencion[i] <- tiempo_inicio_atencion[i] + tiempo_atencion[i]
    }
    }

    tiempo_inicio_atencion
    tiempo_fin_atencion

    #2. (0,3 puntos) Cuál es el tempo promedio y desviación estándar de espera de un vehículo para ser
    #atendido.

    tiempo_espera <- tiempo_inicio_atencion - tiempo_llegada
    mean(tiempo_espera)
    sd(tiempo_espera)

    #3. (0,3 puntos) Entre que rangos se encuentra el tempo máximo de espera de un vehículo.

    max(tiempo_espera)
    min(tiempo_espera)

    #4. (0,3 puntos) Entre que rangos se encuentra el tempo ocioso del operador.

    tiempo_ocioso <- c()
    tiempo_ocioso <- c(tiempo_ocioso, tiempo_inicio_atencion[1])

    for (i in 2:numero_autos -1) {
        tiempo_ocioso <- c(tiempo_ocioso, tiempo_inicio_atencion[i] - tiempo_fin_atencion[i-1])
    }

    tiempo_ocioso

    sum(tiempo_ocioso)
    mean(tiempo_ocioso)
    max(tiempo_ocioso)

    # 5. (2,1 puntos) Entre que rangos se encuentra el número máximo de vehículos en el sistema

    numero_vehiculos <- c()
    numero_vehiculos <- c(numero_vehiculos, 1)

    for (i in 2:numero_autos) {
        numero_vehiculos <- c(numero_vehiculos, sum(tiempo_llegada <= tiempo_fin_atencion[i]))
    }

    numero_vehiculos

    max(numero_vehiculos)

    

    tiempos_promedio_espera_media <- c(tiempos_promedio_espera_media, mean(tiempo_espera))  # promedio de los tiempos de espera
    tiempos_promedio_espera_d <- c(tiempos_promedio_espera_d, sd(tiempo_espera))  # desviacion estandar de los tiempos de espera
    tiempos_maximos_espera <- c(tiempos_maximos_espera, max(tiempo_espera))   # maximos tiempos de espera en cada simulasion
    tiempos_ososios <- c(tiempos_ososios, sum(tiempo_ocioso))
    numero_maximo_vehiculos <- c(numero_maximo_vehiculos, max(numero_vehiculos))
}




tiempos_promedio_espera_media
tiempos_promedio_espera_d
tiempos_maximos_espera
tiempos_ososios
numero_maximo_vehiculos

max(tiempos_promedio_espera_media)
max(tiempos_promedio_espera_d)
max(tiempos_maximos_espera)
max(tiempos_ososios)
max(numero_maximo_vehiculos)

min(tiempos_promedio_espera_media)
min(tiempos_promedio_espera_d)
min(tiempos_maximos_espera)
min(tiempos_ososios)
min(numero_maximo_vehiculos)



















