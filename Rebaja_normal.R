install.packages('e1071')
install.packages('caret')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('randomForest')
install.packages("dplyr")
library("dplyr")
library(randomForest)
library(caret)
library(rpart)
library(rpart.plot)

install.packages("readxl")
library(readxl)

install.packages("writexl")
library("writexl")

# Semilla
set.seed(666)

# Listas para llenar el DF al final del "for"
tarifa <- list()
fugados <- list()
vigentes <- list()
total <- list()
ganancia_actual <- list()
ganancia_nueva <- list()
ganancia_neta <- list()
condicion <- list()
num_tarifa <- list()

# DF vacioS para llenar
mat <- matrix(ncol = 0, nrow = 0) # Matriz vacia
resultado <- data.frame(mat) # De matriz a DF
fug_vig_tarifa <- data.frame(mat) # De matriz a DF

# Base de datos original
data_raw <- read_excel("C:/Harold Personal/Company/Proyecto 1 - Base analisis de fuga2022.xls")
columnas = c("IDCliente","PlanInternacionalPreferente","PlanInternetIlim","MBNavegados","TotalMinHoraNormal","TotalLLamaHoraNormal","CobroLLamaHoraNormal","TotalMinHoraReduc","TotalLLamaHoraReduc","CobroLlamaHoraReduc",
             "TotalMinHoraNoct","TotalLLamaHoraNoct","CobroLLamaHoraNoct","TotalMinInternacional","TotalLLamaInternacional",
             "CobroLLamaInternacional", "LlamadaAtencionCliente", "Fuga")
colnames(data_raw)<- columnas
data_raw$IDCliente <- NULL # se elimina esa columna porque no es útil para el modelo de prediccón

# Para entrenar el modelo, saco TotalMinHoraNormal para que el predictor use el Cobro y no los minutos como atributo importante
data_raw_train <- data_raw
data_raw_train$TotalMinHoraNormal <- NULL 

# Eliminando valores nan
data_raw.clean <- na.omit(data_raw_train)


# ------------------------------- Aquí empieza el entrenamiento del modelo ------------------------------------------------
# Aquí se arman dos subconjuntos con los datos de cada una de las dos clases.
# Se pueden ver los respectivos tamaños al terminar, evidenciando un desbalance.
clean.data.YES <- data_raw.clean[data_raw.clean$Fuga == 'Fugado',]  
clean.data.NO <- data_raw.clean[data_raw.clean$Fuga == 'Vigente',]
#cat("Cantidad de ejemplos por clase\n")
#dim(clean.data.YES) 
#dim(clean.data.NO) 

#Hay que balancear
balance_ratio <- 1.2 # Se elige un balanceo de 20% más de ejemplos de la clase negativa que la positiva 

clean.subdata.YES <- clean.data.YES  # No se aplica sample(): se usan todos los ejemplos de la clase OK (que es la que tiene menos ejemplos)
clean.subdata.NO <- clean.data.NO[sample(nrow(clean.data.NO), balance_ratio*dim(clean.data.YES)[1]), ] 

# Muestra cantidad de ejemplos contenidos en cada subconjunto
#cat("Cantidad de ejemplos por clase luego del balance entre clases\n")
#dim(clean.subdata.YES)
#dim(clean.subdata.NO)

# Se juntan para el conjunto de referencia, ahora más balanceado
clean.subdata <- rbind(clean.subdata.YES, clean.subdata.NO)

#summary(clean.subdata)


# Primero, se saca una copia del dataset para trabajar sin modificar el original
# Esto permite hacer más modificaciones y correr este código varias veces sin alterar clean.subdata
working.data <- clean.subdata

# Ahora se configuran los conjuntos de entrenamiento y testing en una proporción
ratio = sample(1:nrow(working.data), size = 0.75*nrow(working.data)) 
training.data = working.data[ratio,]
testing.data = working.data[-ratio,] 

# Se comparan los tamaños de ejemplos para entrenamiento y evaluación.
#dim(training.data) 
#dim(testing.data)

#head(training.data)

# Decision Tree
# Se entrena con el DF original
DT_model <- rpart(as.factor(Fuga) ~ ., data=training.data, method="class", control = rpart.control(minsplit = 1))

#cat("\n****Desempeño Decision Tree en conjunto de evaluación****\n")
DT_predict_test <- predict(DT_model, testing.data, type = "class") # Esta lista guarda las predicciones
#confusionMatrix(DT_predict_test, as.factor(testing.data$Fuga), positive="Fugado") 
#rpart.plot(DT_model)


# ------------------------------------------------------ Aquí termina el modelo de predicción -----------------------

# ----------------------------------------------------- INICIO "for" para iterar con distintas tarifas y ver si cada cliente se queda o se va con este nuevo Cobro

for(i in 0:39) # El "for" va iterando por distintas tarifas (desde el 60% hasta el 99% de la tarifa actual)
  {
# Cada iteración debe empezar con la base de datos original
data <- data_raw

#Acá modifiqué con nueva tarifa
tarifa_nueva <- 93.67 * (0.6 + 0.01*i)
tarifa <- append(tarifa, tarifa_nueva) # Agrega la tarifa_nueva a la lista de tarifas

# Agrega nueva columna al DB original, con el nuevo Cobro por llamadas en horario normal, obtenido con la nueva tarifa
data$CobroNuevoNormal <- data$CobroLLamaHoraNormal # Primero se llena con los mismos datos del Cobro actual, y en la siguiente linea se reemplazan solamente los de los clientes que pagaban más de 23K (o hablaban más de 240 minutos)
data$CobroNuevoNormal[data$CobroLLamaHoraNormal > 23000] <- data$TotalMinHoraNormal[data$CobroLLamaHoraNormal > 23000] * tarifa_nueva 

# Eliminando valores nan
data.clean <- na.omit(data)


# Creo nueva base de datos para que prediga que pasa con estos nuevos precios
# Solo con los datos modificados
data_modif <- data.clean[data.clean$CobroLLamaHoraNormal > 23000,]

# Dejo el nuevo DF con las mismas columnas (nombres y posición) que la original, para mantener el código que viene después.
data_modif$CobroLLamaHoraNormal <- data_modif$CobroNuevoNormal
data_modif$CobroNuevoNormal <- NULL


# Saco la columna CobroNuevoNormal del DB data.clean
data.clean$CobroNuevoNormal <- NULL


# --------------------------- Inicio Predicciones del modelo con la tarifa nueva --------------------------------------
# Resultado modelo predictivo con DB modificada
DT_predict_modif <- predict(DT_model, data_modif, type = "class") # Esta lista guarda las predicciones

# ¿Cuántos se quedan y cuántos se van?
# Prediccón
summary(DT_predict_modif) 

# Cambiando la etiqueta "Fuga" de data_modif según el resultado del modelo
data_modif$Fuga <- DT_predict_modif

# Base de datos Original
# Se crea DB solo con los que pagaban más de 23000 (que fueron los modificados)
cobros_caros <- data.clean[data.clean$CobroLLamaHoraNormal > 23000,] 

# En PLATA:
# cobro_actual guarda la sumatoria de los Cobros de las personas VIGENTES con la tarifa actual (DB original)
cobro_actual <- sum(cobros_caros[cobros_caros$Fuga == 'Vigente',]$CobroLLamaHoraNormal)
# y se agrega a una lista
ganancia_actual <- append(ganancia_actual, cobro_actual)

# cobro_nuevo guarda la sumatoria de los Cobros de las personas VIGENTES con la tarifa NUEVA (DB modificado)
cobro_nuevo <- sum(data_modif[data_modif$Fuga == 'Vigente',]$CobroLLamaHoraNormal)
# y se agrega a una lista
ganancia_nueva <- append(ganancia_nueva, cobro_nuevo)

# Ganancia neta es lo que se gana con la NUEVA tarifa, menos lo que se gana con la tarifa ACTUAL
neto <- cobro_nuevo - cobro_actual
# y se agrega a una lista
ganancia_neta <- append(ganancia_neta, neto)


# número de fugados y vigentes en la prediccion, y el total de clientes estudiados (que debiera se constante)
fugados <- append(fugados, summary(DT_predict_modif)[1])
vigentes <- append(vigentes, summary(DT_predict_modif)[2])
total <- append(total, summary(DT_predict_modif)[1] + summary(DT_predict_modif)[2])


# Generación de DB de proporciones de Fugados / Vigentes para hacer los pie charts repsectivos
# Fugados
  for(i in 1:summary(DT_predict_modif)[1]){   #for desde 1 hasta el número de fugados
    num_tarifa <- append(num_tarifa, tarifa_nueva)
    condicion <- append(condicion, 'Fugado')
  }
# Vigentes
  for(i in (summary(DT_predict_modif)[1] + 1):(summary(DT_predict_modif)[1] + summary(DT_predict_modif)[2])){
    num_tarifa <- append(num_tarifa, tarifa_nueva)
    condicion <- append(condicion, 'Vigente')
  }
    
}

# Creando Df con los resultados, usando las listas creadas en el "for" anterior
resultado <- data.frame(unlist(tarifa), unlist(fugados), unlist(vigentes), unlist(total), unlist(ganancia_actual), unlist(ganancia_nueva), unlist(ganancia_neta))
names(resultado) <- c('Tarifa', 'Fugados', 'Vigentes', 'Total', 'Ganancia_actual', 'Ganancia_nueva', 'Ganancia_neta')

# Exportando a Excel
write_xlsx(resultado, "C:/Harold Personal/Company/Resultado.xlsx")

# Creando DF con la lista de cada Fugado y Vigente según tarifa, usando las listas creadas en el "for" anterior
fug_vig_tarifa <- data.frame(unlist(num_tarifa), unlist(condicion))
names(fug_vig_tarifa) <- c('Tarifa', 'Condicion')

# Exportando a Excel
write_xlsx(fug_vig_tarifa, "C:/Harold Personal/Company/Condicion_Tarifa.xlsx")
