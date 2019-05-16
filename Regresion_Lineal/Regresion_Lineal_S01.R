# Remove all the variables in the workspace
rm(list = ls())
# Clear console
cat("\014")

#PAQUETES INSTALADOS!
# Comment this lines after intalling the packages
# install.packages("ggplot2")
# install.packages("rstudioapi")
# install.packages("caret")
# install.packages("lattice")

# Load ggplot2 library
library(ggplot2)
library(caret)
library(lattice)

# creation of DATA.FRAME:
myData <- read.csv("Baseball.csv", header=TRUE, sep=",")#Obtenemos matriz del fichero

#Ejemplo del problema:?
print(head(myData))

#Calculamos correlacion entre todos:
print(cor(myData))

#Lista de folds:
folds <- list()

#Error medio total:
errorMedioTotal <- 0

#Creamos repeticiones de folds para columnas aleatorias de "data": 5 folds!
folds <- createMultiFolds(y=myData$dureza, k=5, times=1)

for (i in 1:length(folds))
{
  #Para training.data se crearan listas de vectores con valores aleatorios a partirc
  #de unos indices de los folds (los que no se coloquen en taining.data los almacenaremos
  #en test.data:
  #(Ejemplo: "myData$cemento" + "myData$desecho" + "myData$ceniza": en training.data)
  training.data <- myData[folds[[i]],]
  
  #Los restantes los colocamos en "test.data":
  #(Ejemplo: todos los restantes - ("myData$cemento" + "myData$desecho" + "myData$ceniza"): en test.data)
  test.data <- myData[-folds[[i]],]
  
  #Creamos el modelo linea:
  linealModel <- lm(formula = dureza ~ cemento + desecho + ceniza + agua + superplastico + grosor + finura + anyo, data = training.data )
  #Printeamos salida:
  print(summary(linealModel))
  
  #Creation of prediction del "test.data":
  predictLinealModelTest <- predict(object=linealModel, data = test.data)
  
  #Calculamos el error medio para cada modelo:
  errorMedio <-  mean(abs(predictLinealModelTest - test.data$dureza))
  errorMedioTotal <- errorMedioTotal + errorMedio
}

print(paste0(": ",errorMedioTotal/length(folds)))

