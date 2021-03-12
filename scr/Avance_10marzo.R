#Elaborado por: Monica Palomino 200611835, Paula Urbina 201633091, Nicolás Danies
#Colaborador: Todos los miembros!
#Fecha de elaboración:01 de marzo del 2021
#Fecha última modificación: 10 de marzo del 2021


#initial configuration
rm(list=ls())
install.packages("pacman")
install.packages("data.table")
pacman::p_load(tidyverse,haven,readxl,WriteXLS,data.table)


###Desarrollo del Taller###
###Hemos escogido desarollar el Taller A

#Punto 1:
###creacion vector 1 a 100
vector1=seq(1,100)
vector1
###creacion vector con numeros impares
vector2=seq(1,100,2)
vector2
###unir el vector2 con los pares del vector1
"%ni%" = Negate("%in%")
vector3=which(vector1 %ni% vector2)  ##creamos un vector 3 a partir del vector 1 extrayendo los impares del vector 2
vector4=sort( c (vector2,vector3))
vector4
#### El vector 4 responde al último requerimiento del enunciado de este punto. 


#Punto2
list.files("data/input/")
#Importar Base de Datos "cultivos"
cultivos= read_excel("data/input/cultivos.xlsx", skip = 8)
#limpiar observaciones que no tienen información relevante

cultivos = head(cultivos, -2)
View (cultivos)

long = melt(setDT(cultivos),
             id.vars = c("CODDEPTO", "DEPARTAMENTO", "CODMPIO",	"MUNICIPIO")
             , variable.name = "year")

long = na.omit(long, c("value", "CODMPIO"))
View(long)


#Punto 3 GEIH
#Punto 3.1 Importar
list.files("data/input/2019")
carac_generales=readRDS('data/input/2019/Cabecera - Caracteristicas generales (Personas).rds')
ocupados=readRDS('data/input/2019/Cabecera - Ocupados.rds')
view(carac_generales)
view(ocupados)

##Verificamos que no hayan observaciones duplicadas 
duplicated(carac_generales)
duplicated(ocupados)

base_unida = full_join(ocupados, carac_generales,
                by = c("directorio", "secuencia_p", "orden"),
                all = TRUE)
view(base_unida)


#Punto 3.2
###cargamos librerias necesarias para graficar
pacman::p_load(tidyverse,viridis,forcats,gapminder)


#### Estadisticas descriptivas
summarise(base_unida)
group_by(base_unida,P6020,.add=TRUE)

###Histogramas para ver distribucion

#### Histograma1: ¿como esta distribuida la muestra en las horas de trabajo semanales?
ggplot() +
  geom_histogram( data = base_unida, aes(x=P6800),colour="blue", fill="blue") +
  ggtitle('Histograma Horas Trabajo Semanales')
#### teniendo encuenta la distribucion observad ase toma un numero de bins que agrupe la muestra 

ggplot() +
  geom_histogram( data = base_unida, aes(x=P6800),colour="blue", fill="blue", bins=8) +
  ggtitle('Histograma Horas Trabajo Semanales')
####este grafico es mas facil de analizar donde se observa que la muestra se concentra entre 25 y 60 hrs trabajadas semanalmente. 

###lo vamos a guardar como Hist1 y le ponemos nombres a los ejes
Hist1=ggplot() +
  geom_histogram( data = base_unida, aes(x=P6800),colour="blue", fill="blue", bins=8) +
  ggtitle('Histograma Horas Trabajo Semanales') + ylab("Frecuencia")+xlab("Horas trabajo semanales") + theme_minimal()

ggsave(plot= Hist1 , file = "views/Distribución Horas de trabajo semanales.jpeg")

#### distirbución de la muestra por edad
ggplot() +
  geom_histogram( data = base_unida, aes(x=P6040)) +
  ggtitle('Distribución por Edad')

Hist2=Hist1=ggplot() +
  geom_histogram( data = base_unida, aes(x=P6040),colour="blue", fill="blue", bins=20) +
  ggtitle('Distribución Muestra por edad') + ylab("Frecuencia")+xlab("Edad") + theme_minimal()

ggsave(plot= Hist2 , file = "views/Distribución muestra por edad.jpeg")


####distribución por año, sexo, urbano/rural



