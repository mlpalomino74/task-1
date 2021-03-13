#Elaborado por: Monica Palomino 200611835, Paula Urbina 201633091, Nicolás Danies 201822710
#Colaborador: Todos los miembros!
#Fecha de elaboración:01 de marzo del 2021
#Fecha última modificación: 10 de marzo del 2021


#initial configuration
rm(list=ls())
install.packages("pacman")
install.packages("data.table")
pacman::p_load(tidyverse,haven,readxl,WriteXLS,data.table)

setwd("/Users/purbina/Documents/uni/9S/Taller de R/task-1-main")
getwd()

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
#Le incluimos skip 8 para que no incluya las primeras 8 filas

cultivos = head(cultivos, -2)
View (cultivos)

install.packages("reshape")
install.packages("data.table")

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
View(carac_generales)
View(ocupados)


##Verificamos que no hayan observaciones duplicadas 
duplicated(carac_generales)
duplicated(ocupados)

base_unida = full_join(ocupados, carac_generales,
                by = c("directorio", "secuencia_p", "orden"),
                all = TRUE)
View(base_unida)


#Punto 3.2
###cargamos librerias necesarias para graficar
pacman::p_load(tidyverse,viridis,forcats,gapminder)

###cargamos bases de datos de Desocupados, Fuerza de Trabajo e Inactivos
desoc_cabe=readRDS('data/input/2019/Cabecera - desocupados.rds')
desoc_resto=readRDS('data/input/2019/Resto - desocupados.rds')
ft_cabe=readRDS('data/input/2019/Cabecera - Fuerza de trabajo.rds')
inac_cabe=readRDS('data/input/2019/Cabecera - Inactivos.rds')
ocupados_resto=readRDS('data/input/2019/Resto - Ocupados.rds')


#### Análisis para Ocupados Cabecera y Rural:

####  Cantidad de personas Ocupadas por Departamento
sum(ocupados$fex_c_2011)##total de ocupados urbanos  
ocupados %>% group_by(dpto) %>% summarize(total = sum(fex_c_2011)) 
table_OU=ocupados %>% group_by(dpto) %>% summarize(total = sum(fex_c_2011))
table_OU ##tabla de Ocupados Urbanos por dpto

sum(ocupados_resto$fex_c_2011) ##total de ocupados rurales
ocupados_resto %>% group_by(P388) %>% summarize(total = sum(fex_c_2011)) 
table_OR=ocupados_resto %>% group_by(P388) %>% summarize(total = sum(fex_c_2011)) 
table_OR ##tabla de Ocupados Rurales por dpto
plot(table_OR)

#### Análisis para Desocupados Cabecera y Rural:

#### Cantidad de personas Decupadas por Departamento
sum(desoc_cabe$fex_c_2011)##total de desocupados urbanos  
desoc_cabe %>% group_by(dpto) %>% summarize(total = sum(fex_c_2011)) 
table_DU=desoc_cabe %>% group_by(dpto) %>% summarize(total = sum(fex_c_2011)) 
table_DU ##tabla de Desocupados Urbanos por dpto

sum(desoc_resto$fex_c_2011)##total de desocupados rurales 
desoc_resto %>% group_by(DPTO) %>% summarize(total = sum(fex_c_2011)) 
table_DR=desoc_resto %>% group_by(DPTO) %>% summarize(total = sum(fex_c_2011)) 
table_DR ##tabla de Desocupados Rurales por dpto


#### Ingresos laborales promedio

###Ingresos laborales promedio por departamento
ocupados$P6500[is.na(ocupados$P6500)] = 0
mean(ocupados$P6500)##promedio de p6500 ingresos antes de descuentos



###Histogramas para ver distribucion

#### Histograma1: ¿como esta distribuida la base "base_unida" en las horas de trabajo semanales?
ggplot() +
  geom_histogram( data = base_unida, aes(x=P6800),colour="blue", fill="blue") +
  ggtitle('Histograma Horas Trabajo Semanales')

#### teniendo encuenta la distribucion observada se toma un numero de bins que agrupe la muestra: 
ggplot() +
  geom_histogram( data = base_unida, aes(x=P6800),colour="blue", fill="blue", bins=8) +
  ggtitle('Histograma Horas Trabajo Semanales')

####este grafico es mas facil de analizar donde se observa que la muestra se concentra entre 25 y 60 hrs trabajadas semanalmente. 

###lo vamos a guardar como hist1 y le ponemos nombres a los ejes
hist1=ggplot() +
  geom_histogram( data = base_unida, aes(x=P6800),colour="blue", fill="blue", bins=8) +
  ggtitle('Histograma Horas Trabajo Semanales') + ylab("Frecuencia")+xlab("Horas trabajo semanales") + theme_minimal()

###exportamos a la carpeta views
ggsave(plot= hist1 , file = "views/Distribución Horas de trabajo semanales.jpeg")

#### distirbución de la base_unida por edad
ggplot() +
  geom_histogram( data = base_unida, aes(x=P6040)) +
  ggtitle('Distribución por Edad')

##nuevamente, escogemos número d ebins para analizar mejor la grafica
hist2=ggplot() +
  geom_histogram( data = base_unida, aes(x=P6040),colour="blue", fill="blue", bins=20) +
  ggtitle('Distribución Muestra por edad') + ylab("Frecuencia")+xlab("Edad") + theme_minimal()

hist2

###exportamos a la carpeta views
ggsave(plot= hist2 , file = "views/Distribución muestra por edad.jpeg")


#####Distribución de Ocupados Rurales por horas de trabajo
ggplot() +
  geom_histogram( data = ocupados_resto, aes(x=P6800),colour="blue", fill="blue", bins=8) +
  ggtitle('Histograma Horas Trabajo Semanales_Rurales') + ylab("Frecuencia")+xlab("Horas trabajo semanales") + theme_minimal()
### Se observa que las horas trabajadas de los ocupados rurales se concentran entre 40 y 50 horas.

hist3=ggplot() +
  geom_histogram( data = ocupados_resto, aes(x=P6800),colour="blue", fill="blue", bins=8) +
  ggtitle('Histograma Horas Trabajo Semanales_Rurales') + ylab("Frecuencia")+xlab("Horas trabajo semanales") + theme_minimal()
hist3

ggsave(plot= hist3 , file = "views/Histograma_Ocupados_Rurales por horas de trabajo.jpeg")


#####Distribución de Ocupados Rurales por ingresos antes de descuento
ggplot() +
  geom_histogram( data = ocupados_resto, aes(x=P6500),colour="blue", fill="blue", bins=30) +
  ggtitle('Histograma Ingresos Ocupados Rurales') + ylab("Frecuencia")+xlab("Ingresos antes de descuentos") + theme_minimal()
### Se observa que los ingresos de los ocupados rurales se concentran por debajo de $2,5 millones mensuales

hist4=ggplot() +
  geom_histogram( data = ocupados_resto, aes(x=P6500),colour="blue", fill="blue", bins=30) +
  ggtitle('Histograma Ingresos Ocupados Rurales') + ylab("Frecuencia")+xlab("Ingresos antes de descuentos") + theme_minimal()

ggsave(plot= hist4 , file = "views/Histograma_Ingresos_Ocupados_Rurales.jpeg")


#####Distribución desocupados urbanos por semanas buscando trabajo
hist5=ggplot() +
  geom_histogram( data = desoc_cabe, aes(x=P7250),colour="blue", fill="blue", bins=5) +
  ggtitle('Histograma Semanas Buscando Trabajo_Desocupados Urbanos') + ylab("Frecuencia")+xlab("Semanas") + theme_minimal()
### Se observa que los desocupados urbanos han estado buscando trabajo, en su gran mayoría, entre 0 y 200 semanas.

hist5

ggsave(plot= hist5 , file = "views/Histograma_Ingresos_desocupados_urbanos.jpeg")


#####Distribución desocupados rurales por semanas buscando trabajo
hist6= ggplot() +
  geom_histogram( data = desoc_resto, aes(x=P7250),colour="red", fill="red", bins=5) +
  ggtitle('Histograma Semanas Buscando Trabajo_Desocupados rurales') + ylab("Frecuencia")+xlab("Semanas") + theme_minimal()
### Se observa que los desocupados urbanos han estado buscando trabajo, en su gran mayoría, entre 0 y 200 semanas.

hist6

ggsave(plot= hist6 , file = "views/Histograma_Ingresos_desocupados_rurales.jpeg")


