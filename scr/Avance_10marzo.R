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

##ojo esto no esta corriendo: 
table_Ocup_dpto = full_join(x = table_OU , y = table_OR , by = c('dpto','P388') , suffixes = c('O_urbanos','O_ruralesl')) 




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

###ojo esto no esta corriendo

table_DR <- table_DR %>% rename(DPTO = dpto)

table_Desoc_dpto = full_join(x = table_DR , y = table_DU , by = NULL , suffix = c('D_rurales','O_urbanos')) 
View(table_Desoc_dpto)

rlang::last_error()


#### Ingresos laborales promedio

###Ingresos laborales promedio por departamento
ocupados$P6500[is.na(ocupados$P6500)] = 0
mean(ocupados$P6500)##promedio de p6500 ingresos antes de descuentos





ggplot(data=ocupados, aes(x=ocupados$dpto,y=sum(ocupados$fex_c_2011)))

ocupados %>% group_by(dpto) %>% summarize(total = sum(fex_c_2011)) %>% 
  ggplot(data = ocupados, aes(x=dpto, y=total)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() + xlab('') + ylab("Cantidad de personas") + theme_bw()



#### 1.2.1. Cantidad de personas
geih %>% group_by(dpto) %>% summarize(total = sum(fex_c_2011)) %>% 
  ggplot(data = ., aes(x=dpto, y=total)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() + xlab('') + ylab("Cantidad de personas") + theme_bw()




ocupados %>% group_by(dpto) %>% summarize(total = sum(directorio)) %>% 
  ggplot(data = ocupados, aes(x=dpto, y=total)) 

ocupados %>% group_by(dpto) %>% summarize(total = count(directorio)) %>% 
ggplot(data=ocupados,mapping=aes(x=dpto,y=total))

+
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() + xlab('') + ylab("Cantidad de personas") + theme_bw()


  ocupados %>% group_by(dpto, Oficio) 



+
  ggplot(data = ocupados, aes(x=dpto, y=total)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() + xlab('') + ylab("Cantidad de personas") + theme_bw()






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

##Tablas
View(ocupados)
ocupados.resto=readRDS('data/input/2019/Resto - Ocupados.rds')



