#Elaborado por: Monica Palomino, Paula Urbina, Nicolás Danies
#Colaborador: Todos los miembros!
#Fecha de elaboración:01 de marzo del 2021
#Fecha última modificación: 10 de marzo del 2021


#initial configuration
rm(list=ls())
install.packages("pacman")
pacman::p_load(tidyverse,haven,readxl,WriteXLS)

###Desarrollo del Taller###
###Hemos escogido desarollar el Taller A

#Punto 1:
###creacion vector 1 a 100
vector1=seq(1,100)
###creacion vector con numeros impacres
vector2=seq(1,100,2)
###unir con vector2 con los pares del vector1
"%ni%" = Negate("%in%")
vector3=which(vector1 %ni% vector2)  ##creamos un vector 3 a partir del vector 1 extrayendo los impares del vector 2
vector4=sort( c (vector2,vector3))
#### El vector 4 responde alúltimo requerimiento del enunciado del punto. 


#Punto2
list.files("data/input/")
#Importar Base de Datos "cultivos"
cultivos= read_xlsx("data/input/cultivos.xlsx")
#limpiar observaciones que no tienen información relevante
install.packages("data.table")

####OJO esto falta verificar!
cultivos = head(cultivos, -2)
# View (datos)

long = melt(setDT(cultivos),
             id.vars = c("CODDEPTO", "DEPARTAMENTO", "CODMPIO",	"MUNICIPIO")
             , variable.name = "year")
long = na.omit(long, c("value", "CODMPIO"))
View(long)
####OJO hasta aquí verifica!

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


###Histogramas para ver distribucion muestra

####¿como esta distribuida la muestra en las horas de trabajo semanales?
ggplot() +
  geom_histogram( data = base_unida, aes(x=P6800),bins=8) +
  ggtitle('Histograma Horas Trabajo Semanales')
####Se observa que la muestra se concentra entre 25 y 60 hrs trabajadas semanalmente. 

####¿cuál fue la distirbución de salarios antes de descuentos?
ggplot() +
  geom_histogram( data = base_unida, aes(x=P6500),bins=5) +
  ggtitle('Salario promedio antes de descuentos')


###pendiente gráficos##
