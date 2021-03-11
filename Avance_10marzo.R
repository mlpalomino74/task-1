#Elaborado por: Monica Palomino, Paula Urbina, Nicolás Danies
#Colaborador: Todos los miembros!
#Fecha de elaboración:01 de marzo del 2021
#Fecha última modificación: 10 de marzo del 2021


#initial configuration
rm(list=ls())
install.packages("pacman")
pacman::p_load(tidyverse,haven,readxl,WriteXLS)

###Desarrollo del Taller###
#Punto 1
vector1=seq(1,100)
vector2=seq(1,100,2)

#Punto2
list.files("data/input/")
#Cargarmos base de datos
cultivos= read_xlsx("data/input/cultivos.xlsx")

#Punto 3 GEIH
#Punto 3.1 Importar
list.files("data/input/2019")
carac_generales=readRDS('data/input/2019/Cabecera - Caracteristicas generales (Personas).rds')
ocupados=readRDS('data/input/2019/Cabecera - Ocupados.rds')
view(carac_generales)
view(ocupados)

base_unida <- merge(ocupados, carac_generales,
                by = c("directorio", "secuencia_p", "orden"),
                all = TRUE)
view(base_unida)

#Punto 3.2
pacman::p_load(tidyverse,viridis,forcats,gapminder)


