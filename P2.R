#Sesión 2
#21 septiembre de 2022

#Directorio de trabajo ----
setwd("C:/Users/molar/Dropbox/2022_Trabajos/CONEVAL/Curso/proyecto")

#Paquetería ####
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, haven, sjlabelled, foreign, openxlsx)

#Importación de datos ####
#Desde y hacia excel
#Importar
ejemploxl <- readxl::read_excel("datos/ejemplo_xlsx.xlsx", sheet= "para_importar")
#Exportar
writexl::write_xlsx(ejemploxl, path="datos/Mi_Exportación.xlsx")
openxlsx::write.xlsx(ejemploxl, file="datos/miexportacion.xlsx")

#Desde y hacia Stata
#Importar
concentrado2020 <- haven::read_dta("datos/concentrado2020.dta")
#Exportar
haven::write_dta(concentrado2020, "datos/mi_exportacion.dta", version=13)

#Desde y hacia SPSS
#Importar
encevi_hogar <- haven::read_sav("datos/encevi_hogar.sav")
#Exportar
haven::write_sav(encevi_hogar, "datos/mi_exportación.sav")

#Ver nuestros datos
class(concentrado2020)
class(concentrado2020$edad_jefe)
#numeric
#character
#integer
#factor

names(concentrado2020) #veo los nombres de las variables

concentrado2020<- concentrado2020 %>% 
  rename(integrantes=tot_integ)

install.packages("tidyverse")
library(tidyverse)

#Exploramos nuestros datos ####           
head(concentrado2020)

class(concentrado2020$clase_hog)
concentrado2020$clase_hog<- as.character(concentrado2020$clase_hog)

concentrado2020 <- concentrado2020 %>% 
  arrange(folioviv)

tabla1<- table(concentrado2020$clase_hog)
tabla2<-prop.table(table(concentrado2020$clase_hog))


concentrado2020 %>% 
  dplyr::arrange(edad_jefe) %>% 
  dplyr::select(sexo_jefe, edad_jefe) %>% 
  head

concentrado2020 %>% 
  dplyr::select(sexo_jefe, edad_jefe) %>% 
  glimpse

str(concentrado2020)
dim(concentrado2020)

#Etiquetas ####
class(concentrado2020$sexo_jefe)

etiqueta_sexo <- c("Hombre", "Mujer")

concentrado2020 <- concentrado2020 %>% 
  mutate(sexo=as.numeric(sexo_jefe)) %>% 
  sjlabelled::set_labels(sexo, labels=etiqueta_sexo)

table(concentrado2020$sexo)
table(sjlabelled::as_label(concentrado2020$sexo))

print(get_labels(concentrado2020))
print(get_labels(concentrado2020$clase_hog))
print(get_labels(concentrado2020[,20:30]))

#no tienen
print(get_label(concentrado2020))
print(get_label(concentrado2020[,20:30]))
print(get_label(concentrado2020$sexo_jefe))


#Selección de casos y de variables
x<-concentrado2020$ing_cor
x<-concentrado2020[["ing_cor"]]
x<-concentrado2020[,23]
x<-concentrado2020[,"ing_cor"]

x <- concentrado2020 %>% 
  select(ing_cor)

x<- concentrado2020 %>% 
  select(-ing_cor)

rm(x)

concentrado2020$aproba2 <- concentrado2020$ing_cor
concentrado2020$aproba2 <- NULL

subset1 <- concentrado2020[concentrado2020$ing_cor>5000,]
subset2 <- concentrado2020[,c("sexo_jefe", "edad_jefe")]
subset3 <- concentrado2020[concentrado2020$ing_cor>5000,c("sexo_jefe", "edad_jefe", "ing_cor")]

subset4 <- concentrado2020 %>% 
  dplyr::filter(ing_cor>5000 & sexo_jefe==1) %>% 
  dplyr::select(ing_cor, sexo_jefe, edad_jefe)
