#Sesión 2
#26 septiembre de 2022

#Directorio de trabajo ----
setwd("C:/Users/molar/Dropbox/2022_Trabajos/CONEVAL/Curso/proyecto")

#Paquetería ####
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, haven, sjlabelled, foreign, openxlsx, janitor, writexl)

#Importar ####
concentrado2020 <- haven::read_dta("datos/concentrado2020.dta")

#Leer desde archivos de texto y desde una url
mig_inter_quin_proyecciones <- read.csv("http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/mig_inter_quin_proyecciones.csv", encoding="latin1")
View(mig_inter_quin_proyecciones)
names(mig_inter_quin_proyecciones)

#cuando es foreign: era read.csv
#ahora en havan: es read_csv

#Podemos contar
concentrado2020 %>% 
  dplyr::count(sexo_jefe==2) # cuentan los casos que cumplen con la condición "sexo_jefe==2"

#Podemos hacer una tabla
concentrado2020 %>% 
  filter(edad_jefe>50) %>% 
  with(
    table(sexo_jefe)
  )

#Muy importante etiquetar ####
etiqueta_sex <- c("Hombre", "Mujer")
concentrado2020 <- concentrado2020 %>% 
  mutate(sexo_jefe=as.numeric(sexo_jefe)) %>% #para quitar el string
  sjlabelled::set_labels(sexo_jefe, labels=etiqueta_sex)

#equitar clase hogar
concentrado2020<-concentrado2020 %>% 
  mutate(clase_hog=as_numeric(clase_hog)) %>% # para quitar el "string"
  sjlabelled::set_labels(clase_hog, labels=c("unipersonal",
                                             "nuclear", 
                                             "ampliado",
                                             "compuesto",
                                             "corresidente"))

## Tabulados más bonitos ----
concentrado2020 %>%
  dplyr::mutate(sexo_jefe=as_label(sexo_jefe))  %>%
  janitor::tabyl(sexo_jefe) %>% 
  janitor::adorn_totals()

## Diferentes tipos de variables

class(concentrado2020$sexo_jefe) # variable sin etiqueta
class(as_label(concentrado2020$sexo_jefe)) # variable con etiqueta

class(as_label(concentrado2020$educa_jefe)) # variable ordinal
class(concentrado2020$ing_cor) # variable de intervalo/razón


dplyr::glimpse(concentrado2020$sexo_jefe)


concentrado2020 %>% mutate(sexo_jefe=as_label(sexo_jefe)) %>% # cambia los valores de la variable a sus etiquetas
  janitor::tabyl(sexo_jefe) %>% # para hacer la tabla
  adorn_totals() %>% # añade totales
  adorn_pct_formatting(digits=3)  # nos da porcentaje 

glimpse(concentrado2020$educa_jefe)

# Etiquetemos también nuestra variable ordinal

concentrado2020 <-concentrado2020 %>% 
  mutate(educa_jefe=as.numeric(educa_jefe)) %>% 
  set_labels(educa_jefe,
             labels=c("Sin instrucción", 
                      "Preescolar",
                      "Primaria incompleta",
                      "Primaria completa",
                      "Secundaria incompleta",
                      "Secundaria completa",
                      "Preparatoria incompleta",
                      "Preparatoria completa",
                      "Profesional incompleta",
                      "Profesional completa",
                      "Posgrado"))


# Hoy hacemos la tabla, con las etiquetas y vemos que se ve más bonita:

concentrado2020 %>%
  mutate(educa_jefe=as_label(educa_jefe)) %>% 
  tabyl(educa_jefe)

# Para que no nos salgan las categorías sin datos podemos poner una opción dentro del comando "tabyl()"

tabla2<- concentrado2020 %>% 
  filter(educa_jefe>5) %>% 
  mutate(educa_jefe=as_label(educa_jefe)) %>% 
  tabyl(educa_jefe, show_missing_levels=F ) %>% # esta opción elimina los valores con 0
  adorn_totals()  


## Bivariado cualitativo

### Cálculo de frecuencias

# Las tablas de doble entrada tiene su nombre porque en las columnas entran
# los valores de una variable categórica, y en las filas de una segunda.
# Basicamente es como hacer un conteo de todas las combinaciones posibles
# entre los valores de una variable con la otra.

# Por ejemplo, si quisiéramos combinar las dos variables que ya estudiamos
# lo podemos hacer, con una tabla de doble entrada:

tabla1 <- concentrado2020 %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels=F ) %>% # incluimos aquí 
  adorn_totals(c("col", "row"))  

lista<- list(tabla1, tabla2)
writexl::write_xlsx(lista, path="datos/tabulado.xlsx")


### Totales y porcentajes ----

# De esta manera se colocan todos los datos. 
# Si observa al poner la función "adorn_totals()" lo agregó como una nueva fila de totales, 
# pero también podemos pedirle que agregue una columna de totales.

concentrado2020 %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels=F ) %>% # incluimos aquí dos variables
  adorn_totals("col")  

# O bien agregar los dos, introduciendo en el argumento "c("col", "row")"
# un vector de caracteres de las dos opciones requeridas:

concentrado2020 %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels=F ) %>% # incluimos aquí dos variable
  adorn_totals(c("col", "row")) 

# Del mismo modo, podemos calcular los porcentajes. 
# Pero los podemos calcular de tres formas.
# Uno es que lo calculemos para los totales calculados para las filas, 
# para las columnas o para el gran total poblacional.

# Para columnas tenemos el siguiente código y los siguientes resultados:

concentrado2020 %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels=F ) %>% # incluimos aquí dos variable
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("col") %>% # Divide los valores entre el total de la columna
  adorn_pct_formatting() # lo vuelve porcentaje

# Cuando se hagan cuadros de distribuciones (que todas sus partes suman 100), 
# los porcentajes pueden ser una gran ayuda para la interpretación,
# sobre todos cuando se comparar poblaciones de categorías de diferente tamaño. 
# Por lo general, queremos que los cuadros nos den información de donde están los totales y su 100%, 
# de esta manera el lector se puede guiar de porcentaje con respecto a qué está leyendo.
# En este caso, vemos que el 100% es común en la última fila.

# Veamos la diferencia de cómo podemos leer la misma celda, pero hoy,
# hemos calculado los porcentajes a nivel de fila:
#   

concentrado2020 %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels=F ) %>% # incluimos aquí dos variable
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("row") %>% # Divide los valores entre el total de la fila
  adorn_pct_formatting() # lo vuelve porcentaje

# Finalmente, podemos calcular los porcentajes con referencia a la población total en análisis.
# Es decir la celda en la esquina inferior derecha de nuestra tabla original.
concentrado2020 %>% 
  mutate(clase_hog=as_label(clase_hog)) %>% 
  mutate(sexo_jefe=as_label(sexo_jefe)) %>% # para que las lea como factor
  tabyl(clase_hog, sexo_jefe, show_missing_levels=F ) %>% # incluimos aquí dos variable
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("all") %>% # Divide los valores entre el total de la población
  adorn_pct_formatting() # lo vuelve porcentaje

concentrado2020 %>% 
  group_by(as_label(sexo_jefe), as_label(clase_hogar)) %>% 
  dplyr::summarise(media_ingresos=mean(ing_cor, na.rm=T),
                   sd_ingresos=sd(ing_cor, na.rm=T),
                   meda_ind=mean(ing_ind, na.rm=T)) %>% 
  writexl::write_xlsx(path="resumen.xlsx")


## Descriptivos para variables cuantitativas

# Vamos a empezar a revisar los gráficos para variables cuantitativas.

### Medidas numéricas básicas

# 5 números

summary(concentrado2020$ing_cor) ## ingresos

# Con pipes se pueden crear "indicadores" de nuestras variables es un tibble

concentrado2020 %>% 
  summarise(nombre_indicador=mean(ing_cor, na.rm=T))

### Histograma básico

hist(concentrado2020$ing_cor)

# Le podemos modificar el título del eje de las x y de las y

hist(concentrado2020$ing_cor, 
     main="Histograma de los ingresos corrientes", 
     xlab="pesos mexicanos", ylab="Frecuencia") 

# ¡A ponerle colorcitos! Aquí hay una lista <http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf>

hist(concentrado2020$ing_cor, 
     main="Histograma de los ingresos corrientes", 
     xlab="pesos mexicanos", ylab="Frecuencia",
     col="deeppink1") 


#Con pipes:

concentrado2020 %>% 
  with(hist(ing_cor)) # con with, para que entienda

# Cuando usamos pipes, se debe de recordar que no es necesario escribir el nombre del data.frame en el filtro porque es lo primero que colocamos en nuestro "pipe".
# 
# Checa que cualquier aditamiento debe ir en el pipe donde está el comando de hist(). Ten cuidado con los paréntesis.

concentrado2020 %>% 
  select(starts_with("folio"), ends_with("hog"))

concentrado2020 %>% 
  select(contains("ing"))
