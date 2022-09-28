#Directorio de trabajo ----
setwd("C:/Users/molar/Dropbox/2022_Trabajos/CONEVAL/Curso/proyecto")

#Paquetería ####
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, haven, sjlabelled, foreign, janitor, esquisse)

#Importar ####
concentrado2020 <- haven::read_dta("datos/concentrado2020.dta")

#Variables cualitativas ####

#Primero vamos a hacer nuestro primer gráfico de barras o columnas
#Al igual que en la sesión anterior, vamos agregando capas a nuestros gráficos

#Datos: incluiremos la variable que queremos graficar, en este caso: el sexo del jefe del hogar
concentrado2020 %>% 
  ggplot(aes(x=sexo_jefe))

#Ahora agregaremos la geometría
concentrado2020 %>% 
  ggplot(aes(x=sexo_jefe)) +
  geom_bar()

#Podemos voltear la geometría
concentrado2020 %>% 
  ggplot(aes(x=sexo_jefe)) +
  geom_bar()+
  coord_flip()

#Aunque, se ve mejor como estaba antes.

#Para hacer nuestra gráfica de barras y que se vean las etiquetas con facilidad, convertiremos nuestra variable sexo_jefe en factor
concentrado2020 <- concentrado2020 %>% 
  mutate(sexo_factor = factor(concentrado2020$sexo_jefe, labels = c("Hombre", "Mujer")))

#Ahora sí podemos ver las etiquetas en nuestro gráfico
concentrado2020 %>% 
  ggplot(aes(x=sexo_factor)) +
  geom_bar()

#Vamos a ponerle un color a hombre y otro a mujer y a quitar el fondo gris
concentrado2020 %>% 
  ggplot(aes(x = sexo_factor, fill = sexo_factor)) +
  geom_bar() +
  scale_fill_manual(
    values = c(Hombre = "#440154",
               Mujer = "#3EB0F2")
  ) +
  theme_classic()
  
  
#Pero, resulta que no queremos el conteo, sino los porcentajes
concentrado2020 %>% 
  count(sexo_factor) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = sexo_factor, y = pct, fill = sexo_factor, label = scales::percent(pct))) +
  geom_col(position = 'dodge') + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(Hombre = "#440154",
               Mujer = "#3EB0F2")) +
  theme_classic()

#Ahora, le agregamos las etiquetas de los datos
concentrado2020 %>% 
  count(sexo_factor) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = sexo_factor, y = pct, fill = sexo_factor, label = scales::percent(pct))) +
  geom_col(position = 'dodge') + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(Hombre = "#440154",
               Mujer = "#3EB0F2")) +
  theme_classic()+
geom_text(position = position_dodge(width = .9),    
          vjust = -0.5,    
          size = 3,
          colour = "black")

#Modificamos los títulos y ejes
concentrado2020 %>% 
  count(sexo_factor) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = sexo_factor, y = pct, fill = sexo_factor, label = scales::percent(pct))) +
  geom_col(position = 'dodge') + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(Hombre = "#440154",
               Mujer = "#3EB0F2")) +
  theme_classic()+
  geom_text(position = position_dodge(width = .9),    
            vjust = -0.5,    
            size = 3,
            colour = "black") +
  labs(
    x = "Sexo del jefe del hogar",
    y = "Porcentaje",
    title = "Sexo de los jefes y jefas de hogar en México, 2020",
    caption = "Fuente: elaboración propia con base en ENIGH 2020",
    fill = "Sexo del jefe del hogar"
  )

#Modificamos el estilo del título y la fuente
concentrado2020 %>% 
  count(sexo_factor) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = sexo_factor, y = pct, fill = sexo_factor, label = scales::percent(pct))) +
  geom_col(position = 'dodge') + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(Hombre = "#440154",
               Mujer = "#3EB0F2")) +
  theme_classic()+
  geom_text(position = position_dodge(width = .9),    
            vjust = -0.5,    
            size = 3,
            colour = "black") +
  labs(
    x = "Sexo del jefe del hogar",
    y = "Porcentaje",
    title = "Sexo de los jefes y jefas de hogar en México, 2020",
    caption = "Fuente: elaboración propia con base en ENIGH 2020",
    fill = "Sexo del jefe del hogar"
  ) +
  theme(
    plot.title = element_text(color = "darkgreen", size = 14, face = "bold"),
    plot.caption = element_text(face = "italic")
  )


#Práctica, escoge una variable cualitativa y elabora tu gráfico.

