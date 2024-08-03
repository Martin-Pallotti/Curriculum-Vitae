#Empiezo definiendo el seteo inicial de mi script

#Librerías a utilizar:

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)


#shortcut por paste characters
"%+%" <- function(x,y) paste(x,y,sep = "")    

#defino el path
root <- "C:/Users/marto/OneDrive/Documentos/Maestría/2024/Primer cuatrimestre/Instrumentos Computacionales/Parcial"

#Defino el directorio donde tengo la data
data_dir <- root %+% "/data/"

# ---- Ejercicio 1 ----

## Inciso a ----

#Cargo la base pura de Vivienvas
base_pura_v <- read.csv(data_dir %+% "Ejercicio 1/Viviendas/viviendas.csv", sep =",", dec = ",")  

#Selecciono solo aquellas columnas que quiero y creo un nuevo df
b_viv <- base_pura_v %>%
  select(c("folioviv":"renta", "ubica_geo","factor"))


## Inciso b ----

#Descomprimo el archivo
unzip(data_dir %+% "/Ejercicio 1/Sociodemográficas/enigh2016_ns_poblacion_csv.zip", 
      exdir = data_dir %+% "Ejercicio 1/Sociodemográficas")

base_pura_i <- read.csv(data_dir %+% "Ejercicio 1/Sociodemográficas/poblacion.csv", sep=",", dec=",")

#Selecciono sólo aquellas columnas que quiero y creo un nuevo df
b_ind <- base_pura_i %>%
          select(1:45)

## Inciso c ----

#El atributo que relaciona entre sí las bases de viviendas y personas es folioviv. Por ende
#las fusiono a traves de este atributo

b_completa <- left_join(b_ind,b_viv , by = c("folioviv"))

#Compruebo que sea el correcto número filas
nrow(b_completa) == nrow(b_ind)

#Compruebo que sea el correcto número de columnas
ncol(b_completa) > ncol (b_ind)

#Ambas son correctas

## Incido d ----

#Observo si la variable tiene valores missing
unique(b_completa$edad)

#No pareciera haber missing, de todas formas aclaro luego que me tome NA a aquellos valores
#que no están en el rango para evitar confusiones


b_completa <- b_completa %>%
              mutate(rango_edad = case_when(edad>=0 & edad<=4 ~ "0-4 AÑOS",
                                            edad>=5 & edad<=11 ~ "5-11 AÑOS",
                                            edad>=12 & edad<= 19 ~ "12-19 AÑOS",
                                            edad>=20 & edad<=29 ~ "20-29 AÑOS",
                                            edad>=30 & edad<=39 ~ "30-39 AÑOS",
                                            edad>=40 & edad<=49 ~ "40-49 AÑOS",
                                            edad>=50 & edad<=59 ~ "50-59 AÑOS",
                                            edad>=60 ~ "60 Y MÁS AÑOS",
                                            TRUE ~ NA))

#Compruebo que se haya realizado correctamente
table(b_completa$rango_edad)

## Inciso e ----

#Creo la tabla con la cantidad de población según rango de edad y sexo
tabla_pob <- b_completa %>%
              filter(!is.na(rango_edad)) %>%
              group_by(rango_edad,sexo) %>%
              summarise(tot_pob= sum(factor)) %>%
              as_tibble()

## Inciso f ----

#Armo la tabla de forma tal que sea identica a la tabla 1.f
tabla_pob_wide <- tabla_pob %>%
                  mutate(sexo = ifelse(sexo==1,"Hombre","Mujer"))%>%        #Establezco que sea una variable string
                  pivot_wider(names_from = sexo, values_from = tot_pob)%>%  #Establezco que los nombres de las columnas sean "Hombre y Mujer"
                  group_by(rango_edad, Hombre, Mujer)%>%                    #Agrupo
                  mutate(Total = sum(Hombre,Mujer))                         #Agrego la columna con el total

## Inciso g ----

#Replico el análisis anterior pero incluyendo la proporción de personas en cada franja etaria sobre
#el total
tabla_pob_g <- b_completa %>%
                filter(!is.na(rango_edad)) %>%
                group_by(rango_edad) %>%
                summarise(tot_edad = sum(factor)) %>%                       #Calculo la cantidad de personas por franja etaria
                mutate(prop_edad =round(tot_edad/sum(tot_edad)*100,2))%>%   #Calculo la proporción de personas
                select(rango_edad, prop_edad)                               #Selecciono sólo las variables que quiero


## Inciso h ----

#Replico el análisis anterior pero desagregando por sexo
tabla_pob_h <- b_completa %>%
                filter(!is.na(rango_edad)) %>%
                group_by(rango_edad, sexo) %>%
                summarize(tot_edad = sum(factor)) %>%                       #Calculo la cantidad de personas según sexo y rango de edad
                group_by(sexo) %>%                                          #Agrupo únicamente por sexo dado que quiero calcularlo según el total por sexo
                mutate(prop_edad = round(tot_edad/sum(tot_edad)*100,2))%>%  #Calcular la proporción sobre el total de hombres y mujeres
                select(rango_edad, sexo, prop_edad) %>%                     #Selecciono sólo las variables que quiero mantener
                mutate(sexo = ifelse(sexo==1,"Hombre","Mujer")) %>%         #Establezco la variable de sexo como string para la tabla
                pivot_wider(names_from = sexo, values_from = prop_edad)     #Le cambio el formato a la tabla.

## Inciso i ----

#Establezco la tabla en formato long a fines de poder realizar el gráfico de torta
tabla_pob_i <- tabla_pob_h %>%
  pivot_longer(cols = c("Hombre","Mujer"), names_to = "sexo", values_to = "prop_sexo") #Hombre y Mujeres ahora estarían bajo la columna sexo y prop_sexo tendría las proporciones

#Armo el gráfico de torta
pie <- ggplot(tabla_pob_i, aes(x="" , y=prop_sexo, fill = rango_edad))+
              geom_bar(stat="identity", width=1, color="white")+                #Mediante stat=identity establezco que las barras tomen los valores de Y, con width establezco el ancho y con white el color del borde
              coord_polar("y")+                                                 #Me permite transformar el gráfico de barras en uno de torta al utilizar coordenadas polares
              theme_void()+                                                     #Es una temática que establece que el fondo sea todo blanco, quitando los ejes
              facet_wrap(~sexo)+                                                #Comando que me permite reflejar dos gráficos en un mismo plot
              theme(legend.position = "bottom",                                 #Establezco la ubicación de la leyenda y del título
              plot.title = element_text(hjust=0.5, vjust=2))+
              labs(title= "Estructura poblacional por género")+                 
              scale_fill_brewer(palette = "BrBG", name="Rango por Edades")      #Establezco una paleta de colores y el nombre de la variable que da lugar a los colores

#Visualizo el gráfico de torta        
pie

# ---- Ejercicio 2 ----

## Inciso j ----

#Me fijo la cantidad de caracteres en mi variable
nchar(b_completa$ubica_geo)

#Creo la variable entidad
b_completa <- b_completa %>%
mutate(entidad = as.numeric(ifelse(nchar(ubica_geo)==8, substr(ubica_geo, 1, 1), substr(ubica_geo, 1, 2))))

#En el comando anterior, estoy creando la variable entidad numérica mediante un ifelse, de forma tal de que
#si tiene 8 caracteres me tome el primer dígito de ubica_geo y si tiene 9 que me tome 2 caracteres.


#Compruebo de haberlo hecho bien
table(b_completa$entidad)


## Inciso K ----

#Miro la relación entre la renta y la tenencia de la vivienda
table(b_completa$renta, b_completa$tenencia)

#Calculo para cada estado lo requerido
alq_entidad <- b_completa %>%
                filter(tenencia==1) %>%   #Filtro para obtener las las viviendas rentadas
                group_by(entidad) %>%     #Agrupo por entidad
                summarize(renta_media = mean(renta, na.rm = TRUE),       #Calculo para cada entidad el valor promedio, mediana, desvío, mín y max
                          renta_mediana = median(renta, na.rm = TRUE),
                          renta_desvío = sd(renta, na.rm = TRUE),
                          renta_mín = min(renta, na.rm = TRUE),
                          renta_max = max(renta, na.rm = TRUE))


#Cargo la base con los datos regionales
region_inc_estado <- read_xlsx(root %+% "/region_inc_estados.xlsx", sheet="clean")

#Cambio el nombre a la variable id_estado para que pase a llamarse entidad
region_inc_estado <- region_inc_estado %>%
                      rename(entidad = id_estado)


#Unifico las bases
b_regiones <- left_join(alq_entidad, region_inc_estado, by=("entidad"))

#Verifico que se hayan unificado correctamente
nrow(b_regiones)==nrow(alq_entidad)
ncol(b_regiones)>=ncol(alq_entidad)


## Inciso m ----

#Realizo el gráfico requerido
share_prop <- ggplot(b_regiones, aes(x=ing_mean, y=renta_media, size= propiedad))+         #size es la variable que será considerada para determinar que tan grande es el punto según la proporción de propietarios de vivienda
                      geom_point(color = "black",aes(fill=region), alpha=0.4, shape=21)+   #color determinar el borde, fill el color según región, alpha la opacidad de los puntos y shape me permite que sean puntos bordeados
                      xlab ("Ingreso Promedio") +
                      ylab ("Renta Media")+
                      scale_size(range = c(1,20), guide = "none")                          #Establezco que tan grandes pueden ser los puntos y con guide=none hago que no se vea la leyenda

#Visualizo el gráfico
share_prop

# ---- Ejercicio 3 ----

## Inciso a ----

#El estado elegido es Chiapas.

#Cargo el archivo en R
chiapas_original <- read_xls(data_dir %+% "Ejercicio 3/inciso a/resloc_07xls14.xls")


## Inciso b ----

#Me quedo sólo con las variables requeridas
chiapas <- chiapas_original %>%
            select("CVEGEO":"LOC_NOM","ALUMBCOB") %>% 
            rename_with(tolower)                       #Las renombro a minúsculas


## Inciso c ----

#Creo una variable númerica de clasificación según alumbcob
chiapas <- chiapas %>%
            mutate(clasificacion = case_when(alumbcob=="NA" ~ NA_real_,
                                              alumbcob=="Toda la localidad" ~ 5,
                                              alumbcob=="La mayor parte de la localidad" ~ 4,
                                              alumbcob=="Aproximadamente la mitad de la localidad" ~ 3,
                                              alumbcob=="Menos de la mitad de la localidad" ~ 2,
                                              alumbcob=="No hay alumbrado público" ~ 1))

#Verifico que se haya realizado correctamente
class(chiapas$clasificacion)
table(chiapas$alumbcob, chiapas$clasificacion)


## Inciso d ----

"https://www.inegi.org.mx/contenidos/programas/ccpv/iter/zip/resloc2014/resloc_07xls14.xls"
"https://www.inegi.org.mx/contenidos/programas/ccpv/iter/zip/resloc2014/resloc_08xls14.xls"
"https://www.inegi.org.mx/contenidos/programas/ccpv/iter/zip/resloc2014/resloc_09xls14.xls"
"https://www.inegi.org.mx/contenidos/programas/ccpv/iter/zip/resloc2014/resloc_10xls14.xls"

#Lo que cambia es al final, resloc_"num" el "num" va cambiando.

## Incido e ----

#Armo un vector con los nombres de los estados
estados <- c("Aguascalientes","Baja California", "Baja California Sur", "Campeche", 
             "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México")


for (i in seq(01,05,01)) {
    i_2dg <- sprintf("%02d",i)  #Debo formatear a i para que aparezcan los valores con 0 delante porque si no, toma 01 como 1, por ejemplo
    print("Descargando archivo entidad:" %+% estados[i])
    url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/iter/zip/resloc2014/resloc_" %+% i_2dg %+% "xls14.xls"  #Establezco el url que voy a utilizar, el cual itera en base a i
    download.file(url, destfile = data_dir %+% "Ejercicio 3/Inciso d/" %+% i_2dg %+% ").xls", mode = "wb")  #Descargo el archivo y lo guardo en mi carpeta
}


## Inciso f ----

#Creo una lista sin elementos
lista <- list()

#Establezco el timeout para que corte si supera el tiempo
options(timeout = 120)

#Realizo los procedimientos previos pero en este caso para 9 estados
for (i in seq(01,09,01)) {
     i_2dg <- sprintf("%02d",i)
     
     #Establezco los url y guardo los xls en mi carpeta
     print("Descargando archivo entidad:" %+% estados[i])
     url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/iter/zip/resloc2014/resloc_" %+% i_2dg %+% "xls14.xls"
     download.file(url, destfile = data_dir %+% "Ejercicio 3/Inciso d/" %+% i_2dg %+% ".xls", mode = "wb")
     
     #Cargo los xls que descargue
     estados_df <- read_xls(data_dir %+% "Ejercicio 3/Inciso d/" %+% i_2dg %+%".xls")%>%
                  select("CVEGEO":"LOC_NOM","ALUMBCOB") %>%
                  rename_with(tolower)
     
     #Creo la variable de clasificiación según la cobertura de alumbrado
     estados_df <- estados_df %>%
                   mutate(clasificacion = case_when(alumbcob=="NA" ~ NA_real_,
                                                    alumbcob=="Toda la localidad" ~ 5,
                                                    alumbcob=="La mayor parte de la localidad" ~ 4,
                                                    alumbcob=="Aproximadamente la mitad de la localidad" ~ 3,
                                                    alumbcob=="Menos de la mitad de la localidad" ~ 2,
                                                    alumbcob=="No hay alumbrado público" ~ 1))
    
    
    #Incluyo en la lista previamente creada, los dataframes resultantes por estado
    estados_df<- assign(estados[i],estados_df)
    
    lista[[i]] <- as.data.frame(estados_df)
}

## Inciso g ----

#Creo la lista en la cual se van a encontrar todos los xls descargados
listas_g <-list.files(data_dir %+% "Ejercicio 3/Inciso d")  #Mediante list.files genero el vector que contiene todas las bases descargadas
print(listas_g)

#Creo una lista que voy a utilizar después para poder cargar de a una base
load_list <- list()

#Creo un data frame vacío en el cual voy a incluir los data frames resultantes
df_estados <- data.frame()

#Realizo los procedimientos previos pero en este utilizando la lista con todos los xls descargados
for (i in listas_g){
     #Cargo las bases
     load_list[[i]] <- read_xls(data_dir %+% "Ejercicio 3/Inciso d/" %+% i)
     
     #Selecciono las variables que deseo utilizar
     estados_df <- load_list[[i]] %>%
     select("CVEGEO":"LOC_NOM","ALUMBCOB") %>%
     rename_with(tolower)
     
     #Creo la variable de clasificación
     estados_df <- estados_df %>%
     mutate(clasificacion = case_when(alumbcob=="NA" ~ NA_real_,
                                      alumbcob=="Toda la localidad" ~ 5,
                                      alumbcob=="La mayor parte de la localidad" ~ 4,
                                      alumbcob=="Aproximadamente la mitad de la localidad" ~ 3,
                                      alumbcob=="Menos de la mitad de la localidad" ~ 2,
                                      alumbcob=="No hay alumbrado público" ~ 1))
     
     #Unifico los data frame
     df_estados <- bind_rows(df_estados, estados_df)

}


## Inciso h ----

#Creo una lista sin elementos que me va a ayudar a cargar las bases
load_list_h <- list()

#Creo el dataframe vacío en el cual se van a incluir todos los dataframes resultantes
df_estados_h <- data.frame()

#Realizo el mismo procedimiento del inciso anterior
for (i in listas_g){
     #Cargo las bases
     load_list_h[[i]] <- read_xls(data_dir %+% "Ejercicio 3/Inciso d/" %+% i)
  
     #Selecciono las variables que deseo utilizar
     estados_df <- load_list_h[[i]] %>%
                   select("CVEGEO":"LOC_NOM","ALUMBCOB") %>%
                   rename_with(tolower)
     
     #Creo la variable de clasificación
     estados_df <- estados_df %>%
                  mutate(clasificacion = case_when(alumbcob=="NA" ~ NA_real_,
                                                   alumbcob=="Toda la localidad" ~ 5,
                                                   alumbcob=="La mayor parte de la localidad" ~ 4,
                                                   alumbcob=="Aproximadamente la mitad de la localidad" ~ 3,
                                                   alumbcob=="Menos de la mitad de la localidad" ~ 2,
                                                   alumbcob=="No hay alumbrado público" ~ 1))
  
     #Agrupo por las columnas idetificadoras del estado y del municipio para calcular el valor 
     #promedio de cobertura
     estados_df <- estados_df %>%
     filter(!is.na(clasificacion)) %>%
     group_by(ent,mun) %>%
     summarise(media_mun = mean(clasificacion, na.rm=TRUE))
      
     #Unifico todo en un mismo dataframe
     df_estados_h <- bind_rows(df_estados_h, estados_df)
  
}

#En el anterior caso teníamos desagregado por localidades a los municipios, en este caso se 
#concentra todos los valores de las localidades a nivel municipal y obtenemos la media de alumbrado
