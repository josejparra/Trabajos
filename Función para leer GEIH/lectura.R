library(data.table)
library(purrr)
library(readr)
library(dplyr)
library(ForImp)
library(tempdisagg)

setwd("C:/Users/josej/Desktop/Gobernación/Tareas/Cuentas 2019/GEIH2019")

#####LECTURA DE DATOS#######

geih <- function(año,bases,variables){
  bases_anuales <- vector("list",length=length(bases))
  for(i in 1:length(bases)){
    common_path <- paste(getwd(),"/",año,sep="")
    directorios <- list.files(common_path)
    combinaciones <- CJ(directorios, bases[[i]])
    sub_ruta <- paste(año,"/",combinaciones[[1]],"/",combinaciones[[2]],sep="")
    lista_mensual <- lapply(sub_ruta, function(j) {read_csv2(j)})
    for(a in 1:12){
      colnames(lista_mensual[[a]]) <-tolower(colnames(lista_mensual[[a]]))
    }
    for(j in 1:length(lista_mensual)){
      lista_mensual[[j]]$mes <-rep(substring(substring(sub_ruta[j],6), 1, regexpr(".csv", substring(sub_ruta[j],6))-1)
                                   ,length(lista_mensual[[j]][[1]]))  
    }
    for(j in 1:length(lista_mensual)){
      lista_mensual[[j]] <-lista_mensual[[j]][,variables[[i]]]  
    }
    bases_anuales[[i]] <- Reduce(function(d1, d2) rbind(d1, d2), lista_mensual)
    
  }
  datos_totales <- data.frame()
  datos_totales <- Reduce(function(d1, d2) merge(d1, d2,  all=TRUE), bases_anuales)
  rownames(datos_totales) <- NULL
  
  assign(paste("GEIH","_",año,sep=""), data.frame(datos_totales), envir = .GlobalEnv)
  
}


base_area <- list("Área - Ocupados.csv")
base_resto <- list("Cabecera - Ocupados.csv","Resto - Ocupados.csv")


#bases <- list("Área - Ocupados.csv","Cabecera - Ocupados.csv","Resto - Ocupados.csv")


variables <- list(c("directorio","secuencia_p","orden",
                    "area","inglabo","p6430","mes","dpto"),
                  c("directorio","secuencia_p","orden",
                    "area","inglabo","p6430","mes","dpto"),
                  c("directorio","secuencia_p","orden",
                    "area","inglabo","p6430","mes","dpto"))

variables_area <- list(c("directorio","secuencia_p","orden",
                         "area","inglabo","p6430","mes","dpto"))

variables_resto <-list(c("directorio","secuencia_p","orden",
                    "clase","inglabo","p6430","mes","dpto"), 
                  c("directorio","secuencia_p","orden",
                    "clase","inglabo","p6430","mes","dpto"))


geih_area <- geih(2019, base_area, variables_area)

geih_area <- geih_area[geih_area$area=="05",]

geih_resto <- geih(2019, base_resto, variables_resto)

geih_resto <- geih_resto[geih_resto$dpto=="05",]

nrow(geih_area)
nrow(geih_resto)

table(geih_resto$directorio %in% geih_area$directorio)#Las unidades muestrales son las mismas




geih_resto <- geih_resto[!is.na(geih_resto$inglabo),]

tapply(X=geih_resto$inglabo,as.factor(geih_resto$mes),median)
tapply(X=geih_resto$inglabo,as.factor(geih_resto$mes),mean)


geih_area <- geih_area[!is.na(geih_area$inglabo),]

tapply(X=geih_area$inglabo,as.factor(geih_area$mes),median)
tapply(X=geih_area$inglabo,as.factor(geih_area$mes),mean)




a <- 
  
  plot(sal_med,type="l",ylab="Salario",xlab="Mes")
  lines(sal_mean,lty=2)
  legend("bottom",legend=c("Me","Ma"),lty=1:2)


####SELECCIONAR EMPLEO DOMÉSTICO####

geih_domestico <- filter(GEIH_2019,p6430 %in% c(3))
glimpse(geih_domestico)

####SELECCIONAR ANTIOQUIA####
geih_domestico_ant <- filter(geih_domestico,dpto %in% c("05"))


##### Datos faltantes ####

table(is.na(geih_domestico$inglabo))
#Son muy pocos, se eliminan
geih_domestico=geih_domestico[!is.na(geih_domestico$inglabo),]
table(is.na(geih_domestico$inglabo))#No queda ningún faltante

####SALARIO POR AREA METROPOLITANA Y RESTO#####

geih_domestico_aburra=geih_domestico[geih_domestico$area %in% c("05"),]

geih_domestico_resto=geih_domestico[geih_domestico$clase %in%c(1,2),]



hist(geih_domestico$inglabo[geih_domestico$mes=="Enero"])
hist(geih_domestico$inglabo[geih_domestico$mes=="Febrero"])





#### No lo sé####

La encuesta solamente tiene representatividad a nivel urbano o resto, o a nivel de área metropolitana, pero no simultáneamente, esto se confirma por el hecho de que solo aproximadamente el 10% de los datos de Antioquia pertenecen clase "resto" y no están afiliados a un área metropolitan, a pesar de que existe más gente fuera del área metropolitana en el. Por tanto, no es posible estimar un salario para el área metropolitana y otro para el resto del departamento...... ¿es cierto?
  
  ```{r}
geih_ant <- geih_domestico[geih_domestico$dpto=="05",]
geih_ant_resto <- geih_ant[geih_ant$clase=="2",]
table(geih_ant_resto$area,useNA="ifany")#Existen 52 datos que no pertenecen a un área metropolitana en el departamento




geih_domestico_aburra=geih_domestico[geih_domestico$area %in% c("05"),]
geih_domestico_resto=geih_domestico[geih_domestico$clase %in%c(1,2),]
geih_solo_resto=geih_domestico_resto

table(geih_domestico_aburra$clase, useNA="ifany")
```



```{r}
hist(geih_domestico$inglabo[geih_domestico$mes=="Enero"])
hist(geih_domestico$inglabo[geih_domestico$mes=="Febrero"])

```

