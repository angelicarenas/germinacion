#####################################################################################
###################LABORATORIO GERMINACIÓN EN PLANTAS##############################
#####################################################################################
###OBJETIVO: MEDIR LA GERMINACIÓN EN SEMILLAS APLICANDO CONCEPTOS DE BD##############
#####################################################################################
###PROCEDIMIENTO: 1. Cargar bibliotecas #############################################
################# 2. Análisis de datos sobre germinación#############################
################# 3. Figuras

#####################################################################################
########################## 1. Cargar bibliotecas (load libraries)####################
#####################################################################################
### Opción 1

loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }

loadandinstall("shiny")
loadandinstall("agricolae")
loadandinstall("ggplot2")
loadandinstall("dplyr")

loadandinstall("GerminaR")

loadandinstall("tidyverse")
loadandinstall("knitr")
loadandinstall("cowplot")

#### Opción 2

install.packages("GerminaR")
install.packages("shiny")
install.packages("agricolae")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("knitr")
install.packages("cowplot")

library("GerminaR")
library("shiny")
library("agricolae")
library("ggplot2")
library("dplyr")
library("knitr")
library("cowplot")

#####################################################################################
###################### 2. Análisis de datos de germinación #########################
#####################################################################################

# Get data (Obtener los datos desde mi computador)
# Abrir el archivo en formato CSV (Separado por comas)
# Abre una ventana para escoger el archivo "ensayo.csv"

dt<-read.table(file.choose(), sep=",", header=TRUE)
head(dt)

# load data ("Cargar" los datos para el an?lisis de germinaci?n)
##dplyr::mutate

data <- dt %>% mutate(across(c(nacl, cond, rep, PL, BM), as.factor))
## nacl = Tratamientos de solución salina
## cond = 1: luz y 2: oscuridad
## rep  = Números de réplicas
## PL   = Promedio de la longitud de la raíz
## BM   = Biomasa

#Resumen de cálculos
smr <- ger_summary(SeedN = "seeds", evalName = "D", data = data)
knitr::kable(head(smr, 12),align = "c")
View(smr)

### 2.1. Germination Seed Percentage (GRP)###########################################
###Description: This function calculates the germination percentage related at total 
#########seed sown for experimental unit.
###Arguments:
###"SeedN" =	Name of the colum with the number of seeds sown. "evalName" =	Prefix of the names of the periods of evaluation. "data" =	The name of the data frame containing the data.
###"evalName" =	Prefix of the names of the periods of evaluation.
###"Data" =	The name of the data frame containing the data.
###Details: According GOUVEA LABOURIAU (1983), the germinability of a sample of is the percentage of seeds in which the seed germination process comes to an end, in experimental conditions by the seminal intrauterine growth resulting protrusion (or emergence) of a living embryo.
###Value: It returns an vector with the percentage of seed germinated.

grp <- ger_GRP(SeedN ="seeds", evalName="D", data=data)
grp

gsm <- ger_summary(SeedN ="seeds", evalName="D", data=data)
gsm

# Tabla de los Datos de los experimentos de las semillas

knitr::kable(x = data,booktabs = TRUE, caption = "Experimento germinación semillas Chía BD24-02 USCO")

# Analisis de varianza

av <- aov(formula = grp ~ nacl*cond + rep, data = gsm)
av

# Prueba de comparación de promedios 
mc_grp <- ger_testcomp(aov = av,comp = c("cond","nacl"),type ="snk", sig = 0.05)
mc_grp

### NOTA: Para la interpretación de la gráfica se recomienda https://www.qualtrics.com/support/es/stats-iq/analyses/regression-guides/interpreting-residual-plots-improve-regression/

# 2. Número total de semillas germinadas en cada tratamiento

aov <- aov(grp ~ nacl*cond, data=smr)

mc <- ger_testcomp(aov = aov, comp = c("nacl", "cond"))

datagrp <- mc$table
datagrp

## Figura 1. Efecto de la luz en el número de semillas germinadas

fplot(
  data=datagrp, 
  type = "line",
  x = "cond", 
  y = "grp", 
  group = "nacl",
  ylab = "Número de semillas germinadas", 
  xlab = "Condición ('1=luz,' '2=oscuridad')", 
  glab = "Concentraciones de NaCl", 
  ylimits = NULL,
  xrotation = NULL,
  xtext = NULL,
  gtext = NULL,
  legend = "top",
  sig = NULL,
  sigsize = 3,
  error = NULL,
  color = TRUE,
  opt = NULL
)

### 2.2. Tiempo promedio de germinación: Mean Germination Time (MGT)
### Description: This function calculates the mean germination time of germination according at the time lapse of the evaluations.
### Details: It was proposed by Haberlandt in 1875. It is calculated as the weighted average germination time. The number of germinated seeds at the intervals established for the collection of data is used as weight. It is expressed in terms of the same units of time used in the germination count (CZABATOR, 1962).
### Value: It returns an vector with the values of Mean Germination Time.
### References: CZABATOR, F. J. Germination value: an index combining speed and completeness of pine seed germination. Forest Science, v. 8, n. 4, p. 386-396, 1962.

mgt <- ger_MGT(evalName = "D", data = data)
mgt

# Análisis de varianza
av <- aov(formula = mgt ~ nacl*cond + rep, data = gsm)
av

# Prueba de comparación de medias 

mc_mgt <- ger_testcomp(aov = av, comp = c("cond", "nacl"), type = "snk", sig = 0.05)
mc_mgt

# Figura 2. Tiempo promedio de germinación en los tratamientos

datamgt <- mc_mgt$table
datamgt

fplot(
  data=datamgt
  , type = "bar"
  , x = "cond"
  , y = "mgt"
  , group = "nacl"
  , ylab = "Tiempo promedio germinación (días)" 
  , xlab = "Condición ('1=luz,' '2=oscuridad')"
  , glab = "Concentraciones de NaCl"
  , ylimits = NULL
  , xrotation = NULL
  , xtext = NULL
  , gtext = NULL
  , legend = "top"
  , sig = NULL
  , sigsize = 5
  , error = NULL
  , color = TRUE
  , opt = NULL
)

### 2.3. EFECTO DEL NaCl

# data frame with percentual or relative germination in time by NaCl

grt <- ger_intime(Factor = "nacl", SeedN = "seeds", evalName = "D", method = "relative", data = data)
head(grt, 10)

# Figura 3. Germinación en los diferentes tratamientos según el día
fplot(data = grt
      , type = "line"
      , x = "evaluation"
      , y = "mean"
      , group = "nacl"
      , ylab = "Porcentaje Semillas germinadas" 
      , xlab = "Días"
      , glab = "Concentraciones de NaCl"
      , ylimits = NULL
      , xrotation = NULL
      , xtext = NULL
      , gtext = NULL
      , legend = "top"
      , sig = NULL
      , sigsize = 5
      , error = NULL
      , color = TRUE
      , opt = NULL
)

### 2.4. EFECTO DE LA CONDICI?N (LUZ Y OSCURIDAD)

# data frame with percentual or relative germination in time by condition
git <- ger_intime(Factor = "cond", SeedN = "seeds", evalName = "D", method = "relative", data = data)
git

# Figura 4. Efecto en la germinación en el tiempo según la condición luz/oscuridad

fplot(data = git
      , type = "line"
      , x = "evaluation"
      , y = "mean"
      , group = "cond"
      , ylab = "Porcentaje Semillas germinadas" 
      , xlab = "Días"
      , glab = "Condición ('1=Luz,' '2=Oscuridad')"
      , ylimits = NULL
      , xrotation = NULL
      , xtext = NULL
      , gtext = NULL
      , legend = "top"
      , sig = NULL
      , sigsize = 5
      , error = NULL
      , color = TRUE
      , opt = NULL
)
### 2.5. EFECTO DE LA CONDICIÓN (LUZ Y OSCURIDAD) EN LA LONGITUD DE LAS RAÍCES

fplot(
  data=data, 
  type = "bar",
  x = "cond", 
  y = "PL_P", 
  group = "nacl",
  ylab = "Promedio de la longitud de raíz (cm)", 
  xlab = "Luz                              Oscuridad", 
  glab = "Concentraciones de NaCl", 
  ylimits = NULL,
  xrotation = NULL,
  xtext = NULL,
  gtext = NULL,
  legend = "bottom",
  sig = NULL,
  sigsize = 4,
  error = NULL,
  color = TRUE,
  opt = NULL
)
### 2.6. EFECTO DE LA CONDICI?N (LUZ Y OSCURIDAD) EN LA BIOMASA

fplot(
  data=data, 
  type = "bar",
  x = "cond", 
  y = "BM_P", 
  group = "nacl",
  ylab = "Promedio de la biomasa germinada (g)", 
  xlab = "Luz                              Oscuridad", 
  glab = "Concentraciones de NaCl", 
  ylimits = NULL,
  xrotation = NULL,
  xtext = NULL,
  gtext = NULL,
  legend = "bottom",
  sig = NULL,
  sigsize = 4,
  error = NULL,
  color = TRUE,
  opt = NULL
)

### REFERENCIA: Modificado de https://flavjack.github.io/GerminaQuant-usm/germinar-data-analysis-with-code.html