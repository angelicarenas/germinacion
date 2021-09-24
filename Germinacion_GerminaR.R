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
loadandinstall ("GerminaR")
loadandinstall("tidyverse")
loadandinstall("knitr")
loadandinstall("cowplot")

#### Opción 2

install.packages("GerminaR")
install.packages("tidyverse")
install.packages("knitr")
install.packages("cowplot")
install.packages("ggplot2")

library("GerminaR")
library("tidyverse")
library("knitr")
library("cowplot")
library("ggplot2")

#### Opción 3

if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("Flavjack/GerminaR")
1

#####################################################################################
###################### 2. Análisis de datos de germinacion #########################
#####################################################################################

# Get data (Obtener los datos desde mi computador)
# Abrir el archivo en formato CSV (Separado por comas)
# Abre una ventana para escoger el archivo "ensayo.csv"

dt<-read.table(file.choose(), sep=";", header=TRUE)

head(dt)

# load data ("Cargar" los datos para el análisis de germinación)
##dplyr::mutate

data <- dt %>% mutate(across(c(nacl, cond, rep, PL), as.factor))

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

grp <- ger_GRP(SeedN ="seeds", evalName="D", data=dt)
grp

gsm <- ger_summary(SeedN = "seeds", evalName = "D",data=dt)
gsm

# Tabla de los Datos de los experimentos de las semillas

knitr::kable(x = dt,booktabs = TRUE, caption = "Experimento germinación semillas Zea mays BD2130PUJ")

# Analisis de varianza

av <- aov(formula = grp ~ nacl*cond + rep, data = gsm)
av

# Prueba de comparación de promedios 
mc_grp <- ger_testcomp(aov = av,comp = c("cond","nacl"),type ="snk")

# 2. Número total de semillas germinadas en cada tratamiento

aov <- aov(grp ~ nacl*cond, data=smr)

mc <- ger_testcomp(aov = aov, comp = c("nacl", "cond"))

data <- mc$table
data

## Figura 1. Efecto de la luz en el número de semillas germinadas

fplot(
  data, 
  type = "bar",
  x = "cond", 
  y = "nacl", 
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

mgt <- ger_MGT(evalName = "D", data = dt)
mgt

# Análisis de varianza
av <- aov(formula = mgt ~ nacl*cond + rep, data = gsm)
av

# Prueba de comparación de medias 
mc_mgt <- ger_testcomp(aov = av, comp = c("cond", "nacl"), type = "snk")
mc_mgt

# Figura 2. Tiempo promedio de germinación en los tratamientos

data <- mc_mgt$table
data

fplot(
  data
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

grt <- ger_intime(Factor = "nacl", SeedN = "seeds", evalName = "D", method = "percentage", data = dt)
head(grt, 10)

# Figura 3. Germinación en los diferentes tratamientos según el día
fplot(data = grt
      , type = "line"
      , x = "evaluation"
      , y = "mean"
      , group = "nacl"
      , ylab = "Semillas germinadas" 
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

### 2.4. EFECTO DE LA CONDICIÓN (LUZ Y OSCURIDAD)

# data frame with percentual or relative germination in time by condition
git <- ger_intime(Factor = "cond", SeedN = "seeds", evalName = "D", method = "percentage", data = dt)
git

# Figura 4. Efecto en la germinación en el tiempo según la condición luz/oscuridad

fplot(data = git
      , type = "line"
      , x = "evaluation"
      , y = "mean"
      , group = "cond"
      , ylab = "Semillas germinadas" 
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

dt
fplot(
  data=dt, 
  type = "bar",
  x = "cond", 
  y = "PL", 
  group = "nacl",
  ylab = "Promedio de la longitud de raíz (cm)", 
  xlab = "Condición ('1=luz,' '2=oscuridad')", 
  glab = "Concentraciones de NaCl", 
  ylimits = NULL,
  xrotation = NULL,
  xtext = "cond",
  gtext = NULL,
  legend = "bottom",
  sig = NULL,
  sigsize = 4,
  error = NULL,
  color = TRUE,
  opt = NULL
)
########################### OTROS CALCULOS ###############################
### OPCIONAL: Puedo obtener los promedios de cada día con:

promedios<-summary.data.frame(dt)
View(promedios)
### Quitar head(select (prome, 3))

md0<-mean(dt$D0)
md1<-mean(dt$D1)
md2<-mean(dt$D2)
md3<-mean(dt$D3)
md4<-mean(dt$D4)
md5<-mean(dt$D5)
md6<-mean(dt$D6)

dia=c("D0", "D1","D2","D3", "D4", "D5", "D6")
promd=c(md0,md1,md2,md3,md4,md5,md6)

meanComp<-data.frame(dia, promd)
View(meanComp)

### 1. Cumulative sum of germination matrix
###Description: This function makes a data table with the cumulative sum of values of germination.
###Value: Data frame with the cumulative sum

gcs <- ger_cumsum(SeedN = "seeds", evalName = "D", method = "percentage", data = data)
head(gcs, 10)
View(gcs)

#### 2. Coefficient of Variance of the Mean Germination Time
###Description: This function calculates the coefficient of variation of the mean germination time
###Value: It returns an vector with the values of Coefficient of Variance of germination

cvg <- ger_CVG(evalName = "D", data = dt)
View(cvg)

### 3. Germinated Seed Number
###Description: This function calculates the number of seed germinated.
###Value: Number of seed germianated

grs <- ger_GRS(evalName = "D", data = dt)
View(grs)

### 4. Germination Speed
###Description: This function calculates the number of seed germinated.
###Value: Number of seed germianated

gsp <- ger_GSP(evalName = "D", data = dt)
View(gsp)

### 5. Cumulative sum of germination by period of time for line graphic
###Description: This function makes a data table with the cumulative sum of values of germination by days.
###Details: Need a summary by factor before use it with function SummaryBy.
###Value: Data frame with the germination by period

grt <- ger_intime(Factor = "nacl", SeedN = "seeds",
                  evalName = "D", method = "percentage", data = dt)
head(grt, 10)
View(grt)

### 6. Mean Germination Rate
### Description: This function calculates the mean germination rate of the germination.
### Details: The average speed of germination is defined as the reciprocal of the average time germination (RANAL; SANTANA, 2006).
### Value: It returns an vector with the values of Mean Germination Rate
### References: RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination process? Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar. 2006.

mgr <- ger_MGR(evalName = "D", data = dt)
View(mgr)

### 7. Standard deviation of the Mean Germination Time
### Description: This function calculates the standard desviation of the mean germination time
### Value: It returns an vector with the values of Standard desviation of germination

sdg <- ger_SDG(evalName = "D", data = dt)
View(sdg)

### 8. Germination Synchronization Index
### Description: This function calculates the germination synchronization of the germination process.
### Details: The Synchory Index Z has been proposed to assess the degree of overlap between flowering individuals in a population. By adopting the idea expressed by PRIMACK, R.B. (1980) the synchrony of one seed with other included in the same replication. Z = 1 when germination of all the seeds occurs at the same time and Z = 0 when at least two seeds can germinate one each time. Z produces a number if and only if there are two seeds finishing the seed germination process at the same time. Thus, the value of Z assessments is the grade of overlap between seed germination.
### Value: It returns an vector with the values of Germination Synchrony
### References: RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination process? Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar. 2006.

syn <- ger_SYN(evalName = "D", data = dt)
View(syn)

### 9. Germination Uncertainty
### Description: This function calculates the germination uncertainty in the germination process.
### Details: The uncertainty index u is an adaptation of Shannon index measures the degree of uncertainty in predicting the informational entropy or uncertainty associated with the distribution of the relative frequency of germination (GOUVEA LABOURIAU 1983; LABOURIAU; VALADARES, 1983). Low values of u indicate frequencies with short peaks, i.e. the more concentrated the germination in time. Just a germinated seed changes the value of u. This means that u measures the degree of germination scattering.
### Value:It returns an vector with the values of Germination Uncertainty.
### References: GOUVEA LABOURIAU, L. L. G. L. A germinacao das sementes. Washington: [s.n.]. LABOURIAU, L. G.; VALADARES, M. E. B. The germination of seeds. OEA, Washington, DC, 1983.

unc <- ger_UNC(evalName = "D", data = dt)
View(unc)

### 10. Variance of the Mean Germination Time
### Description: This function calculates the variance of the mean germination time.
### Value: It returns an vector with the values of Variance of Germination

vgt <- ger_VGT(evalName = "D", data = dt)
View(vgt)

### REFERENCIA: Modificado de https://flavjack.github.io/GerminaQuant-usm/germinar-data-analysis-with-code.html
