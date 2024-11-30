### Ejemplo ANOVA. Pruebas no paramétricas
rm(list = ls(all.names = TRUE))
gc()


### Datos
# Los datos corresponden a un experimento sobre el crecimiento de plantas
# El objetivo es analizar si el crecimiento de las plantas se diferencia de acuerdo con 
# la condición de crecimiento que se establece con dos diferentes tratamientos y 
# el caso donde no se aplica nada (comúnmente llamado tratamiento control).


# Las preguntas de investigación son:

# ¿Existe alguna condición (tratamiento) que favorece el crecimiento de las plantas?
# ¿Se puede concluir que existe una condición que favorece el crecimiento de las 
#  plantas en comparación con el resto?


data(PlantGrowth)
help(PlantGrowth)

summary(PlantGrowth)

is.factor(PlantGrowth$group)


library(GGally)
ggpairs(PlantGrowth[,c(1,2)])

### Por lo general, en este tipo de problemas se debe
### revisar si la asignación de los tratamientos se hizo de
### forma aleatorizada

### Además, para ser tratado como un problema de regresión
### lineal múltiple el supuesto de homocedasticidad se debe cumplir
### para cada grupo, lo mismo que la normalidad

### Normalidad?

library(nortest)

# Lilliefors
lillie.test(PlantGrowth$weight[PlantGrowth$group=="ctrl"])
lillie.test(PlantGrowth$weight[PlantGrowth$group=="trt1"])
lillie.test(PlantGrowth$weight[PlantGrowth$group=="trt2"])



#Prueba de homocedasticidad
# H_o: las varianzas de los grupos es la misma vs H_a: al menos un grupo tiene una varianza diferente
bartlett.test(weight ~ group, data = PlantGrowth)

#Otra prueba más robusta 
library(car)
leveneTest(weight ~ group, data = PlantGrowth)

### En este caso es factible el
### uso de una regresión lineal múltiple
### 



### Una alternativa es la prueba no parámetrica de Kruskal-Wallis

library(PMCMRplus)

### Misma varianza entre los grupos
# Prueba no parámetrica
# H_o: las varianzas de los grupos es la misma vs H_a: al menos un grupo tiene una varianza diferente

fligner.test(weight ~ group, data = PlantGrowth)

### Nota. En caso de no cumplirse la igualdad de varianzas, aún se puede usar
### esta prueba, pues al estar basada en rangos, es robusta ante transformaciones
### uno a uno de los valores. Así que si se asume que existe 
### alguna transformación uno a uno que hace que las varianzas sean iguales
### se podría proceder.

rank(PlantGrowth$weight)
rank(log(PlantGrowth$weight))
rank(sqrt(PlantGrowth$weight))


### ¿Los grupos son iguales con respecto a la mediana?
# Algo equivalente a la prueba de la tabla ANOVA cuando se usa regresión 
# lineal múltiple.
kruskal.test(weight ~ group, data = PlantGrowth)

#Distribución exacta sobre la estadística que es función de los rangos
kruskalTest(weight ~ group, data = PlantGrowth, dist="KruskalWallis")

kruskalTest(weight ~ group, data = PlantGrowth, dist="Chisquare")

#Como p-value es menor a .05, se rechaza H_0, es decir
#al menos un tratamiento es diferente en cuanto a su mediana.

#Ahora se procede a identificar las diferencias entre tratamientos que son significativas

#Todos los pares para identificar grupos diferentes
#Como son todos los pares al mismo tiempo es posible que haya menos cosas
#significativas, además sólo se trabaja con pruebas de dos colas.

#Tipo prueba donde se usan comparaciones tipo Tukey en regresión lineal múltiple

#Son pruebas simultáneas
ans <- kwAllPairsDunnTest(weight ~ group, data = PlantGrowth)
summary(ans)

#Una alternativa es elegir un grupo y hacer sólo las comparaciones 
#de ese grupo con los demás

ans1 <- kwManyOneDunnTest(weight ~ group, data = PlantGrowth)
summary(ans1)
levels(PlantGrowth$group)

PlantGrowth$group <- relevel(PlantGrowth$group, ref = "trt2")
ans1 <- kwManyOneDunnTest(weight ~ group, data = PlantGrowth)
summary(ans1)

# Se puede especificar sentido para poder obtener más control en la significancia
# Sólo si los investigadores tienen esa hipótesis

ans1 <- kwManyOneDunnTest(weight ~ group, data = PlantGrowth, alternative = c("less"))
summary(ans1)
# con base en lo último no se puede conluir con alpha=.05 que el tratamiento 2 es mejor que ambos, el tratamiento 1 y el control.
# sin embargo con alpha=.1 sí es posible concluir eso




### A mano
library(tidyverse)

Datos= PlantGrowth %>% mutate(RangoYi = rank(weight))

DatosPorGrupo=Datos %>% group_by(group) %>% summarise(R_r=sum(RangoYi), n_r=n())
(DatosPorGrupo)

n=sum(DatosPorGrupo$n_r)

#Sin corrección por empates
(H= ( 12/(n*(n+1)) )* sum(DatosPorGrupo$R_r^2 / DatosPorGrupo$n_r)-3*(n+1))


##Cuando hay empates se debe dividir H por un factor de corrección
### Denominador para corrección:
DatosPorTamTies=Datos %>% group_by(RangoYi) %>% summarise(sumt=n()*(n()^2-1))
(DatosPorTamTies)
DemCorr= 1-sum(DatosPorTamTies$sumt)/(n*(n^2-1))

#Se calcula H con los rangos corregidos y con fórmula para correccion por empates

(H= ( ( 12/(n*(n+1)) )* sum(DatosPorGrupo$R_r^2 / DatosPorGrupo$n_r)-3*(n+1) )/DemCorr )

(pvalue=pchisq(H, 2,lower.tail = FALSE))

