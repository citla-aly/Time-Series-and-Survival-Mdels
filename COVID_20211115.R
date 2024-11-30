library(readr)
#Aquí se debe indicar la dirección donde se ubica el archivo de datos
COVID19MEXICO_20211115 <- read_csv("~/211115COVID19MEXICO.csv",
col_types = cols(FECHA_ACTUALIZACION = col_character(),
FECHA_DEF = col_character(), FECHA_INGRESO = col_character(),
FECHA_SINTOMAS = col_character()))


library(dplyr)

COVID19_FILTRADO_20211115<-COVID19MEXICO_20211115 %>% filter(CLASIFICACION_FINAL%in% c(1,2,3))

COVID19_FILTRADO_20211115$ENTIDAD_UM_NOM <- factor(COVID19_FILTRADO_20211115$ENTIDAD_UM,
                                           levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
                                                      "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", 
                                                      "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32"),
                                           labels = c("AGS", "BC", "BCS", 
                                                      "CAMP", "COAH", "COL", "CHIA", "CHIH", 
                                                      "CDMX", "DUR", "GTO", "GRO", "HGO", 
                                                      "JAL", "MEX", "MICH", "MOR", "NAY", 
                                                      "NL", "OAX", "PUE", "QRO", "Q-ROO", 
                                                      "SLP", "SIN", "SON", "TAB", "TAM", 
                                                      "TLX", "VER", "YUC", "ZAC"
                                           ))

COVID19_FILTRADO_20211115$SEXO_L<-factor(COVID19_FILTRADO_20211115$SEXO,
                                         levels = c(1,2),
                                         labels = c("MUJER","HOMBRE"))

COVID19_FILTRADO_20211115$DIABETES_L<-factor(COVID19_FILTRADO_20211115$DIABETES,
                                         levels = c(1,2),
                                         labels = c("SÍ","NO"))

COVID19_FILTRADO_20211115$OBESIDAD_L<-factor(COVID19_FILTRADO_20211115$OBESIDAD,
                                             levels = c(1,2),
                                             labels = c("SÍ","NO"))

COVID19_FILTRADO_20211115$TIPO_PACIENTE_L<-factor(COVID19_FILTRADO_20211115$TIPO_PACIENTE,
                                             levels = c(1,2),
                                             labels = c("AMBULATORIO",
                                                        "HOSPITALIZADO"))

COVID19_FILTRADO_20211115<-COVID19_FILTRADO_20211115 %>%
filter(as.Date(FECHA_SINTOMAS,"%Y-%m-%d")>= as.Date("2020-02-27","%Y-%m-%d"))





COVID19_FILTRADO_20211115<-COVID19_FILTRADO_20211115%>%
  filter(as.Date(FECHA_SINTOMAS,"%Y-%m-%d") >=
           as.Date("2020-02-27","%Y-%m-%d") & 
           as.Date(FECHA_SINTOMAS,"%Y-%m-%d") <=
           as.Date("2021-11-15","%Y-%m-%d") )
 

COVID19_FILTRADO_20211115<-
  COVID19_FILTRADO_20211115%>%
  mutate(TIEMPO=ifelse(FECHA_DEF!="9999-99-99" & as.Date(FECHA_DEF,"%Y-%m-%d")<= as.Date("2021-11-15","%Y-%m-%d"),
                       difftime(
                         as.Date(FECHA_DEF,"%Y-%m-%d"),
                         as.Date(FECHA_SINTOMAS,"%Y-%m-%d"), 
                         units = "days")+1,
                       difftime(
                         as.Date("2021-11-15","%Y-%m-%d"),
                         as.Date(FECHA_SINTOMAS,"%Y-%m-%d"), 
                         units = "days")+1 ))                                                           


COVID19_FILTRADO_20211115<-
  COVID19_FILTRADO_20211115%>%filter(TIEMPO>0)





  
  
  
  

  
 


COVID19_FILTRADO_20211115$CENSURA<-1
  
 
COVID19_FILTRADO_20211115$CENSURA[COVID19_FILTRADO_20211115$FECHA_DEF=="9999-99-99" |
                                    as.Date(COVID19_FILTRADO_20211115$FECHA_DEF,"%Y-%m-%d")>
                                    as.Date("2021-11-15","%Y-%m-%d")]<-0
library(anchors)

VARIABLES_EXPLICATIVAS<-
  c( "INTUBADO", "NEUMONIA", "NACIONALIDAD", "EMBARAZO", "HABLA_LENGUA_INDIG", "INDIGENA", 
    "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", "OTRA_COM", 
    "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO", 
    "OTRO_CASO", "TOMA_MUESTRA_LAB", "RESULTADO_LAB", "TOMA_MUESTRA_ANTIGENO", 
    "RESULTADO_ANTIGENO", "CLASIFICACION_FINAL", "MIGRANTE", "UCI")

COVID19_FILTRADO_20211115<-
  replace.value(COVID19_FILTRADO_20211115, VARIABLES_EXPLICATIVAS ,from = c(97,98,99),to=NA) 




library(survival)
library(survminer)
library(survivalAnalysis)
library(ggfortify)
    


#===================================================
#1. Ajustar un modelo de KM  y riesgo acumulativo
#   con un subconjunto de datos
#   distinto (filter se puede cambiar) y presentar la
#    gráfica
#===================================================
survfit(Surv(TIEMPO,CENSURA)~1,
        data=COVID19_FILTRADO_20211115%>%filter(CENSURA==1 ), conf.int=0.95 )->covid.table



autoplot(covid.table, xlab='Tiempo días', ylab='Probabilidad de supervivencia ')

autoplot(covid.table, xlab='Tiempo días', ylab='Probabilidad acumulada de riesgo ',
         fun = "cumhaz")


#===================================================
#2. Comparar dos funciones de supervivencia, definidos
#   por una variable distinta a SEXO, graficar las
#   funciones de supervivencia y de riesgo acumulado
#   hacer la prueba de log rangos y explicar el resultado
#   de la prueba
#===================================================    
survfit(Surv(TIEMPO,CENSURA)~SEXO_L,
                    data=COVID19_FILTRADO_20211115%>%filter(CENSURA==1 ) )->covid.table.sexo
    
summary(covid.table.sexo)
    
    
autoplot(covid.table.sexo, xlab='Tiempo dias', ylab='Probabilidad de supervivencia ',
             lty=1:2)
    
autoplot(covid.table.sexo, xlab='Tiempo dias', ylab='Probabilidad acumulada de riesgo ',
             lty=1:2, fun = "cumhaz")
    
survdiff(Surv(TIEMPO,CENSURA)~SEXO_L,
            data=COVID19_FILTRADO_20211115%>%filter(CENSURA==1 ) )->covid.test
    
    stargazer(covid.test)
    
survfit(Surv(TIEMPO,CENSURA)~ENTIDAD_UM_NOM,
            data=COVID19_FILTRADO_20211115%>%filter(CENSURA==1 ) )->covid.table.entidad
covid.table.entidad <- broom::tidy(covid.table.entidad)
    
ggplot(data = covid.table.entidad, aes(x = time, y = estimate))+
      geom_line()+ 
      facet_wrap(strata~.)
    
    
library(knitr)
    

    
    
    

    
s<-table(COVID19_FILTRADO_20211115%>%filter(CENSURA==1)%>%dplyr::select(ENTIDAD_UM_NOM))
    
ggplot(as.data.frame(s), aes(x=reorder(Var1,-Freq), y = Freq, fill=Var1)) + 
      geom_bar(stat="identity")+ theme(legend.position = " none")+
      labs(x="Entidad de la Unida Médica",y="Frecuencias")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
      geom_text(aes(label = Freq), 
                size = 3, 
                color = "black",
                position = position_dodge(width = 0.9),
                vjust = -2)
    
#===================================================
#3. Ajustar un modelo de Cox y de Weibull,
#  con otro conjunto de variables o subconjunto de datos
#
#=================================================== 

    
    
cox.regression <- coxph(
                 Surv(TIEMPO,CENSURA)~EDAD+SEXO_L+DIABETES_L+OBESIDAD_L+TIPO_PACIENTE_L,
                 data=COVID19_FILTRADO_20211115%>%filter(CENSURA==1) )
  
summary(cox.regression)
  

    
COVID19_FILTRADO_20211115%>%filter(SEXO==2 & CENSURA==1)%>%summarise(mean(TIEMPO))
    
#Estima un modelo con el valor promedio (referencia) de las variables
    autoplot(survfit(cox.regression),surv.linetype = 1, surv.colour = 'blue',
             conf.int.fill = 'dodgerblue3', conf.int.alpha = 0.5,conf.int = 0.99,
             xlab='Tiempo (semanas)', ylab='Probabilidad de supervivencia S(t)  ',
             main='Modelos de Riesgos proporcionales de Cox')
    
#Evaluación del supuesto de proporcionalidad
    cox.zph(cox.regression)
    
ggforest(cox.regression, data=COVID19_FILTRADO_20211115%>%filter(CENSURA==1))  
    
weibull.regression <- survreg(Surv(TIEMPO,CENSURA)~EDAD+SEXO_L+DIABETES_L+OBESIDAD_L+TIPO_PACIENTE_L,
                                  data=COVID19_FILTRADO_20211115%>%filter(CENSURA==1),
                                  dist = "weibull")
summary(weibull.regression)
    
    
    
    lfit <- weibull.regression
    b <- lfit$coefficients #parámetros estimados
    s <- lfit$scale #parámetro de forma recíproco 
    l <- exp(-(b[1]+b[2]+b[3]+b[4]+b[5]+b[6])/s) # estimación parámetro de escala
    g <- 1/s #parámetro de  forma 
    
    
    pct <- 1:100/100   
    ptime <- predict(lfit, newdata=data.frame(EDAD=20,
                                              SEXO_L="MUJER",
                                              DIABETES_L="NO",
                                              OBESIDAD_L="NO",
                                              TIPO_PACIENTE_L="HOSPITALIZADO"),
                     type='quantile', p=pct, se=TRUE)
    
    matplot(cbind(ptime$fit, ptime$fit + 2*ptime$se.fit,
                  ptime$fit - 2*ptime$se.fit), 1-pct, 
            xlab='Tiempo (días)', ylab='Probabilidad de supervivencia S(t)  ',
            main='Modelos de Weibull\n EDAD=20, SEXO=MUJER,DIABETES=NO, OBESIDAD=NO, 
            TIPO_PACIENTE=HOSPITALIZADO', type='l', lty=c(1,2,2), 
            col=c("red","blue","blue"))
    

    T <- seq(0,60,0.1)
    S0 <- exp(-l*T^g)
    lines(T, S0, lty=1, col="green")    
    
    
    legend("topright", legend=c("Curva predicción", "Curva paramétrica"),
           col=c("red", "green"), lty=1:1, cex=0.8)
    #===================================================
    #4. Graficar los modelos de Cox y de Weibull,
    #  y presentar la tabla de coeficientes
    #===================================================  
    
    
    stargazer(cox.regression, weibull.regression)   

 

 