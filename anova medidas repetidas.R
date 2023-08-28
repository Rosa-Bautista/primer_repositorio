####MEDIDAS REPETIDAS ANOVA#####

##PAQUETES QUE SE NECESITAN##
#tidyverse: 
#ggpubr : todas las graficas se hacen apartir de ggplot y las graficas ya estan bonitas
#rstatix :aqui esta la funcion para hacer el analisis como tal. anova_test
#datarium

install.packages("datarium")
library(datarium)

install.packages("rstatix")
library(rstatix)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggpubr")
library(ggpubr)

#NOTA: formato vertical: tiempos que esten para abajo en lugar de que tenga 
#dos variables diferentes hay funciones que mueven la base de datos



##PASOS PARA HACER EL ANALISIS SIGUIENDO A https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/##

##PASOS PARA HACER DE UNA VIA##

#BASE DE DATOS
data ("selfesteem", package = "datarium")
head (selfesteem, 3) #esta parte no es necesaria

#convertir el tiempo y el id en factores para tener la base en vertical, pone los
#identificadores para cada resultado... si no sale cargar librerias otra vez
selfesteem1 <- selfesteem %>%
  gather (key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor (id, time)

#SUMMARY
selfesteem1 %>%
  group_by (time) %>%
  get_summary_stats (score, type = "mean_sd")
#los resultados que arroja: n= numero de replicas, el score es la variable de 
#respuesta, lo que signifique mean (promedio y desviacion estandar) depende de 
#nosotros

#VISUALIZACIÓN: podemos nosotros hacer el boxplot como hemos visto o con este
se_bxplot <- ggboxplot (selfesteem1, x = "time", y = "score", add = "point")
se_bxplot

##CHECAR SUPUESTOS

#Outliers: fijarnos en la parte de los outliers extremos xq son los que importan
#aunque existan outliers si no son extremos siguen cumpliendo el supuesto de que
#no hayan outliers
selfesteem1 %>%
  group_by (time) %>%
  identify_outliers (score)

#Normalidad analisis
selfesteem1 %>%
  group_by (time) %>%
  shapiro_test (score)

#Normalidad gráfica: la mayoría de los puntitos deben caer sobre la linea
ggqqplot(selfesteem1, "score", facet.by = "time")

#Esfericidad: se usa el anova test que esta rstatix
res.aov <- anova_test (data = selfesteem1, dv = score, wid = id, within = time)
get_anova_table (res.aov)
#¿qué significa lo que arroja? The self-esteem score was statistically 
#significantly different at the different time points during the diet, 
#F(2, 18) = 55.5, p < 0.0001, eta2[g] = 0.83.
#where,
#F Indicates that we are comparing to an F-distribution (F-test); (2, 18) 
#indicates the degrees of freedom in the numerator (DFn) and the denominator 
#(DFd), respectively; 55.5 indicates the obtained F-statistic value
#p specifies the p-value
#ges is the generalized effect size (amount of variability due to the 
#within-subjects factor)

#PRUEBAS POST HOC: sirven to identify which groups are different...
#pairwise comparisons: se hace entre los within-subjects factor (en este caso es tiempo)
#hace los pares de los factores y vemos cuál es lo que sale significativo
posthoc_pwc <- selfesteem1 %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
posthoc_pwc


##PASOS PARA HACER DE DOS VIAS##

#BASE DE DATOS: wide format
set.seed(123)
data("selfesteem2", package = "datarium")
selfesteem2 %>% sample_n_by(treatment, size = 1)

#BASE DE DATOS: vertical format
selfesteem2.2 <- selfesteem2 %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
selfesteem2.2
# lo que hacemos es que se juntan todos los valores para cada treatment
set.seed(123)
selfesteem2.2 %>% sample_n_by(treatment, time, size = 1)

#SUMMARY
selfesteem2.2 %>%
  group_by(treatment, time) %>%
  get_summary_stats(score, type = "mean_sd")

#VISUALIZACIÓN:
se2.2_bxplot <- ggboxplot(
  selfesteem2.2, x = "time", y = "score",
  color = "treatment", palette = "jco"
)
se2.2_bxplot

#CHECAR SUPUESTOS

#Outliers
se_outlier <- selfesteem2.2 %>%
  group_by(treatment, time) %>%
  identify_outliers(score)
se_outlier

#Normalidad analisis
selfesteem2.2 %>%
  group_by(treatment, time) %>%
  shapiro_test(score)

#Normalidad gráfica
ggqqplot(selfesteem2.2, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ treatment, labeller = "label_both")
#labeller???

#Esfericidad/Computation
res.aov <- anova_test(
  data = selfesteem2.2, dv = score, wid = id,
  within = c(treatment, time)
)
get_anova_table(res.aov)

#PRUEBAS POST-HOC para interacciones two-way interactions

#ver el efecto del tratamiento at each point
posthoc_one.way <- selfesteem2.2 %>%
  group_by(time) %>%
  anova_test(dv = score, wid = id, within = treatment) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
posthoc_one.way

#pairwise comparisons entre los grupos de tratamiento
posthoc_pwc2.2 <- selfesteem2.2 %>%
  group_by(time) %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
posthoc_pwc2.2

#ver el efecto del tiempo en cada nivel del tratamiento

#pairwise comparisons between time points

