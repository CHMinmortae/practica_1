install.packages("boot")
install.packages("Rtools")
install.packages("ggsignif")

library(ggsignif)
library(boot)
library(dunn.test)
library(ggplot2)
library(Rmisc)
library(ICSNP)
library(corpcor)
library(ICS)
library(mvtnorm)
library(Hotelling)
library(permute)
library(lattice)
library(vegan)
library(aplpack)
library(factoextra)
library(car)
library(MASS)
library(dendextend)
library(circlize)
library(rattle)
library(randomForest)
library(car)
library(carData)
library(psych)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(stats)
library(datarium)
library(ggsignif)
library(installr)
library(MVN)
library(ggplot2)
library(gridExtra)
library(agricolae)
library(rcompanion)
library(ggsignif)


#AQUÍ INSERTE LA BASE DATOS CON EL IMPORT# XQ NO ME DEJA PONERLA EN EL SCRIPT Y 
#ME MARCA UN ERROR, ENTONCES CON LA OPCIÓN DE "IMPORT DATASET"


#MULTIVARIADA
datos_final<-datas_ants_rstudio_defff_xd
datos_final
dats_ants_sintrataz<-datas_ants_rstudio_defff_xd[ ,1:5]
dats_ants_sintrataz
dats_ants_sintrataz_dos<-dats_ants_sintrataz[ ,-2]
dats_ants_sintrataz_dos
##MEDIDAS_REPETIDAS##
###ACOMODO DE LOS DATOS###

datos_final_acomod <- dats_ants_sintrataz_dos %>%
  gather(key = "time", value = "score", day_1, day_2, day_3) %>%
  convert_as_factor(ant, time)
datos_final_acomod
head(datos_final_acomod, 3)
#Promedios
datos_final_acomod%>%
  group_by(time) %>%
  get_summary_stats(score, type = "mean_sd")
#esfericidad
res.aov_ants <- anova_test(data = datos_final_acomod, dv = score, wid = ant, within = time)
get_anova_table(res.aov_ants)  # es significativo (p=6.5e-07)
#Boxplot_uno_SENCILLO
bxp_ants_reps <- ggboxplot(datos_final_acomod, x = "time", y = "score", add = "point")
bxp_ants_reps
#Outliners
datos_final_acomod %>%
  group_by(time) %>%
  identify_outliers(score)
#SupuestoII
ggqqplot(datos_final_acomod, "score", facet.by = "time") #no se salen de la marca so esta bien
#Post-hoc #Pruebas T
pwc_ants <- datos_final_acomod %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
#GRAFICA BONITA#
pwc_ants
bxp_ants_reps_colores_8 +
  stat_compare_means(comparisons = list(c("day_1", "day_2"), c("day_1", "day_3"), c("day_2", "day_3")),
                     method = "t.test",
                     label = "p.format",
                     hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov_ants, detailed = TRUE),
    caption = get_pwc_label(pwc_ants)
  )
###ANOSIM###
datos_anosim<-datas_ants_rstudio_defff_xd[,-3]
datos_anosim
datos_anosim_2<-datos_anosim[,-3]
datos_anosim_2
datos_anosim_3<-datos_anosim_2[,-3]
datos_anosim_3
datos_anosim_4<-datos_anosim_3[,-1]
datos_anosim_4

#MATRIZ
matriz_disimilitud <- dist(datos_anosim_4)
matriz_disimilitud

#ANALISIS
resultado_anosim <- anosim(matriz_disimilitud, tratamiento)
print(resultado_anosim)
analisis_anosim<-summary(resultado_anosim)
plot(resultado_anosim) # demuestra alta disimilitud entre los grupos con P=0.001, es significativo

###UNIVARIADOS####
#ANCOVA
#Hacer data para eso
mancova_dat<-datos_anosim_4[,-3]
mancova_dat
mancova_dat_2<-mancova_dat[,-3]
mancova_dat_2
mancova_dat_3<-mancova_dat_2[,-3]
mancova_dat_3
mancova_dat_4<-mancova_dat_3[,-3]
mancova_dat_4
mancova_dat_5<-mancova_dat_4[,-3]
mancova_dat_5
mancova_dat_6<-mancova_dat_5[,-3]
mancova_dat_6
mancova_dat_7<-mancova_dat_6[,-3]
mancova_dat_7
mancova_dat_8<-mancova_dat_7[,-3]
mancova_dat_8
mancova_dat_9<-mancova_dat_8[,-3]
mancova_dat_9

#ANALISIS
variable_dependiente <- mancova_dat_9$prom_days
variable_independiente <- mancova_dat_9$trat
covariante <- mancova_dat_9$artejo_segmentos
resultado_ancova <- lm(variable_dependiente ~ variable_independiente + covariante, data = mancova_dat_9)
resultado_ancova
summary(resultado_ancova)
#Supuestos
#Prueba de Shapiro-Wilk
shapiro.test(resultado_ancova$residuals) #NO ES POSIBLE

#kRUSKA#
resultado_kruska_nice<- kruskal.test(variable_dependiente~variable_independiente,mancova_dat_9)
print(resultado_kruska_nice)

data_plot_kruska <- data.frame(
  Tratamiento = factor(mancova_dat_9$variable_independiente),
  Variable_Dependiente = mancova_dat_9$variable_dependiente
)

boxplot <- ggplot(mancova_dat_9, aes(x = variable_independiente, y = variable_dependiente)) +
  geom_boxplot() +
  labs(x = "Tratamiento", y = "Variable Dependiente") +
  theme_bw()
boxplot

boxplot_bas <- ggplot(mancova_dat_9, aes(x = variable_independiente, y = variable_dependiente)) +
  geom_boxplot() +
  labs(x = "Tratamiento", y = "Variable Dependiente") +
  theme_bw() +
  facet_wrap(~ variable_independiente, scales = "free")

# Mostrar el gráfico
print(boxplot_bas)


boxplot_bas <- ggplot(mancova_dat_9, aes(x = trat, y = prom_days)) +
  geom_boxplot() +
  labs(x = "Tratamiento", y = "Variable Dependiente") +
  theme_bw() +
  facet_wrap(~ trat, scales = "free")

# Mostrar el gráfico
print(boxplot_bas)


boxplot_data <- data.frame(Treatment = mancova_dat_9$variable_independiente, 
                           Dependent_Variable = mancova_dat_9$variable_dependiente)

boxplot_plot <- ggplot(mancova_dat_9, aes(x = trat, y = variable_dependiente)) +
  geom_boxplot() +
  labs(x = "Tratamiento", y = "Promedio días") +
  theme_bw() +
  facet_wrap(~ trat, ncol = 1)
print(boxplot_plot)
mancova_dat_9$trat <- factor(mancova_dat_9$trat)

boxplot_plot <- ggplot(mancova_dat_9, aes(x = trat, y = variable_dependiente)) +
  geom_boxplot() +
  labs(x = "Tratamiento", y = "Promedio días") +
  theme_bw() +
  facet_wrap(~ trat, ncol = 1)
print(boxplot_plot) #este es el bueno

#kruska para tratamiento y consumo
kruska2<-kruskal.test(consumo~tratamiento,datas_ants_rstudio_defff_xd)
kruska2

boxplot_plot_2 <- ggplot(datas_ants_rstudio_defff_xd, aes(x = trat, y = consumo)) +
  geom_boxplot() +
  labs(x = "Tratamiento", y = "consumo") +
  theme_bw() +
  facet_wrap(~ trat, ncol = 1)
print(boxplot_plot_2) 

#Kruska para tratamiento y tiempo consumo
kruska3<-kruskal.test(time_consumo~tratamiento,datas_ants_rstudio_defff_xd)
kruska3
boxplot_plot_3 <- ggplot(datas_ants_rstudio_defff_xd, aes(x = trat, y = time_consumo)) +
  geom_boxplot() +
  labs(x = "Tratamiento", y = "Tiempo de consumo") +
  theme_bw() +
  facet_wrap(~ trat, ncol = 1)
print(boxplot_plot_3) 

#kruska para tratamiento y veces regresa
kruska4<-kruskal.test(veces_regresa~tratamiento,datas_ants_rstudio_defff_xd)
kruska4
boxplot_plot_4 <- ggplot(datas_ants_rstudio_defff_xd, aes(x = trat, y = veces_regresa)) +
  geom_boxplot() +
  labs(x = "Tratamiento", y = "Veces que regresa") +
  theme_bw() +
  facet_wrap(~ trat, ncol = 1)
print(boxplot_plot_4) 
#kruska para tratamiento y tiempo custodia
kruska5<-kruskal.test(tiempo_custodia~tratamiento,datas_ants_rstudio_defff_xd)
kruska5
boxplot_plot_5 <- ggplot(datas_ants_rstudio_defff_xd, aes(x = trat, y = tiempo_custodia)) +
  geom_boxplot() +
  labs(x = "Tratamiento", y = "Veces que regresa") +
  theme_bw() +
  facet_wrap(~ trat, ncol = 1)
print(boxplot_plot_5) 


####CORRELACIÓN_SPEARMAN### 
correlation_consumo_horizontal <- cor(datas_ants_rstudio_defff_xd$consumo, datas_ants_rstudio_defff_xd$crecimiento_gaster_horizontal
                                 , method = "spearman")
correlation_consumo_horizontal


scatter_plot_conhor <- ggplot(datas_ants_rstudio_defff_xd, aes(x = consumo, y = crecimiento_gaster_horizontal)) +
  geom_point() +
  labs(x = "Consumo", y = "Crecimiento gaster horizontal") +
  theme_bw()

# Agregar la línea de tendencia
scatter_plot_with_line <- scatter_plot_conhor +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = max(datas_ants_rstudio_defff_xd$consumo), y = max(datas_ants_rstudio_defff_xd$crecimiento_gaster_horizontal),
           label = paste("Correlación:", round(correlation_consumo_horizontal, 2)), hjust = 1, vjust = -1)

# Mostrar el gráfico
print(scatter_plot_with_line)

correlation_consumo_vertical <- cor(datas_ants_rstudio_defff_xd$consumo, datas_ants_rstudio_defff_xd$crecimiento_gaster_vertical
                                      , method = "spearman")
correlation_consumo_vertical

scatter_plot_conver <- ggplot(datas_ants_rstudio_defff_xd, aes(x = consumo, y = crecimiento_gaster_vertical)) +
  geom_point() +
  labs(x = "Consumo", y = "Crecimiento gaster vertical") +
  theme_bw()
scatter_plot_with_line_2 <- scatter_plot_conver +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = max(datas_ants_rstudio_defff_xd$consumo), y = max(datas_ants_rstudio_defff_xd$crecimiento_gaster_vertical),
           label = paste("Correlación:", round(correlation_consumo_vertical, 2)), hjust = 1, vjust = -1)
print(scatter_plot_with_line_2)

citation
citation()
