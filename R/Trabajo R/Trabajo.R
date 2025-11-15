# NoraCPalmero_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento


# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)

#Compruebo si en el directorio que estoy se encuentra nuestro archivo
list.files(pattern = "datos_biomed.csv") #Efectivamente el archivo se encuentra en la misma carpeta.

# Cargamos los siguientes paquetes: ("tidyverse", "ggplot2", "patchwork", "car"))
library(tidyverse)
library(ggplot2)
library(patchwork)  
suppressPackageStartupMessages({
  if (requireNamespace("car", quietly = TRUE)) library(car)
})

# Guardar el archivo en "datos"
datos <- read.csv("datos_biomed.csv", stringsAsFactors = FALSE)

# Convertir la variable tratamiento a factor   
datos$Tratamiento <- factor(datos$Tratamiento,
                            levels = sort(unique(datos$Tratamiento)))

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
cat("\n--- Exploración inicial ---\n")
print(head(datos))     # Muestra las primeras filas
print(summary(datos)) # Resumen por columnas
dim(datos) # Devuelve la dimension de la tabla (filas, columnas)
str(datos) # Devuelve la estructura,osea el tipo de dato

#Se observa que hay 3 variables (glucosa, presión y colesterol) y 3 tratamientos (Fármaco A, Fármaco B y placebo)

#Lo siguiente da aún más información sobre el archivo, especificamente el número de columnas (5:ID, Tratamiento, Glucosa, Presion, Colesterol)
num_variables <- ncol(datos)
num_tratamientos <- nlevels(datos$Tratamiento)    # Nos dice cuantos tratamientos distintos existen (3:FarmacoA, FarmacoB, Placebo)
cat("Número de variables:", num_variables, "\n")
cat("Número de tratamientos:", num_tratamientos, " (", paste(levels(datos$Tratamiento), collapse=", "), ")\n", sep = "")


# Ahora antes de comenzar a crear gráficas, creo una carpeta llamada figuras  para guardarlas.
if (!dir.exists("figuras")) dir.create("figuras")

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
datos_long <- datos %>%
  pivot_longer(cols = c(Glucosa, Presion, Colesterol),
               names_to = "Variable", values_to = "Valor")


p_box <- ggplot(datos_long, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
  geom_boxplot(outlier.alpha = 0.6) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Boxplots por tratamiento",
       x = "Tratamiento", y = "Valor") +
  theme_bw() + theme(legend.position = "none")


print(p_box)
ggsave(filename = "figuras/01_boxplots_por_tratamiento.png", p_box,
       width = 9, height = 5, dpi = 300)


# Creo otro gráfico de boxplots con diferente distribución. Esta vez los tres boxplots se dividen por tipo de tratamiento.
# Reutilizo la versión "larga" de los datos
datos_long <- datos %>%
  pivot_longer(cols = c(Glucosa, Presion, Colesterol),
               names_to = "Variable", values_to = "Valor")

# Boxplots divididos por TRATAMIENTO (un panel por FarmacoA/FarmacoB/Placebo)
p_box_trat <- ggplot(datos_long, aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot(outlier.alpha = 0.6) +
  facet_wrap(~ Tratamiento, nrow = 1, scales = "free_y") +
  labs(title = "Boxplots por variable dentro de cada tratamiento",
       x = "Variable", y = "Valor") +
  theme_bw() + theme(legend.position = "none")

print(p_box_trat)
ggsave("figuras/01b_boxplots_por_tratamiento.png", p_box_trat,
       width = 10, height = 4, dpi = 300)

# CONCLUSIONES de los BOXPLOTS:
#FármacoA parece ser el tratamiento más efectivo para las tres variables analizadas.
#FármacoB tiene menor efecto sobre las variables que el otro fármaco.
#Placebo tiene menor variabilidad por lo que puede ser que no tenga gran efecto en las variables analizadas.



# 4. Realiza un violin plot (investiga qué es). (1 pt)
# Un violin plot muestra la distribución de los datos y su densidad de probabilidad, permitiendo comparar la forma  y la tendencia central de varias categorías.
# Voy a crear un violin plot por cada variable: glucosa, presión y  colesterol
# 4.a Violin plot de Glucosa
p_violin <- ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_jitter(width = 0.12, alpha = 0.5) +
  labs(title = "Distribución de Glucosa por tratamiento (violin)",
       x = "Tratamiento", y = "Glucosa") +
  theme_bw() + theme(legend.position = "none")

print(p_violin)
ggsave(filename = "figuras/02_violin_glucosa.png", p_violin,
       width = 7, height = 5, dpi = 300)


# 4.b violin plot de presión
p_violin_presion <- ggplot(datos, aes(x = Tratamiento, y = Presion, fill = Tratamiento)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_jitter(width = 0.12, alpha = 0.5, color = "black") +
  labs(title = "Distribución de Presión por tratamiento (violin)",
       x = "Tratamiento", y = "Presión") +
  theme_bw() + theme(legend.position = "none")

print(p_violin_presion)
ggsave("figuras/02b_violin_presion.png", p_violin_presion,
       width = 7, height = 5, dpi = 300)

# 4.c violin plot de colesterol
p_violin_colesterol <- ggplot(datos, aes(x = Tratamiento, y = Colesterol, fill = Tratamiento)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_jitter(width = 0.12, alpha = 0.5, color = "black") +
  labs(title = "Distribución de Colesterol por tratamiento (violin)",
       x = "Tratamiento", y = "Colesterol") +
  theme_bw() + theme(legend.position = "none")

print(p_violin_colesterol)
ggsave("figuras/02c_violin_colesterol.png", p_violin_colesterol,
       width = 7, height = 5, dpi = 300)

# CONCLUSIONES de los VIOLINPLOTS:
#a) Comparado con FarmacoB y Placebo,  FarmacoA tiene  una distribución de glucosa más amplia, lo que podría indicar que este tratamiento tiene un efecto variable, con algunos pacientes mostrando valores muy altos.  
#b) Farmaco A y el placebo tienen una distribucion similar con la mayoría de los valores alrededor de los 130-140 mmHg. Por otra parte, el FarmacoB muestra una mayor variabilidad en los niveles de presión arterial, lo que sugiere que puede tener un efecto menos controlado o predecible sobre la presión.
#c) El FarmacoA tiene un efecto más variable en los niveles de colesterol, lo que sugiere que el tratamiento puede tener diferentes efectos en diferentes individuos. Mientras qie el farmaco B y el placebo tienen distribuciones más estrechas y estables, lo que podría indicar que los efectos son más consistentes y predecibles en esos grupos.



# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
colores <- c("#1f77b4", "#ff7f0e", "#2ca02c")
levels_trat <- levels(datos$Tratamiento)
cols_por_trat <- setNames(colores[seq_along(levels_trat)], levels_trat)


plot(datos$Presion, datos$Glucosa,
     col = cols_por_trat[datos$Tratamiento], pch = 19,
     xlab = "Presión", ylab = "Glucosa",
     main = "Glucosa vs Presión")
legend("bottomright", legend = levels_trat,
       col = colores[seq_along(levels_trat)], pch = 19, bty = "n")


dev.copy(png, filename = "figuras/03_dispersion_glucosa_vs_presion.png",
         width = 800, height = 600, res = 120)
dev.off()


# CONCLUSIONES del gráfico de dispersión:
# No parece haber una relación directa entre Glucosa y Presión.
# La distribución de los puntos para FarmacoA y FarmacoB es más dispersa, lo que sugiere que estos tratamientos pueden tener efectos variables en ambas variables (glucosa y presión).
# Placebo tiene una dispersión más estrecha en ambas variables, lo que indica una menor variabilidad.


# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
# Un facet grid es una figura en la que cada celda del gráfico  representa un subconjunto de los datos, definidos por dos variables categóricas.
p_fac <- ggplot(datos, aes(x = Presion, y = Colesterol, color = Tratamiento)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ Tratamiento) +
  labs(title = "Colesterol vs Presión por tratamiento",
       x = "Presión", y = "Colesterol") +
  theme_bw()

print(p_fac)
ggsave(filename = "figuras/04_facetgrid_colesterol_vs_presion.png", p_fac,
       width = 10, height = 4, dpi = 300)

# CONCLUSIONES del facet grid:
#1.FarmacoA tiene una relación positiva clara entre Presión y Colesterol, esto sugiere que este tratamiento tiene un impacto directo en ambas variables. A medida que aumenta la presión arterial, el colesterol tiende a aumentar también en este grupo. Esto puede ser relevante al evaluar los efectos cardiovasculares del tratamiento.
#2.FarmacoB, en contraste, no muestra una relación clara entre Presión y Colesterol, lo que podría implicar que el tratamiento no tiene un efecto significativo sobre esta relación. Los pacientes tratados con FarmacoB tienen más variabilidad en sus datos que sugiere que el tratamiento puede estar afectando estas variables de manera menos predecible.
#3.Placebo muestra una relación negativa débil, lo que puede ser un efecto no deseado debido a la variabilidad en el grupo control. Sin embargo, como es de esperar, Placebo muestra menos efectos que los tratamientos farmacológicos, con una tendencia débil entre las variables.

# 7. Realiza un histogramas para cada variable. (0.5 pts)
p_hist <- ggplot(datos_long, aes(x = Valor, fill = Variable)) +
  geom_histogram(bins = 20, color = "white", alpha = 0.8) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Histogramas por variable", x = "", y = "Frecuencia") +
  theme_bw() + theme(legend.position = "none")

print(p_hist)
ggsave(filename = "figuras/05_histogramas.png", p_hist,
       width = 9, height = 5, dpi = 300)

# CONCLUSIONES del Histograma:
#Estos gráficos ayudan a identificar las distribuciones de los parámetros, lo que es útil para la elección de los métodos estadísticos.
#Los tres histogramas muestran distribuciones de datos que son asimétricas en general, con Colesterol y Presión mostrando más variabilidad, mientras que Glucosa parece más normal


# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
# Convertir a factor y fijar el orden de niveles (puedes cambiar el orden si tu profe lo pide)
datos$Tratamiento <- factor(
  datos$Tratamiento,
  levels = sort(unique(datos$Tratamiento))  # o c("FarmacoA","FarmacoB","Placebo")
)

# Comprobaciones útiles para el informe
str(datos$Tratamiento)                   # tendría que decir: Factor w/ 3 levels ... #Y efectivamente lo dice
levels(datos$Tratamiento)                # lista los niveles: nos da  "FarmacoA" "FarmacoB" "Placebo" 
table(datos$Tratamiento, useNA = "ifany")# recuento por tratamiento, resultando en : "FarmacoA=36" "FarmacoB=31" "Placebo=33" 


# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
# Media de glucosa por tratamiento
media_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos, FUN = mean)

# Desviación estándar de glucosa por tratamiento
sd_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos, FUN = sd)

# Combinamos ambas tablas en una sola
resumen_glu <- merge(media_glucosa, sd_glucosa, by = "Tratamiento")

# Renombramos columnas para mayor claridad
colnames(resumen_glu) <- c("Tratamiento", "Media_Glucosa", "SD_Glucosa")

# Mostramos resultados
print(resumen_glu)

#Los resultados son: 
#  Tratamiento    Media_Glucosa   SD_Glucosa
#  FarmacoA        110.4750        13.58309
#  FarmacoB        105.5839        12.15064
#  Placebo        103.0455         17.18486

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
placebo <- subset(datos, Tratamiento == "Placebo")
farmacoA <- subset(datos, Tratamiento == "FarmacoA")
farmacoB <- subset(datos, Tratamiento == "FarmacoB")

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
cat("\n--- Pruebas de normalidad (Shapiro-Wilk) en Glucosa por grupo ---\n")
by(datos$Glucosa, datos$Tratamiento, function(x) print(shapiro.test(x)))


# Homogeneidad de varianzas
if ("package:car" %in% search()) {
  cat("\nPrueba de Levene (más robusta):\n")
  print(car::leveneTest(Glucosa ~ Tratamiento, data = datos))
} else {
  cat("\nPaquete 'car' no disponible; uso Bartlett (sensible a no-normalidad):\n")
  print(bartlett.test(Glucosa ~ Tratamiento, data = datos))
}


# Elección del test global de medias
# (Regla sencilla): si todos los p de Shapiro > 0.05 y varianzas homogéneas -> ANOVA
# en caso contrario -> Kruskal-Wallis


p_shapiro <- by(datos$Glucosa, datos$Tratamiento, function(x) shapiro.test(x)$p.value)


todos_normales <- all(unlist(p_shapiro) > 0.05)


p_homog <- tryCatch({
  if ("package:car" %in% search()) {
    car::leveneTest(Glucosa ~ Tratamiento, data = datos)$`Pr(>F)`[1]
  } else {
    bartlett.test(Glucosa ~ Tratamiento, data = datos)$p.value
  }
}, error = function(e) NA)


var_homog <- !is.na(p_homog) && p_homog > 0.05


if (todos_normales && var_homog) {
  cat("\nSupuestos cumplidos -> ANOVA de una vía\n")
  fit <- aov(Glucosa ~ Tratamiento, data = datos)
  print(summary(fit))
  cat("\nComparaciones múltiples (Tukey HSD):\n")
  print(TukeyHSD(fit))
} else {
  cat("\nSupuestos no cumplidos -> Kruskal-Wallis\n")
  print(kruskal.test(Glucosa ~ Tratamiento, data = datos))
}


#CONCLUSIÓN:
#Los resultados indican que no existen diferencias significativas en los niveles de Glucosa entre los tres tratamientos (FarmacoA, FarmacoB y Placebo), ya que tanto el ANOVA como las comparaciones múltiples de Tukey HSD tienen valores p mayores que 0.05.
#Esto sugiere que,  los tratamientos no tienen efectos diferentes sobre los niveles de glucosa en la muestra estudiada.

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)

anova_glu <- aov(Glucosa ~ Tratamiento, data = datos)
cat("\n--- ANOVA (Glucosa ~ Tratamiento) ---\n")
print(summary(anova_glu))
cat("\nTukey HSD para ANOVA de Glucosa:\n")
print(TukeyHSD(anova_glu))

#CONCLUSIÓN:
#Los resultados del análisis de Tukey HSD muestran que no hay diferencias significativas entre los tratamientos en cuanto a los niveles de glucosa. 
#Los valores p ajustados para todas las comparaciones entre tratamientos son mayores que 0.05, osea que podemos pensar que los tratamientos no afectan significativamente los niveles de glucosa en los individuos estudiados.
