#############################################################################
#
# PRACTICA R
#
# Expresión diferencial de genes de ratón
# Microarray de Affymetrix (Affymetrix Murine Genome U74A version 2 MG_U74Av2
# Origen de los datos: GEO GSE5583 (http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5583)
# Publicación: Mol Cell Biol 2006 Nov;26(21):7913-28.  16940178 (http://www.ncbi.nlm.nih.gov/pubmed/16940178)
#
# Muestras: 3 Wild Type x 3 Histone deacetylase 1 (HDAC1)
#
# R código original (credits): Ahmed Moustafa
#
#
##############################################################################

# Instalar RCurl

if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("RCurl")

# Si esto falla, que seguro lo hace tratar de instalarlo usando el menú, Paquetes, Servidor Spain A Coruña, RCurl

# Cargamos el paquete y los datos
library(RCurl)
url = getURL ("http://bit.ly/GSE5583_data", followlocation = TRUE)
data = as.matrix(read.table (text = url, row.names = 1, header = T))

# Chequeamos las dimensiones de los datos, y vemos las primeras y las últimas filas
dim(data) # nos dice cuantas filas y columnas tiene
head(data) #nos permite ver los primeros datos de la tabla, vemos las 3 primreas columnas: W(wildtype) y las 3 ultimas KO (knockout)
tail(data) #nos permite ver los ultioms datos de la tabla

# Hacemos un primer histograma para explorar los datos
hist(data, col = "gray", main="GSE5583 - Histogram"). #ten en cuenta que puedes cambiar los colores, es personalizable

# Transformamos los datos con un logaritmo 
# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve?
data2 = log2(data)
hist(data2, col = "gray", main="GSE5583 (log2) - Histogram")
# Al hacer la transformacion logaritma, el histograma cambia de forma, poedemos decir que el grafico a cambiado a una distribucion normal aparentemente
# Esta transformacion sirve para mejorar la visualizacion de los datos y que sea mas fácil de entender.


# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado?
# ¿Qué es un boxplot? Un boxplot es un gráfico que permite ver la distribución de nuestros datos dibujando la mediana, la dispersión y los valores atípicos (o outliers). 
# Los boxplots estan compuestos de los siguientes elementos: la caja que representa el rango intercuartilico (osea el rango entre los cuartiles Q1 y Q3) en el están el 50% de los datos. Además en la caja esta la mediana (indicada con una línea); los bigotes que son los datos maximos y minimos de la muestra; puntos (son los outliers) son los valores atipicos
boxplot(data2, col=c("blue", "blue", "blue",
	"orange", "orange", "orange"),
	main="GSE5583 - boxplots", las=2)
	#primero hemos cargado los datos con "data2", con el col hemos definido los colores separando por colores los wildtypes y los knockout, las=2 sirve para que los nombres de los titulos paarezcan en vertical

# Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación  de los valores de expresión. ¿Es correcta la separación?
hc = hclust(as.dist(1-cor(data2)))
plot(hc, main="GSE5583 - Hierarchical Clustering")
#nos sirve para agrupar los datos por perfiles similares. En este caso los wildtypes y los KO deberian estar agrupadas. Si se entremezclan significaría que los datos estan mal, osea que las muestras no tienen consistencia.
#Resultado: el gráfico generado separa Wt y KO, asique es correcto.



#######################################
# Análisis de Expresión Diferencial 
#######################################
head (data) #para ver la tabla
# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado?
#Al separar el data en wt y ko hemos generado dos subconjuntos de datos (que corresponden con los valores de expresion genica de cada uno).
# Son genes que tienen expresiones en replica de wt y ko. En este analisis el objetivo es que detecte una diferencia significativa entre estos dos  grupos, indicando su papel en la exxpresion.
wt <- data[,1:3]
ko <- data[,4:6]
class(wt)
head(wt)
head(ko)

# Calcula las medias de las muestras para cada condición. Usa apply
# para cada gen sacamos su media. Usamos el comando apply para calcular una funcion en todos los genes. (1 filas, 2 columnas)
wt.mean = apply(wt, 1, mean)
ko.mean = apply(ko, 1, mean)
# nos sale cada gen y su media, pero solo aparecen los primeros 6
head(wt.mean) 
head(ko.mean)

# ¿Cuál es la media más alta? Resultado: nos da=  [1] 37460.5
limit = max(wt.mean, ko.mean) #esto nos servira para poner en la cima del grafico el punto mas alto
limit

# Ahora hacemos un scatter plot (gráfico de dispersión)
plot(ko.mean ~ wt.mean, xlab = "WT", ylab = "KO",
	main = "GSE5583 - Scatter", xlim = c(0, limit), ylim = c(0, limit))
	#bigote significa asociacion, osea que asocie las medias de  ko y wt, los compara comparando el eje y con el eje x. xlab y ylab pone los titulos de los ejes respectivamente.
	
# Añadir una línea diagonal con abline
abline(0, 1, col = "red")


# ¿Eres capaz de añadirle un grid? Si, mediante el comando grid ()
grid()
#abline(a, b): línea de pendiente b y ordenada en el origen a
#abline(h=y): línea horizontal
#abline(v=x): línea vertical
abline(1, 2, col = "blue")     # línea y = 2x + 1
abline(h = 2, col = "green")  # línea y = 2
abline(v = 3, col = "violet") # línea x = 3
# con abline podemos poner lineas en nuestro grafico mediante las ecuaciones. Por ejemplo, x=3 añade una linea vertical. Además, podemos ponerle a cada linea un color con "col".
# Calculamos la diferencia entre las medias de las condiciones
diff.mean = wt.mean - ko.mean 


# Hacemos un histograma de las diferencias de medias
hist(diff.mean, col = "gray")

# Calculamos la significancia estadística con un t-test.
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test.
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué? Para mantener la diferencia real entre los datos , ya que al transformarlos logaritmicamente esta diferencia puede verse afectada.
# ¿Cuántas valores tiene cada muestra? Cada gen tiene 3 valores, osea tenemos 3 WT y 3 KO
pvalue = NULL 
tstat = NULL 
for(i in 1 : nrow(data)) { #Para cada gen queremos que cree un vector x con los 3 valores de wt y 3 valores de ko en el vector y .  i significa por cada gen.	

	x = wt[i,] # gene wt número i
	y = ko[i,] # gene ko número i

#Una vez tenemos estos datos hacemos la t-student . 
	# Hacemos el test
	t = t.test(x, y)
	
	#Luego, sacmamos los pvalue y lo guardamos : 
	# Añadimos el p-value a la lista
	pvalue[i] = t$p.value
	# Añadimos las estadísticas a la lista
	tstat[i] = t$statistic
}

head(pvalue)

# Ahora comprobamos que hemos hecho TODOS los cálculos
length(pvalue) # tenemos que tener la misma longitud que genes. En este caso no esta muy bien utilizar la t-student. No hemos evaluado la normalidad 
#Resultado= [1] 12488


# Hacemos un histograma de los p-values.
# ¿Qué pasa si le ponemos con una transformación de -log10? Esta transformacion hace que los valores pequeños resalten visualmente en el grafico, asi podemos identificar mas facilmente los genes mas significativos
hist(pvalue,col="gray") # pvalues brutos
hist(-log10(pvalue), col = "gray") #pvalues transformados con logaritmo en base 10

# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano")

# Queremos establecer que el mínimo para considerar una diferencia significativa, es con una diferencia de 2 y un p-value de 0.01
# ¿Puedes representarlo en el gráfico?
diff.mean_cutoff = 2
pvalue_cutoff = 0.01
abline(v = diff.mean_cutoff, col = "blue", lwd = 3)
#abline(v = -diff.mean_cutoff, col = "red", lwd = 3)
abline(h = -log10(pvalue_cutoff), col = "green", lwd = 3)
# Recuerda la   diferencia de medias. Los genes con la misma expresion en wt y ko estaran agrupados al rededor de 0, aquellos con mayor expresion de wt estaran en la derecha (eje positivo), si son ko estaran en la izq(eje negativo). 
# el corte de p value equivale a 0,01 (marcado como 2)

# Ahora buscamos los genes que satisfagan estos criterios (hemos filtrado nuestros datos con dos filtros: el de diferencia de medias y el de pvalue). Nos sale como resultado que 426 genes cumplen los dos filtros, que estos genes van a ser los genes de interes.
# Primero hacemos el filtro para la diferencia de medias (fold)
filter_by_diff.mean = abs(diff.mean) >= diff.mean_cutoff
dim(data[filter_by_diff.mean, ])
#Resultado= [1] 11859     6

# Ahora el filtro de p-value
filter_by_pvalue = pvalue <= pvalue_cutoff
dim(data[filter_by_pvalue, ])
#Resultado= [1] 426   6

# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios? Solo 426 genes
filter_combined = filter_by_diff.mean & filter_by_pvalue
filtered = data[filter_combined,]
dim(filtered)
head(filtered)

# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #2")
points (diff.mean[filter_combined], -log10(pvalue[filter_combined]),col = "red")

# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés? Por como se calculó la diferencia de medias (diff.mean = wt.mean - ko.mean)
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #3")
points (diff.mean[filter_combined & diff.mean < 0],
	-log10(pvalue[filter_combined & diff.mean < 0]), col = "red")
points (diff.mean[filter_combined & diff.mean > 0],
	-log10(pvalue[filter_combined & diff.mean > 0]), col = "blue")
#Si nos fijamos en la imagen  producida, los genes de interes aparecen coloreados (por encima del p value). 

# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap?
# los arboles son los clusters, 

# ¿Eres capaz de cambiar los colores del heatmap? Pista: usar el argumento col y hcl.colors
rowv = as.dendrogram(hclust(as.dist(1-cor(t(filtered)))))
colv = as.dendrogram(hclust(as.dist(1-cor(filtered))))
heatmap(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,labRow=FALSE)

heatmap(filtered)
# Hacemos el heatmap unicamente con los genes diferencialmente expresados. Ten en cuenta que si un gen esta sobreexpresado en WT estara reprimido en KO

# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#if (!requireNamespace("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install(c("gplots","RColorBrewer"))
install.packages("gplots")		
install.packages("RColorBrewer")	

library(gplots)
library(RColorBrewer)

# Hacemos nuestro heatmap
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row")

# Lo guardamos en un archivo PDF
pdf ("GSE5583_DE_Heatmap.pdf")
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row",labRow=FALSE)
dev.off()
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,col = redgreen(75), scale = "row",labRow=FALSE)

# Guardamos los genes diferencialmente expresados y filtrados en un fichero
write.table (filtered, "GSE5583_DE.txt", sep = "\t",quote = FALSE)
