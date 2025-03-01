
# Se junta estos dos datos, el id con el precio de la vivienda con el resto de datos para poder
#hacer un analisis extenso ante los datos de entrenamiento y prueba.
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering
library(hopkins) #Para revisar si vale la pena hacer agrupamiento
library(GGally) #Para hacer el conjunto de graficos
library(FeatureImpCluster) #Para revisar la importancia de las variables en los grupos.
library(pheatmap) #Para hacer mapa de calor



data <- read.csv("test.csv")
datasample <- read.csv("sample_submission.csv")
datatrain <- read.csv("train.csv")

data_analysis <- merge(data, datasample, by="Id")

data_analysis <- rbind(data_analysis, datatrain)


#Hacer agrupamiento con datos numericos
data_analysis <- data_analysis[,  c("MSSubClass", "LotFrontage", "LotArea",  "OverallQual", "OverallCond",
                 "YearBuilt", "YearRemodAdd", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", 
                 "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", 
                 "GrLivArea", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath" ,
                 "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageYrBlt"
                 , "GarageCars", "GarageArea", "WoodDeckSF" , "OpenPorchSF", "EnclosedPorch"
                 , "X3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal", "MoSold", "YrSold",
                 "SalePrice")]

na.omit(data_analysis)

#escalado de datos para ajustarlo a rangos especificos
data_analysis <- scale(data_analysis)
data_hop <- scale(data_analysis)


#Realizar hopkins para ver si podemos agruparlos o no
hopkins(data_hop)
#El valor del estadístico de hopkins está alejado de 0.5 por lo que los datos no son aleatorios hay altas posibilidades de que sea factible el agrupamiento.  
#Ademas vemos que alcanza a 1 lo cual significa que si hay tendencia fuerte de agrupamientos

#1`
datos_dist<- dist(data_hop)
fviz_dist(datos_dist, show_labels = F)




wss=0
for (i in 1:10) 
  wss[i] <- sum(kmeans(na.omit(data_analysis), centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")


fviz_nbclust(na.omit(data_analysis), kmeans, method = "wss") +
  labs(subtitle = "Elbow method")


#en este caso podemos observar que el numero de clusters a elegir es 3 como se obseva en la grafica ya esta empezando a reducir su decenso a llegar a uno estable


km<-kmeans(data_analysis,3,iter.max =100)

data$grupo<-km$cluster


#CLUSTERING Y PCA

