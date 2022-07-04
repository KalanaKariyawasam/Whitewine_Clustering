library(readxl)
library(factoextra)
library(ggplot2)
library(tidyverse)


#reading data set
Whitewine_v2 <- read_excel("Whitewine_v2.xlsx")

#data set summary
summary(whitewine_v2)

#Outlier Detection
outliers = c()

for (i in 1:11) {
  quantile1 <- quantile(whitewine_v2[[i]], probs = .25)
  quantile3 <- quantile(whitewine_v2[[i]], probs = .75)
  
  iqr <- quantile3 - quantile1
  
  bottom_outlier_rows = which(whitewine_v2[[i]] < quantile1 - (iqr * 1.5))
  top_outlier_rows = which(whitewine_v2[[i]] > quantile3 + (iqr * 1.5))
  
  outliers = c(outliers, top_outlier_rows[!top_outlier_rows %in% outliers])
  outliers = c(outliers, bottom_outlier_rows[!bottom_outlier_rows %in% outliers])
}

#Remove Outliers
cleanDat = whitewine_v2[-outliers, ]

#Scaling
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

scale_data <- as.data.frame(lapply(cleanDat[,-12], normalize))

boxplot(scale_data)

#elbow method
fviz_nbclust(scale_data, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

#silhouette method
fviz_nbclust(scale_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

k2 <- kmeans(scale_data, centers = 2, nstart = 25)

k2

fviz_cluster(k2, data = scale_data,
             palette = "jco",
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

fviz_cluster(list(data = scale_data, cluster = k2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic()
)

k3 <- kmeans(scale_data, centers = 3, nstart = 25)

k3

fviz_cluster(k3, data = scale_data,
             palette = "jco",
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

fviz_cluster(list(data = scale_data, cluster = k3$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic()
)

k4 <- kmeans(scale_data, centers = 4, nstart = 25)

k4

fviz_cluster(k4, data = scale_data,
             palette = "jco",
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

fviz_cluster(list(data = scale_data, cluster = k4$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic()
)


# CMatrix
pam.res <- pam(wineData_norm, 2)
fviz_cluster(pam.res)


exp_val.2 <- factor(cleanWhiteDat$quality)
prd_val.2 <- factor(k2$cluster)
CrossTable(exp_val.2, prd_val.2)

#PCA

pca = prcomp(cleanDat[,-12], center = TRUE, scale = TRUE)

summary(pca)

plot(pca)

biplot(pca)

transform = as.data.frame(-pca$x[,9:11])

head (transform)

pca_k <- kmeans(transform, centers = 2, nstart = 25)

fviz_cluster(pca_k, data = transform,
             palette = "jco",
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

pca_k