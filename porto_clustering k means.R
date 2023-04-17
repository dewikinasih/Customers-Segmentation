
library(cluster)
library(factoextra)
library(tidyverse)

library(readxl)
data = read_excel('kemiskinan 2022.xlsx')
View(data)

# eksplorasi data
str(data)
colSums(is.na(data))
anyNA(data) # CEK MISSING
colnames(data) = c('provinsi','P1','P2','P0','TPT','AMH','RLS')
prov = c(data$provinsi)
data = data[,-1]
rownames(data) = prov


summary(data)

# transformasi normalisasi standardisasi krn satuannya beda-beda
trans = scale(data, scale = TRUE)
View(trans)
summary(trans)

#====================================================================================
#1 non hierarki k-means
# nentuin jml klaster
# misal pake metode elboW
# YG DIliat titik belok/sikunya adlh jml optimal dr klasternya
fviz_nbclust(trans, kmeans, method = 'wss')

# cara dua pake silhouette
# liat titik maksimumnya
fviz_nbclust(trans, kmeans)

# clustering
set.seed(1)
hasil = kmeans(trans,2)
# liat hasil clustering
hasil
hasil$centers

# visualisasi
fviz_cluster(hasil,trans)

#===========================================================================================
# 2 hierarki agglomerative
cor(data) #matriks korelasi antar variabel, sebagian besar tdk trdpt korelasi tinggi shg no multikolinearitas

# perhitungan jarak DIS(SIMILARITY) DATA
jarak = dist(trans, method = 'euclidean')
jarak

# milih cluster terbaik
m = c('single','complete','average','ward')
names(m) = m
ac = function(x){
  agnes(jarak, method=x)$ac
}
map_dbl(m,ac)


# ward koefisien agglomerative nya paling mendekati 1
ward = hclust(jarak,method = 'ward.D')
plot(ward,cex=1)

ward_clust = cutree(ward, k = 5)
head(ward_clust)
table(ward_clust)
fviz_dend(ward, k = 5, k_colors = 'jco')



#karakteristik tiap kluster
tabel = data.frame(data,cluster=ward_clust)
karak = aggregate(tabel[,-7], list(tabel$cluster),mean)
colnames(karak)[1] = 'cluster' 
karak

anggota = data.frame(provinsi=rownames(tabel),cluster=ward_clust)

write.csv(karak,'karakteristik kluster 22.csv', row.names = F)



