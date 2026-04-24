df<- read.csv("/Users/ad??n??z_k??sm??/desktop/crocodile_dataset.csv")
df
summary(df)
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("ggplot2")
install.packages("dendextend")
install.packages("corrplot")
library(corrplot)
library(dendextend)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
colnames(df)

# 2. Veri Cn D0Eleme
# KC<meleme iC'in sadece sayD1sal verileri (Uzunluk ve ADD1rlD1k) alD1yoruz
# ID veya D0sim gibi etiketleri C'D1karD1yoruz
data_clustering <- df %>%
  select("Observed.Length..m.", "Observed.Weight..kg.") %>%
  na.omit() # KayD1p veri varsa temizle

# Verileri C6lC'eklendirme (Scaling) - KC<meleme iC'in C'ok C6nemlidir
data_scaled <- scale(data_clustering)

# ---------------------------------------------------------
# 3. YC6ntemlerin UygulanmasD1
# ---------------------------------------------------------

# Optimum kC<me sayD1sD1nD1 belirlemek iC'in (Genelde 2 veya 3 C'D1kacaktD1r)
# Elbow metodu ile kontrol edelim:
fviz_nbclust(data_scaled, kmeans, method = "wss") +
  labs(title = "Optimum KC<me SayD1sD1 (Elbow Metodu)")

# Biz karED1laEtD1rma iC'in k = 3 seC'elim (KC<C'C<k, Orta, BC<yC<k gibi dC<EC<nebiliriz)
k <- 3 

# --- YC6ntem A: K-Means ---
set.seed(123)
km_res <- kmeans(data_scaled, centers = k, nstart = 25)
# --- YC6ntem B: K-Medoids (PAM) ---
pam_res <- pam(data_scaled, k = k)
# --- YC6ntem C: HiyerarEik KC<meleme ---
dist_mat <- dist(data_scaled, method = "euclidean")
hc_res <- hclust(dist_mat, method = "ward.D2")
# HiyerarEik aDacD1 k gruba kesme
grp_hc <- cutree(hc_res, k = k)

# ---------------------------------------------------------
# 4. GC6rselleEtirme ve KarED1laEtD1rma
# ---------------------------------------------------------

# K-Means GrafiDi
p1 <- fviz_cluster(km_res, data = data_scaled, geom = "point",
                   ellipse.type = "convex", 
                   ggtheme = theme_bw(),
                   main = "K-Means SonuC'larD1")

# K-Medoids GrafiDi
p2 <- fviz_cluster(pam_res, data = data_scaled, geom = "point",
                   ellipse.type = "convex", 
                   ggtheme = theme_bw(),
                   main = "K-Medoids (PAM) SonuC'larD1")

# HiyerarEik KC<meleme GrafiDi
# HiyerarEik sonuC'larD1 fviz_cluster formatD1na uyarlayalD1m
p3 <- fviz_cluster(list(data = data_scaled, cluster = grp_hc),
                   geom = "point", 
                   ellipse.type = "convex", 
                   ggtheme = theme_bw(),
                   main = "HiyerarEik KC<meleme SonuC'larD1")

# Grafikleri gC6ster
print(p1)
print(p2)
print(p3)

# ---------------------------------------------------------
# 5. Hangisi En MantD1klD1? (Silhouette Analizi)
# ---------------------------------------------------------

# Silhouette skorlarD1nD1 hesaplayarak hangisinin daha iyi ayrD1EtD1DD1nD1 bulalD1m
sil_kmeans <- silhouette(km_res$cluster, dist(data_scaled))
sil_pam <- silhouette(pam_res$clustering, dist(data_scaled))
sil_hc <- silhouette(grp_hc, dist(data_scaled))

mean_sil_kmeans <- mean(sil_kmeans[, 3])
mean_sil_pam <- mean(sil_pam[, 3])
mean_sil_hc <- mean(sil_hc[, 3])

cat("Ortalama Silhouette SkorlarD1 (1'e ne kadar yakD1nsa o kadar iyi):\n")
cat("K-Means: ", mean_sil_kmeans, "\n")
cat("K-Medoids: ", mean_sil_pam, "\n")
cat("HiyerarEik: ", mean_sil_hc, "\n")

if(mean_sil_hc > mean_sil_kmeans & mean_sil_hc > mean_sil_pam){
  cat("\nSONUC: HiyerarEik KC<meleme bu veri seti iC'in en iyi sonucu veriyor.\n")
} else if(mean_sil_pam > mean_sil_kmeans){
  cat("\nSONUC: K-Medoids bu veri seti iC'in en iyi sonucu veriyor.\n")
} else {
  cat("\nSONUC: K-Means bu veri seti iC'in en iyi sonucu veriyor.\n")
}









# Verinin hazD1r olduDundan emin olalD1m (Cnceki adD1mlardan)
# data_scaled <- scale(...) # Bu kD1smD1n zaten hafD1zada olduDunu varsayD1yorum

# Mesafe matrisini hesaplayalD1m (SilC<et hesabD1 iC'in EarttD1r)
dist_mat <- dist(data_scaled, method = "euclidean")

# --- 1. K-Means D0C'in DC<zeltilmiE KD1sD1m ---
set.seed(123)
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# CNEMLD0 ADIM: Cnce silC<et skorunu hesaplD1yoruz
sil_km <- silhouette(km_res$cluster, dist_mat)


# Eimdi grafiDi, hesapladD1DD1mD1z 'sil_km' nesnesi ile C'iziyoruz
p1 <- fviz_silhouette(sil_km, palette = "jco",
                      ggtheme = theme_classic(),
                      main = "K-Means SilC<et GrafiDi")

# --- 2. K-Medoids (PAM) ---
# pam() fonksiyonu silC<eti kendi iC'inde hesapladD1DD1 iC'in bu genelde hata vermez
pam_res <- pam(data_scaled, k = 3)
p2 <- fviz_silhouette(pam_res, palette = "jco",
                      ggtheme = theme_classic(),
                      main = "K-Medoids (PAM) SilC<et GrafiDi")

# --- 3. HiyerarEik KC<meleme D0C'in DC<zeltilmiE KD1sD1m ---
hc_res <- hclust(dist_mat, method = "ward.D2")
grp_hc <- cutree(hc_res, k = 3)

# CNEMLD0 ADIM: HiyerarEik iC'in de silC<eti elle hesaplD1yoruz
sil_hc <- silhouette(grp_hc, dist_mat)

p3 <- fviz_silhouette(sil_hc, palette = "jco",
                      ggtheme = theme_classic(),
                      main = "HiyerarEik KC<meleme SilC<et GrafiDi")

# --- Grafiklerin GC6sterimi ---
print(p1)
print(p2)
print(p3)






# Gerekli mesafe matrisini ve hiyerarEik kC<meyi hazD1rlayalD1m
# (Cnceki adD1mlardan zaten hafD1zada olabilir ama garanti olsun)
dist_mat <- dist(data_scaled, method = "euclidean")
hc_res <- hclust(dist_mat, method = "ward.D2") # Ward metodunu kullanmD1EtD1k

# 1. Kofenetik Mesafeleri Hesaplama
# Dendrogram C<zerindeki uzaklD1klarD1 C'D1karD1r
coph_dist <- cophenetic(hc_res)

# 2. Korelasyon KatsayD1sD1nD1 Hesaplama
# Orijinal uzaklD1klar ile dendrogram uzaklD1klarD1 arasD1ndaki iliEki
res_cor <- cor(dist_mat, coph_dist)

cat("Kofenetik Korelasyon KatsayD1sD1:", res_cor, "\n")
cat("Not: Bu deDer 1'e ne kadar yakD1nsa, hiyerarEik aDaC' veriyi o kadar iyi temsil ediyor demektir.\n")

# 3. Kofenetik Korelasyon GrafiDi (Shepard Plot) Cizimi
plot(dist_mat, coph_dist,
     main = paste("Shepard GrafiDi (Korelasyon =", round(res_cor, 3), ")"),
     xlab = "Orijinal UzaklD1klar (Euclidean)",
     ylab = "Kofenetik UzaklD1klar (Dendrogram)",
     pch = 19, col = rgb(0.2, 0.4, 0.6, 0.5), # Hafif Eeffaf mavi noktalar
     cex = 0.5)

# D0liEkiyi gC6steren kD1rmD1zD1 regresyon doDrusunu ekleyelim
abline(lm(coph_dist ~ dist_mat), col = "red", lwd = 2)
#0.77 deDeri C'D1ktD1 0.7 C<stC< iyi bir kC<meleme sayD1lD1r







# ClC'eklendirme (StandartlaEtD1rma)
data_scaled <- scale(data_clustering)

# 2. Mesafe Matrisini Hesaplama
# Cklid (Euclidean) mesafesi kullanD1lD1r
dist_mat <- dist(data_scaled, method = "euclidean")

# 3. Ward Metodu ile HiyerarEik KC<meleme
# R'da Ward metodunun gC<ncel ve doDru versiyonu "ward.D2"dir.
hc_ward <- hclust(dist_mat, method = "ward.D2")

# 4. Dendrogram Cizimi (GC6rselleEtirme)
# KC<meleri renklendirmek iC'in k = 3 seC'iyoruz (Analizlerimize gC6re optimum sayD1)
fviz_dend(hc_ward, 
          k = 3,                 # KC<me sayD1sD1
          cex = 0.5,             # Etiket boyutu (C'ok veri olduDu iC'in kC<C'C<k tuttum)
          k_colors = "jco",      # Renk paleti
          color_labels_by_k = TRUE, 
          rect = FALSE,           # KC<meleri kutu iC'ine al
          main = "HiyerarEik KC<meleme DendrogramD1 (Ward Metodu)",
          ylab = "UzaklD1k (Height)")









# ClC'eklendirme
data_scaled <- scale(data_clustering)

# 2. Mesafe Matrisi
dist_mat <- dist(data_scaled, method = "euclidean")

# 3. Average Metodu ile KC<meleme
# method parametresini "average" yapD1yoruz
hc_avg <- hclust(dist_mat, method = "average")

# 4. Kofenetik Korelasyon HesabD1 (BaEarD1 C6lC'C<mC< iC'in)
# Average metodunun veriyi ne kadar iyi temsil ettiDini gC6relim
coph_avg <- cophenetic(hc_avg)
cor_avg <- cor(dist_mat, coph_avg)

cat("Average Metodu iC'in Kofenetik Korelasyon:", cor_avg, "\n")

# 5. Dendrogram Cizimi
# GC6rselde kesim C'izgisini (3 kC<me iC'in) hesaplayalD1m
yukseklikler <- sort(hc_avg$height, decreasing = TRUE)
kesme_noktasi <- (yukseklikler[2] + yukseklikler[3]) / 2

fviz_dend(hc_avg, 
          k = 3,                 
          cex = 0.5,             
          k_colors = "jco",
          color_labels_by_k = TRUE, 
          rect = FALSE,
          main = paste("HiyerarEik KC<meleme (Average Metodu) - Kor:", round(cor_avg, 3))) +
  geom_hline(yintercept = kesme_noktasi, linetype = "dashed", color = "gray")






library(tidyverse)

# 1. Veriyi KC<C'C<ltme (Crneklem Alma)
# Grafik okunabilir olsun diye rastgele 30 veri seC'iyoruz
set.seed(123) # Sonucun hep aynD1 C'D1kmasD1 iC'in
ornek_index <- sample(1:nrow(df_temiz), 30)
df_sample <- df_temiz[ornek_index, ]

# Sadece sayD1sal verileri alD1p C6lC'ekleyelim
data_sample <- df_sample %>% 
  select("Observed.Length..m.", "Observed.Weight..kg.")
data_scaled_sample <- scale(data_sample)
dist_sample <- dist(data_scaled_sample)

# 2. D0ki FarklD1 Modeli OluEturma
# Model A: Ward Metodu
hc_ward <- hclust(dist_sample, method = "ward.D2")

# Model B: Average Metodu
hc_avg <- hclust(dist_sample, method = "average")

# 3. Dendrogram Nesnesine Cevirme
dend1 <- as.dendrogram(hc_ward)
dend2 <- as.dendrogram(hc_avg)

# GC6rsellik: DallarD1 3 kC<meye gC6re renklendirelim
dend1 <- color_branches(dend1, k = 3)
dend2 <- color_branches(dend2, k = 3)

# 4. Tanglegram (KarED1laEtD1rma GrafiDi) Cizimi
tanglegram(dend1, dend2,
           common_subtrees_color_lines = FALSE, # Ortak alt aDaC'larD1 renklendirme
           highlight_distinct_edges  = FALSE, 
           highlight_branches_lwd = FALSE, 
           margin_inner = 7,       # Ortadaki boEluk
           lwd = 2,                # Cizgi kalD1nlD1DD1
           main_left = "Ward Metodu",
           main_right = "Average Metodu",
           lab.cex = 0.8           # YazD1 boyutu
)

# 5. Benzerlik Skoru (Entanglement)
# 0'a ne kadar yakD1nsa o kadar iyi (daha az karD1ED1k) demektir.
skor <- entanglement(dend1, dend2)
cat("DolaED1klD1k Skoru (Entanglement):", skor, "\n(0 = MC<kemmel Uyum, 1 = Tamamen FarklD1)\n")


# Gerekli kC<tC<phaneleri yC<kle



# 1. Veri HazD1rlD1DD1 (HD1z iC'in C6rneklem alD1yoruz)
# 1000 satD1rlD1k veride dendrogram korelasyonu hesaplamak C'ok uzun sC<rebilir.
# Bu yC<zden rastgele 50-100 veri ile test etmek en saDlD1klD1sD1dD1r.
set.seed(123)
ornek_index <- sample(1:nrow(df_temiz), 50) # 50 timsah seC'iyoruz
df_sample <- df_temiz[ornek_index, ]

# Sadece sayD1sal sC<tunlar ve C6lC'ekleme
data_sub <- df_sample %>% select("Observed.Length..m.", "Observed.Weight..kg.")
dist_sub <- dist(scale(data_sub))

# 2. FarklD1 YC6ntemlerle Dendrogramlar OluEturma
# KarED1laEtD1rmak istediDimiz yC6ntemleri listeliyoruz
hc_ward     <- hclust(dist_sub, method = "ward.D2")
hc_average  <- hclust(dist_sub, method = "average")
hc_complete <- hclust(dist_sub, method = "complete")
hc_single   <- hclust(dist_sub, method = "single")
hc_mcquitty <- hclust(dist_sub, method="mcquitty")
# Hepsini bir dendrogram listesinde (dendlist) topluyoruz
dend_listesi <- dendlist(
  "Ward"      = as.dendrogram(hc_ward),
  "Average"   = as.dendrogram(hc_average),
  "Complete"  = as.dendrogram(hc_complete),
  "Single"    = as.dendrogram(hc_single),
  "McQuitty"  = as.dendrogram(hc_mcquitty)
)

# 3. Korelasyon Matrisini Hesaplama
# "cophenetic": ADaC' C<zerindeki mesafelerin korelasyonu (En yaygD1n yC6ntem)
cor_matris <- cor.dendlist(dend_listesi, method = "cophenetic")

# Matrisi ekrana yazdD1ralD1m
print("Dendrogram Korelasyon Matrisi:")
print(round(cor_matris, 3))

# 4. Corrplot ile GC6rselleEtirme (Pie Chart)
# method = "pie" diyerek pasta grafiDi istiyoruz
corrplot(cor_matris, 
         method = "pie",       # Pasta grafiDi
         type = "lower",       # Sadece alt C<C'geni gC6ster (tekrarD1 C6nlemek iC'in)
         tl.col = "black",     # Etiket rengi
         tl.srt = 45,          # Etiket aC'D1sD1
         diag = FALSE,         # KC6Eegenleri (kendisiyle korelasyonu) gC6sterme
         title = "Dendrogramlar ArasD1 Korelasyon",
         mar = c(0,0,2,0)      # BaElD1k iC'in kenar boEluDu
)




data_clustering <- df_temiz %>%
  select("Observed.Length..m.", "Observed.Weight..kg.")
data_scaled <- scale(data_clustering)

# 2. HiyerarEik KC<meleme (Ward Metodu)
dist_mat <- dist(data_scaled, method = "euclidean")
hc_res <- hclust(dist_mat, method = "ward.D2")

# 3. Dairesel Dendrogram Cizimi
fviz_dend(hc_res, 
          k = 3,                 # 3 FarklD1 kC<me rengi
          k_colors = "jco",      # Renk paleti (Alternatif: "npg", "aaas", "lancet")
          type = "circular",     # D0ETE BU KOD DAD0RESEL YAPAR
          repel = TRUE,          # Etiketlerin C<st C<ste binmesini engeller (eDer aC'D1ksa)
          show_labels = FALSE,   # 1000 veri olduDu iC'in isimleri gizliyoruz (yoksa simsiyah olur)
          rect = TRUE,           # KC<melerin etrafD1na C'erC'eve C'izer
          rect_border = "jco",   # CerC'eve renkleri
          rect_fill = TRUE,      # CerC'evelerin iC'ini hafif boya
          main = "Timsah Verisi - Dairesel HiyerarEik KC<meleme"
)

