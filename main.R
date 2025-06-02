# Load libraries
library(dplyr)
library(factoextra)
library(cluster)
library(corrplot)

# Load dataset
df <- Student_Performance_Metrics_Dataset

# Cek struktur dan nilai unik variabel kategorikal
str(df)
unique(df$Department)
unique(df$Income)
unique(df$Hometown)
unique(df$Preparation)
unique(df$Gaming)
unique(df$Attendance)
unique(df$Semester)

# Encoding variabel ordinal dan nominal
df_encoded <- df %>%
    mutate(
        # Ordinal Encoding
        Preparation_num = recode(Preparation,
                                 "0-1 Hour" = 1,
                                 "2-3 Hours" = 2,
                                 "More than 3 Hours" = 3),
        Gaming_num = recode(Gaming,
                            "0-1 Hour" = 1,
                            "2-3 Hours" = 2,
                            "More than 3 Hours" = 3),
        Attendance_num = recode(Attendance,
                                "Below 40%" = 1,
                                "40%-59%" = 2,
                                "60%-79%" = 3,
                                "80%-100%" = 4),
        Semester_num = as.numeric(gsub("[^0-9]", "", Semester)),
        Income_num = recode(Income,
                            "Low (Below 15,000)" = 1,
                            "Lower middle (15,000-30,000)" = 2,
                            "Upper middle (30,000-50,000)" = 3,
                            "High (Above 50,000)" = 4),

        # Nominal Encoding (binary)
        Job_num = ifelse(Job == "Yes", 1, 0),
        Extra_num = ifelse(Extra == "Yes", 1, 0),
        Hometown_num = ifelse(Hometown == "City", 1, 0),
        Gender_num = ifelse(Gender == "Male", 1, 0)
    )

# Pilih variabel numerik dan encoded
df_ready <- df_encoded %>%
    select(
        HSC, SSC, Computer, English, Last, Overall,         # skor numerik
        Preparation_num, Gaming_num, Attendance_num,         # ordinal
        Semester_num, Income_num,                            # ordinal
        Job_num, Extra_num, Hometown_num, Gender_num        # nominal binary
    )

# One-hot encode Department
dept_onehot <- model.matrix(~ Department - 1, data = df)

# Gabungkan data
df_ready_full <- cbind(df_ready, dept_onehot)

# Skala data
df_scaled <- scale(df_ready_full)

# Tentukan jumlah cluster dengan Elbow Method
set.seed(6969)
fviz_nbclust(df_scaled, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2) +
    labs(subtitle = "Elbow Method untuk menentukan jumlah cluster")

# Jalankan K-Means
set.seed(6969)
kmeans_result <- kmeans(df_scaled, centers = 3, nstart = 25)

# Lihat distribusi cluster
table(kmeans_result$cluster)

# Tambahkan cluster hasil K-Means ke data
df_final <- df_encoded %>%
    mutate(Cluster = kmeans_result$cluster)

# Visualisasi cluster K-Means
fviz_cluster(kmeans_result, data = df_scaled,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Statistik rata-rata fitur utama per cluster
df_final %>%
    group_by(Cluster) %>%
    summarise(across(c(Last, Overall, Attendance_num, Gaming_num, Preparation_num, Computer, Income_num), mean, na.rm = TRUE))


# PCA untuk visualisasi
pca <- prcomp(df_scaled)
library(factoextra)

fviz_pca_biplot(pca,
                label = "var",        # tampilkan label variabel
                habillage = kmeans_result$cluster,  # warna berdasarkan cluster
                addEllipses = TRUE,   # gambar elips tiap cluster
                palette = "jco",      # palette warna
                repel = TRUE,         # supaya label variabel tidak bertumpuk
                ggtheme = theme_minimal())

# Silhouette analysis
sil <- silhouette(kmeans_result$cluster, dist(df_scaled))
summary(sil)
fviz_silhouette(sil)

# Gap Statistic untuk jumlah cluster optimal
set.seed(6969)
gap_stat <- clusGap(df_scaled, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Korelasi antar fitur
num_features <- df_ready_full %>% select(where(is.numeric))
cor_mat <- cor(num_features, use = "complete.obs")
corrplot(cor_mat, method = "color", tl.cex = 0.7)

# Variansi fitur
apply(num_features, 2, var)

# PCA analisis fitur
pca_res <- prcomp(num_features, scale. = TRUE)
summary(pca_res)

# Visualisasi PCA biplot fitur
fviz_pca_biplot(pca_res,
                label = "var",       # hanya label variabel yang muncul
                geom = "point",
                col.ind = "orange",
                col.var = "steelblue",
                title = "PCA Biplot Label Variabel")

# Kontribusi fitur pada PC1 dan PC2
fviz_contrib(pca_res, choice = "var", axes = 1, top = 10)  # PC1
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10)  # PC2



# DONT NEED TO DO THIS LMAO
# Clustering ulang dengan fitur terpilih berdasarkan PCA
selected_features <- df_ready_full[, c("Last", "Overall", "Attendance_num", "Gaming_num", "Preparation_num", "Extra_num", "DepartmentComputer Science and Engineering", "SSC", "DepartmentJournalism, Communication and Media Studies", "HSC")]

# Skala fitur terpilih
selected_scaled <- scale(selected_features)

# Tentukan jumlah cluster (Elbow Method)
set.seed(123)
fviz_nbclust(selected_scaled, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2) +
    labs(subtitle = "Elbow Method - Fitur Terpilih")

# Jalankan K-Means dengan k=2 (contoh)
set.seed(123)
kmeans_selected <- kmeans(selected_scaled, centers = 3, nstart = 25)

# Tambahkan hasil cluster fitur terpilih
df_final_selected <- df_encoded %>%
    mutate(Cluster_Selected = kmeans_selected$cluster)

# Visualisasi cluster dengan PCA
pca_selected <- prcomp(selected_scaled)
plot(pca_selected$x[, 1:2], col = kmeans_selected$cluster, pch = 19,
     xlab = "PC1", ylab = "PC2", main = "Clustering dengan Fitur Terpilih")

# Visualisasi silhouette
sil_selected <- silhouette(kmeans_selected$cluster, dist(selected_scaled))
fviz_silhouette(sil_selected)

