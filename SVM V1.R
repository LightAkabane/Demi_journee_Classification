# Charger les bibliothèques nécessaires
if (!require("e1071")) install.packages("e1071")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("GGally")) install.packages("GGally")
library(e1071)
library(ggplot2)
library(GGally)

# Charger les données
data <- read.csv("farms_train.csv", header = TRUE, sep = ";")

# Convertir la première colonne en facteur (étiquette)
data[, 1] <- as.factor(data[, 1])

# Créer le modèle SVM
model <- svm(data[, 1] ~ ., data = data, kernel = "linear")

# Visualisation avec ggplot2
ggplot(data, aes(x = R2, y = R32, color = data[, 1])) +  # Utiliser directement la colonne de l'étiquette (DIFF)
  geom_point() +
  stat_smooth(method = "svm", formula = data[, 1] ~ ., method.args = list(kernel = "linear"), se = FALSE) +
  labs(title = "Séparation des classes avec SVM (R2 vs R7)", x = "R2", y = "R7") +
  theme_minimal()



