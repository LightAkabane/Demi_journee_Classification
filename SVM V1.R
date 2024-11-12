# Charger les bibliothèques nécessaires
if (!require("e1071")) install.packages("e1071")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("GGally")) install.packages("GGally")
if (!require("caret")) install.packages("caret")  # Pour les métriques de performance
library(e1071)
library(ggplot2)
library(GGally)
library(caret)  # Pour calculer les métriques de performance

# Charger les données
data <- read.csv("farms_train.csv", header = TRUE, sep = ";")

# Convertir la première colonne en facteur (étiquette)
data[, 1] <- as.factor(data[, 1])

# Créer le modèle SVM
model <- svm(data[, 1] ~ ., data = data, kernel = "linear")

# Prédictions avec le modèle SVM
predictions <- predict(model, newdata = data)

# Calcul des métriques de performance
conf_matrix <- confusionMatrix(predictions, data[, 1])  # Matrice de confusion
accuracy <- conf_matrix$overall['Accuracy']  # Précision
precision <- conf_matrix$byClass['Pos Pred Value']  # Précision par classe
recall <- conf_matrix$byClass['Sensitivity']  # Rappel par classe
f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score par classe

# Afficher les métriques
cat("Précision : ", accuracy, "\n")
cat("Précision par classe : ", precision, "\n")
cat("Rappel par classe : ", recall, "\n")
cat("F1-score par classe : ", f1_score, "\n")

# Visualisation avec ggplot2
ggplot(data, aes(x = R17, y = R8, color = data[, 1])) +  # Utiliser directement la colonne de l'étiquette (DIFF)
  geom_point() +
  stat_smooth(method = "svm", formula = data[, 1] ~ ., method.args = list(kernel = "linear"), se = FALSE) +
  labs(title = "Séparation des classes avec SVM (R17 vs R8)", x = "R17", y = "R8") +
  theme_minimal()

