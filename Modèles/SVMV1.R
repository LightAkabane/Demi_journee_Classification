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
ggplot(data, aes(x = R2, y = R17, color = data[, 1])) +  # Utiliser directement la colonne de l'étiquette (DIFF)
  geom_point() +
  stat_smooth(method = "svm", formula = data[, 1] ~ ., method.args = list(kernel = "linear"), se = FALSE) +
  labs(title = "Séparation des classes avec SVM (R2 vs R17)", x = "R2", y = "R17") +
  theme_minimal()






# Charger les bibliothèques nécessaires
if (!require("e1071")) install.packages("e1071")
if (!require("caret")) install.packages("caret")
if (!require("caret")) install.packages("lattice") # Pour les métriques d'évaluation

library(e1071)
library(caret)
library(lattice)


# Charger les données
data <- read.csv("farms_train.csv", header = TRUE, sep = ";")

# Convertir la première colonne en facteur (étiquette)
data[, 1] <- as.factor(data[, 1])

# Créer le modèle SVM
model <- svm(data[, 1] ~ ., data = data, kernel = "linear")

# Prédictions sur le jeu d'entraînement
predictions <- predict(model, data)

# Calcul des métriques de performance
confusion_matrix <- confusionMatrix(predictions, data[, 1])

# Affichage des métriques
cat("Accuracy:", confusion_matrix$overall['Accuracy'], "\n")
cat("Kappa:", confusion_matrix$overall['Kappa'], "\n")

# Afficher des métriques pour chaque classe (sensibilité, spécificité, etc.)
cat("Sensitivity (Recall):", confusion_matrix$byClass['Sensitivity'], "\n")
cat("Specificity:", confusion_matrix$byClass['Specificity'], "\n")
cat("Precision:", confusion_matrix$byClass['Pos Pred Value'], "\n")
cat("F1 Score:", 2 * (confusion_matrix$byClass['Sensitivity'] * confusion_matrix$byClass['Pos Pred Value']) / 
      (confusion_matrix$byClass['Sensitivity'] + confusion_matrix$byClass['Pos Pred Value']), "\n")
