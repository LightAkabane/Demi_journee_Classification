library(rpart)
library(rpart.plot)
library(caret)
library(pROC)

data <- read.table("farms_train.csv", header = TRUE, sep = ";")

set.seed(123)

trainIndex <- createDataPartition(data$DIFF, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

train_data$DIFF <- as.factor(train_data$DIFF)
test_data$DIFF <- as.factor(test_data$DIFF)

levels(test_data$DIFF) <- levels(train_data$DIFF)

tree_model <- rpart(DIFF ~ R2 + R7 + R8 + R17 + R22 + R32, 
                    data = train_data, 
                    method = "class", 
                    control = rpart.control(cp = 0.01))

print("Résumé du modèle d'arbre de décision :")
print(summary(tree_model))

print("Visualisation de l'arbre de décision :")
rpart.plot(tree_model, main = "Arbre de décision pour prédire DIFF")

probabilities <- predict(tree_model, test_data, type = "prob")[, 2]

roc_curve <- roc(test_data$DIFF, probabilities)

print(paste("AUC : ", auc(roc_curve)))

plot(roc_curve, main = "Courbe ROC pour prédire DIFF", col = "blue", lwd = 2)


predictions <- predict(tree_model, test_data, type = "class")

