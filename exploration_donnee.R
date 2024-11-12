data <- read.table("farms_train.csv", header = TRUE, sep = ";")

head(data)
str(data)
summary(data)

cor_matrix <- cor(data[, c("R2", "R7", "R8", "R17", "R22", "R32")])

library(tidyverse)
data %>%
  gather(key = "Variable", value = "Value", R2, R7, R8, R17, R22, R32) %>%
  ggplot(aes(x = factor(DIFF), y = Value, fill = Variable)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Distribution des variables numériques par classe de DIFF",
       x = "Classe de DIFF",
       y = "Valeur") +
  theme_minimal()

library(ggcorrplot)
ggcorrplot(cor_matrix, method = "circle", type = "lower", 
           lab = TRUE, title = "Matrice de corrélation")

