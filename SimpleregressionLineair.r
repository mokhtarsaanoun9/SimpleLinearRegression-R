chemin_fichier <- "C:/Users/MSI/Desktop/NotreProjet/mokh.csv"
data <- read.csv(chemin_fichier, sep = ";")
View(data)
# Check the column names
colnames(data)

# Adjust the code with the correct column names
data = subset(data, select = -c(NomEtudiant,Age) )
View(data)


data$ScoreExam<- as.numeric(data$ScoreExam)
data$NbHeure<- as.numeric(data$NbHeure)
box1 = boxplot( data$ScoreExam)
box2 = boxplot( data$NbHeure)


str(data)
# Création du graphique avec un nuage de points et une régression linéaire
library(ggplot2)
ggplot(data, aes(x = NbHeure  , y = ScoreExam)) +
  geom_point() +  # Nuage de points
  geom_smooth(method = "lm", se = FALSE)  # Régression linéaire (sans intervalles de confiance)

# Fit a linear regression model
lm_model <- lm(ScoreExam ~ NbHeure, data = data)
residuals <- resid(lm_model)
# Create a data frame for the residuals and fitted values
residual_data <- data.frame(Residuals = residuals, Fitted = fitted(lm_model))

# Create the scatterplot
ggplot(residual_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a dashed line at y=0
  labs(x = "Fitted Values", y = "Residuals")  # Label the axes


# Create the scatterplot of residuals with reference lines
ggplot(residual_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_segment(aes(xend = Fitted, yend = 0), color = "red") +
  labs(x = "Fitted Values", y = "Residuals")




# Assuming you have x_test, y_test, and y_predict defined
data_test <- data.frame(NbHeure = data$NbHeure, ScoreExam = data$ScoreExam, y_predict = fitted(lm_model))

ggplot(data_test) +
  geom_point(aes(x = NbHeure, y = ScoreExam), color = 'green') +
  geom_point(data = data_test, aes(x = NbHeure, y = y_predict), color = 'blue') +
  geom_segment(data = data_test, aes(x = NbHeure, y = ScoreExam, xend = NbHeure, yend = y_predict), color = 'red')