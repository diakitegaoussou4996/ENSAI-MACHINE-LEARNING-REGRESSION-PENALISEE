# Chargement des bibliothèques nécessaires
if (!require("glmnet")) install.packages("glmnet")
library(glmnet)
if (!require("nnet")) install.packages("nnet")
library(nnet)
if (!require("MASS")) install.packages("MASS")
library(MASS)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("reshape2")) install.packages("reshape2")
library(reshape2)
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)
if (!require("pROC")) install.packages("pROC")
library(pROC)

# Lecture des données
transp <- read.table("C:/Users/diaki/OneDrive/Bureau/Projet_2/PROJET_081223/Projet_final/Data/transportmod3.txt", header = TRUE)

# Préparation des données
X <- as.matrix(transp[, !(names(transp) %in% c('CO2', 'Incid', 'CA', 'CA3'))])
Y <- transp$CA3  # Utilisation de CA3 au lieu de CA

# Normalisation des données
X_stand <- scale(X)

# Régression multinomiale pénalisée (sans considérer l’aspect ordonné)
# Utilisation de cv.glmnet avec family='multinomial'
rescv_multinom <- cv.glmnet(X_stand, Y, family = 'multinomial', alpha = 0.5, type.multinomial = "grouped", lambda = seq(0.01, 20, 0.02))
plot(rescv_multinom)

seuil <- rescv_multinom$lambda.min
seuil




# ------------Modèle etanalyse des coefs è------------------------

# Réalisation de la régression logistique Elastic Net pour un modèle multinomial
reselasticnet <- glmnet(X_stand, Y, family = 'multinomial', alpha = 0.5, lambda = seuil)

# Extraction des coefficients pour la régression Elastic Net multinomiale
coefelasticnet <- coef(reselasticnet)[-1]  # Exclusion de l'intercept

# Convertir les coefficients en un dataframe
coef_df_elasticnet <- do.call(rbind, lapply(seq_along(coefelasticnet), function(i) {
  data.frame(Class = paste0("Class_", i - 1), 
             Variable = rownames(coefelasticnet[[i]]), 
             Coefficient = as.numeric(coefelasticnet[[i]]))
}))

# Tri des coefficients par leur valeur absolue en ordre décroissant
coef_df_elasticnet <- coef_df_elasticnet[order(-abs(coef_df_elasticnet$Coefficient)), ]

# Sélection des 20 premières variables les plus importantes
top20_coef_df_elasticnet <- head(coef_df_elasticnet, 20)
# Création du graphique
g <- ggplot(top20_coef_df_elasticnet, aes(x = reorder(Variable, Coefficient), y = Coefficient, fill = Class)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_hline(yintercept = 0.1, col = "red", linetype = "dashed") +
  geom_hline(yintercept = -0.1, col = "red", linetype = "dashed") +
  theme_minimal() +
  xlab("Variable") +
  ylab("Coefficient") +
  ggtitle("Top 20 des Coefficients les plus importants dans Elastic Net (Multinomial)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Affichage du graphique
print(g)

# Sauvegarde du graphique (optionnel)
ggsave("top20_coef_elasticnet.png")

#----------------------------------SELECTION DE VARIABLE ET CORRELATION ---------------------------

# Chargement des bibliothèques nécessaires
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("reshape2")) install.packages("reshape2")
library(reshape2)

# Filtrage pour garder uniquement les coefficients supérieurs à 0 en valeur absolue, en excluant l'intercept
coef_df_elasticnet_significant <- coef_df_elasticnet[abs(coef_df_elasticnet$Coefficient) > 0 & coef_df_elasticnet$Variable != "(Intercept)", ]

# Affichage du dataframe des coefficients significatifs sans l'intercept
print(coef_df_elasticnet_significant)


# Chargement des bibliothèques nécessaires
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("reshape2")) install.packages("reshape2")
library(reshape2)

# Filtrage pour garder uniquement les coefficients supérieurs à 0 en valeur absolue, en excluant l'intercept
coef_df_elasticnet_significant <- coef_df_elasticnet[abs(coef_df_elasticnet$Coefficient) > 0 & coef_df_elasticnet$Variable != "(Intercept)", ]

# Affichage du dataframe des coefficients significatifs sans l'intercept
print(coef_df_elasticnet_significant)





