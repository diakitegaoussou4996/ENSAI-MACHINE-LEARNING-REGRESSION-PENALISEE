
library(ggplot2)
library(reshape2)
library(MASS)
library(glmnet)
#------------Partie 3 : CA (binaire)--------------------------

transp=read.table("C:/Users/diaki/OneDrive/Bureau/Projet_2/PROJET_081223/Projet_final/Data/transport1.txt",h=T)
summary(transp)

# Préparation des données 

# Assurez-vous que X est une matrice et Y est un vecteur
X <- as.matrix(transp[, !(names(transp) %in% c('CO2', 'Incid', 'CA'))])
Y <- transp$CA

# je normalise 
X_stand=scale(X)

# Utilisation de cv.glmnet
rescv <- cv.glmnet(X_stand, Y, family='binomial', alpha=0, lambda=seq(0.01, 20, 0.02))
plot(rescv)

seuil=rescv$lambda.min # pas de s?lection => lambda.min 
seuil

#--------Je réalise une regression ridge avec le seuil min et ensute analyser mes coefs------

# Réalisation de la régression logistique ridge
resridge <- glmnet(X_stand, Y, family = 'binomial', alpha = 0, lambda = seuil)

# Extraction des coefficients (en supprimant la constante)
coefridge <- as.numeric(coef(resridge)[-1])

# Création d'un dataframe pour la visualisation
coef_df <- data.frame(Variable = colnames(X_stand), Coefficient = coefridge)

# Tri des coefficients par leur valeur absolue en ordre décroissant
coef_df <- coef_df[order(-abs(coef_df$Coefficient)), ]

# Sélection des 20 premières variables les plus importantes
top20_coef_df <- head(coef_df, 20)

# Création du graphique
ggplot(top20_coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Pour un graphique horizontal
  geom_hline(yintercept = 0.1, col = "red", linetype = "dashed") +  # Ligne pour le seuil positif
  geom_hline(yintercept = -0.1, col = "red", linetype = "dashed") + # Ligne pour le seuil négatif
  theme_minimal() +
  xlab("Variable") +
  ylab("Coefficient") +
  ggtitle("Top 20 des Coefficients les plus importants") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#---------------------------------------------------------------------------------


#-----------------Je conserve les 12 variables les plus importante en terme de coef---------------


# Création d'un dataframe pour la visualisation
coef_df <- data.frame(Variable = colnames(X), Coefficient = coefridge)

# Tri des coefficients par leur valeur absolue en ordre décroissant
coef_df <- coef_df[order(-abs(coef_df$Coefficient)), ]

# Sélection des 6 premières variables les plus importantes
top12_coef_df <- head(coef_df, 12)

# Affichage du dataframe des 6 variables les plus importantes
print(top12_coef_df)




#-------------------------------------------------------------------------

# Je regarde la corrélation entre ces 12 variables selectionnées par Lasso 

#-------------------------------------------------------------------
# Assurez-vous que les bibliothèques nécessaires sont chargées
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)

# Utilisation du dataframe top6_coef_df que vous avez créé précédemment
top12_variables <- top12_coef_df$Variable

# Création de la sous-matrice de corrélation pour les 6 variables
cor_matrix <- cor(X[, top12_variables])

# Visualisation de la matrice de corrélation
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         cl.pos = "b", cl.ratio = 0.1,
         addCoef.col = "black")  # Ajoute les valeurs de corrélation

# Si vous préférez une visualisation avec ggplot2, cela nécessiterait un traitement de données supplémentaire

#------------------------------------------------------------------

                     # ----LASSO-----

# 2. LASSO
# Assurez-vous que les bibliothèques nécessaires sont chargées
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Initialisation
res <- 0
K <- 10
res_values <- data.frame(Iteration = integer(0), Res = numeric(0))

# Boucle pour stabiliser la valeur de lambda
for (i in 1:K) {
  rescv <- cv.glmnet(X_stand, Y, family = 'binomial', alpha = 1)
  res <- res + rescv$lambda.1se
  res_values <- rbind(res_values, data.frame(Iteration = i, Res = res))
}

# Calcul de lambda1se
lambda1se <- res / K
seuil <- lambda1se

# Création du graphique avec légendes
ggplot(res_values, aes(x = Iteration, y = Res)) +
  geom_line(aes(color = "Évolution de res")) +  # Légende pour la ligne
  geom_hline(yintercept = lambda1se, linetype = "dashed", color = "red", aes(color = "lambda1se")) +
  annotate("text", x = K, y = lambda1se, label = paste("lambda1se =", round(lambda1se, 4)), vjust = -0.5) +
  scale_color_manual(name = "", values = c("Évolution de res" = "blue", "lambda1se" = "red")) +
  theme_minimal() +
  xlab("Itération") +
  ylab("Valeur cumulée de res") +
  ggtitle("Évolution de res et valeur finale de lambda1se") +
  theme(legend.position = "bottom")


#-----------------------------------------------------

# -----------------------------Analyse des coefs LASSO--------------------------------
# Réalisation de la régression logistique LASSO

rescv <- cv.glmnet(X_stand, Y, family='binomial', alpha=1, lambda=seq(0.01, 20, 0.02))

seuil=rescv$lambda.min # pas de s?lection => lambda.min 
seuil
reslasso <- glmnet(X_stand, Y, family = 'binomial', alpha = 1, lambda = seuil)

# Extraction des coefficients (en supprimant la constante)
coeflasso <- as.numeric(coef(reslasso)[-1])

# Création d'un dataframe pour la visualisation
coef_df_lasso <- data.frame(Variable = colnames(X), Coefficient = coeflasso)

# Tri des coefficients par leur valeur absolue en ordre décroissant
coef_df_lasso <- coef_df_lasso[order(-abs(coef_df_lasso$Coefficient)), ]

# Sélection des 20 premières variables les plus importantes
top20_coef_df_lasso <- head(coef_df_lasso, 20)

# Création du graphique
ggplot(top20_coef_df_lasso, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Pour un graphique horizontal
  geom_hline(yintercept = 0.1, col = "red", linetype = "dashed") +  # Ligne pour le seuil positif
  geom_hline(yintercept = -0.1, col = "red", linetype = "dashed") + # Ligne pour le seuil négatif
  theme_minimal() +
  xlab("Variable") +
  ylab("Coefficient") +
  ggtitle("Top 20 des Coefficients les plus importants dans LASSO") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#---------------------------------------------------------------------

# --------------------Selezction  des coefs différent de 0 de LASSO-----------------------------

# Extraction des coefficients (en supprimant la constante)
coeflasso <- as.numeric(coef(reslasso)[-1])

# Création d'un dataframe pour la visualisation
coef_df_lasso <- data.frame(Variable = colnames(X), Coefficient = coeflasso)

# Filtrage pour garder uniquement les coefficients supérieurs à 0 en valeur absolue
coef_df_lasso <- coef_df_lasso[abs(coef_df_lasso$Coefficient) > 0, ]

# Affichage du dataframe des coefficients supérieurs à 0 en valeur absolue
print(coef_df_lasso)

#----------------- Matrice de corrélation pour les variables sélectionnées -----------------
# Chargement des bibliothèques nécessaires
library(ggplot2)
library(reshape2)

# Sélection des variables à partir du dataframe de coefficients
selected_vars <- coef_df_lasso$Variable

# Extraction des données correspondantes à ces variables
selected_data <- X_stand[, selected_vars]

# Calcul de la matrice de corrélation
cor_matrix <- cor(selected_data)

# Transformation de la matrice de corrélation pour la visualisation
melted_cor_matrix <- melt(cor_matrix)

# Création du graphique de corrélation avec les valeurs de corrélation
ggplot(melted_cor_matrix, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 1) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Matrice de Corrélation des Variables Sélectionnées avec Valeurs")


#----------------------------------------------------------------------------------------------






              # -----------------Elastic net----------------------------

# Assurez-vous que les bibliothèques nécessaires sont chargées
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Initialisation
res_elasticnet <- 0
K <- 10
res_values_elasticnet <- data.frame(Iteration = integer(0), Res = numeric(0))

# Boucle pour stabiliser la valeur de lambda
for (i in 1:K) {
  rescv_elasticnet <- cv.glmnet(X_stand, Y, family = 'binomial', alpha = 0.5) # alpha = 0.5 pour Elastic Net
  res_elasticnet <- res_elasticnet + rescv_elasticnet$lambda.1se
  res_values_elasticnet <- rbind(res_values_elasticnet, data.frame(Iteration = i, Res = res_elasticnet))
}

# Calcul de lambda1se pour Elastic Net
lambda1se_elasticnet <- res_elasticnet / K
seuil = lambda1se_elasticnet

# Création du graphique avec légendes pour Elastic Net
ggplot(res_values_elasticnet, aes(x = Iteration, y = Res)) +
  geom_line(aes(color = "Évolution de res")) +  # Légende pour la ligne
  geom_hline(yintercept = lambda1se_elasticnet, linetype = "dashed", color = "red", aes(color = "lambda1se_elasticnet")) +
  annotate("text", x = K, y = lambda1se_elasticnet, label = paste("lambda1se =", round(lambda1se_elasticnet, 4)), vjust = -0.5) +
  scale_color_manual(name = "", values = c("Évolution de res" = "blue", "lambda1se_elasticnet" = "red")) +
  theme_minimal() +
  xlab("Itération") +
  ylab("Valeur cumulée de res") +
  ggtitle("Évolution de res et valeur finale de lambda1se pour Elastic Net") +
  theme(legend.position = "bottom")




# Réalisation de la régression logistique Elastic Net
# alpha entre 0 et 1 pour Elastic Net (0 = Ridge, 1 = LASSO)
reselasticnet <- glmnet(X_stand, Y, family = 'binomial', alpha = 0.5, lambda = seuil)

# Extraction des coefficients (en supprimant la constante)
coefelasticnet <- as.numeric(coef(reselasticnet)[-1])

# Création d'un dataframe pour la visualisation
coef_df_elasticnet <- data.frame(Variable = colnames(X), Coefficient = coefelasticnet)

# Tri des coefficients par leur valeur absolue en ordre décroissant
coef_df_elasticnet <- coef_df_elasticnet[order(-abs(coef_df_elasticnet$Coefficient)), ]

# Sélection des 20 premières variables les plus importantes
top20_coef_df_elasticnet <- head(coef_df_elasticnet, 20)

# Création du graphique
ggplot(top20_coef_df_elasticnet, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_hline(yintercept = 0.1, col = "red", linetype = "dashed") +
  geom_hline(yintercept = -0.1, col = "red", linetype = "dashed") +
  theme_minimal() +
  xlab("Variable") +
  ylab("Coefficient") +
  ggtitle("Top 20 des Coefficients les plus importants dans Elastic Net") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Filtrage pour garder uniquement les coefficients supérieurs à 0 en valeur absolue
coef_df_elasticnet <- coef_df_elasticnet[abs(coef_df_elasticnet$Coefficient) > 0, ]

# Affichage du dataframe des coefficients supérieurs à 0 en valeur absolue
print(coef_df_elasticnet)




# Sélection des variables à partir du dataframe de coefficients
selected_vars_elasticnet <- coef_df_elasticnet$Variable

# Extraction des données correspondantes à ces variables
selected_data_elasticnet <- X_stand[, selected_vars_elasticnet]

# Calcul de la matrice de corrélation
cor_matrix_elasticnet <- cor(selected_data_elasticnet)

# Transformation de la matrice de corrélation pour la visualisation
melted_cor_matrix_elasticnet <- melt(cor_matrix_elasticnet)

# Création du graphique de corrélation avec les valeurs de corrélation
ggplot(melted_cor_matrix_elasticnet, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 1) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Matrice de Corrélation des Variables Sélectionnées (Elastic Net) avec Valeurs")




# 4
selected_vars_lasso <- coef_df_lasso$Variable
selected_vars_lasso
selected_data_lasso <- data.frame(X_stand[, selected_vars_lasso])

# Conversion des données sélectionnées en data.frame
selected_data_elasticnet <- data.frame(X_stand[, selected_vars_elasticnet])

#coef_df_elasticnet
modlogit=glm(Y~ ., data= selected_data_elasticnet, family='binomial')

# Résumé du modèle
summary(modlogit)
resAIC=step(modlogit)
summary(resAIC)




library(pROC)

# Calcul des prédictions du modèle
predlogit <- predict(resAIC, type = "response")

# Création de la courbe ROC
courbe <- roc(Y, predlogit)

# Affichage de la courbe ROC
plot(courbe, main="Courbe ROC")

# Ajout de la valeur AUC à la légende
auc_value <- auc(courbe)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)))



