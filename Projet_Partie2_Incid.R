library(ggplot2)
library(reshape2)
library(MASS)


transp=read.table("C:/Users/diaki/OneDrive/Bureau/Projet_2/PROJET_081223/transportmod3.txt",h=T)
summary(transp)


#Y est une variable de comptage 

# Extraction de la colonne 'Incid'
y <- transp$Incid

# Suppression des colonnes spécifiées et création de X
X <- transp[, !(names(transp) %in% c('CO2', 'Incid', 'CA', 'CA3'))]

# je normalise 
X_stand=scale(X)



# 1. Ridge 
# Avant tout on fait un plot des MSE par validation crois?e pour 
# v?rifier que l'amplitude de l'intervalle dans lequel nous 
# cherchons lambda est assez grande  
library(glmnet)
rescv=cv.glmnet(X_stand,y,family='poisson',alpha=0,lambda=seq(0.1,100,0.1))
plot(rescv)

# 2 - Stabilisation de lambda 

# Initialisation
lambda_vals <- numeric(10)  # Vecteur pour stocker les valeurs de lambda.min
lambda_sum <- 0             # Variable pour accumuler les valeurs de lambda

# Boucle pour la validation croisée et le calcul de lambda.min
for (j in 1:10) {
  rescv <- cv.glmnet(X_stand, y, family = 'poisson', alpha = 0, lambda = seq(0.1, 100, 0.1))
  lambda_min <- rescv$lambda.min
  
  lambda_vals[j] <- lambda_min
  lambda_sum <- lambda_sum + lambda_min
}

# Calcul du seuil moyen
seuil <- lambda_sum / 10

# Création du graphique
plot(1:10, lambda_vals, type = "o", col = "blue", xlab = "Itération", ylab = "Lambda.min",
     main = "Évolution de Lambda.min sur les itérations")
abline(h = seuil, col = "red", lty = 2)
legend("topright", legend = c("Lambda.min", "Seuil moyen"), col = c("blue", "red"), lty = 1:2, pch = c(1, NA))

# Ajout de la valeur du seuil sur le graphique
text(x = 5, y = seuil, labels = paste("Seuil moyen:", round(seuil, 2)), pos = 3, col = "red")



# 3 - Regression de lambda avec le seul moyen 


resridge=glmnet(X_stand,y,family='poisson',alpha=0,lambda=seuil)

#------------------------------------------------------------------------------
# Extraction des coefficients (en supprimant la constante)
coefridge <- as.numeric(coef(resridge)[-1])

# Création d'un dataframe pour la visualisation
coef_df <- data.frame(Variable = colnames(X_stand), Coefficient = coefridge)

# Tri des coefficients par leur valeur absolue en ordre décroissant
coef_df <- coef_df[order(-abs(coef_df$Coefficient)), ]

# Sélection des 20 premières variables les plus importantes
top20_coef_df <- head(coef_df, 20)

# Création du graphique vertical
ggplot(top20_coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("Variable") +
  ylab("Coefficient") +
  ggtitle("Top 20 des Coefficients les plus importants") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotation des étiquettes de l'axe X

# Création du graphique vertical
ggplot(top20_coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Rendre le graphique horizontal
  geom_hline(yintercept = 0.1, col = "red", linetype = "dashed") +  # Ligne pour le seuil positif
  geom_hline(yintercept = -0.1, col = "red", linetype = "dashed") + # Ligne pour le seuil négatif
  theme_minimal() +
  xlab("Variable") +
  ylab("Coefficient") +
  ggtitle("Top 20 des Coefficients les plus importants") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#--------------------------------------------------------------------------


# Les coefs le plus importants 
box=boxplot(coefridge)
box
which(coefridge > box$stats[5])
which(coefridge < box$stats[1])


# 3 : je selectionnes les variables qui ont un coef supérieur à 0.1
#--------------------------------------------------------------------------------------
# Tri des coefficients par leur valeur absolue en ordre décroissant
coefficients_sorted_indices <- order(abs(coefridge), decreasing = TRUE)

# Sélection des indices des coefficients supérieurs à 0.1
significant_coef_indices <- coefficients_sorted_indices[abs(coefridge[coefficients_sorted_indices]) > 0.1]

# Extraction des noms des variables correspondantes
select_ridge <- colnames(X_stand)[significant_coef_indices]

# Affichage des noms des variables sélectionnées
print(select_ridge)


#----------------------------------------------------------------------------------
# Je regarde la corrélation des variables qui ont été selectionnée



# Je regarde la corrélation des variables qui ont été selectionnées 
# Extraction des données pour les variables sélectionnées
selected_data <- X_stand[, select_ridge, drop = FALSE]

# Calcul de la matrice de corrélation
cor_matrix <- cor(selected_data)

# Transformation de la matrice de corrélation en format long
cor_melted <- melt(cor_matrix)

# Création de la carte de chaleur avec les valeurs de corrélation
ggplot(cor_melted, aes(Var1, Var2)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1),
        axis.text.y = element_text(size = 10)) +
  labs(x = '', y = '', title = 'Correlation Matrix of Selected Variables') +
  coord_fixed()


# LASSO 

rescv=cv.glmnet(X_stand,y,family='poisson',alpha=1)
plot(rescv)



# Je stabilise lambda

#-------------------------------------------------------------------------------
# Initialisation
lambda_vals_lasso <- numeric(10)  # Vecteur pour stocker les valeurs de lambda.1se
lambda_sum_lasso <- 0             # Variable pour accumuler les valeurs de lambda

# Boucle pour la validation croisée et le calcul de lambda.1se
for (j in 1:10) {
  rescv <- cv.glmnet(X_stand, y, family = 'poisson', alpha = 1)
  lambda_1se <- rescv$lambda.1se
  
  lambda_vals_lasso[j] <- lambda_1se
  lambda_sum_lasso <- lambda_sum_lasso + lambda_1se
}

# Calcul du seuil moyen
seuil_lasso <- lambda_sum_lasso / 10

# Création du graphique
plot(1:10, lambda_vals_lasso, type = "o", col = "blue", xlab = "Itération", ylab = "Lambda.1se",
     main = "Évolution de Lambda.1se pour Lasso sur les itérations")
abline(h = seuil_lasso, col = "red", lty = 2)
legend("topright", legend = c("Lambda.1se", "Seuil moyen pour Lasso"), col = c("blue", "red"), lty = 1:2, pch = c(1, NA))

# Ajout de la valeur du seuil sur le graphique
text(x = 5, y = seuil_lasso, labels = paste("Seuil moyen pour Lasso:", round(seuil_lasso, 2)), pos = 3, col = "red")

#-------------------------------------------------------------------------------)

# 2ème je réalise une regression ade Poisson avec le seuil lambda selectionné 
#------------------------------------------------------------------------------------

# Ajustement du modèle Lasso
reslasso <- glmnet(X_stand, y, family='poisson', alpha=1, lambda=seuil)

# Extraction des coefficients (en supprimant la constante)
coeflasso <- as.numeric(coef(reslasso)[-1])

# Création d'un dataframe pour la visualisation
coef_df_lasso <- data.frame(Variable = colnames(X_stand), Coefficient = coeflasso)

# Tri des coefficients par leur valeur absolue en ordre décroissant
coef_df_lasso <- coef_df_lasso[order(-abs(coef_df_lasso$Coefficient)), ]

# Sélection des 20 premières variables les plus importantes
top20_coef_df_lasso <- head(coef_df_lasso, 20)

# Création du graphique horizontal pour Lasso
ggplot(top20_coef_df_lasso, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Pour un graphique horizontal
  geom_hline(yintercept = 0.1, col = "red", linetype = "dashed") +
  geom_hline(yintercept = -0.1, col = "red", linetype = "dashed") +
  theme_minimal() +
  ylab("Variable") +
  xlab("Coefficient") +
  ggtitle("Top 20 des Coefficients les plus importants pour Lasso") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Tri des coefficients par leur valeur absolue en ordre décroissant pour Lasso
coefficients_sorted_indices_lasso <- order(abs(coeflasso), decreasing = TRUE)

# Sélection des indices des coefficients supérieurs à 0.1 pour Lasso
significant_coef_indices_lasso <- coefficients_sorted_indices_lasso[abs(coeflasso[coefficients_sorted_indices_lasso]) > 0.0]

# Extraction des noms des variables correspondantes pour Lasso
select_lasso <- colnames(X_stand)[significant_coef_indices_lasso]

# Affichage des noms des variables sélectionnées pour Lasso
print(select_lasso)

#------------------------------------------------------------------------------


# Je regarde la corrélation des variables qui ont été selectionnées 
# Extraction des données pour les variables sélectionnées
selected_data <- X_stand[, select_lasso, drop = FALSE]

# Calcul de la matrice de corrélation
cor_matrix <- cor(selected_data)

# Transformation de la matrice de corrélation en format long
cor_melted <- melt(cor_matrix)

# Création de la carte de chaleur avec les valeurs de corrélation
ggplot(cor_melted, aes(Var1, Var2)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1),
        axis.text.y = element_text(size = 10)) +
  labs(x = '', y = '', title = 'Correlation Matrix of Selected Variables') +
  coord_fixed()

#-------------------------------------------------------------------------------

# correlation avec y 

#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------


# 3. Elasticnet 
# Je stabilise lambda 

#---------------------------------------------------------------------------------
# Initialisation pour Elastic Net
lambda_vals_enet <- numeric(10)  # Vecteur pour stocker les valeurs de lambda.1se
lambda_sum_enet <- 0             # Variable pour accumuler les valeurs de lambda

# Boucle pour la validation croisée et le calcul de lambda.1se pour Elastic Net
for (j in 1:10) {
  rescv <- cv.glmnet(X_stand, y, family = 'poisson', alpha = 0.5)
  lambda_1se_enet <- rescv$lambda.1se
  
  lambda_vals_enet[j] <- lambda_1se_enet
  lambda_sum_enet <- lambda_sum_enet + lambda_1se_enet
}

# Calcul du seuil moyen pour Elastic Net
seuil_enet <- lambda_sum_enet / 10

# Création du graphique pour Elastic Net
plot(1:10, lambda_vals_enet, type = "o", col = "blue", xlab = "Itération", ylab = "Lambda.1se",
     main = "Évolution de Lambda.1se pour Elastic Net sur les itérations")
abline(h = seuil_enet, col = "red", lty = 2)
legend("topright", legend = c("Lambda.1se", "Seuil moyen pour Elastic Net"), col = c("blue", "red"), lty = 1:2, pch = c(1, NA))

# Ajout de la valeur du seuil sur le graphique
text(x = 5, y = seuil_enet, labels = paste("Seuil moyen pour Elastic Net:", round(seuil_enet, 2)), pos = 3, col = "red")

#-----------------------------------------------------------------------------------

# Je les coefs 

#-------------------------------------------------------------------------
library(ggplot2)

# Ajustement du modèle Elastic Net
resnet <- glmnet(X_stand, y, family='poisson', alpha=0.5, lambda=seuil)

# Extraction des coefficients (en supprimant la constante)
coefnet <- as.numeric(coef(resnet)[-1])

# Création d'un dataframe pour la visualisation
coef_df_net <- data.frame(Variable = colnames(X_stand), Coefficient = coefnet)

# Tri des coefficients par leur valeur absolue en ordre décroissant
coef_df_net <- coef_df_net[order(-abs(coef_df_net$Coefficient)), ]

# Sélection des 20 premières variables les plus importantes
top20_coef_df_net <- head(coef_df_net, 20)

# Création du graphique horizontal pour Elastic Net
ggplot(top20_coef_df_net, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Pour un graphique horizontal
  geom_hline(yintercept = 0.1, col = "red", linetype = "dashed") +
  geom_hline(yintercept = -0.1, col = "red", linetype = "dashed") +
  theme_minimal() +
  ylab("Variable") +
  xlab("Coefficient") +
  ggtitle("Top 20 des Coefficients les plus importants pour Elastic Net") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#--------------------------------------------------------------------------

# 2ème version 

#---------------------------------------------------------------------------------------------
# Tri des coefficients par leur valeur absolue en ordre décroissant pour Elastic Net
coefficients_sorted_indices_enet <- order(abs(coefnet), decreasing = TRUE)

# Sélection des indices des coefficients supérieurs à 0.1 pour Elastic Net
significant_coef_indices_enet <- coefficients_sorted_indices_enet[abs(coefnet[coefficients_sorted_indices_enet]) > 0.1]

# Extraction des noms des variables correspondantes pour Elastic Net
select_enet <- colnames(X_stand)[significant_coef_indices_enet]

# Affichage des noms des variables sélectionnées pour Elastic Net
print(select_enet)

#-----------------------------------------------------------------------------------------------

# Supposons que select_enet contient les noms des variables sélectionnées par Elastic Net
# ...

# Extraction des données pour les variables sélectionnées par Elastic Net
selected_data_enet <- X_stand[, select_enet, drop = FALSE]

# Calcul de la matrice de corrélation pour ces données
cor_matrix_enet <- cor(selected_data_enet)

# Transformation de la matrice de corrélation en format long
cor_melted_enet <- melt(cor_matrix_enet)

# Création de la carte de chaleur pour Elastic Net
ggplot(cor_melted_enet, aes(Var1, Var2)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1),
        axis.text.y = element_text(size = 10)) +
  labs(x = '', y = '', title = 'Correlation Matrix of Variables Selected by Elastic Net') +
  coord_fixed()


# Partie 4 de l'excercice 2 :

# 1- Regression de poisson 

# Conversion de X_stand en data.frame si ce n'est pas déjà le cas
X_stand_df <- as.data.frame(X_stand)

# Assurez-vous que y est un vecteur
y_vec <- as.vector(y)

# Ajustement du modèle de régression de Poisson
mod_poisson <- glm(y_vec ~ ., data = X_stand_df[, select_enet, drop = FALSE], family = poisson())

# Résumé du modèle Poisson
summary(mod_poisson)



# 2. Évaluation de la Surdispersion


# Calcul des résidus de Pearson
res_pearson <- residuals(mod_poisson, type = "pearson")

# Calcul de l'indicateur de surdispersion
n <- nrow(X_stand)
p <- length(select_enet)
surdispersion_indicator <- sum(res_pearson^2) / (n - p)

# Affichage de l'indicateur de surdispersion
print(paste("Indicateur de surdispersion:", surdispersion_indicator))


# 3. Modèle Binomial Négatif

library(MASS)

# Ajustement du modèle binomial négatif
mod_negbin <- glm.nb(y ~ ., data = X_stand_df[, select_enet, drop = FALSE])

# Résumé du modèle binomial négatif
summary(mod_negbin)

# 4. Interprétation de la Variable la Plus Significative

# Trouver la variable la plus significative
most_significant_var <- which.min(summary(mod_negbin)$coefficients[, "Pr(>|z|)"])
name_most_significant_var <- rownames(summary(mod_negbin)$coefficients)[most_significant_var]

# Affichage et interprétation
print(paste("Variable la plus significative:", name_most_significant_var))
print(summary(mod_negbin)$coefficients[most_significant_var, ])
