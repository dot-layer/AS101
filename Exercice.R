# Exemples en R
# Auteurs: Thierry Duchesne, Sophie Baillargeon et Marie-Pier Côté
  

library(nycflights13)  # données vols d'avion
library(car)           # fonction Anova (tests globaux variables catégoriques)
library(olsrr)         # fonction ols_step_all_possible
library(glmnet)        # lasso



# Présentation des données

airports[airports$faa %in% unique(flights$origin), ]

## Préparation du jeu de données

# Vols en direction de Boston en juillet
BOS <- subset(flights, subset = dest == "BOS" & month == 7)

# Ajout du nombre de sièges dans l'avion
BOS <- merge(x = BOS,
             y = subset(planes, select = c(tailnum, seats)),
             by = "tailnum", all.x = TRUE, all.y = FALSE)

# Ajout de variables météo
BOS <- merge(x = BOS,
             y = subset(weather, select = c(origin, time_hour, dewp, humid, wind_dir, wind_speed, precip, pressure, visib)),
             by = c("origin", "time_hour"), all.x = TRUE, all.y = FALSE)

# Ajout de nouvelles variables créées par transformation d'autres variables :
# - jour de la semaine du départ du vol
BOS$week_day <- weekdays(BOS$time_hour)

# - indicatrice de la fin de semaine (TRUE pour un vol de fin de semaine, FALSE sinon)
# Selon la langue d'installation de R, vous devez soit rouler la ligne en français ou celle en anglais.
BOS$wknd <- BOS$week_day %in% c("samedi", "dimanche")
#BOS$wknd <- BOS$week_day %in% c("Saturday", "Sunday")

# - indicatrice de l'heure de pointe de fin de journée 
#   (TRUE si le vol a décolé entre 17 et 19 heures du soir, FALSE sinon)
BOS$evening_rush_hour <- (BOS$hour %in% c(17, 18))

# - variables pour représenter correctement modéliser les vents
BOS$wind_sin <- BOS$wind_speed*sin(BOS$wind_dir*pi/180)
BOS$wind_cos <- BOS$wind_speed*cos(BOS$wind_dir*pi/180)

# - indicatrice de la présence de précipitation (TRUE s'il y a des précipitations, FALSE sinon)
BOS$precip_indic <- BOS$precip > 0

# On ne conserve que les lignes avec aucune donnée manquante, pour les besoins de notre exemple.
BOS <- BOS[complete.cases(BOS),]


### Données test

# Sélection aléatoire de 20% des observations
set.seed(27) # pour avoir des résultats réplicables
index_test <- sample.int(n = nrow(BOS), size = 0.2*nrow(BOS))
# Données test
BOStest <- BOS[index_test, ]
# Données d'entrainement
BOStrain <- BOS[-index_test, ]


# Fonction pour mesurer de la qualité de la prédiction

RMSEp <- function(obs, pred){ sqrt(mean((obs - pred)^2, na.rm = TRUE)) }

#### Exercice. #### 
# Exécutez toutes les lignes de code ci-haut en vérifiant 
# qu'aucun message d'erreur n'est obtenu. Utilisez la fonction `dim` 
# pour voir les dimensions des bases de données `BOStrain` et `BOStest`.



# Analyse préliminaire des données


#### Exercice.#### 
# Utilisez la fonction `summary` pour obtenir les statistiques
# descriptives de chaque variable de `BOStrain`. Remarquez les
# différents types de variables: continues, booléen, catégorielles.


# Graphiques
plot(arr_delay ~ dep_delay, data = BOStrain,
main = "Relation entre les temps de retard à l'arrivée et au départ")
abline(a = 0, b = 1)

boxplot(arr_delay - dep_delay ~ carrier, data = BOStrain,
ylab = "arr_delay - dep_delay", xlab = "carrier",
main = "Relation entre la déviation de durée et le transporteur")
abline(h = 0 , lty = 2)
airlines[airlines$carrier %in% unique(BOStrain$carrier), ]

addmargins(xtabs(~ carrier + origin, data = BOStrain))

#### Exercice.#### 
# Utilisez la fonction `boxplot` pour tracer le diagramme 
# à moustache de `arr_delay - dep_delay` en fonction de la
# variable `origin`.



#### Exercice. #### 
# Combien y a-t-il de vols la semaine et la fin de semaine? 
# Utilisez la fonction `table` et la variable indicatrice `wknd`.
# Tracez le diagramme à moustache de `arr_delay - dep_delay` en
# fonction de `wknd`. 

#### Exercice. #### 
# Tracez le diagramme à moustache de `arr_delay - dep_delay`
# en fonction de l'indicatrice `evening_rush_hour`


#### Exercice. #### 
# Est-ce que la taille d'un avion a une influence sur 
# les retard des vols à l'arrivée? Tracez un nuage de points
# de `arr_delay - dep_delay` en fonction de `seats`.


#### Exercice. #### 
# Tracez les nuages de points de `arr_delay - dep_delay`
# en fonction de `wind_sin` et `wind_cos`.


#### Exercice. #### 
# Utilisez le graphique approprié pour visualiser la relation
# entre `arr_delay - dep_delay` et la variable qui indique s'il
# y a des précipitations, `precip_indic`.


#### On retourne maintenant à la théorie.####

# Régression linéaire simple

#### Exercice. #### 
# Utilisez la fonction `lm` avec les données `BOStrain`
# pour ajuster le modèle de régression linéaire simple.
# Stockez votre modèle dans un objet nommé `modele_simple`. 

modele_simple <- lm(# à compléter)
summary(modele_simple)


#### Exercice.#### 
# Quelles sont les estimations des paramètres beta0
# et beta1 ? Interprétez ces deux valeurs dans le 
# contexte des vols d'avion.


# Illustration
plot(BOStrain$dep_delay,BOStrain$arr_delay,pch=16)
abline(a=modele_simple$coef[1], b=modele_simple$coef[2], col=2, lwd=2)


#### Exercice.#### 
# Quel est le pourcentage de la variabilité dans le délai 
# à l'arrivée qui est expliqué par le délai au décollage ?


# Si ce modèle simple était utilisé pour prédire `arr_delay`, 
# nous pourrions nous attendre à une erreur quadratique moyenne
# de prédiction valant :

mse_simple <- RMSEp(obs = BOStest$arr_delay,
                    pred = predict(object = modele_simple, newdata = BOStest))^2
mse_simple


#### On retourne maintenant à la théorie.####

# Régression linéaire multiple

modele_complet <- lm(arr_delay ~ dep_delay + origin + wknd + evening_rush_hour + seats +
dewp + humid + wind_sin + wind_cos + precip_indic + pressure + visib, 
data = BOStrain, x=TRUE, y=TRUE)
summary(modele_complet)


#### Exercice #### 
# Quel pourcentage de la variabilité observée dans `arr_delay`
# est expliquée par le modèle?


#### Exercice. ####
# Interpréter les coefficients estimés pour les variables
# `seats` et `wknd` dans le contexte du problème.


# Test sur la variable origin
Anova(modele_complet, type = 3)

# On considère le premier vol de la base de données test.
(new.dat <- BOStest[1,])

#### Exercice. #### 
# Donner un intervalle de confiance au niveau 95% pour le délai 
# moyen à l'arrivée dans ces conditions. Donner également un intervalle
# de confiance pour le délai à l'arrivée d'un vol en particulier avec
# ces conditions. Est-ce que la vraie valeur observée se trouve dans 
# ce dernier intervalle de confiance?

#### Exercice #### 
# Tracer un nuage de points des prévisions en fonction des observations
# dans l'échantillon test. Est-ce que le modèle semble bon?

# Erreur quadratique moyenne de prédiction
mse_complet <- RMSEp(obs = BOStest$arr_delay,
                     pred = predict(object = modele_complet, newdata = BOStest))^2
mse_complet



## Sélection de variables


### Sélection de variables par sélection de sous-modèles

# Ajustons tous les sous-modèles possibles. 
# (Cette étape prend environ 5 minutes à rouler sur mon ordinateur.)

all_possible <- ols_step_all_possible(modele_complet)

# Le meilleur modèle selon le critère du AIC est le suivant.
as.data.frame(all_possible[all_possible$aic == min(all_possible$aic), ])

#### Exercice. #### 
# Ajustez ce modèle sur les données BOStrain et nommez le `modele_select`.

modele_select <- lm(# à compléter)
summary(modele_select)

#### Exercice. #### 
# Est-ce que ce modèle est meilleur que les précédents pour prédire
# les délais d'arrivée de nouveaux vols entre New York et Boston en
# juillet? Utilisez la fonction `RMSEp` pour comparer les observations
# dans l'échantillon test et les prévisions obtenues avec la fonction
# `predict` et le modèle choisi.



#### Exercice. ####
# Quel est le meilleur modèle selon le critère du coefficient 
# de détermination ajusté `adjr`? 


### Lasso
# Ajustons un modèle de régression lasso sur nos données d'entraînement,
# en considérant les mêmes variables que celles dans le modèle complet.

modele_lasso <- glmnet(x = modele_complet$x[, -1], y = modele_complet$y,
                       family = "gaussian", alpha = 1)
plot(modele_lasso, xvar = "lambda")

# Le poids associé au terme de régularisation est choisi par validation
# croisée à 10 plis.

set.seed(413) # pour avoir des résultats réplicables
cv_out <- cv.glmnet(x = modele_complet$x[, -1], y = modele_complet$y, family = "gaussian", alpha = 1)
plot(cv_out)

best_lam <- cv_out$lambda.1se
log(best_lam)

coef(modele_lasso, s = best_lam)


# Dans cet exemple, la régression lasso propose de conserver seulement
# 5 variables explicatives. Par contre l'erreur quadratique moyenne 
# remonte:

modele_lasso <- lm(arr_delay ~ dep_delay + origin + evening_rush_hour + seats + pressure, data = BOStrain)
summary(modele_lasso)
mse_lasso <- RMSEp(obs = BOStest$arr_delay,
pred = predict(object = modele_lasso, newdata = BOStest))^2
mse_lasso



#### Résumé des résultats pour la modélisation de `arr_delay`

resume_mod_arr_delay <- 
  data.frame(AIC = c(AIC(modele_simple), AIC(modele_complet),
                     AIC(modele_select), AIC(modele_lasso)),
             RMSEtest = sqrt(c(mse_simple, mse_complet, mse_select, mse_lasso)))
rownames(resume_mod_arr_delay) <- c("modèle simple", "modèle complet",
                                    "modèle avec sélection de variables",
                                    "modèle avec sélection LASSO")
resume_mod_arr_delay