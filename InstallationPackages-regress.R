# Formation continue en apprentissage automatique
# Installation des packages R requis
# 9 fevrier 2018
# M-P Cote et D Beauchemin

install.packages(
  c(
    "olsrr",         # utilitaire modèles linéaires
    "nycflights13",  # donnees vols d'avion
    "car" ,           # fonction Anova (tests globaux variables categoriques)
    "glmnet"          # lasso
  ),
  dependencies = TRUE
)


