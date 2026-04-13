# Prédiction du Risque de Défaut de Crédit
### Classification Supervisée — Naïve Bayes vs Régression Logistique

> **Projet de Compléments de Mathématiques — L3 MIASHS**  
> Auteurs : Mohamed FOFANA · Abdou FALL  
> Année universitaire : 2025-2026

---

## Présentation

Ce projet aborde la **prédiction du risque de crédit** à l'aide de méthodes de classification supervisée. L'objectif est de déterminer, à partir des caractéristiques d'un emprunteur et de son prêt, s'il est susceptible d'être en **situation de défaut de paiement** (`loan_status = 1`) ou non (`loan_status = 0`).

Deux classifieurs sont entraînés, évalués et comparés :
- **Classifieur Naïve Bayes** — modèle probabiliste basé sur le théorème de Bayes avec hypothèse d'indépendance conditionnelle des variables
- **Régression Logistique** — modèle linéaire généralisé qui modélise directement la probabilité de défaut

Les résultats sont présentés via une **application web interactive Shiny** permettant l'exploration des données et la prédiction en temps réel.

---

## Jeu de données

| Caractéristique | Valeur |
|---|---|
| Source | [Kaggle — Credit Risk Dataset](https://www.kaggle.com/datasets/laotse/credit-risk-dataset) |
| Observations | 32 581 emprunteurs |
| Variables | 12 variables explicatives + 1 variable cible |
| Variable cible | `loan_status` (0 = Remboursé, 1 = Défaut) |
| Taux de défaut | ~21,8 % |

### Variables explicatives

| Variable | Type | Description |
|---|---|---|
| `person_age` | Numérique | Âge de l'emprunteur |
| `person_income` | Numérique | Revenu annuel (USD) |
| `person_home_ownership` | Catégorielle | Statut résidentiel (RENT, MORTGAGE, OWN, OTHER) |
| `person_emp_length` | Numérique | Ancienneté professionnelle (années) |
| `loan_intent` | Catégorielle | Motif du prêt (PERSONAL, EDUCATION, MEDICAL…) |
| `loan_grade` | Catégorielle | Note de risque attribuée (A à G) |
| `loan_amnt` | Numérique | Montant du prêt (USD) |
| `loan_int_rate` | Numérique | Taux d'intérêt (%) |
| `loan_percent_income` | Numérique | Ratio montant du prêt / revenu |
| `cb_person_default_on_file` | Catégorielle | Historique de défaut (Y/N) |
| `cb_person_cred_hist_length` | Numérique | Durée de l'historique de crédit (années) |

### Variables dérivées (feature engineering)

| Variable | Formule | Description |
|---|---|---|
| `risk_score` | `loan_percent_income × loan_int_rate` | Score de risque composite |
| `high_risk_grade` | `loan_grade ∈ {D, E, F, G}` | Indicateur de grade à haut risque |

---

## Structure du projet

```
Projet/
├── app.R                       # Application Shiny (UI + Server)
├── credit_risk_dataset.csv     # Jeu de données brut
├── modele_nb.rds               # Modèle Naïve Bayes entraîné (généré par ProjetCompltMaths.R)
├── modele_logit.rds            # Modèle Régression Logistique entraîné (généré par ProjetCompltMaths.R)
├── ProjetCompltMaths.R         # Script R principal (entraînement + évaluation des modèles)
├── ProjetCompltMaths.Rmd       # Rapport R Markdown
├── ProjetCompltMaths.pdf       # Rapport compilé (PDF)
├── ProjetCompltMaths.html      # Rapport compilé (HTML)
├── .gitignore                  # Fichiers exclus du versionnement
└── README.md                   # Ce fichier
```

---

## Application Shiny

L'application comprend quatre onglets :

1. **Aperçu des données** — tableau interactif, résumé statistique, valeurs manquantes, répartition de la variable cible
2. **Analyse univariée** — histogrammes et boxplots pour les variables numériques, diagrammes en barres pour les variables catégorielles
3. **Analyse bivariée** — comparaison des distributions selon le statut de défaut, matrice de corrélation
4. **Prédiction** — formulaire de saisie permettant d'obtenir en temps réel la probabilité de défaut selon les deux modèles

### Packages R requis

```r
install.packages(c(
  "shiny", "shinydashboard", "ggplot2", "corrplot",
  "DT", "caret", "rpart", "glmnet", "naivebayes", "scales"
))
```

---

## Lancer l'application en local

```r
# 1. Cloner le dépôt
# git clone https://github.com/<votre-compte>/<votre-repo>.git

# 2. Générer les modèles (si modele_nb.rds et modele_logit.rds sont absents)
source("ProjetCompltMaths.R")

# 3. Lancer l'application
shiny::runApp("app.R")
```

> **Note :** les fichiers `modele_nb.rds` et `modele_logit.rds` doivent être présents dans le répertoire avant de lancer l'app.

---

## Résultats principaux

| Métrique | Naïve Bayes | Régression Logistique |
|---|---|---|
| Accuracy | ~83 % | ~86 % |
| AUC-ROC | ~0.83 | ~0.90 |
| Sensibilité | Élevée | Modérée |
| Spécificité | Modérée | Élevée |

La **régression logistique** obtient de meilleures performances globales, notamment en termes d'AUC-ROC. Le **Naïve Bayes** reste compétitif avec l'avantage d'être plus rapide à entraîner et plus interprétable dans un contexte probabiliste.

---

## Déploiement

L'application est déployable sur [shinyapps.io](https://www.shinyapps.io) via le package `rsconnect` :

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(
  name   = "<votre-compte>",
  token  = "<votre-token>",
  secret = "<votre-secret>"
)
rsconnect::deployApp()
```

Voir la section déploiement dans le guide ci-dessous pour plus de détails.

---

## Licence

Projet académique — L3 MIASHS, Université Paul Valéry Montpellier 3. Usage personnel et éducatif uniquement.
