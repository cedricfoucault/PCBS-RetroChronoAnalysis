Projet PCBS : Modélisation du Temporal Order Judgement dans l'effet de rétro-perception
===========================

**L'ensemble du rapport et du code du projet PCBS sont contenus dans le fichier `TOJ_model_report.Rmd`.**

Il s'agit d'un document R markdown **dynamique**, il faut donc l'exécuter pour visualiser et interagir avec le rapport.

Pour l'exécuter, je recommande d'utiliser RStudio, disponible gratuitement sur [rstudio.com](https://www.rstudio.com).

Une fois RStudio installé, il faut installer le package `rmarkdown`. Pour cela, exécuter la ligne de commande suivante dans la console R :
```install.packages("rmarkdown")```

Ensuite, il ne reste plus qu'à ouvrir le fichier TOJ\_model\_report.Rmd avec RStudio et cliquer sur le bouton "Run Document" dans la toolbar, ou bien exécuter dans la console `rmarkdown::run("TOJ_model_report.Rmd")`.

Merci d'ignorer les dossiers old\_code et old\_figures qui ne rentrent plus dans le cadre du projet PCBS.

## Questionnaire

*What was your level in programming before starting the class (roughly)?*

Niveau avancé de programmation en général. Par contre, je n'avais jamais utilisé R et R Markdown auparavant.

*What you learned while working for this class (throught the lectures and/or the project):*

Ce projet a été l'occasion pour moi d'apprendre R et R Markdown et de les appliquer à un projet d'analyse scientifique expérimentale.

L'écriture du rapport m'a forcé à rendre mon code plus lisible et plus simple, à utiliser une approche plus basée sur des structures fonctionnelles que des structures impératives (modélisation par des fonctions plutôt que par des matrices, suppression des boucles for) et à minimiser les dépendances (injection de dépendances dans les fonctions).

Cela m'a aussi appris à écrire un rapport d'analyse de données partageable qui combine code, visualisation et explications. Avant le cours de Christophe Pallier, je n'avais jamais vu de tels documents scientifiques et je ne savais pas qu'il existait des outils comme R Markdown pour les produire aussi facilement. Je trouve cela "assez génial" et j'ai hâte de les utiliser à nouveau dans mes recherches ! :-)

*Any suggestions to improve the class for the future:*

Il est difficile de faire un seul cours qui soit intéressant et utile pour tous les étudiants, vu que leur niveau en programmation est très hétérogène.

Je pense que les étudiants qui ne connaissent pas ou peu la programmation gagneraient plus à apprendre les bases de la programmation en général, plutôt que d'apprendre à utiliser un framework comme pygame/expyriment/scipy, car ce sont des compétences plus transférables. De plus, comme je l'ai constaté en leur donnant de l'aide, c'est sur les fondamentaux qu'ils bloquent, par exemple : comment utiliser bash et la ligne de commande, incompréhension du concept de current working directory, savoir utiliser des listes, trouver comment randomiser les essais, trouver comment générer des séquences aléatoires, savoir comment décomposer un problème et utiliser des fonctions pour résoudre les sous-problèmes...

Pour les étudiants ayant un niveau de programmation avancé, l'idéal serait d'avoir un cours séparé. Apprendre pygame/expyriment/scipy n'est pas non plus très utile car on peut se débrouiller seul avec la doc et les tutos dispos en ligne ou dans des bouquins. Par contre, c'est bien de nous faire connaître des outils spécifiquement utiles pour la recherche en sciences cognitives. Par exemple, j'ai vraiment apprécié de découvrir l'existence de R Markdown et Jupyter Notebook, j'aurai aimé avoir plus de temps pour les creuser.
