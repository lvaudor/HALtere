# HALtere

Cette application shiny propose des visualisations portant sur des **corpus bibliographiques** issus de **HAL** (par exemple listes de documents établis par les structures elles-même, sous forme de collections, ou recherches par auteur ou mots-clé).
Elle présente notamment :

- les **liens de co-autorat** existant entre les membres du collectif
- les **mots** (éventuellement spécifiques à certaines périodes) les plus représentés dans le corpus.
- la liste des références elle-même, avec des possibilités de filtrage (par année, par laboratoire, par type de document, etc.).

Ces représentations et tables peuvent porter sur la **totalité du corpus** (all documents) ou **seulement sur les articles**, et sur une **période de temps choisie**.

# HALtere en ligne

Pour la version de HALtere déployée en ligne (https://isig-apps.ens-lyon.fr/app/HALtere), nous avons choisi de montrer les listes de publications de 

- BIOEENVIS
- EVS (collection EVS_UMR5600
- DRIIHM (collection LABEX-DRIIHM)
- OHM-Vallée du Rhône (collection OHM-VALLEE_DU_RHONE)
- GloUrb
- OneWater 
- OSR 
- Packages_R

Les données utilisées pour ces visualisations sont issues de HAL, et ont été récupérées en utilisant les API de HAL. Les données présentées ici sont **mises à jour une fois par jour**.

# HALtere en local

Pour utiliser HALtere **pour votre structure ou à votre thématique d'intérêt**, il est nécessaire de **faire tourner l'application en local**.

Le plus simple est de cloner le repo (https://github.com/lvaudor/HALtere) et de faire tourner l'application à partir de RStudio (en ouvrant le projet RStudio et en lançant `shiny::runApp()` dans la console). 

Pour préparer les données HALtere de votre choix, vous pourrez utiliser la fonction `prepare_all_data()`, qui permet de récupérer les données de HAL à partir d'une requête personnalisée, et de les formater pour l'application.

# Formater la requête pour récupérer les données de HAL

Il est possible de formater la requête pour récupérer les données de HAL en utilisant les champs de recherche disponibles sur HAL. Par exemple, pour récupérer les publications d'une structure, on peut utiliser le champ `collCode_s` (code de collection), ou pour récupérer les publications d'un auteur, on peut utiliser le champ `authIdHal_s` (identifiant HAL de l'auteur).

Voici quelques exemples de requêtes pour récupérer les données de HAL. Ici, on récupère les données de la collection "BIOEENVIS":

```{r, eval=FALSE}
HALtere::prepare_all_data(
    custom_name="BIOEENVIS", # nom qui sera donné à la liste de publications
    query = 'collCode_s:"BIOEENVIS"' # requête servant à récupérer la liste sur l'API HAL
)
```

Ici, on récupère les données publiées par une personne en particulier:

```{r, eval=FALSE}
HALtere::prepare_all_data(
    custom_name="Lise Vaudor", 
    query ='authIdHal_s:"lise-vaudor"' 
)
```

Là, on récupère l'ensemble des publications qui comprennent "R package" dans le titre ou titre abrégé:

```{r, eval=FALSE}
HALtere::prepare_all_data(
    custom_name="Packages_R",
    query = 'title_autocomplete:"R package"'
)
```

Les données sont créées, par défaut, dans le répertoire de travail du projet HALtere sous "inst/data_HALtere".

L'import des données peut prendre un certain temps, en fonction du nombre de publications récupérées et traitées. La partie la plus chronophage de l'import consiste à **traduire** en anglais les titres, mots-clés et abstracts des publications si ceux-ci ont été renseignés en français et non en anglais sur l'outil de saisie HAL. En effet, le parti pris est d'homogénéiser la langue des publications vers l'**anglais** (en tant que "langue internationale"). Lorsque les titres, mots-clés et abstracts sont déjà renseignés en anglais, l'import est plus rapide. Lorsque les titres, mots-clés et abstracts sont renseignés dans d'autres langues que français ou anglais, ils ne sont pas pris en compte dans l'analyse textuelle.

Il est recommandé de faire tourner la fonction `prepare_all_data()` pour chaque liste de publications que l'on souhaite suivre régulièrement, afin de bénéficier de la mise à jour automatique des données (voir ci-dessous).

# Mise à jour des données

Si l'on refait tourner la fonction `prepare_all_data()` pour un même répertoire (custom_name) sans l'avoir supprimé au préalable, les **données seront mises à jour** en récupérant et traitant **uniquement les publications dans HAL ayant été modifiées depuis le dernier import**. Il est recommandé de conserver les données dans le répertoire pour les listes de publications que l'on souhaite suivre régulièrement, afin de bénéficier de cette mise à jour automatique (le processus de mise à jour est plus rapide que le processus d'import initial, qui traite un grand nombre de publications et qui peut prendre un certain temps).
