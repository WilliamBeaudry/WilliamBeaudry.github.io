# williambeaudry.github.io

Site personnel de **William Beaudry** — étudiant à la maîtrise en science
politique à l'Université Laval, conseiller à la recherche chez Élections Québec.

**→ [williambeaudry.github.io](https://williambeaudry.github.io)**

Le site présente mon parcours, mes publications et communications, mes projets
de recherche, et donne accès à mes documents en PDF.

---

## Comment le site est construit

Une page statique hébergée sur GitHub Pages, mais dont le contenu n'est pas
écrit à la main : il vit dans des fichiers YAML, et un script R le transforme
en HTML.

```
contenu/*.yml  ──►  build.R  ──►  index.html
```

| Dossier / fichier | Rôle |
|---|---|
| `contenu/` | Le contenu du site : identité, parcours, documents, publications, projets |
| `modeles/gabarit.html` | La structure de la page |
| `build.R` | Génère `index.html` à partir du contenu |
| `publier.R` | Génère puis publie (`git add` / `commit` / `push`) |
| `documents/` | Les PDF consultables depuis le site |
| `assets/` | Feuilles de style, scripts et polices du thème |
| `projet_session/` | Projet d'analyse en R : participation électorale autochtone, élections fédérales de 2021 |
| `GUIDE.md` | Mode d'emploi détaillé |

`index.html` est un fichier **généré** : il ne doit pas être modifié à la main.

## Mettre le site à jour

```r
# après avoir modifié contenu/*.yml
source("build.R")     # regénère index.html
source("publier.R")   # regénère et publie
```

Le détail — ajouter un PDF, une expérience, une publication, une section — est
dans [`GUIDE.md`](GUIDE.md). Seul le paquet R `yaml` est nécessaire.

## Un projet visible depuis le site

**« Plus c'est mieux ? »** — La proportion d'électeurs autochtones dans une
circonscription influence-t-elle la participation électorale ? Analyse en R
(`tidyverse`, `ggplot2`, `rvest`) des résultats des élections fédérales de 2021,
croisés avec les données de participation des communautés autochtones de 2004
à 2011. Sources : `projet_session/`.

## Crédits

Thème [Read Only](https://html5up.net/read-only) par
[HTML5 UP](https://html5up.net) (@ajlkn), sous licence
[CC BY 3.0](https://creativecommons.org/licenses/by/3.0/) — voir `LICENSE.txt`.
Contenu, mise en forme des sections et chaîne de génération : William Beaudry.
