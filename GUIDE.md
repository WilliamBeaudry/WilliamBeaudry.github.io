# Guide d'entretien du site

Ton site n'est plus écrit à la main. Le contenu vit dans des fichiers texte
simples (`contenu/*.yml`) et un script R (`build.R`) fabrique `index.html`
à partir de ceux-ci.

**Règle d'or : ne modifie jamais `index.html` directement.** Il est écrasé
à chaque génération.

---

## Le cycle de travail

```r
# 1. Modifie ce que tu veux dans contenu/*.yml
# 2. Regénère la page
source("build.R")
# 3. Ouvre index.html dans ton navigateur pour vérifier
# 4. Publie en ligne (build + commit + push)
source("publier.R")
```

Le site est en ligne sur <https://williambeaudry.github.io> une ou deux minutes
après le `push`.

---

## Où se trouve quoi

| Fichier / dossier | Ce qu'il contient |
|---|---|
| `contenu/site.yml` | Ton nom, ta photo, tes réseaux sociaux, **l'ordre des sections** et les textes libres |
| `contenu/documents.yml` | Les PDF consultables sur le site |
| `contenu/parcours.yml` | Ta ligne du temps : expériences et formation |
| `contenu/publications.yml` | Publications, rapports, communications |
| `contenu/projets.yml` | Les vignettes de projets |
| `documents/` | Dépose ici les PDF que tu ajoutes |
| `modeles/gabarit.html` | La structure de la page (à toucher seulement pour changer la mise en page) |
| `assets/css/custom.css` | Les styles des nouvelles sections (parcours, documents, publications) |
| `build.R` | Le script qui fabrique `index.html` |
| `publier.R` | Le script qui fabrique **et** publie |
| `index.html` | **Généré. Ne pas éditer.** |

---

## Ajouter un PDF

**Méthode rapide** (le script copie le fichier et met le YAML à jour) :

```r
source("build.R")
ajouter_document(
  chemin_pdf  = "C:/mes_dossiers/emploi/lettre_cegep.pdf",
  titre       = "Lettre de présentation — Cégep de Lanaudière",
  categorie   = "Candidature",
  description = "Lettre déposée en août 2026."
)
```

**Méthode manuelle** : dépose le PDF dans `documents/`, puis ajoute dans
`contenu/documents.yml` :

```yaml
- titre: "Titre du document"
  fichier: "documents/mon_fichier.pdf"
  date: "2026-08"
  categorie: "Candidature"
  epingle: false          # true = affiché en premier
  apercu: ""              # image de vignette, optionnel
  description: |
    Une ou deux phrases de description.
```

Puis `source("build.R")`.

---

## Le parcours : entrées et rubriques

`contenu/parcours.yml` est découpé en **rubriques**. Chaque rubrique de premier
niveau devient un bloc de la ligne du temps, **dans l'ordre du fichier**.

### Ajouter une entrée à une rubrique existante

```yaml
  - poste: "Chargé de cours"
    organisation: "Cégep de Lanaudière"
    lieu: "Joliette"
    debut: "2026-08"
    fin: ""                # vide = « à ce jour », et l'entrée passe en tête
    description: |
      Enseignement de deux cours d'introduction à la science politique.
```

L'intitulé de l'entrée peut s'appeler `poste`, `diplome`, `formation`, `titre`
ou `nom` — le premier trouvé est utilisé. L'organisation peut s'appeler
`organisation` ou `etablissement`. Le tri du plus récent au plus ancien est
automatique : ajoute ton entrée où tu veux dans la rubrique.

### Créer une rubrique

Ajoute une clé de premier niveau, colle des entrées dessous, et c'est tout —
`build.R` n'a pas besoin d'être modifié :

```yaml
implication_etudiante:

  - titre: "Secrétaire aux finances"
    organisation: "AEESPUL, puis APEUL"
    lieu: "Québec"
    debut: "2022-09"
    fin: "2024-05"
    description: |
      Gestion budgétaire et reddition de comptes.
```

### Nommer une rubrique

Sans rien de plus, le titre affiché est dérivé de la clé
(`implication_etudiante` → « Implication etudiante », sans accent). Pour
maîtriser le texte, ajoute en haut du fichier une ligne `titre_<clé>` :

```yaml
titre_experiences: "Expérience professionnelle"
titre_formation: "Formation académique"
titre_formations_connexes: "Formations connexes"
titre_implication_etudiante: "Implication étudiante"
```

Une faute de frappe dans un `titre_` déclenche un avertissement qui liste les
rubriques réellement présentes — rien ne disparaît en silence.

### Réordonner ou retirer une rubrique

L'ordre des rubriques à l'écran est l'ordre des clés dans le fichier : déplace
le bloc complet. Une rubrique vide ne s'affiche pas.

---

## Ajouter une publication

Dans `contenu/publications.yml` :

```yaml
- titre: "Titre de la publication"
  type: "Article"          # apparaît dans la pastille verte
  auteurs: "Beaudry, W."
  editeur: "Revue ou institution"
  date: "2026-05"
  numero: ""               # ex. « vol. 12, no 3 »
  url: ""                  # lien externe...
  fichier: ""              # ...ou PDF local (l'un ou l'autre)
  resume: |
    Résumé en quelques lignes.
```

---

## Ajouter ou réordonner une section

Dans `contenu/site.yml`, sous `sections:`. **L'ordre des blocs = l'ordre à
l'écran et dans le menu.** Chaque section a un `type` :

| `type` | Effet |
|---|---|
| `texte` | Texte libre, avec image de bannière optionnelle |
| `texte_icones` | Texte libre + liste à pictogrammes |
| `parcours` | Ligne du temps (lit `parcours.yml`) |
| `documents` | Liste de PDF (lit `documents.yml`) |
| `publications` | Liste de publications (lit `publications.yml`) |
| `projets` | Vignettes de projets (lit `projets.yml`) |
| `contact` | Bouton d'envoi de courriel |

Options utiles sur n'importe quelle section :

- `afficher: false` — masque la section sans la supprimer
- `menu: false` — affiche la section mais pas son lien dans le menu

Une section dont la source de données est vide disparaît automatiquement :
tant que `publications.yml` est vide, aucune section « Publications » ne
s'affiche.

---

## Mise en forme des textes

Dans tous les champs `texte`, `description`, `resume` et `accroche`.

### À l'intérieur d'une ligne

| Tu écris | Tu obtiens |
|---|---|
| `**gras**` ou `\gras{...}` | **gras** |
| `*italique*` ou `\italique{...}` | *italique* |
| `\rouge{...}` | le mot en rouge, mis en évidence |
| `[libellé](https://adresse)` | un lien cliquable |

Les deux syntaxes cohabitent : utilise celle qui te vient naturellement.

### D'une ligne à l'autre

C'est ici que les surprises arrivent. **Deux lignes qui se suivent sont
recollées en un seul paragraphe** — c'est la règle du Markdown, et elle évite
qu'un texte se disloque quand tu passes à la ligne pour rester lisible dans
l'éditeur. Pour séparer réellement deux idées, trois outils :

```yaml
    description: |
      Un premier paragraphe. Cette ligne
      et celle-ci n'en formeront qu'un seul.

      Une ligne vide au-dessus : voici un deuxième paragraphe.

      - Une puce
      - Une deuxième puce
      - Une troisième

      Une adresse \\
      sur deux lignes, grâce aux deux barres obliques.
```

| Ce que tu veux | Ce que tu écris |
|---|---|
| Un nouveau paragraphe | Une **ligne vide** entre les deux |
| Un point de forme | La ligne commence par `- ` (tiret + espace) |
| Un simple retour à la ligne | `\\` à la **fin** de la ligne, comme en LaTeX |

Les puces se reconnaissent aussi avec `*` ou `+` en début de ligne. Une suite
de lignes à puces forme une seule liste ; dès qu'une ligne sans tiret apparaît,
la liste se referme et le texte reprend.

Les caractères `<`, `>` et `&` sont protégés automatiquement : tu peux écrire
« R&D » ou « < 5 % » sans rien casser.

---

## En cas de pépin

- **`source("build.R")` plante avec une erreur YAML** → une virgule, un
  guillemet ou une indentation manquante dans le fichier `.yml` que tu viens
  de modifier. Le message d'erreur indique la ligne.
- **L'indentation compte** en YAML : deux espaces, jamais de tabulation.
- **Le paquet yaml est manquant** → `install.packages("yaml")`.
- **Récupérer l'ancien site** → `git show HEAD:index.html` dans le terminal,
  ou l'onglet Git de RStudio (History).
- **Le `push` échoue** → `index.html` est quand même regénéré ; termine avec
  l'onglet Git de RStudio.
