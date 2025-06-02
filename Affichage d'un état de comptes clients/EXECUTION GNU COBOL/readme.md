# ARI011B - Traitement d'opérations bancaires en COBOL

[![COBOL](https://img.shields.io/badge/Language-COBOL-blue)](https://gnucobol.sourceforge.io/)
[![GnuCOBOL](https://img.shields.io/badge/Compiler-GnuCOBOL-green)](https://sourceforge.net/projects/gnucobol/)

## 📝 Description

Adaptation et amélioration d'un programme COBOL de gestion d'opérations bancaires, initialement développé lors d'une formation, pour le compiler avec **GnuCOBOL 3+**.

**Fonctionnalités principales** :
- Lecture d'un fichier séquentiel de mouvements bancaires
- Calcul des cumuls par type d'opération (retraits, dépôts, cartes bleues)
- Édition d'un état détaillé avec balances
- Gestion robuste des erreurs


## 🛠 Installation et exécution

### Prérequis
- GnuCOBOL 3.0+
- Fichier d'entrée `INP001` dans le répertoire d'exécution

### Compilation
```bash
cobc -x ARI011B.cbl -o ARI011B