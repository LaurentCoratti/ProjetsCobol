# 💼 Assortiment de deux fichiers à lecture sequentielle et édition d'états

[![COBOL](https://img.shields.io/badge/Language-COBOL-blue)](https://gnucobol.sourceforge.io/)

---

## 📝 Description

Ce projet est une programme COBOL écrit lors d’une formation. Il contient à la fois :

- Le code **original destiné aux mainframes
- Un dossier JCL pour le traitement batch
- Une version adaptée pour être compatible avec des environnements comme **GnuCOBOL** dans un dossier séparé

### Fonctionnalités :
- 📂 Lecture d’un fichier séquentiel (`INP001`) contenant des mouvements bancaires
- 📂 Lecture d’un fichier séquentiel (`INP002`) contenant des comptes clients
- 🧲 Calcul de cumuls par type d’opération :  
  → Retraits, dépôts, cartes bleues
- 🧲 Operations sur les comptes clients
  → Mise à jour et création de comptes
- 💾 Génération d’un état détaillé des opérations, avec balances par client
  → 📂 Création d’un fichier (`OUT001`) mettant à jour le fichier (`INP002`)
  → 📂 Création d’un fichier (`ETATANO`) contenant l'édition d'états d'anomalies
  → 📂 Création d’un fichier (`ETATCLI`) contenant l'édition d'états de comptes clients
  → 📂 Création d’un fichier (`ETATANO`) contenant l'édition d'états d'anomalies
  → 📂 Affichage des statistiques en `SYSOUT`
- 🚨 Gestion des erreurs renforcée

---

## 📁 Arborescence du projet

```text
.
├── icl/                   # Version d'origine du programme COBOL de formation (mainframe)
│   └── JCL/               # Scripts JCL associés
├── EXECUTION GNUCOBOL/     # Version adaptée pour un environnement Unix/Linux
│   ├── ARI031B.cbl        # Programme adapté
│   └── INP001.txt         # Fichier d'entrée attendu
│   └── INP002.txt         # Fichier d'entrée attendu
└── README.md              # Ce fichier
