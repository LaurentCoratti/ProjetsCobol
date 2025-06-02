# 💼 Affichage d'un état de comptes clients

[![COBOL](https://img.shields.io/badge/Language-COBOL-blue)](https://gnucobol.sourceforge.io/)

---

## 📝 Description

Ce projet est une programme COBOL écrit lors d’une formation. Il contient à la fois :

- Le code **original destiné aux mainframes
- Un dossier JCL pour le traitement batch
- Une version adaptée pour être compatible avec des environnements comme **GnuCOBOL** dans un dossier séparé

### Fonctionnalités :
- 📂 Lecture d’un fichier séquentiel (`INP001`) contenant des mouvements bancaires
- 🧲 Calcul de cumuls par type d’opération :  
  → Retraits, dépôts, cartes bleues  
- 💾 Génération d’un état détaillé des opérations, avec balances par client
- 🚨 Gestion des erreurs renforcée

---

## 📁 Arborescence du projet

```text
.
├── icl/                   # Version d'origine du programme COBOL de formation (mainframe)
│   └── JCL/               # Scripts JCL associés
├── EXECUTION GNUCOBOL/     # Version adaptée pour un environnement Unix/Linux
│   ├── ARI011B.cbl        # Programme adapté
│   └── INP001.txt         # Fichier d'entrée attendu
│   └── SYSOUT.pgn         # Capture SYSOUT de l'execution
└── README.md              # Ce fichier
