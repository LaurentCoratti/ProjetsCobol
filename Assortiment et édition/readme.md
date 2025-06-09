# 💼 Assortiment de deux fichiers à lecture séquentielle et édition d'états

[![COBOL](https://img.shields.io/badge/Language-COBOL-blue)](https://gnucobol.sourceforge.io/)

---

## 📝 Description

Ce projet est un programme COBOL écrit lors d’une formation. Il contient :

- Le code **original** destiné aux mainframes  
- Un dossier **JCL** pour le traitement batch  
- Une **version adaptée** pour des environnements comme **GnuCOBOL**

### ✨ Fonctionnalités

- 📂 Lecture d’un fichier séquentiel `INP001` contenant des mouvements bancaires  
- 📂 Lecture d’un fichier séquentiel `INP002` contenant des comptes clients  
- 🧲 Calcul de cumuls par type d’opération :  
  → Retraits, dépôts, cartes bleues  
- 🧲 Opérations sur les comptes clients :  
  → Mise à jour et création de comptes  
- 💾 Génération d’un état détaillé des opérations, avec balances par client :  
  - 📂 Création du fichier `OUT001` : mise à jour de `INP002`  
  - 📂 Création du fichier `ETATANO` : édition des anomalies  
  - 📂 Création du fichier `ETATCLI` : édition des comptes clients  
  - 📂 Affichage des statistiques dans `SYSOUT`  
- 🚨 Gestion des erreurs renforcée

---

## 📁 Arborescence du projet

```text
.
├── icl/                        # Version d'origine (mainframe)
│   └── JCL/                    # Scripts JCL associés
├── EXECUTION GNUCOBOL/        # Version adaptée pour Unix/Linux
│   ├── ARI031B.cbl            # Programme adapté
│   ├── INP001.txt             # Fichier d'entrée (mouvements bancaires)
│   └── INP002.txt             # Fichier d'entrée (comptes clients)
└── README.md                  # Ce fichier