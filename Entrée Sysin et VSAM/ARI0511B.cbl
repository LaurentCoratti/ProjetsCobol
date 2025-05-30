      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO51B                                   *
      *  NOM DU REDACTEUR : CORATTI                                   *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 21/04/2023                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *    METTRE A JOUR LE FICHIER DES COMPTES CLIENTS A PARTIR DES  *
      *      MOUVEMENTS BANCAIRES ET EDITER UN ETAT DES OPERATIONS.   *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * JJ/MM/SSAA    !                                               *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO51B.
      *
      *                  ==============================               *
      *=================<  ENVIRONMENT      DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      **********************
       ENVIRONMENT DIVISION.
      **********************
      *
      *======================
       CONFIGURATION SECTION.
      *======================
      *
      *--------------
       SPECIAL-NAMES.
      *--------------
           DECIMAL-POINT IS COMMA.
      *
      *=====================
       INPUT-OUTPUT SECTION.
      *=====================
      *
      *-------------
       FILE-CONTROL.
      *-------------
      *
      *                      -------------------------------------------
      *                      F-CPTE  : FICHIERS DES COMPTES CLIENTS
      *                      -------------------------------------------
           SELECT  F-CPTE-E          ASSIGN TO INP001
                                     ORGANIZATION IS INDEXED
                                     ACCESS MODE IS DYNAMIC
                                     RECORD KEY IS FS-CPTE-CPT
                                     ALTERNATE RECORD KEY IS FS-CPTE-CLI
                                     WITH DUPLICATES
                   FILE STATUS       IS WS-FS-F-CPTE.
      *                      -------------------------------------------
      *                      ETATCLI : FICHIER DES ETATS CLIENTS
      *                      -------------------------------------------
           SELECT  F-ETATCLI-S       ASSIGN TO ETATCLI
                   FILE STATUS       IS WS-FS-F-ETATCLI-S.
      *                      -------------------------------------------
      *                      ETATANO : FICHIER DES ETATS D'ANOMALIES
      *                      -------------------------------------------
           SELECT  F-ETATANO-S       ASSIGN TO ETATANO
                   FILE STATUS       IS WS-FS-F-ETATANO-S.
      *
      *                     -------------------------------------------
      *
      *
      *                  ==============================               *
      *=================<       DATA        DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      ***************
       DATA DIVISION.
      ***************
      *
      *
       FILE SECTION.
      *=============
      *
       FD  F-CPTE-E
           RECORD CONTAINS 50 CHARACTERS.
       01  FS-ENRG-F-CPT.
           05 FS-CPTE-CPT                  PIC X(10).
              88 FS-CPTE-CPT-MIN           VALUE LOW-VALUE.
           05 FS-CPTE-CLI                  PIC X(20).
              88 FS-CPTE-CLI-MIN           VALUE LOW-VALUE.
           05 FILLER                       PIC X(20).
      *
       FD  F-ETATCLI-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATCLI                 PIC X(80).
      *
       FD  F-ETATANO-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATANO                 PIC X(80).
      *
      *--------------------DESCRITION DE L'ENREGISTREMENT---------------
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
           COPY TP5LEDIT.
           COPY TP5CPTE.
      *
      *---------------------------------------------------------------*
      * FILE STATUS                                                   *
      *---------------------------------------------------------------*
      *
       01  WS-FS-F-CPTE                    PIC XX.
           88  OK-F-CPTE-E                 VALUE '00'.
           88  EOF-F-CPTE-E                VALUE '10'.
           88  OK-LEC-F-CPTE-E             VALUE '00' '02' '10'.
           88  OK-START                    VALUE '00' '23'.
           88  CLE-INEXISTANTE             VALUE '23'.
       01  WS-FS-F-ETATCLI-S               PIC XX.
           88  OK-F-ETATCLI-S              VALUE '00'.
       01  WS-FS-F-ETATANO-S               PIC XX.
           88  OK-F-ETATANO-S              VALUE '00'.
      *
      *---------------------------------------------------------------*
      * COMPTEURS                                                     *
      *---------------------------------------------------------------*
      *
       77  WS-DEM                          PIC S9(4) COMP
                                           VALUE 0.
       77  WS-DER                          PIC 9(4)  COMP
                                           VALUE 0.
      *
      *---------------------------------------------------------------*
      * VARIABLES D'EDITION COMPLEMENTAIRES                           *
      *---------------------------------------------------------------*
      *
       01  WS-LETAT-VIDE-ED.
           05 FILLER                       PIC X
                                           VALUE '!'.
           05 FILLER                       PIC X(28)
                                           VALUE ALL SPACE.
           05 FILLER                       PIC X(20)
                                           VALUE 'AUCUN ENREGISTREMENT'.
           05 FILLER                       PIC X(28)
                                           VALUE ALL SPACE.
           05 FILLER                       PIC X
                                           VALUE '!'.
      *
      *---------------------------------------------------------------*
      * VARIABLES DE CALCULS ET DE MOUVEMENTS                         *
      *---------------------------------------------------------------*
      *
       77  WS-BUFFER                       PIC X(80).
       77  WS-ERREUR                       PIC 9.
           88 WS-OK                        VALUE 0.
           88 WS-NOK                       VALUE 1.
       77  WS-CPT-MIN                      PIC 9(10).
       77  WS-CLI-MIN                      PIC X(14).
      *
      *---------------------------------------------------------------*
      * SYSIN                                                         *
      *---------------------------------------------------------------*
      *
       01  WS-SYSIN.
           05 WS-DEM-TYP                   PIC X.
              88 WS-DEM-A                  VALUE 'A'.
              88 WS-DEM-B                  VALUE 'B'.
           05 FILLER                       PIC X(79).
       01  WS-SYSIN-A REDEFINES WS-SYSIN.
           05 FILLER                       PIC X.
           05 WS-DEM-NOM                   PIC X(14).
           05 WS-DEM-CPT-DEB               PIC X(10).
           05 WS-DEM-CPT-FIN               PIC X(10).
           05 FILLER                       PIC X(45).
       01  WS-SYSIN-B REDEFINES WS-SYSIN.
           05 FILLER                       PIC X(15).
              88 FIN-SYSIN                 VALUE '$$$'.
           05 WS-DEM-CLI-DEB               PIC X(20).
           05 WS-DEM-CLI-FIN               PIC X(20).
           05 FILLER                       PIC X(25).
      *
      *                  ==============================               *
      *
      *=================<   PROCEDURE       DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
       PROCEDURE           DIVISION.
      *
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE ALGORITHMIQUE DU PROGRAMME      *
      *---------------------------------------------------------------*
      *                                                               *
      *    1 : LES COMPOSANTS DU DIAGRAMME SONT CODES A L'AIDE DE     *
      *        DEUX PARAGRAPHES  XXXX-COMPOSANT-DEB                   *
      *                          XXYY-COMPOSANR-FIN                   *
      *                                                               *
      *    2 : XX REPRESENTE LE NIVEAU HIERARCHIQUE                   *
      *        YY DIFFERENCIE LES COMPOSANTS DE MEME NIVEAU           *
      *                                                               *
      *    3 : TOUT COMPOSANT EST PRECEDE D'UN CARTOUCHE DE           *
      *        COMMENTAIRE QUI EXPLICITE LE ROLE DU COMPOSANT         *
      *                                                               *
      *                                                               *
      *===============================================================*
      *===============================================================*
      *
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT TRT PRINCIPAL          *
      *               =====================================           *
      *---------------------------------------------------------------*
      * DEBUT DU PROGRAMME                                            *
      *---------------------------------------------------------------*
       0000-TRT-PRINCIPAL-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
      *
           PERFORM 6000-ACCEPT-SYSIN-DEB
              THRU 6000-ACCEPT-SYSIN-FIN.
      *
           IF FIN-SYSIN
              DISPLAY 'SYSIN VIDE'
           END-IF.
      *
           PERFORM 6010-OUVRIR-F-CPTE-DEB
              THRU 6010-OUVRIR-F-CPTE-FIN.
      *
           PERFORM 6020-OUVRIR-ETATCLI-DEB
              THRU 6020-OUVRIR-ETATCLI-FIN.
      *
           PERFORM 6030-OUVRIR-ETATANO-DEB
              THRU 6030-OUVRIR-ETATANO-FIN.
      *
           PERFORM 7000-LOW-VALUE-CPT-DEB
              THRU 7000-LOW-VALUE-CPT-FIN.
      *
           PERFORM 6090-START-CPT-F-CPTE-DEB
              THRU 6090-START-CPT-F-CPTE-FIN.
      *
           PERFORM 6040-READ-NEXT-F-CPTE-DEB
              THRU 6040-READ-NEXT-F-CPTE-FIN.
      *
           PERFORM 7140-LIMITE-MIN-CPT-DEB
              THRU 7140-LIMITE-MIN-CPT-FIN.
      *
           PERFORM 7130-LOW-VALUE-CLI-DEB
              THRU 7130-LOW-VALUE-CLI-FIN.
      *
           PERFORM 6100-START-CLI-F-CPTE-DEB
              THRU 6100-START-CLI-F-CPTE-FIN.
      *
           PERFORM 6040-READ-NEXT-F-CPTE-DEB
              THRU 6040-READ-NEXT-F-CPTE-FIN.
      *
           PERFORM 7150-LIMITE-MIN-CLI-DEB
              THRU 7150-LIMITE-MIN-CLI-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 1000-TRT-SYSIN-DEB
              THRU 1000-TRT-SYSIN-FIN
              UNTIL FIN-SYSIN.
      *
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)                          *
      *---------------------------------------------------------------*
      *
           IF WS-DER NOT = 0
                 PERFORM 8060-BAS-ETATANO-DEB
                    THRU 8060-BAS-ETATANO-FIN
           END-IF.
      *
           PERFORM 8999-EDITION-STATISTIQUE-DEB
              THRU 8999-EDITION-STATISTIQUE-FIN.
      *
           PERFORM 6110-FERMER-F-CPTE-DEB
              THRU 6110-FERMER-F-CPTE-FIN.
      *
           PERFORM 6120-FERMER-ETATCLI-DEB
              THRU 6120-FERMER-ETATCLI-FIN.
      *
           PERFORM 6130-FERMER-ETATANO-DEB
              THRU 6130-FERMER-ETATANO-FIN.
      *
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *
      *---------------------------------------------------------------*
      *FIN DU PROGRAMME                                               *
      *---------------------------------------------------------------*
       0000-TRT-PRINCIPAL-FIN.
           STOP RUN.
      *---------------------------------------------------------------*
      *             DESCRIPTION DU COMPOSANT TRT SYSIN                *
      *             ===================================               *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT                                           *
      *---------------------------------------------------------------*
      *
       1000-TRT-SYSIN-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7010-INCR-WS-DEM-DEB
              THRU 7010-INCR-WS-DEM-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTENATIVE MULTIPLE)              *
      *---------------------------------------------------------------*
      *
           EVALUATE TRUE
             WHEN WS-DEM-A   PERFORM 2000-TRT-A-CPTE-DEB
                                THRU 2000-TRT-A-CPTE-FIN
             WHEN WS-DEM-B   PERFORM 2010-TRT-B-NOM-DEB
                                THRU 2010-TRT-B-NOM-FIN
             WHEN OTHER      PERFORM 2020-TRT-ANO-DEB
                                THRU 2020-TRT-ANO-FIN
           END-EVALUATE.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT  (OREILLETTE DROITE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 6000-ACCEPT-SYSIN-DEB
              THRU 6000-ACCEPT-SYSIN-FIN.
      *
       1000-TRT-SYSIN-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT ENRGISTREMENT TYPE A             *
      *     =============================================             *
      *---------------------------------------------------------------*
      *
       2000-TRT-A-CPTE-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7040-CPTE-CLE-DEB
              THRU 7040-CPTE-CLE-FIN.
      *
           PERFORM 6090-START-CPT-F-CPTE-DEB
              THRU 6090-START-CPT-F-CPTE-FIN.
      *
           EVALUATE TRUE
               WHEN WS-DEM-NOM        = SPACE
                    OR WS-DEM-CPT-DEB = SPACE
                    OR WS-DEM-CPT-FIN = SPACE
                    PERFORM 7090-ERR-VIDE-DEB
                       THRU 7090-ERR-VIDE-FIN
               WHEN WS-DEM-CPT-DEB IS NOT NUMERIC
                    OR WS-DEM-CPT-FIN IS NOT NUMERIC
                    PERFORM 7100-ERR-CARACTERES-DEB
                       THRU 7100-ERR-CARACTERES-FIN
               WHEN WS-DEM-CPT-DEB > WS-DEM-CPT-FIN
                    PERFORM 7110-ERR-BORNES-DEB
                       THRU 7110-ERR-BORNES-FIN
               WHEN WS-DEM-CPT-FIN < WS-CPT-MIN OR CLE-INEXISTANTE
                    PERFORM 7120-ERR-LIMITES-DEB
                       THRU 7120-ERR-LIMITES-FIN
           END-EVALUATE.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE SIMPLE)               *
      *---------------------------------------------------------------*
      *
           IF WS-NOK
                PERFORM 3000-TRT-ERR-A-DEB
                   THRU 3000-TRT-ERR-A-FIN
           ELSE PERFORM 3010-TRT-OK-A-DEB
                   THRU 3010-TRT-OK-A-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)                         *
      *---------------------------------------------------------------*
      *
      *
       2000-TRT-A-CPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT ENRGISTREMENT TYPE B             *
      *     =============================================             *
      *---------------------------------------------------------------*
      *
       2010-TRT-B-NOM-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7050-CLI-CLE-DEB
              THRU 7050-CLI-CLE-FIN.
      *
           PERFORM 6100-START-CLI-F-CPTE-DEB
              THRU 6100-START-CLI-F-CPTE-FIN.
      *
           EVALUATE TRUE
               WHEN WS-DEM-NOM        = SPACE
                    OR WS-DEM-CLI-DEB = SPACE
                    OR WS-DEM-CLI-FIN = SPACE
                    PERFORM 7090-ERR-VIDE-DEB
                       THRU 7090-ERR-VIDE-FIN
               WHEN WS-DEM-CLI-DEB    IS NOT ALPHABETIC
                    OR WS-DEM-CLI-FIN IS NOT ALPHABETIC
                     PERFORM 7100-ERR-CARACTERES-DEB
                       THRU 7100-ERR-CARACTERES-FIN
               WHEN WS-DEM-CLI-DEB > WS-DEM-CLI-FIN
                    PERFORM 7110-ERR-BORNES-DEB
                       THRU 7110-ERR-BORNES-FIN
               WHEN WS-DEM-CLI-FIN < WS-CLI-MIN OR CLE-INEXISTANTE
                    PERFORM 7120-ERR-LIMITES-DEB
                       THRU 7120-ERR-LIMITES-FIN
           END-EVALUATE.
      *
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE SIMPLE)               *
      *---------------------------------------------------------------*
      *
           IF WS-NOK
                PERFORM 3020-TRT-ERR-B-DEB
                   THRU 3020-TRT-ERR-B-FIN
           ELSE PERFORM 3030-TRT-OK-B-DEB
                   THRU 3030-TRT-OK-B-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)                         *
      *---------------------------------------------------------------*
      *
       2010-TRT-B-NOM-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT DEMANDE ERRONEE                  *
      *     ========================================                  *
      *---------------------------------------------------------------*
      *
       2020-TRT-ANO-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7060-OP-DEMANDE-NOK-DEB
              THRU 7060-OP-DEMANDE-NOK-FIN.
      *
           IF WS-DER = 0
              PERFORM 8040-ENTETE-ETATANO-DEB
                 THRU 8040-ENTETE-ETATANO-FIN
           END-IF.
      *
           PERFORM 8050-DETAIL-ETATANO-DEB
              THRU 8050-DETAIL-ETATANO-FIN.
      *
       2020-TRT-ANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT A ERRONEE                        *
      *     ==================================                        *
      *---------------------------------------------------------------*
      *
       3000-TRT-ERR-A-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           IF WS-DER = 0
              PERFORM 8040-ENTETE-ETATANO-DEB
                 THRU 8040-ENTETE-ETATANO-FIN
           END-IF.
      *
           PERFORM 8050-DETAIL-ETATANO-DEB
              THRU 8050-DETAIL-ETATANO-FIN.
      *
           PERFORM 7030-INCR-WS-DER-DEB
              THRU 7030-INCR-WS-DER-FIN.
      *
       3000-TRT-ERR-A-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT A OK                             *
      *     ==============================                            *
      *---------------------------------------------------------------*
      *
       3010-TRT-OK-A-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
      *
           PERFORM 7070-OP-A-OK-DEB
              THRU 7070-OP-A-OK-FIN.
      *
           PERFORM 8000-ENTETE-ETATCLI-DEB
              THRU 8000-ENTETE-ETATCLI-FIN.
      *
           PERFORM 6040-READ-NEXT-F-CPTE-DEB
              THRU 6040-READ-NEXT-F-CPTE-FIN.
      *
           IF WS-DEM-CPT-FIN < WS-CPT-CPTE
              PERFORM 8070-ETATCLI-OK-DEB
                 THRU 8070-ETATCLI-OK-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 4000-TRT-EDITION-DEB
              THRU 4000-TRT-EDITION-FIN
              UNTIL (WS-CPT-CPTE > WS-DEM-CPT-FIN) OR EOF-F-CPTE-E.
      *
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)                          *
      *---------------------------------------------------------------*
      *
           PERFORM 8020-BAS-ETATCLI-DEB
              THRU 8020-BAS-ETATCLI-FIN.
      *
       3010-TRT-OK-A-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT B ERRONEE                        *
      *     ==================================                        *
      *---------------------------------------------------------------*
      *
       3020-TRT-ERR-B-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           IF WS-DER = 0
              PERFORM 8040-ENTETE-ETATANO-DEB
                 THRU 8040-ENTETE-ETATANO-FIN
           END-IF.
      *
           PERFORM 8050-DETAIL-ETATANO-DEB
              THRU 8050-DETAIL-ETATANO-FIN.
      *
           PERFORM 7030-INCR-WS-DER-DEB
              THRU 7030-INCR-WS-DER-FIN.
      *
       3020-TRT-ERR-B-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT B OK                             *
      *     ==============================                            *
      *---------------------------------------------------------------*
      *
       3030-TRT-OK-B-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
      *
           PERFORM 7080-OP-B-OK-DEB
              THRU 7080-OP-B-OK-FIN.
      *
           PERFORM 8000-ENTETE-ETATCLI-DEB
              THRU 8000-ENTETE-ETATCLI-FIN.
      *
           PERFORM 6040-READ-NEXT-F-CPTE-DEB
              THRU 6040-READ-NEXT-F-CPTE-FIN.
      *
           IF WS-DEM-CLI-FIN < WS-CPT-NOM
              PERFORM 8070-ETATCLI-OK-DEB
                 THRU 8070-ETATCLI-OK-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 4000-TRT-EDITION-DEB
              THRU 4000-TRT-EDITION-FIN
             UNTIL WS-CPT-NOM > WS-DEM-CLI-FIN OR EOF-F-CPTE-E.
      *
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)                          *
      *---------------------------------------------------------------*
      *
           PERFORM 8020-BAS-ETATCLI-DEB
              THRU 8020-BAS-ETATCLI-FIN.
      *
       3030-TRT-OK-B-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT EDITION                          *
      *     ================================                          *
      *---------------------------------------------------------------*
      *
       4000-TRT-EDITION-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 8010-DETAIL-ETATCLI-DEB
              THRU 8010-DETAIL-ETATCLI-FIN.
      *
           PERFORM 6040-READ-NEXT-F-CPTE-DEB
              THRU 6040-READ-NEXT-F-CPTE-FIN.
      *
       4000-TRT-EDITION-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
      *---------------------------------------------------------------*
      *                                                               *
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *                                                               *
      *===============================================================*
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *                                                               *
      *6000-ORDRE-FICHIER-DEB.
      *
      *6000-ORDRE-FICHIER-FIN.
      *    EXIT.
      *
       6000-ACCEPT-SYSIN-DEB.
           ACCEPT WS-SYSIN.
       6000-ACCEPT-SYSIN-FIN.
           EXIT.
      *
       6010-OUVRIR-F-CPTE-DEB.
           OPEN INPUT F-CPTE-E.
           IF NOT OK-F-CPTE-E
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-CPTE-E'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6010-OUVRIR-F-CPTE-FIN.
           EXIT.
      *
       6020-OUVRIR-ETATCLI-DEB.
           OPEN OUTPUT F-ETATCLI-S.
           IF NOT OK-F-ETATCLI-S
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER ETATCLI'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATCLI-S
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6020-OUVRIR-ETATCLI-FIN.
           EXIT.
      *
       6030-OUVRIR-ETATANO-DEB.
           OPEN OUTPUT F-ETATANO-S.
           IF NOT OK-F-ETATANO-S
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER ETATANO'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6030-OUVRIR-ETATANO-FIN.
           EXIT.
      *
       6040-READ-NEXT-F-CPTE-DEB.
           READ F-CPTE-E NEXT INTO WS-ENRG-F-CPTE
           END-READ.
           IF NOT OK-LEC-F-CPTE-E
             DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-CPTE-E'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-READ-NEXT-F-CPTE-FIN.
           EXIT.
      *
       6050-ENTETE-ETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI
                 FROM WS-BUFFER AFTER PAGE
           END-WRITE.
           IF NOT OK-F-ETATCLI-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATCLI-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATCLI-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6050-ENTETE-ETATCLI-FIN.
           EXIT.
      *
       6060-LIGNE-ETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI
                 FROM WS-BUFFER
           END-WRITE.
           IF NOT OK-F-ETATCLI-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATCLI-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATCLI-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6060-LIGNE-ETATCLI-FIN.
           EXIT.
      *
       6070-ENTETE-ETATANO-DEB.
           WRITE FS-ENRG-ETATANO
                 FROM WS-BUFFER AFTER PAGE
           END-WRITE.
           IF NOT OK-F-ETATANO-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATANO-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6070-ENTETE-ETATANO-FIN.
           EXIT.
      *
       6080-LIGNE-ETATANO-DEB.
           WRITE FS-ENRG-ETATANO
                 FROM WS-BUFFER
           END-WRITE.
           IF NOT OK-F-ETATANO-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATANO-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6080-LIGNE-ETATANO-FIN.
           EXIT.
      *
       6090-START-CPT-F-CPTE-DEB.
           START F-CPTE-E KEY >= FS-CPTE-CPT.
           IF NOT OK-START
                DISPLAY 'PROBLEME DE POSITIONNEMENT DU FICHER F-CPTE-E'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6090-START-CPT-F-CPTE-FIN.
           EXIT.
      *
       6100-START-CLI-F-CPTE-DEB.
           START F-CPTE-E KEY >= FS-CPTE-CLI.
           IF NOT OK-START
                DISPLAY 'PROBLEME DE POSITIONNEMENT DU FICHER F-CPTE-E'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6100-START-CLI-F-CPTE-FIN.
           EXIT.
      *
       6110-FERMER-F-CPTE-DEB.
           CLOSE F-CPTE-E.
           IF NOT OK-F-CPTE-E
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-CPTE-E'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6110-FERMER-F-CPTE-FIN.
           EXIT.
      *
       6120-FERMER-ETATCLI-DEB.
           CLOSE F-ETATCLI-S.
           IF NOT OK-F-ETATCLI-S
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-ETATCLI-S'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATCLI-S
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6120-FERMER-ETATCLI-FIN.
           EXIT.
      *
       6130-FERMER-ETATANO-DEB.
           CLOSE F-ETATANO-S.
           IF NOT OK-F-ETATANO-S
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-ETATANO-S'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6130-FERMER-ETATANO-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-LOW-VALUE-CPT-DEB.
           SET FS-CPTE-CPT-MIN          TO TRUE.
       7000-LOW-VALUE-CPT-FIN.
           EXIT.
      *
       7010-INCR-WS-DEM-DEB.
           ADD  1                       TO WS-DEM.
           MOVE WS-DEM-CPT-DEB          TO FS-CPTE-CPT.
           MOVE 0                       TO WS-ERREUR.
       7010-INCR-WS-DEM-FIN.
           EXIT.
      *
       7030-INCR-WS-DER-DEB.
           ADD  1                       TO WS-DER.
       7030-INCR-WS-DER-FIN.
           EXIT.
      *
       7040-CPTE-CLE-DEB.
           MOVE WS-DEM-CPT-DEB          TO FS-CPTE-CPT.
       7040-CPTE-CLE-FIN.
           EXIT.
      *
       7050-CLI-CLE-DEB.
           MOVE WS-DEM-CLI-DEB          TO FS-CPTE-CLI.
       7050-CLI-CLE-FIN.
           EXIT.
      *
       7060-OP-DEMANDE-NOK-DEB.
           ADD  1                               TO WS-DER.
           MOVE '01'                            TO WS-LANO-NUM-ED.
           MOVE 'TYPE DE DEMANDE INCORRECTE'    TO WS-LANO-TYP-ED.
       7060-OP-DEMANDE-NOK-FIN.
           EXIT.
      *
       7070-OP-A-OK-DEB.
           MOVE 'NUMEROS DE COMPTES'            TO WS-LETAT-TYPE-ED.
           MOVE WS-DEM-CPT-DEB                  TO WS-LETAT-REFDEB-ED.
           MOVE WS-DEM-CPT-FIN                  TO WS-LETAT-REFFIN-ED.
       7070-OP-A-OK-FIN.
           EXIT.
      *
       7080-OP-B-OK-DEB.
           MOVE 'NOMS DE CLIENTS'               TO WS-LETAT-TYPE-ED.
           MOVE WS-DEM-CLI-DEB                  TO WS-LETAT-REFDEB-ED.
           MOVE WS-DEM-CLI-FIN                  TO WS-LETAT-REFFIN-ED.
       7080-OP-B-OK-FIN.
           EXIT.
      *
       7090-ERR-VIDE-DEB.
           MOVE 1                               TO WS-ERREUR.
           MOVE '02'                            TO WS-LANO-NUM-ED.
           MOVE 'MANQUE INFORMATIONS REQUISES'  TO WS-LANO-TYP-ED.
       7090-ERR-VIDE-FIN.
           EXIT.
      *
       7100-ERR-CARACTERES-DEB.
           MOVE 1                               TO WS-ERREUR.
           MOVE '03'                            TO WS-LANO-NUM-ED.
           MOVE 'TYPE DE CARACTERES INCORRECT'  TO WS-LANO-TYP-ED.
       7100-ERR-CARACTERES-FIN.
           EXIT.
      *
       7110-ERR-BORNES-DEB.
           MOVE 1                               TO WS-ERREUR.
           MOVE '04'                            TO WS-LANO-NUM-ED.
           MOVE 'DEFINITION BORNES INCORRECTE'  TO WS-LANO-TYP-ED.
       7110-ERR-BORNES-FIN.
           EXIT.
      *
       7120-ERR-LIMITES-DEB.
           MOVE 1                               TO WS-ERREUR.
           MOVE '05'                            TO WS-LANO-NUM-ED.
           MOVE 'PLAGE HORS LIMITE'             TO WS-LANO-TYP-ED.
       7120-ERR-LIMITES-FIN.
           EXIT.
      *
       7130-LOW-VALUE-CLI-DEB.
           SET FS-CPTE-CLI-MIN                  TO TRUE.
       7130-LOW-VALUE-CLI-FIN.
           EXIT.
      *
       7140-LIMITE-MIN-CPT-DEB.
           MOVE WS-CPT-CPTE                     TO WS-CPT-MIN.
       7140-LIMITE-MIN-CPT-FIN.
           EXIT.
      *
       7150-LIMITE-MIN-CLI-DEB.
           MOVE WS-CPT-NOM                      TO WS-CLI-MIN.
       7150-LIMITE-MIN-CLI-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
      *8000-ORDRE-EDITION-DEB.
      *
      *8000-ORDRE-EDITION-FIN.
      *    EXIT.
      *
       8000-ENTETE-ETATCLI-DEB.
      *
           MOVE WS-LETAT-TIRET        TO WS-BUFFER.
           PERFORM 6050-ENTETE-ETATCLI-DEB
              THRU 6050-ENTETE-ETATCLI-FIN.
      *
           MOVE  WS-DEM-NOM           TO WS-LETAT-NOMD-ED.
           MOVE  WS-DEM               TO WS-LETAT-NUM-ED.
           MOVE '1'                   TO WS-LETAT-PAGE-ED.
           MOVE WS-LETAT-ENTETE       TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC        TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TITRE        TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC        TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-REFDEB       TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-REFFIN       TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC        TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-INTITULE     TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC        TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
       8000-ENTETE-ETATCLI-FIN.
           EXIT.
      *
       8010-DETAIL-ETATCLI-DEB.
           MOVE WS-CPT-CPTE           TO WS-LETAT-NUMCPT-ED.
           MOVE WS-CPT-DCREA-JJ       TO WS-LETAT-DCREA-JJ-ED.
           MOVE WS-CPT-DCREA-MM       TO WS-LETAT-DCREA-MM-ED.
           MOVE WS-CPT-DCREA-SS       TO WS-LETAT-DCREA-SS-ED.
           MOVE WS-CPT-DCREA-AA       TO WS-LETAT-DCREA-AA-ED.
           MOVE WS-CPT-DMAJ-JJ        TO WS-LETAT-DMAJ-JJ-ED.
           MOVE WS-CPT-DMAJ-MM        TO WS-LETAT-DMAJ-MM-ED.
           MOVE WS-CPT-DMAJ-SS        TO WS-LETAT-DMAJ-SS-ED.
           MOVE WS-CPT-DMAJ-AA        TO WS-LETAT-DMAJ-AA-ED.
           MOVE WS-CPT-SOLDE          TO WS-LETAT-SOLDE-ED.
           MOVE WS-CPT-NOM            TO WS-LETAT-NOMC-ED.
           MOVE WS-LETAT-DETAIL       TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
       8010-DETAIL-ETATCLI-FIN.
           EXIT.
      *
       8020-BAS-ETATCLI-DEB.
           MOVE WS-LETAT-TIRET       TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
       8020-BAS-ETATCLI-FIN.
           EXIT.
      *
       8040-ENTETE-ETATANO-DEB.
           MOVE WS-LANO-ASTER        TO WS-BUFFER.
           PERFORM 6070-ENTETE-ETATANO-DEB
              THRU 6070-ENTETE-ETATANO-FIN.
      *
           MOVE WS-LANO-TITRE        TO WS-BUFFER.
           PERFORM 6080-LIGNE-ETATANO-DEB
              THRU 6080-LIGNE-ETATANO-FIN.
      *
           MOVE WS-LANO-ASTER        TO WS-BUFFER.
           PERFORM 6080-LIGNE-ETATANO-DEB
              THRU 6080-LIGNE-ETATANO-FIN.
      *
       8040-ENTETE-ETATANO-FIN.
           EXIT.
      *
       8050-DETAIL-ETATANO-DEB.
      *
           MOVE WS-LANO-ERREUR       TO WS-BUFFER.
           PERFORM 6080-LIGNE-ETATANO-DEB
              THRU 6080-LIGNE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENR1         TO WS-BUFFER.
           PERFORM 6080-LIGNE-ETATANO-DEB
              THRU 6080-LIGNE-ETATANO-FIN.
      *
           MOVE WS-SYSIN             TO WS-LANO-ENR-ED.
           MOVE WS-LANO-ENR2         TO WS-BUFFER.
           PERFORM 6080-LIGNE-ETATANO-DEB
              THRU 6080-LIGNE-ETATANO-FIN.
      *
           MOVE WS-LANO-INTERL       TO WS-BUFFER.
           PERFORM 6080-LIGNE-ETATANO-DEB
              THRU 6080-LIGNE-ETATANO-FIN.
      *
       8050-DETAIL-ETATANO-FIN.
           EXIT.
      *
       8060-BAS-ETATANO-DEB.
      *
           MOVE WS-LANO-ASTER        TO WS-BUFFER.
           PERFORM 6080-LIGNE-ETATANO-DEB
              THRU 6080-LIGNE-ETATANO-FIN.
      *
       8060-BAS-ETATANO-FIN.
           EXIT.
      *
       8070-ETATCLI-OK-DEB.
           MOVE WS-LETAT-VIDE-ED TO WS-BUFFER.
           PERFORM 6060-LIGNE-ETATCLI-DEB
              THRU 6060-LIGNE-ETATCLI-FIN.
       8070-ETATCLI-OK-FIN.
           EXIT.
      *
       8999-EDITION-STATISTIQUE-DEB.
           DISPLAY WS-LCRE-ASTER.
           DISPLAY WS-LCRE-TITRE.
           DISPLAY WS-LCRE-ASTER.
      *
           MOVE 'NOMBRE DE DEMANDES            ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-DEM                           TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
      *
           MOVE 'NOMBRE DE DEMANDES ERRONEES   ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-DER                           TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
      *
           DISPLAY WS-LCRE-ASTER.
       8999-EDITION-STATISTIQUE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *---------------------------------------------------------------*
      *
      *9000-APPEL-SP-DEB.
      *
      *9000-APPEL-SP-FIN.
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO51B         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO51B        *'.
           DISPLAY '*==============================================*'.
           MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.

