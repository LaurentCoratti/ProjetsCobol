      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO31B                                   *
      *  NOM DU REDACTEUR : CORATTI                                   *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 08/04/2023                                *
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
       PROGRAM-ID.      ARIO31B.
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
      *                      F-MVTS  : FICHIER DES MOUVEMENTS
      *                      -------------------------------------------
           SELECT  F-MVTS-E            ASSIGN TO 'INP001.txt'
                   FILE STATUS         IS WS-FS-F-MVTS-E.
      *                      -------------------------------------------
      *                      F-CPTE  : FICHIERS DES COMPTES CLIENTS
      *                      -------------------------------------------
           SELECT  F-CPTE-E            ASSIGN TO 'INP002.txt'
                   FILE STATUS         IS WS-FS-F-CPTE-E.
      *
           SELECT  F-CPTE-S            ASSIGN TO 'OUT001.txt'
                   FILE STATUS         IS WS-FS-F-CPTE-S.
      *                      -------------------------------------------
      *                      ETATCLI : FICHIER DES ETATS CLIENTS
      *                      -------------------------------------------
           SELECT  F-ETATCLI-S         ASSIGN TO 'ETATCLI.txt'
                   FILE STATUS         IS WS-FS-F-ETATCLI-S.
      *                      -------------------------------------------
      *                      ETATANO : FICHIER DES ETATS D'ANOMALIES
      *                      -------------------------------------------
           SELECT  F-ETATANO-S         ASSIGN TO 'ETATANO.txt'
                   FILE STATUS         IS WS-FS-F-ETATANO-S.
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
       FD  F-MVTS-E
           RECORDING MODE IS F.
       01  FS-ENRG-F-MVTS-E           PIC X(50).
      *
       FD  F-CPTE-E
           RECORDING MODE IS F.
       01  FS-ENRG-F-CPTE-E           PIC X(50).
      *
       FD  F-CPTE-S
           RECORDING MODE IS F.
       01  FS-ENRG-F-CPTE-S           PIC X(50).
      *
       FD  F-ETATCLI-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATCLI            PIC X(80).
      *
       FD  F-ETATANO-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATANO            PIC X(80).
      *
      *--------------------DESCRITION DE L'ENREGISTREMENT---------------
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
           COPY TP3LEDIT.
           COPY TP3MVTS.
           COPY TP3CPTE.
           COPY TP3CPTS.
      *
      *---------------------------------------------------------------*
      * FILE STATUS                                                   *
      *---------------------------------------------------------------*
      *
       01  WS-FS-F-MVTS-E           PIC XX.
           88  OK-F-MVTS-E          VALUE '00'.
           88  OK-LEC-F-MVTS-E      VALUE '00' '10'.
           88  EOF-F-MVTS-E         VALUE '10'.
       01  WS-FS-F-CPTE-E           PIC XX.
           88  OK-F-CPTE-E          VALUE '00'.
           88  OK-LEC-F-CPTE-E      VALUE '00' '10'.
           88  EOF-F-CPTE-E         VALUE '10'.
       01  WS-FS-F-CPTE-S           PIC XX.
           88  OK-F-CPTE-S          VALUE '00'.
       01  WS-FS-F-ETATCLI-S        PIC XX.
           88  OK-F-ETATCLI-S       VALUE '00'.
       01  WS-FS-F-ETATANO-S        PIC XX.
           88  OK-F-ETATANO-S       VALUE '00'.
      *
      *---------------------------------------------------------------*
      * COMPTEURS                                                     *
      *---------------------------------------------------------------*
      *
       77  WS-CCLI                  PIC S9(4) COMP
                                    VALUE 0.
       77  WS-CCLN                  PIC S9(4) COMP
                                    VALUE 0.
       77  WS-CCSO                  PIC S9(4) COMP
                                    VALUE 0.
       77  WS-CCST                  PIC S9(4) COMP
                                    VALUE 0.
       77  WS-CMVT                  PIC S9(4) COMP
                                    VALUE 0.
       77  WS-CERR                  PIC S9(4) COMP
                                    VALUE 0.
       77  WS-CRET                  PIC S9(4) COMP
                                    VALUE 0.
       77  WS-CCB                   PIC S9(4) COMP
                                    VALUE 0.
       77  WS-CDEP                  PIC S9(4) COMP
                                    VALUE 0.
       77  WS-MVT-VAL               PIC S9(4) COMP
                                    VALUE 0.
      *
      *---------------------------------------------------------------*
      * VARIABLES DE CALCULS ET DE MOUVEMENTS                         *
      *---------------------------------------------------------------*
      *
       77  WS-LETAT-TOTDB           PIC S9(10)V99 COMP-3
                                    VALUE 0.
       77  WS-LETAT-TOTCR           PIC S9(10)V99 COMP-3
                                    VALUE 0.
       77  WS-LANO-TOT              PIC S9(10)V99 COMP-3
                                    VALUE 0.
       77  WS-BUFFER-ETATCLI        PIC X(80).
       77  WS-BUFFER-ETATANO        PIC X(80).
       01  WS-DATE-RECUP.
           05 SS                    PIC 99.
           05 AA                    PIC 99.
           05 MM                    PIC 99.
           05 JJ                    PIC 99.
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
           PERFORM 6000-MFI-OUV-F-MVTS-E-DEB
              THRU 6000-MFI-OUV-F-MVTS-E-FIN.
      *
           PERFORM 6010-MFI-OUV-F-CPTE-E-DEB
              THRU 6010-MFI-OUV-F-CPTE-E-FIN.
      *
           PERFORM 6020-MFI-OUV-F-CPTE-S-DEB
              THRU 6020-MFI-OUV-F-CPTE-S-FIN.
      *
           PERFORM 6030-MFI-OUV-F-ETATANO-S-DEB
              THRU 6030-MFI-OUV-F-ETATANO-S-FIN.
      *
           PERFORM 6040-MFI-OUV-F-ETATCLI-S-DEB
              THRU 6040-MFI-OUV-F-ETATCLI-S-FIN.
      *
           PERFORM 6050-MFI-LEC-F-MVTS-E-DEB
              THRU 6050-MFI-LEC-F-MVTS-E-FIN.
      *
           IF EOF-F-MVTS-E
              DISPLAY 'FICHIER F-MVTS-E VIDE'
           END-IF.
      *
           PERFORM 6060-MFI-LEC-F-CPTE-E-DEB
              THRU 6060-MFI-LEC-F-CPTE-E-FIN.
      *
           IF EOF-F-CPTE-E
              DISPLAY 'FICHIER F-CPTE-E VIDE'
           END-IF.
      *
           PERFORM 7000-TRC-DATE-ET-ETATCLI-DEB
              THRU 7000-TRC-DATE-ET-ETATCLI-FIN.
      *
           PERFORM 8000-ED-HT-E-ETATCLI-DEB
              THRU 8000-ED-HT-E-ETATCLI-FIN.
      *
           PERFORM 8030-ED-HT-E-ETATANO-DEB
              THRU 8030-ED-HT-E-ETATANO-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 1000-TRT-ASSORTIMENT-DEB
              THRU 1000-TRT-ASSORTIMENT-FIN
             UNTIL EOF-F-MVTS-E AND EOF-F-CPTE-E.
      *
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)                          *
      *---------------------------------------------------------------*
      *
           PERFORM 7110-TRC-OP-CCSO-DEB
              THRU 7110-TRC-OP-CCSO-FIN.
      *
           IF WS-CERR NOT = 0
                 PERFORM 8070-ED-BAS-ETATANO-DEB
                    THRU 8070-ED-BAS-ETATANO-FIN
           ELSE  PERFORM 8080-ED-LANO-OK-DEB
                    THRU 8080-ED-LANO-OK-FIN
           END-IF.
      *
           PERFORM 8999-EDITION-STATISTIQUE-DEB
              THRU 8999-EDITION-STATISTIQUE-FIN.
      *
           PERFORM 6170-MFI-FER-F-MVTS-E-DEB
              THRU 6170-MFI-FER-F-MVTS-E-FIN.
      *
           PERFORM 6180-MFI-FER-F-CPTE-E-DEB
              THRU 6180-MFI-FER-F-CPTE-E-FIN.
      *
           PERFORM 6190-MFI-FER-F-CPTE-S-DEB
              THRU 6190-MFI-FER-F-CPTE-S-FIN.
      *
           PERFORM 6200-MFI-FER-F-ETATCLI-DEB
              THRU 6200-MFI-FER-F-ETATCLI-FIN.
      *
           PERFORM 6210-MFI-FER-F-ETATANO-DEB
              THRU 6210-MFI-FER-F-ETATANO-FIN.
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
      *             DESCRIPTION DU COMPOSANT TRT ASSORTIMENT          *
      *             ===================================               *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT                                           *
      *---------------------------------------------------------------*
      *
       1000-TRT-ASSORTIMENT-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTENATIVE MULTIPLE)              *
      *---------------------------------------------------------------*
      *
           EVALUATE TRUE
      *
             WHEN WS-CPTE-CPTE < WS-MVTS-CPTE
                  PERFORM 2000-TRT-CPT-S-MVT-DEB
                     THRU 2000-TRT-CPT-S-MVT-FIN
             WHEN WS-CPTE-CPTE = WS-MVTS-CPTE
                  PERFORM 2100-TRT-CPT-MVT-DEB
                     THRU 2100-TRT-CPT-MVT-FIN
             WHEN WS-CPTE-CPTE > WS-MVTS-CPTE
                  PERFORM 2200-TRT-MVT-S-CPT-DEB
                     THRU 2200-TRT-MVT-S-CPT-FIN
           END-EVALUATE.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT  (OREILLETTE DROITE)                        *
      *---------------------------------------------------------------*
      *
       1000-TRT-ASSORTIMENT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT COMPTES SANS MOUVEMENTS          *
      *     ================================================          *
      *---------------------------------------------------------------*
      *
       2000-TRT-CPT-S-MVT-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT DU PLUS BAS NIVEAU                                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7010-TRC-OP-CCSO-DEB
              THRU 7010-TRC-OP-CCSO-FIN.
           PERFORM 6080-MFI-ECR-F-CPTE-S-DEB
              THRU 6080-MFI-ECR-F-CPTE-S-FIN.
           PERFORM 6060-MFI-LEC-F-CPTE-E-DEB
              THRU 6060-MFI-LEC-F-CPTE-E-FIN.
      *
       2000-TRT-CPT-S-MVT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT COMPTES AVEC MOUVEMENTS          *
      *     ================================================          *
      *---------------------------------------------------------------*
      *
       2100-TRT-CPT-MVT-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
      *
           PERFORM 7020-TRC-OP-CCST-DEB
              THRU 7020-TRC-OP-CCST-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 3000-TRT-MVTS-ACPT-DEB
              THRU 3000-TRT-MVTS-ACPT-FIN
             UNTIL WS-MVTS-CPTE NOT = WS-CPTE-CPTE.
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)                          *
      *---------------------------------------------------------------*
      *
           IF WS-MVT-VAL IS >= 1
              PERFORM 7090-TRC-OP-B-ETATCLI-DEB
                 THRU 7090-TRC-OP-B-ETATCLI-FIN
              PERFORM 8060-ED-BAS-ETATCLI-DEB
                 THRU 8060-ED-BAS-ETATCLI-FIN
           END-IF.
      *
           PERFORM 6080-MFI-ECR-F-CPTE-S-DEB
              THRU 6080-MFI-ECR-F-CPTE-S-FIN
      *
           PERFORM 6060-MFI-LEC-F-CPTE-E-DEB
              THRU 6060-MFI-LEC-F-CPTE-E-FIN.
       2100-TRT-CPT-MVT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT MOUVEMENTS SANS COMPTES          *
      *     ================================================          *
      *---------------------------------------------------------------*
      *
       2200-TRT-MVT-S-CPT-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
      *
           PERFORM 7100-TRC-OP-CCSC-DEB
              THRU 7100-TRC-OP-CCSC-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 3100-TRT-MVTS-SCPT-DEB
              THRU 3100-TRT-MVTS-SCPT-FIN
             UNTIL WS-MVTS-CPTE NOT =  WS-CPTS-CPTE.
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)                          *
      *---------------------------------------------------------------*
      *
           IF WS-MVT-VAL > 0
              PERFORM 7090-TRC-OP-B-ETATCLI-DEB
                 THRU 7090-TRC-OP-B-ETATCLI-FIN
              PERFORM 6080-MFI-ECR-F-CPTE-S-DEB
                 THRU 6080-MFI-ECR-F-CPTE-S-FIN
              PERFORM 8060-ED-BAS-ETATCLI-DEB
                 THRU 8060-ED-BAS-ETATCLI-FIN
           END-IF.
      *
       2200-TRT-MVT-S-CPT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *       DESCRIPTION DU COMPOSANT TRT MOUVEMENTS STANDARD        *
      *       ================================================        *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT                                           *
      *---------------------------------------------------------------*
      *
       3000-TRT-MVTS-ACPT-DEB.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7030-TRC-OP-MVT-DEB
              THRU 7030-TRC-OP-MVT-FIN.

           IF (RETRAIT OR CB OR DEPOT) AND WS-MVT-VAL = 0
              PERFORM 8010-ED-HT-C-ETATCLI-DEB
                 THRU 8010-ED-HT-C-ETATCLI-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)             *
      *---------------------------------------------------------------*
      *
           EVALUATE TRUE
             WHEN RETRAIT PERFORM 4000-TRT-RETRAIT-DEB
                             THRU 4000-TRT-RETRAIT-FIN
             WHEN DEPOT   PERFORM 4100-TRT-DEPOT-DEB
                             THRU 4100-TRT-DEPOT-FIN
             WHEN CB      PERFORM 4200-TRT-CB-DEB
                             THRU 4200-TRT-CB-FIN
             WHEN  OTHER  PERFORM 4300-TRT-ANOM-DEB
                             THRU 4300-TRT-ANOM-FIN
           END-EVALUATE.
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)                         *
      *---------------------------------------------------------------*
      *
           PERFORM 7080-TRC-OP-CMVT-DEB
              THRU 7080-TRC-OP-CMVT-FIN.
      *
           PERFORM 6050-MFI-LEC-F-MVTS-E-DEB
              THRU 6050-MFI-LEC-F-MVTS-E-FIN.
      *
       3000-TRT-MVTS-ACPT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *       DESCRIPTION DU COMPOSANT TRT MOUVEMENTS SANS COMPTES    *
      *       ====================================================    *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT                                           *
      *---------------------------------------------------------------*
      *
       3100-TRT-MVTS-SCPT-DEB.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7030-TRC-OP-MVT-DEB
              THRU 7030-TRC-OP-MVT-FIN.
           IF (RETRAIT OR CB OR DEPOT) AND WS-MVT-VAL = 0
              PERFORM 8010-ED-HT-C-ETATCLI-DEB
                 THRU 8010-ED-HT-C-ETATCLI-FIN
              PERFORM 7120-TRC-DCREA-CCSC-DEB
                 THRU 7120-TRC-DCREA-CCSC-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)             *
      *---------------------------------------------------------------*
      *
           EVALUATE TRUE
             WHEN RETRAIT PERFORM 4000-TRT-RETRAIT-DEB
                             THRU 4000-TRT-RETRAIT-FIN
             WHEN DEPOT   PERFORM 4100-TRT-DEPOT-DEB
                             THRU 4100-TRT-DEPOT-FIN
             WHEN CB      PERFORM 4200-TRT-CB-DEB
                             THRU 4200-TRT-CB-FIN
             WHEN  OTHER  PERFORM 4300-TRT-ANOM-DEB
                             THRU 4300-TRT-ANOM-FIN
           END-EVALUATE.
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)                         *
      *---------------------------------------------------------------*
      *
           PERFORM 7080-TRC-OP-CMVT-DEB
              THRU 7080-TRC-OP-CMVT-FIN.
      *
           PERFORM 6050-MFI-LEC-F-MVTS-E-DEB
              THRU 6050-MFI-LEC-F-MVTS-E-FIN.
      *
       3100-TRT-MVTS-SCPT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT TRT RETRAIT               *
      *            ====================================               *
      *---------------------------------------------------------------*
      *
       4000-TRT-RETRAIT-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7040-TRC-OP-RET-DEB
              THRU 7040-TRC-OP-RET-FIN.
      *
           PERFORM 8020-ED-LIN-ETATCLI-DEB
              THRU 8020-ED-LIN-ETATCLI-FIN.
      *
       4000-TRT-RETRAIT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT TRT DEPOT                 *
      *            ==================================                 *
      *---------------------------------------------------------------*
       4100-TRT-DEPOT-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7050-TRC-OP-DEP-DEB
              THRU 7050-TRC-OP-DEP-FIN.
      *
           PERFORM 8020-ED-LIN-ETATCLI-DEB
              THRU 8020-ED-LIN-ETATCLI-FIN.
      *
       4100-TRT-DEPOT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT TRT CB                    *
      *            ===============================                    *
      *---------------------------------------------------------------*
       4200-TRT-CB-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7060-TRC-OP-CB-DEB
              THRU 7060-TRC-OP-CB-FIN.
      *
           PERFORM 8020-ED-LIN-ETATCLI-DEB
              THRU 8020-ED-LIN-ETATCLI-FIN.
      *
       4200-TRT-CB-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT TRT ANOMALIES             *
      *            ======================================             *
      *---------------------------------------------------------------*
       4300-TRT-ANOM-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           IF WS-CERR = 0
              PERFORM 8040-ED-HT-C-ETATANO-DEB
                 THRU 8040-ED-HT-C-ETATANO-FIN
           END-IF.
      *
           PERFORM 7070-TRC-OP-ANO-DEB
              THRU 7070-TRC-OP-ANO-FIN.
      *
           PERFORM 8050-ED-DET-ETATANO-DEB
              THRU 8050-ED-DET-ETATANO-FIN.
      *
       4300-TRT-ANOM-FIN.
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
       6000-MFI-OUV-F-MVTS-E-DEB.
           OPEN INPUT F-MVTS-E.
           IF NOT OK-F-MVTS-E
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-MVTS'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-MVTS-E
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6000-MFI-OUV-F-MVTS-E-FIN.
           EXIT.
      *
       6010-MFI-OUV-F-CPTE-E-DEB.
           OPEN INPUT F-CPTE-E.
           IF NOT OK-F-CPTE-E
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-CPTE-E'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-E
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6010-MFI-OUV-F-CPTE-E-FIN.
           EXIT.
      *
       6020-MFI-OUV-F-CPTE-S-DEB.
           OPEN OUTPUT F-CPTE-S.
           IF NOT OK-F-CPTE-S
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-CPTE-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-S
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6020-MFI-OUV-F-CPTE-S-FIN.
           EXIT.
      *
       6030-MFI-OUV-F-ETATANO-S-DEB.
           OPEN OUTPUT F-ETATANO-S
           IF NOT OK-F-ETATANO-S
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER ETATANO'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6030-MFI-OUV-F-ETATANO-S-FIN.
           EXIT.
      *
       6040-MFI-OUV-F-ETATCLI-S-DEB.
           OPEN OUTPUT F-ETATCLI-S
           IF NOT OK-F-ETATCLI-S
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER ETATCLI'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATCLI-S
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6040-MFI-OUV-F-ETATCLI-S-FIN.
           EXIT.
      *
       6050-MFI-LEC-F-MVTS-E-DEB.
           READ F-MVTS-E INTO WS-ENRG-F-MVTS.
           IF NOT OK-LEC-F-MVTS-E
             DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-MVTS-E'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-MVTS-E
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
           IF EOF-F-MVTS-E
              SET WS-MVTS-CPTE-MAX TO TRUE
           END-IF.
       6050-MFI-LEC-F-MVTS-E-FIN.
           EXIT.
      *
       6060-MFI-LEC-F-CPTE-E-DEB.
           READ F-CPTE-E INTO WS-ENRG-F-CPTE.
           IF NOT OK-LEC-F-CPTE-E
             DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-CPTE-E'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-E
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
           IF EOF-F-CPTE-E
              SET WS-CPTE-CPTE-MAX TO TRUE
           END-IF.
       6060-MFI-LEC-F-CPTE-E-FIN.
           EXIT.
      *
       6070-MFI-ECR-HT-ETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI
                 FROM WS-BUFFER-ETATCLI AFTER PAGE
           END-WRITE.
           IF NOT OK-F-ETATCLI-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATCLI-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATCLI-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6070-MFI-ECR-HT-ETATCLI-FIN.
           EXIT.
      *
       6080-MFI-ECR-F-CPTE-S-DEB.
           WRITE FS-ENRG-F-CPTE-S
                 FROM WS-ENRG-F-CPTS
           END-WRITE.
           IF NOT OK-F-CPTE-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-CPTE-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6080-MFI-ECR-F-CPTE-S-FIN.
           EXIT.
      *
       6100-MFI-ECR-LIN-ETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI
                 FROM WS-BUFFER-ETATCLI
           END-WRITE.
           IF NOT OK-F-ETATCLI-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATCLI-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATCLI-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6100-MFI-ECR-LIN-ETATCLI-FIN.
           EXIT.
      *
       6110-MFI-ECR-HT-ETATANO-DEB.
           WRITE FS-ENRG-ETATANO
                 FROM WS-BUFFER-ETATANO AFTER PAGE
           END-WRITE.
           IF NOT OK-F-ETATANO-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATANO-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6110-MFI-ECR-HT-ETATANO-FIN.
           EXIT.
      *
       6130-MFI-ECR-LIN-ETATANO-DEB.
           WRITE FS-ENRG-ETATANO
                 FROM WS-BUFFER-ETATANO
           END-WRITE.
           IF NOT OK-F-ETATANO-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATANO-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6130-MFI-ECR-LIN-ETATANO-FIN.
           EXIT.
      *
       6170-MFI-FER-F-MVTS-E-DEB.
           CLOSE F-MVTS-E.
           IF NOT OK-F-MVTS-E
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-MVTS-E'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-MVTS-E
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6170-MFI-FER-F-MVTS-E-FIN.
           EXIT.
      *
       6180-MFI-FER-F-CPTE-E-DEB.
           CLOSE F-CPTE-E.
           IF NOT OK-F-CPTE-E
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-CPTE-E'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-E
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6180-MFI-FER-F-CPTE-E-FIN.
           EXIT.
      *
       6190-MFI-FER-F-CPTE-S-DEB.
           CLOSE F-CPTE-S.
           IF NOT OK-F-CPTE-S
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-CPTE-S'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-S
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6190-MFI-FER-F-CPTE-S-FIN.
           EXIT.
      *
       6200-MFI-FER-F-ETATCLI-DEB.
           CLOSE F-ETATCLI-S.
           IF NOT OK-F-ETATCLI-S
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-ETATCLI-S'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATCLI-S
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6200-MFI-FER-F-ETATCLI-FIN.
           EXIT.
      *
       6210-MFI-FER-F-ETATANO-DEB.
           CLOSE F-ETATANO-S.
           IF NOT OK-F-ETATANO-S
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-ETATANO-S'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6210-MFI-FER-F-ETATANO-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-TRC-DATE-ET-ETATCLI-DEB.
           ACCEPT WS-DATE-RECUP         FROM DATE YYYYMMDD
           MOVE SS                      TO WS-L7-SS-ED
           MOVE AA                      TO WS-L7-AA-ED
           MOVE MM                      TO WS-L7-MM-ED
           MOVE JJ                      TO WS-L7-JJ-ED.
       7000-TRC-DATE-ET-ETATCLI-FIN.
           EXIT.
      *
       7010-TRC-OP-CCSO-DEB.
           ADD  1                       TO WS-CCSO
           ADD  1                       TO WS-CCLI
           MOVE WS-ENRG-F-CPTE          TO WS-ENRG-F-CPTS
           MOVE WS-DATE-RECUP           TO WS-CPTE-DMAJ.
       7010-TRC-OP-CCSO-FIN.
           EXIT.
      *
       7020-TRC-OP-CCST-DEB.
           ADD  1                       TO WS-CCST
           ADD  1                       TO WS-CCLI
           MOVE WS-ENRG-F-CPTE          TO WS-ENRG-F-CPTS
           MOVE 0                       TO WS-LETAT-TOTDB
           MOVE 0                       TO WS-LETAT-TOTCR
           MOVE 0                       TO WS-MVT-VAL
           MOVE ALL SPACE               TO WS-LETAT-OPEN-ED
           MOVE WS-DATE-RECUP           TO WS-CPTS-DMAJ.
       7020-TRC-OP-CCST-FIN.
           EXIT.
      *
       7030-TRC-OP-MVT-DEB.
           MOVE 0                       TO WS-LETAT-OP-CREDIT-ED.
           MOVE 0                       TO WS-LETAT-OP-DEBIT-ED.
       7030-TRC-OP-MVT-FIN.
           EXIT.
      *
       7040-TRC-OP-RET-DEB.
           ADD  1                       TO WS-CRET
           ADD  1                       TO WS-MVT-VAL
           MOVE WS-MVTS-MT              TO WS-LETAT-OP-DEBIT-ED
           MOVE WS-MVTS-JJ              TO WS-LETAT-OP-JJ-ED
           MOVE WS-MVTS-MM              TO WS-LETAT-OP-MM-ED
           MOVE WS-MVTS-SS              TO WS-LETAT-OP-SS-ED
           MOVE WS-MVTS-AA              TO WS-LETAT-OP-AA-ED
           MOVE 'RETRAIT DAB'           TO WS-LETAT-OP-LIB-ED
           ADD WS-MVTS-MT               TO WS-LETAT-TOTDB.
       7040-TRC-OP-RET-FIN.
           EXIT.
      *
       7050-TRC-OP-DEP-DEB.
           ADD 1                        TO WS-CDEP
           ADD 1                        TO WS-MVT-VAL
           MOVE WS-MVTS-MT              TO WS-LETAT-OP-CREDIT-ED
           MOVE WS-MVTS-JJ              TO WS-LETAT-OP-JJ-ED
           MOVE WS-MVTS-MM              TO WS-LETAT-OP-MM-ED
           MOVE WS-MVTS-SS              TO WS-LETAT-OP-SS-ED
           MOVE WS-MVTS-AA              TO WS-LETAT-OP-AA-ED
           MOVE 'DEPOT GUICHET'         TO WS-LETAT-OP-LIB-ED
           ADD WS-MVTS-MT               TO WS-LETAT-TOTCR.
       7050-TRC-OP-DEP-FIN.
           EXIT.
      *
       7060-TRC-OP-CB-DEB.
           ADD 1                        TO WS-CCB
           ADD 1                        TO WS-MVT-VAL
           MOVE WS-MVTS-MT              TO WS-LETAT-OP-DEBIT-ED
           MOVE WS-MVTS-JJ              TO WS-LETAT-OP-JJ-ED
           MOVE WS-MVTS-MM              TO WS-LETAT-OP-MM-ED
           MOVE WS-MVTS-SS              TO WS-LETAT-OP-SS-ED
           MOVE WS-MVTS-AA              TO WS-LETAT-OP-AA-ED
           MOVE 'CARTE BLEUE'           TO WS-LETAT-OP-LIB-ED
           ADD  WS-MVTS-MT              TO WS-LETAT-TOTDB.
       7060-TRC-OP-CB-FIN.
           EXIT.
      *
       7070-TRC-OP-ANO-DEB.
           MOVE WS-MVTS-MT              TO WS-LANO-MONTANT-ED
           MOVE WS-MVTS-CPTE            TO WS-LANO-NUMCPT-ED
           MOVE WS-MVTS-CODE            TO WS-LANO-CODEMVT-ED
           ADD WS-MVTS-MT               TO WS-LANO-TOT
           ADD 1                        TO WS-CERR.
       7070-TRC-OP-ANO-FIN.
           EXIT.
      *
       7080-TRC-OP-CMVT-DEB.
           ADD 1                        TO WS-CMVT.
       7080-TRC-OP-CMVT-FIN.
           EXIT.
      *
       7090-TRC-OP-B-ETATCLI-DEB.
           COMPUTE WS-CPTS-SOLDE =  WS-CPTS-SOLDE - WS-LETAT-TOTDB
                                    + WS-LETAT-TOTCR
           MOVE WS-LETAT-TOTDB          TO WS-LETAT-TOTDB-ED
           MOVE WS-LETAT-TOTCR          TO WS-LETAT-TOTCR-ED.
       7090-TRC-OP-B-ETATCLI-FIN.
           EXIT.
      *
       7100-TRC-OP-CCSC-DEB.
           ADD 1                        TO WS-CCLN
           ADD 1                        TO WS-CCLI
           MOVE WS-MVTS-CPTE            TO WS-CPTS-CPTE
           MOVE 0                       TO WS-CPTS-SOLDE
           MOVE 0                       TO WS-LETAT-TOTDB
           MOVE 0                       TO WS-LETAT-TOTCR
           MOVE 0                       TO WS-MVT-VAL
           MOVE 'CREATION DE COMPTE'    TO WS-LETAT-OPEN-ED
           MOVE WS-DATE-RECUP           TO WS-CPTS-DMAJ.
       7100-TRC-OP-CCSC-FIN.
           EXIT.
      *
       7110-TRC-OP-CCSO-DEB.
           MOVE WS-LANO-TOT             TO WS-LANO-TOTAL-ED.
       7110-TRC-OP-CCSO-FIN.
           EXIT.
      *
       7120-TRC-DCREA-CCSC-DEB.
           MOVE WS-MVTS-DATE            TO WS-CPTS-DCREA.
       7120-TRC-DCREA-CCSC-FIN.
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
       8000-ED-HT-E-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1    TO WS-BUFFER-ETATCLI.
           PERFORM 6070-MFI-ECR-HT-ETATCLI-DEB
              THRU 6070-MFI-ECR-HT-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2    TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L3    TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L4    TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2    TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L5    TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L6    TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2    TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L7         TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L8         TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2         TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1         TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
       8000-ED-HT-E-ETATCLI-FIN.
           EXIT.
      *
       8010-ED-HT-C-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1         TO WS-BUFFER-ETATCLI.
           PERFORM 6070-MFI-ECR-HT-ETATCLI-DEB
              THRU 6070-MFI-ECR-HT-ETATCLI-FIN.
      *
           MOVE 1                    TO WS-LETAT-PAGE-ED.
           MOVE JJ                   TO WS-LETAT-JJ-ED.
           MOVE MM                   TO WS-LETAT-MM-ED.
           MOVE SS                   TO WS-LETAT-SS-ED.
           MOVE AA                   TO WS-LETAT-AA-ED.
           MOVE WS-LETAT-DATE-PAGE   TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-CPTS-CPTE         TO WS-LETAT-NUMCPT-ED.
           MOVE WS-LETAT-NUMCPT      TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1         TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-CPTS-SOLDE        TO WS-LETAT-SOLD-ED.
           MOVE 'ANCIEN SOLDE'       TO WS-LETAT-LIB-ED.
           MOVE WS-LETAT-SOLD-OP     TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1         TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TITRES      TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1         TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
       8010-ED-HT-C-ETATCLI-FIN.
           EXIT.
      *
       8020-ED-LIN-ETATCLI-DEB.
           MOVE WS-LETAT-DETAIL-OP   TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
       8020-ED-LIN-ETATCLI-FIN.
           EXIT.
      *
       8030-ED-HT-E-ETATANO-DEB.
      *
           MOVE WS-ENTETE-L1         TO WS-BUFFER-ETATANO.
           PERFORM 6110-MFI-ECR-HT-ETATANO-DEB
              THRU 6110-MFI-ECR-HT-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2         TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L3    TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L4    TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2         TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L5    TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L6    TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2         TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L7         TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L8         TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2         TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L1         TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
       8030-ED-HT-E-ETATANO-FIN.
           EXIT.
      *
       8040-ED-HT-C-ETATANO-DEB.
           MOVE WS-LANO-L1           TO WS-BUFFER-ETATANO.
           PERFORM 6110-MFI-ECR-HT-ETATANO-DEB
              THRU 6110-MFI-ECR-HT-ETATANO-FIN.
           MOVE WS-LANO-TITRES       TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
           MOVE WS-LANO-L3           TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
       8040-ED-HT-C-ETATANO-FIN.
           EXIT.
      *
       8050-ED-DET-ETATANO-DEB.
           MOVE WS-LANO-DETAIL       TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
       8050-ED-DET-ETATANO-FIN.
           EXIT.
      *
       8060-ED-BAS-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1         TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TOT-OP      TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1         TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-CPTS-SOLDE        TO WS-LETAT-SOLD-ED.
           MOVE 'NOUVEAU SOLDE'      TO WS-LETAT-LIB-ED.
           MOVE WS-LETAT-SOLD-OP     TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1         TO WS-BUFFER-ETATCLI.
           PERFORM 6100-MFI-ECR-LIN-ETATCLI-DEB
              THRU 6100-MFI-ECR-LIN-ETATCLI-FIN.
      *
       8060-ED-BAS-ETATCLI-FIN.
           EXIT.
      *
       8070-ED-BAS-ETATANO-DEB.
      *
           MOVE WS-LANO-L3           TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-LANO-TOTAL        TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
           MOVE WS-LANO-L1           TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
       8070-ED-BAS-ETATANO-FIN.
           EXIT.
      *
       8080-ED-LANO-OK-DEB.
      *
           MOVE WS-LANO-OK           TO WS-BUFFER-ETATANO.
           PERFORM 6130-MFI-ECR-LIN-ETATANO-DEB
              THRU 6130-MFI-ECR-LIN-ETATANO-FIN.
      *
       8080-ED-LANO-OK-FIN.
           EXIT.
      *
       8999-EDITION-STATISTIQUE-DEB.
           DISPLAY WS-LCRE-ASTER
           DISPLAY WS-LCRE-TITRE
           DISPLAY WS-LCRE-ASTER
      *
           MOVE 'NOMBRE DE CLIENTS             ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-CCLI                          TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
      *
           MOVE 'NOMBRE DE CLIENTS NOUVEAUX    ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-CCLN                          TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
      *
           MOVE 'NOMBRE DE CLIENTS SANS OPER.  ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-CCSO                          TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
      *
           MOVE 'NOMBRE DE CLIENTS STANDARDS   ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-CCST                          TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
      *
           MOVE 'NOMBRE DE MOUVEMENTS          ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-CMVT                          TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
      *
           MOVE 'NOMBRE DE MOUVEMENTS ERRONES  ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-CERR                          TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
      *
           MOVE 'NOMBRE DE RETRAITS            ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-CRET                          TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
      *
           MOVE 'NOMBRE DE CARTES BLEUES       ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-CCB                           TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
      *
           MOVE 'NOMBRE DE DEPOTS              ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-CDEP                          TO WS-LCRE-DET-TOT-ED
           DISPLAY WS-LCRE-DETAIL
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
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO31B         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO31B        *'.
           DISPLAY '*==============================================*'.
           MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.

