      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO41B                                   *
      *  NOM DU REDACTEUR : CORATTI                                   *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 14/04/2023                                *
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
       PROGRAM-ID.      ARIO41B.
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
           SELECT  F-MVTS-E            ASSIGN TO INP001
                   FILE STATUS         IS WS-FS-F-MVTS-E.
      *                      -------------------------------------------
      *                      F-CPTE  : FICHIERS DES COMPTES CLIENTS
      *                      -------------------------------------------
           SELECT  F-CPTE-ES           ASSIGN TO IO001
                                       ORGANIZATION IS INDEXED
                                       ACCESS MODE IS RANDOM
                                       RECORD KEY IS FS-CPTE-CPT
                   FILE STATUS         IS WS-FS-F-CPTE-ES.
      *                      -------------------------------------------
      *                      ETATCLI : FICHIER DES ETATS CLIENTS
      *                      -------------------------------------------
           SELECT  F-ETATCLI-S         ASSIGN TO ETATCLI
                   FILE STATUS         IS WS-FS-F-ETATCLI-S.
      *                      -------------------------------------------
      *                      ETATANO : FICHIER DES ETATS D'ANOMALIES
      *                      -------------------------------------------
           SELECT  F-ETATANO-S         ASSIGN TO ETATANO
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
       FD  F-CPTE-ES
           RECORD CONTAINS 50 CHARACTERS.
       01  FS-ENRG-F-CPT.
           05 FS-CPTE-CPT             PIC X(10).
           05 FILLER                  PIC X(40).
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
           COPY TP4LEDIT.
           COPY TP4MVTS.
           COPY TP4CPT.
      *
      *---------------------------------------------------------------*
      * FILE STATUS                                                   *
      *---------------------------------------------------------------*
      *
       01  WS-FS-F-MVTS-E           PIC XX.
           88  OK-F-MVTS-E          VALUE '00'.
           88  OK-LEC-F-MVTS-E      VALUE '00' '10'.
           88  EOF-F-MVTS-E         VALUE '10'.
       01  WS-FS-F-CPTE-ES          PIC XX.
           88  OK-F-CPTE-ES         VALUE '00'.
           88  OK-LEC-F-CPTE-ES     VALUE '00' '23'.
           88  COMPTE-INEXISTANT    VALUE '23'.
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
       77  WS-CCCL                  PIC S9(4) COMP
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
       77  WS-PAGE                  PIC 9(4)  COMP
                                    VALUE 0.
      *
      *---------------------------------------------------------------*
      * VARIABLES DE CALCULS ET DE MOUVEMENTS                         *
      *---------------------------------------------------------------*
      *
       77  WS-LETAT-TOTDB           PIC S9(11)V99 COMP-3
                                    VALUE 0.
       77  WS-LETAT-TOTCR           PIC S9(11)V99 COMP-3
                                    VALUE 0.
       77  WS-LANO-TOT              PIC S9(11)V99 COMP-3
                                    VALUE 0.
       77  WS-SOLDE-INT             PIC S9(11)V99 COMP-3
                                    VALUE 0.
       77  WS-BUFFER                PIC X(80).
       01  WS-DATE-JOUR.
           05 SS                    PIC 99.
           05 AA                    PIC 99.
           05 MM                    PIC 99.
           05 JJ                    PIC 99.
       01  WS-CLOTURE               PIC X.
           88 WS-NK                 VALUE '0'.
           88 WS-K                  VALUE '1'.
       77  WS-SOLDE-INTER           PIC S9(11)V99 COMP-3
                                    VALUE 0.
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
           PERFORM 6000-OUVRIR-F-MVTS-E-DEB
              THRU 6000-OUVRIR-F-MVTS-E-FIN.
      *
           PERFORM 6010-OUVRIR-F-CPTE-ES-DEB
              THRU 6010-OUVRIR-F-CPTE-ES-FIN.
      *
           PERFORM 6020-OUVRIR-ETATCLI-DEB
              THRU 6020-OUVRIR-ETATCLI-FIN.
      *
           PERFORM 6030-OUVRIR-ETATANO-DEB
              THRU 6030-OUVRIR-ETATANO-FIN.
      *
           PERFORM 6040-LIRE-F-MVTS-E-DEB
              THRU 6040-LIRE-F-MVTS-E-FIN.
      *
           IF EOF-F-MVTS-E
              DISPLAY 'FICHIER F-MVTS-E VIDE'
           END-IF.
      *
           PERFORM 7000-ACCEPT-DATE-JOUR-DEB
              THRU 7000-ACCEPT-DATE-JOUR-FIN.
      *
           PERFORM 8000-GARDE-ETATCLI-DEB
              THRU 8000-GARDE-ETATCLI-FIN.
      *
           PERFORM 8010-GARDE-ETATANO-DEB
              THRU 8010-GARDE-ETATANO-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 1000-TRT-COMPTE-DEB
              THRU 1000-TRT-COMPTE-FIN
             UNTIL EOF-F-MVTS-E.
      *
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)                          *
      *---------------------------------------------------------------*
      *
           IF WS-CERR NOT = 0
                 PERFORM 8090-BAS-ETATANO-DEB
                    THRU 8090-BAS-ETATANO-FIN
           ELSE  PERFORM 8100-ED-LANO-OK-DEB
                    THRU 8100-ED-LANO-OK-FIN
           END-IF.
      *
           PERFORM 8999-EDITION-STATISTIQUE-DEB
              THRU 8999-EDITION-STATISTIQUE-FIN.
      *
           PERFORM 6100-FERMER-F-MVTS-E-DEB
              THRU 6100-FERMER-F-MVTS-E-FIN.
      *
           PERFORM 6110-FERMER-F-CPTE-ES-DEB
              THRU 6110-FERMER-F-CPTE-ES-FIN.
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
      *             DESCRIPTION DU COMPOSANT TRT COMPTE               *
      *             ===================================               *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT                                           *
      *---------------------------------------------------------------*
      *
       1000-TRT-COMPTE-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 6050-LIRE-F-CPTE-ES-DEB
              THRU 6050-LIRE-F-CPTE-ES-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTENATIVE SIMPLE)                *
      *---------------------------------------------------------------*
      *
           IF COMPTE-INEXISTANT
                PERFORM 2010-TRT-NOUVEAU-COMPTE-DEB
                   THRU 2010-TRT-NOUVEAU-COMPTE-FIN
           ELSE PERFORM 2000-TRT-COMPTE-EXISTANT-DEB
                   THRU 2000-TRT-COMPTE-EXISTANT-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT  (OREILLETTE DROITE)                        *
      *---------------------------------------------------------------*
      *
       1000-TRT-COMPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT COMPTE EXISTANT                  *
      *     ========================================                  *
      *---------------------------------------------------------------*
      *
       2000-TRT-COMPTE-EXISTANT-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7150-INCR-PAGE-DEB
              THRU 7150-INCR-PAGE-FIN.
      *
           PERFORM 7020-COMPTE-EXISTANT-DEB
              THRU 7020-COMPTE-EXISTANT-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 3000-TRT-MVT-COMPTE-EX-DEB
              THRU 3000-TRT-MVT-COMPTE-EX-FIN
             UNTIL (WS-MVTS-CPTE NOT = WS-CPT-CPTE)
                OR EOF-F-MVTS-E.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)                         *
      *---------------------------------------------------------------*
      *
           PERFORM 7110-OP-SOLDE-INTER-DEB
              THRU 7110-OP-SOLDE-INTER-FIN.
      *
           PERFORM 7010-MAJ-SOLDE-DEB
              THRU 7010-MAJ-SOLDE-FIN.
      *
           IF WS-K
                PERFORM 7120-OP-EDITION-CLOTURE-DEB
                   THRU 7120-OP-EDITION-CLOTURE-FIN
                PERFORM 6140-SUPPRIMER-F-CPTE-ES-DEB
                   THRU 6140-SUPPRIMER-F-CPTE-ES-FIN
           ELSE PERFORM 6090-REECRIRE-F-CPTE-ES-DEB
                   THRU 6090-REECRIRE-F-CPTE-ES-FIN
           END-IF.
      *
           IF WS-LETAT-TOTDB NOT = 0 OR WS-LETAT-TOTCR NOT = 0
                PERFORM 8050-BAS-ETATCLI-DEB
                   THRU 8050-BAS-ETATCLI-FIN
           END-IF.
      *
           PERFORM 7040-INIT-MVT-VAL-DEB
              THRU 7040-INIT-MVT-VAL-FIN.
      *
       2000-TRT-COMPTE-EXISTANT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *     DESCRIPTION DU COMPOSANT NOUVEAU COMPTE                   *
      *     =======================================                   *
      *---------------------------------------------------------------*
      *
       2010-TRT-NOUVEAU-COMPTE-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
      *
           PERFORM 7150-INCR-PAGE-DEB
              THRU 7150-INCR-PAGE-FIN.
      *
           PERFORM 7130-COMPTE-INEXISTANT-DEB
              THRU 7130-COMPTE-INEXISTANT-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 3010-TRT-MVT-NEW-COMPTE-DEB
              THRU 3010-TRT-MVT-NEW-COMPTE-FIN
             UNTIL (WS-MVTS-CPTE NOT = WS-CPT-CPTE)
                OR EOF-F-MVTS-E.
      *
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)                          *
      *---------------------------------------------------------------*
      *
           PERFORM 7110-OP-SOLDE-INTER-DEB
              THRU 7110-OP-SOLDE-INTER-FIN.
      *
           PERFORM 7010-MAJ-SOLDE-DEB
              THRU 7010-MAJ-SOLDE-FIN.
      *
           IF WS-K
                PERFORM 7120-OP-EDITION-CLOTURE-DEB
                   THRU 7120-OP-EDITION-CLOTURE-FIN
           ELSE
              IF WS-LETAT-TOTDB NOT = 0 AND WS-LETAT-TOTCR NOT = 0
                   PERFORM 6080-ECRIRE-F-CPTE-ES-DEB
                      THRU 6080-ECRIRE-F-CPTE-ES-FIN
              END-IF
           END-IF.
      *
           IF WS-LETAT-TOTDB NOT = 0 OR WS-LETAT-TOTCR NOT = 0
                PERFORM 8050-BAS-ETATCLI-DEB
                   THRU 8050-BAS-ETATCLI-FIN
           END-IF.
      *
           PERFORM 7040-INIT-MVT-VAL-DEB
              THRU 7040-INIT-MVT-VAL-FIN.
      *
       2010-TRT-NOUVEAU-COMPTE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *       DESCRIPTION DU COMPOSANT TRT MVT COMPTE EXISTANT        *
      *       ================================================        *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT                                           *
      *---------------------------------------------------------------*
      *
       3000-TRT-MVT-COMPTE-EX-DEB.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7030-INIT-OP-DEB
              THRU 7030-INIT-OP-FIN.
      *
           IF (RETRAIT OR CB OR DEPOT) AND WS-MVT-VAL = 5
              PERFORM 7150-INCR-PAGE-DEB
                 THRU 7150-INCR-PAGE-FIN
              PERFORM 7110-OP-SOLDE-INTER-DEB
                 THRU 7110-OP-SOLDE-INTER-FIN
              PERFORM 8040-BAS-INTER-ETATCLI-DEB
                 THRU 8040-BAS-INTER-ETATCLI-FIN
              PERFORM 7040-INIT-MVT-VAL-DEB
                 THRU 7040-INIT-MVT-VAL-FIN
           END-IF.
      *
           IF (RETRAIT OR CB OR DEPOT) AND WS-MVT-VAL = 0
              PERFORM 8020-ENTETE-ETATCLI-DEB
                 THRU 8020-ENTETE-ETATCLI-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)             *
      *---------------------------------------------------------------*
      *
           EVALUATE TRUE
             WHEN RETRAIT PERFORM 4000-TRT-RETRAIT-DEB
                             THRU 4000-TRT-RETRAIT-FIN
             WHEN DEPOT   PERFORM 4010-TRT-DEPOT-DEB
                             THRU 4010-TRT-DEPOT-FIN
             WHEN CB      PERFORM 4020-TRT-CB-DEB
                             THRU 4020-TRT-CB-FIN
             WHEN CLOTURE PERFORM 4040-TRT-CLOTURE-DEB
                             THRU 4040-TRT-CLOTURE-FIN
             WHEN OTHER   PERFORM 4030-TRT-ANOM-DEB
                             THRU 4030-TRT-ANOM-FIN
           END-EVALUATE.
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)                         *
      *---------------------------------------------------------------*
      *
           PERFORM 7100-INCR-CMVT-DEB
              THRU 7100-INCR-CMVT-FIN.
      *
           PERFORM 6040-LIRE-F-MVTS-E-DEB
              THRU 6040-LIRE-F-MVTS-E-FIN.
      *
       3000-TRT-MVT-COMPTE-EX-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *       DESCRIPTION DU COMPOSANT TRT MVT NOUVEAU COMPTE         *
      *       ===============================================         *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT                                           *
      *---------------------------------------------------------------*
      *
       3010-TRT-MVT-NEW-COMPTE-DEB.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7030-INIT-OP-DEB
              THRU 7030-INIT-OP-FIN.
      *
           IF (RETRAIT OR CB OR DEPOT) AND WS-MVT-VAL = 0
               AND WS-LETAT-TOTDB = 0  AND WS-LETAT-TOTCR = 0
               PERFORM 7140-DATE-CREATION-DEB
                  THRU 7140-DATE-CREATION-FIN
           END-IF.
      *
           IF (RETRAIT OR CB OR DEPOT) AND WS-MVT-VAL = 5
              PERFORM 7150-INCR-PAGE-DEB
                 THRU 7150-INCR-PAGE-FIN
              PERFORM 7110-OP-SOLDE-INTER-DEB
                 THRU 7110-OP-SOLDE-INTER-FIN
              PERFORM 8040-BAS-INTER-ETATCLI-DEB
                 THRU 8040-BAS-INTER-ETATCLI-FIN
              PERFORM 7040-INIT-MVT-VAL-DEB
                 THRU 7040-INIT-MVT-VAL-FIN
           END-IF.
      *
           IF (RETRAIT OR CB OR DEPOT) AND WS-MVT-VAL = 0
              PERFORM 8020-ENTETE-ETATCLI-DEB
                 THRU 8020-ENTETE-ETATCLI-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)             *
      *---------------------------------------------------------------*
      *
           EVALUATE TRUE
             WHEN RETRAIT PERFORM 4000-TRT-RETRAIT-DEB
                             THRU 4000-TRT-RETRAIT-FIN
             WHEN DEPOT   PERFORM 4010-TRT-DEPOT-DEB
                             THRU 4010-TRT-DEPOT-FIN
             WHEN CB      PERFORM 4020-TRT-CB-DEB
                             THRU 4020-TRT-CB-FIN
             WHEN CLOTURE PERFORM 4040-TRT-CLOTURE-DEB
                             THRU 4040-TRT-CLOTURE-FIN
             WHEN OTHER   PERFORM 4030-TRT-ANOM-DEB
                             THRU 4030-TRT-ANOM-FIN
           END-EVALUATE.
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)                         *
      *---------------------------------------------------------------*
      *
           PERFORM 7100-INCR-CMVT-DEB
              THRU 7100-INCR-CMVT-FIN.
      *
           PERFORM 6040-LIRE-F-MVTS-E-DEB
              THRU 6040-LIRE-F-MVTS-E-FIN.
      *
       3010-TRT-MVT-NEW-COMPTE-FIN.
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
           PERFORM 7050-OP-RETRAIT-DEB
              THRU 7050-OP-RETRAIT-FIN.
      *
           PERFORM 8030-DETAIL-ETATCLI-DEB
              THRU 8030-DETAIL-ETATCLI-FIN.
      *
       4000-TRT-RETRAIT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT TRT DEPOT                 *
      *            ==================================                 *
      *---------------------------------------------------------------*
       4010-TRT-DEPOT-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7070-OP-DEPOT-DEB
              THRU 7070-OP-DEPOT-FIN.
      *
           PERFORM 8030-DETAIL-ETATCLI-DEB
              THRU 8030-DETAIL-ETATCLI-FIN.
      *
       4010-TRT-DEPOT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT TRT CB                    *
      *            ===============================                    *
      *---------------------------------------------------------------*
       4020-TRT-CB-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7060-OP-CARTE-DEB
              THRU 7060-OP-CARTE-FIN.
      *
           PERFORM 8030-DETAIL-ETATCLI-DEB
              THRU 8030-DETAIL-ETATCLI-FIN.
      *
       4020-TRT-CB-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT TRT ANOMALIES             *
      *            ======================================             *
      *---------------------------------------------------------------*
       4030-TRT-ANOM-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           IF WS-CERR = 0
              PERFORM 8060-ENTETE-ETATANO-DEB
                 THRU 8060-ENTETE-ETATANO-FIN
           END-IF.
      *
           PERFORM 7080-OP-ANOM-DEB
              THRU 7080-OP-ANOM-FIN.
      *
           PERFORM 8070-DETAIL-ETATANO-DEB
              THRU 8070-DETAIL-ETATANO-FIN.
      *
       4030-TRT-ANOM-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT TRT CLOTURE               *
      *            ======================================             *
      *---------------------------------------------------------------*
       4040-TRT-CLOTURE-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7090-OP-CLOTURE-DEB
              THRU 7090-OP-CLOTURE-FIN.
      *
       4040-TRT-CLOTURE-FIN.
           EXIT.
      
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
       6000-OUVRIR-F-MVTS-E-DEB.
           OPEN INPUT F-MVTS-E.
           IF NOT OK-F-MVTS-E
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-MVTS'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-MVTS-E
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN

           END-IF.
       6000-OUVRIR-F-MVTS-E-FIN.
           EXIT.
      *
       6010-OUVRIR-F-CPTE-ES-DEB.
           OPEN I-O F-CPTE-ES.
           IF NOT OK-F-CPTE-ES
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-CPTE-ES'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-ES
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6010-OUVRIR-F-CPTE-ES-FIN.
           EXIT.
      *
       6020-OUVRIR-ETATCLI-DEB.
           OPEN OUTPUT F-ETATCLI-S
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
           OPEN OUTPUT F-ETATANO-S
           IF NOT OK-F-ETATANO-S
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER ETATANO'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6030-OUVRIR-ETATANO-FIN.
           EXIT.
      *
       6040-LIRE-F-MVTS-E-DEB.
           READ F-MVTS-E INTO WS-ENRG-F-MVTS.
           IF NOT OK-LEC-F-MVTS-E
             DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-MVTS-E'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-MVTS-E
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-LIRE-F-MVTS-E-FIN.
           EXIT.
      *
       6050-LIRE-F-CPTE-ES-DEB.
           MOVE WS-MVTS-CPTE TO FS-CPTE-CPT.
           READ F-CPTE-ES INTO WS-ENRG-F-CPT.
           IF NOT OK-LEC-F-CPTE-ES
             DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-CPTE-ES'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-ES
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6050-LIRE-F-CPTE-ES-FIN.
           EXIT.
      *
       6060-ECRIRE-ENTETE-ETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI
                 FROM WS-BUFFER AFTER PAGE
           END-WRITE.
           IF NOT OK-F-ETATCLI-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATCLI-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATCLI-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6060-ECRIRE-ENTETE-ETATCLI-FIN.
           EXIT.
      *
       6070-ECRIRE-LIGNE-ETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI
                 FROM WS-BUFFER
           END-WRITE.
           IF NOT OK-F-ETATCLI-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATCLI-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATCLI-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6070-ECRIRE-LIGNE-ETATCLI-FIN.
           EXIT.
      *
       6150-ECRIRE-ENTETE-ETATANO-DEB.
           WRITE FS-ENRG-ETATANO
                 FROM WS-BUFFER AFTER PAGE
           END-WRITE.
           IF NOT OK-F-ETATANO-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATANO-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6150-ECRIRE-ENTETE-ETATANO-FIN.
           EXIT.
      *
       6160-ECRIRE-LIGNE-ETATANO-DEB.
           WRITE FS-ENRG-ETATANO
                 FROM WS-BUFFER
           END-WRITE.
           IF NOT OK-F-ETATANO-S
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-ETATANO-S'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-ETATANO-S
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6160-ECRIRE-LIGNE-ETATANO-FIN.
           EXIT.
      *
       6080-ECRIRE-F-CPTE-ES-DEB.
           WRITE FS-ENRG-F-CPT
                 FROM WS-ENRG-F-CPT
           END-WRITE.
           IF NOT OK-F-CPTE-ES
             DISPLAY 'PROBLEME D''ECRITURE DU FICHIER-F-CPTE-ES'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-ES
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6080-ECRIRE-F-CPTE-ES-FIN.
           EXIT.
      *
       6090-REECRIRE-F-CPTE-ES-DEB.
           REWRITE FS-ENRG-F-CPT
                   FROM WS-ENRG-F-CPT
           END-REWRITE.
           IF NOT OK-F-CPTE-ES
             DISPLAY 'PROBLEME DE REECRITURE DU FICHIER-F-CPTE-ES'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-ES
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6090-REECRIRE-F-CPTE-ES-FIN.
           EXIT.
      *
       6100-FERMER-F-MVTS-E-DEB.
           CLOSE F-MVTS-E.
           IF NOT OK-F-MVTS-E
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-MVTS-E'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-MVTS-E
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6100-FERMER-F-MVTS-E-FIN.
           EXIT.
      *
       6110-FERMER-F-CPTE-ES-DEB.
           CLOSE F-CPTE-ES.
           IF NOT OK-F-CPTE-ES
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-CPTE-ES'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-ES
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
       6110-FERMER-F-CPTE-ES-FIN.
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
       6140-SUPPRIMER-F-CPTE-ES-DEB.
           DELETE F-CPTE-ES
           END-DELETE.
           IF NOT OK-F-CPTE-ES
             DISPLAY 'PROBLEME DE SUPPRESSION DU FICHIER-F-CPTE-ES'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-CPTE-ES
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6140-SUPPRIMER-F-CPTE-ES-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-ACCEPT-DATE-JOUR-DEB.
           ACCEPT WS-DATE-JOUR          FROM DATE YYYYMMDD
           MOVE SS                      TO WS-L7-SS-ED
           MOVE AA                      TO WS-L7-AA-ED
           MOVE MM                      TO WS-L7-MM-ED
           MOVE JJ                      TO WS-L7-JJ-ED.
       7000-ACCEPT-DATE-JOUR-FIN.
           EXIT.
      *
       7010-MAJ-SOLDE-DEB.
           MOVE WS-SOLDE-INTER          TO WS-CPT-SOLDE.
       7010-MAJ-SOLDE-FIN.
           EXIT.
      *
       7020-COMPTE-EXISTANT-DEB.
           MOVE '1'                     TO WS-PAGE
           MOVE '0'                     TO WS-CLOTURE
           MOVE ALL SPACE               TO WS-LETAT-CLOSE-ED
           MOVE ALL SPACE               TO WS-LETAT-OPEN-ED
           ADD  1                       TO WS-CCST
           ADD  1                       TO WS-CCLI
           MOVE 0                       TO WS-LETAT-TOTDB
           MOVE 0                       TO WS-LETAT-TOTCR
           MOVE ALL SPACE               TO WS-LETAT-OPEN-ED
           MOVE WS-DATE-JOUR            TO WS-CPT-DMAJ.
       7020-COMPTE-EXISTANT-FIN.
           EXIT.
      *
       7030-INIT-OP-DEB.
           MOVE 0                       TO WS-LETAT-OP-CREDIT-ED.
           MOVE 0                       TO WS-LETAT-OP-DEBIT-ED.
       7030-INIT-OP-FIN.
           EXIT.
      *
       7040-INIT-MVT-VAL-DEB.
           MOVE 0                       TO WS-MVT-VAL.
       7040-INIT-MVT-VAL-FIN.
           EXIT.
      *
       7050-OP-RETRAIT-DEB.
           ADD  1                       TO WS-CRET
           ADD  1                       TO WS-MVT-VAL
           MOVE WS-MVTS-MT              TO WS-LETAT-OP-DEBIT-ED
           MOVE 'RETRAIT DAB'           TO WS-LETAT-OP-LIB-ED
           ADD WS-MVTS-MT               TO WS-LETAT-TOTDB.
       7050-OP-RETRAIT-FIN.
           EXIT.
      *
       7060-OP-CARTE-DEB.
           ADD 1                        TO WS-CCB
           ADD 1                        TO WS-MVT-VAL
           MOVE WS-MVTS-MT              TO WS-LETAT-OP-DEBIT-ED
           MOVE 'CARTE BLEUE'           TO WS-LETAT-OP-LIB-ED
           ADD  WS-MVTS-MT              TO WS-LETAT-TOTDB.
       7060-OP-CARTE-FIN.
           EXIT.
      *
       7070-OP-DEPOT-DEB.
           ADD 1                        TO WS-CDEP
           ADD 1                        TO WS-MVT-VAL
           MOVE WS-MVTS-MT              TO WS-LETAT-OP-CREDIT-ED
           MOVE 'DEPOT GUICHET'         TO WS-LETAT-OP-LIB-ED
           ADD WS-MVTS-MT               TO WS-LETAT-TOTCR.
       7070-OP-DEPOT-FIN.
           EXIT.
      *
       7080-OP-ANOM-DEB.
           MOVE WS-MVTS-MT              TO WS-LANO-MONTANT-ED
           MOVE WS-MVTS-CPTE            TO WS-LANO-NUMCPT-ED
           MOVE WS-MVTS-CODE            TO WS-LANO-CODEMVT-ED
           ADD  WS-MVTS-MT              TO WS-LANO-TOT
           ADD  1                       TO WS-CERR.
       7080-OP-ANOM-FIN.
           EXIT.
      *
       7090-OP-CLOTURE-DEB.
           MOVE '1'                     TO WS-CLOTURE.
       7090-OP-CLOTURE-FIN.
           EXIT.
      *
       7100-INCR-CMVT-DEB.
           ADD 1                        TO WS-CMVT.
       7100-INCR-CMVT-FIN.
           EXIT.
      *
       7110-OP-SOLDE-INTER-DEB.
           COMPUTE WS-SOLDE-INTER =  WS-CPT-SOLDE - WS-LETAT-TOTDB
                                     + WS-LETAT-TOTCR.
       7110-OP-SOLDE-INTER-FIN.
           EXIT.
      *
       7120-OP-EDITION-CLOTURE-DEB.
           ADD 1                        TO WS-CCCL
           MOVE 'CLOTURE DE COMPTE'     TO WS-LETAT-CLOSE-ED.
       7120-OP-EDITION-CLOTURE-FIN.
           EXIT.
      *
       7130-COMPTE-INEXISTANT-DEB.
           MOVE 0                       TO WS-CPT-SOLDE
           MOVE '1'                     TO WS-PAGE
           MOVE '0'                     TO WS-CLOTURE
           MOVE ALL SPACE               TO WS-LETAT-CLOSE-ED
           MOVE 'CREATION DE COMPTE'    TO WS-LETAT-OPEN-ED
           ADD  1                       TO WS-CCLN
           ADD  1                       TO WS-CCLI
           MOVE 0                       TO WS-LETAT-TOTDB
           MOVE 0                       TO WS-LETAT-TOTCR
           MOVE WS-MVTS-CPTE            TO WS-CPT-CPTE.
           MOVE WS-DATE-JOUR            TO WS-CPT-DMAJ.
       7130-COMPTE-INEXISTANT-FIN.
           EXIT.
      *
       7140-DATE-CREATION-DEB.
           MOVE WS-MVTS-DATE            TO WS-CPT-DCREA.
       7140-DATE-CREATION-FIN.
           EXIT.
      *
       7150-INCR-PAGE-DEB.
           ADD  1                       TO WS-PAGE.
       7150-INCR-PAGE-FIN.
      *
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
       8000-GARDE-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6060-ECRIRE-ENTETE-ETATCLI-DEB
              THRU 6060-ECRIRE-ENTETE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L3          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L4          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L5          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L6          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L7          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L8          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
       8000-GARDE-ETATCLI-FIN.
           EXIT.
      *
       8010-GARDE-ETATANO-DEB.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6150-ECRIRE-ENTETE-ETATANO-DEB
              THRU 6150-ECRIRE-ENTETE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2          TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L3     TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L4     TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2          TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L5     TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L6     TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2          TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L7          TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L8          TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2          TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
       8010-GARDE-ETATANO-FIN.
           EXIT.
      *
       8020-ENTETE-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6060-ECRIRE-ENTETE-ETATCLI-DEB
              THRU 6060-ECRIRE-ENTETE-ETATCLI-FIN.
      *
           MOVE WS-PAGE               TO WS-LETAT-PAGE-ED.
           MOVE JJ                    TO WS-LETAT-JJ-ED.
           MOVE MM                    TO WS-LETAT-MM-ED.
           MOVE SS                    TO WS-LETAT-SS-ED.
           MOVE AA                    TO WS-LETAT-AA-ED.
           MOVE WS-LETAT-DATE-PAGE    TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-CPT-CPTE           TO WS-LETAT-NUMCPT-ED.
           MOVE WS-LETAT-NUMCPT       TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-CPT-SOLDE          TO WS-LETAT-SOLD-ED.
           MOVE 'ANCIEN SOLDE'        TO WS-LETAT-LIB-ED.
           MOVE WS-LETAT-SOLD-OP      TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TITRES       TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
       8020-ENTETE-ETATCLI-FIN.
           EXIT.
      *
       8030-DETAIL-ETATCLI-DEB.
           MOVE WS-MVTS-JJ            TO WS-LETAT-OP-JJ-ED
           MOVE WS-MVTS-MM            TO WS-LETAT-OP-MM-ED
           MOVE WS-MVTS-SS            TO WS-LETAT-OP-SS-ED
           MOVE WS-MVTS-AA            TO WS-LETAT-OP-AA-ED
           MOVE WS-LETAT-DETAIL-OP    TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
       8030-DETAIL-ETATCLI-FIN.
           EXIT.
      *
       8040-BAS-INTER-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TOTDB        TO WS-LETAT-TOTDB-ED.
           MOVE WS-LETAT-TOTCR        TO WS-LETAT-TOTCR-ED.
           MOVE WS-LETAT-TOT-OP       TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-SOLDE-INTER        TO WS-LETAT-SOLD-ED.
           MOVE 'SOLDE INTERMEDIAIRE' TO WS-LETAT-LIB-ED.
           MOVE WS-LETAT-SOLD-OP      TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
       8040-BAS-INTER-ETATCLI-FIN.
           EXIT.
      *
       8050-BAS-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TOTDB        TO WS-LETAT-TOTDB-ED.
           MOVE WS-LETAT-TOTCR        TO WS-LETAT-TOTCR-ED.
           MOVE WS-LETAT-TOT-OP       TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-SOLDE-INTER        TO WS-LETAT-SOLD-ED.
           MOVE 'NOUVEAU SOLDE'       TO WS-LETAT-LIB-ED.
           MOVE WS-LETAT-SOLD-OP      TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1          TO WS-BUFFER.
           PERFORM 6070-ECRIRE-LIGNE-ETATCLI-DEB
              THRU 6070-ECRIRE-LIGNE-ETATCLI-FIN.
      *
       8050-BAS-ETATCLI-FIN.
           EXIT.
      *
       8060-ENTETE-ETATANO-DEB.
           MOVE WS-LANO-L1           TO WS-BUFFER.
           PERFORM 6150-ECRIRE-ENTETE-ETATANO-DEB
              THRU 6150-ECRIRE-ENTETE-ETATANO-FIN.
           MOVE WS-LANO-TITRES       TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
           MOVE WS-LANO-L3           TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
       8060-ENTETE-ETATANO-FIN.
           EXIT.
      *
       8070-DETAIL-ETATANO-DEB.
           MOVE WS-LANO-DETAIL       TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
       8070-DETAIL-ETATANO-FIN.
           EXIT.
      *
       8090-BAS-ETATANO-DEB.
      *
           MOVE WS-LANO-L3           TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-LANO-TOT          TO WS-LANO-TOTAL-ED.
           MOVE WS-LANO-TOTAL        TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
           MOVE WS-LANO-L1           TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
       8090-BAS-ETATANO-FIN.
           EXIT.
      *
       8100-ED-LANO-OK-DEB.
      *
           MOVE WS-LANO-OK           TO WS-BUFFER.
           PERFORM 6160-ECRIRE-LIGNE-ETATANO-DEB
              THRU 6160-ECRIRE-LIGNE-ETATANO-FIN.
      *
       8100-ED-LANO-OK-FIN.
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
           MOVE 'NOMBRE DE CLOTURES            ' TO WS-LCRE-DET-LIB-ED
           MOVE WS-CCCL                          TO WS-LCRE-DET-TOT-ED
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
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO41B         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO41B        *'.
           DISPLAY '*==============================================*'.
           MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.

