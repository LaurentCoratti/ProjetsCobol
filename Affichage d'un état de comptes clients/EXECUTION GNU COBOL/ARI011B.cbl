      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO11B                                   *
      *  NOM DU REDACTEUR : CORATTI                                   *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 29/03/2023                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * A PARTIR D'UN FICHIER SEQUENTIEL DECRIVANT DES OPERATIONS     *
      * BANQUAIRES, EDITER UN ETAT DES DIVERSES OPERATIONS.           *
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
       PROGRAM-ID.      ARIO11B.
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
      *                      F-MVTS : FICHIER DES MOUVEMENTS
      *                      -------------------------------------------
      *NOM DE FICHIER INTERNE : F-MVTS-E
      *DDNAME                 : INP001
      *                      -------------------------------------------
           SELECT  F-MVTS-E            ASSIGN TO 'INP001.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS         IS WS-FS-F-MVTS-E.
      *                      -------------------------------------------
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
      *=============
       FILE SECTION.
      *=============
      *
       FD  F-MVTS-E
           RECORDING MODE IS F.
       01  FS-ENRG-F-MVTS     PIC X(50).
      *
      *--------------------DESCRITION DE L'ENREGISTREMENT---------------
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================

      *---------------------------------------------------------------*
      * FILE STATUS                                                   *
      *---------------------------------------------------------------*

       77  WS-FS-F-MVTS-E   PIC XX.

      *---------------------------------------------------------------*
      * FICHIER D'ENREGISTREMENT WS-FS-MVTS                           *
      *---------------------------------------------------------------*

       01  WS-ENRG-F-MVTS.
           05  WS-MVTS-CPTE PIC 9(10).
           05  FILLER       PIC X(8).
           05  WS-MVTS-CODE PIC X.
           05  WS-MVTS-MT   PIC 9(8)V99.
           05  FILLER       PIC X(21).

      *---------------------------------------------------------------*
      * COMPTE RENDU D'EXECUTION                                      *
      *---------------------------------------------------------------*

       77  WS-CCLI          PIC 9(3)   VALUE 0.
       77  WS-CERR          PIC 9(3)   VALUE 0.
       77  WS-CCB           PIC 9(3)   VALUE 0.
       77  WS-CMVT          PIC 9(3)   VALUE 0.
       77  WS-CRET          PIC 9(3)   VALUE 0.
       77  WS-CDEP          PIC 9(3)   VALUE 0.

      *---------------------------------------------------------------*
      * CARACTERES DE MISE EN PAGE                                    *
      *---------------------------------------------------------------*

       01  WS-LASTER        PIC X(45)  VALUE ALL '*'.
       01  WS-LTIRET        PIC X(45)  VALUE ALL '-'.

      *---------------------------------------------------------------*
      * ETAT DES ERREURS                                              *
      *---------------------------------------------------------------*

       01  WS-LECPT.
           05  FILLER       PIC X(28)  
                            VALUE 'ERREUR POUR LE COMPTE :'.
           05  WS-ECPT      PIC 9(10).
       01  WS-LEMVT.
           05  FILLER       PIC X(28)  
                            VALUE 'CODE MOUVEMENT        : '.
           05  WS-EMVT      PIC X.
       01  WS-LEMT.
           05  FILLER       PIC X(28)  
                            VALUE 'MONTANT               : '.
           05  WS-EMT       PIC 9(8)V99.

      *---------------------------------------------------------------*
      * ETAT DES OPERATIONS                                           *
      *---------------------------------------------------------------*
       01  WS-LCPTE.
           05  FILLER       PIC X(28)  VALUE 'NUMERO DE COMPTE      :'.
           05  WS-OCPT      PIC 9(10).
       01  WS-LCB.
           05  FILLER       PIC X(28)  
                            VALUE 'CUMUL CARTE-BLEUE     : '.
           05  WS-OCB       PIC 9(9)V99.
       01  WS-LRDAB.
           05  FILLER       PIC X(28)  
                            VALUE 'CUMUL RETRAIT DAB     : '.
           05  WS-ORDAB     PIC 9(9)V99.
       01  WS-LDGUI.
           05  FILLER       PIC X(28)  
                            VALUE 'CUMUL DEPOT GUICHET   : '.
           05  WS-ODGUI     PIC 9(9)V99.
       01  WS-LBAL.
           05  FILLER       PIC X(28)  VALUE 'BALANCE DES OPERATIONS:'.
           05  WS-OBAL      PIC S9(11)V99.

      *                  ==============================               *
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
           PERFORM 6000-OPEN-F-MVTS-E-DEB
              THRU 6000-OPEN-F-MVTS-E-FIN.
      *
           PERFORM 6010-READ-F-MVTS-E-DEB
              THRU 6010-READ-F-MVTS-E-FIN.
      *
           IF WS-FS-F-MVTS-E = '10'
              DISPLAY 'FICHIER F-MVTS-E VIDE'
           END-IF.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*

           PERFORM 1000-TRT-COMPTES-DEB
              THRU 1000-TRT-COMPTES-FIN
             UNTIL WS-FS-F-MVTS-E = '10'.
      *
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)                          *
      *---------------------------------------------------------------*
           PERFORM 8099-EDITION-STATISTIQUE-DEB
              THRU 8099-EDITION-STATISTIQUE-FIN.
      *
           PERFORM 6030-CLOSE-F-MVTS-E-DEB
              THRU 6030-CLOSE-F-MVTS-E-FIN.
      *
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *                                                               *
      *---------------------------------------------------------------*
      *FIN DU PROGRAMME                                               *
      *---------------------------------------------------------------*
       0000-TRT-PRINCIPAL-FIN.
           STOP RUN.
      *---------------------------------------------------------------*
      *             DESCRIPTION DU COMPOSANT TRT COMPTES              *
      *             ===================================               *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT                                           *
      *---------------------------------------------------------------*
      *
       1000-TRT-COMPTES-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      *
           PERFORM 7000-OPERATION-CPT-DEB
              THRU 7000-OPERATION-CPT-FIN.
      **
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ITERATIVE)                        *
      *---------------------------------------------------------------*
      **
           PERFORM 2000-TRT-MVTS-DEB
              THRU 2000-TRT-MVTS-FIN
             UNTIL WS-FS-F-MVTS-E = '10' OR WS-MVTS-CPTE NOT = WS-OCPT.
      *
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT  (OREILLETTE DROITE)                        *
      *---------------------------------------------------------------*
      *
           PERFORM 7060-OP-BAL-CLI-DEB
              THRU 7060-OP-BAL-CLI-FIN.
      *
           IF WS-OCB NOT = 0 OR WS-ORDAB NOT = 0 OR WS-ODGUI NOT = 0
              PERFORM 8010-EDIT-ETAT-OP-DEB
                 THRU 8010-EDIT-ETAT-OP-FIN
           END-IF.
      *
      *
       1000-TRT-COMPTES-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT TRT MVTS                  *
      *            =================================                  *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      * DEBUT DU TRAITEMENT                                           *
      *---------------------------------------------------------------*
      *                                                               *
       2000-TRT-MVTS-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)                 *
      *---------------------------------------------------------------*
      * *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT (ALTERNATIVE MULTIPLE)             *
      *---------------------------------------------------------------*
      *
           EVALUATE WS-MVTS-CODE
             WHEN 'R'     PERFORM 3000-TRT-R-DEB
                             THRU 3000-TRT-R-FIN
             WHEN 'D'     PERFORM 3010-TRT-D-DEB
                             THRU 3010-TRT-D-FIN
             WHEN 'C'     PERFORM 3020-TRT-C-DEB
                             THRU 3020-TRT-C-FIN
             WHEN  OTHER  PERFORM 3030-TRT-ANOM-DEB
                             THRU 3030-TRT-ANOM-FIN
           END-EVALUATE.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT (OREILLETTE DROITE)                         *
      *---------------------------------------------------------------*
      *
           PERFORM 6010-READ-F-MVTS-E-DEB
              THRU 6010-READ-F-MVTS-E-FIN.
      *
           PERFORM 7050-INCR-WS-CMVT-DEB
              THRU 7050-INCR-WS-CMVT-FIN.
      *
       2000-TRT-MVTS-FIN.
           EXIT.
      *
       3000-TRT-R-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7010-OPERATION-RETRAIT-DEB
              THRU 7010-OPERATION-RETRAIT-FIN.
      *
       3000-TRT-R-FIN.
           EXIT.
      *
       3010-TRT-D-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7020-OPERATION-DEBIT-DEB
              THRU 7020-OPERATION-DEBIT-FIN.
      *
       3010-TRT-D-FIN.
           EXIT.
      *
       3020-TRT-C-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7030-OPERATION-CREDIT-DEB
              THRU 7030-OPERATION-CREDIT-FIN.
      *
       3020-TRT-C-FIN.
           EXIT.
      *
       3030-TRT-ANOM-DEB.
      *
      *---------------------------------------------------------------*
      * TRAITEMENT (COMPOSANT DU PLUS BAS NIVEAU)                     *
      *---------------------------------------------------------------*
      *
           PERFORM 7040-OPERATION-ANOM-DEB
              THRU 7040-OPERATION-ANOM-FIN.
      *
           PERFORM 8030-EDITION-ETAT-ERREUR-DEB
              THRU 8030-EDITION-ETAT-ERREUR-FIN.
      *
       3030-TRT-ANOM-FIN.
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
       6000-OPEN-F-MVTS-E-DEB.
           OPEN INPUT F-MVTS-E.
           IF WS-FS-F-MVTS-E NOT = '00'
             DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-MVTS'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-MVTS-E
             PERFORM 9999-FIN-PROGRAMME-DEB
                THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
      *
       6000-OPEN-F-MVTS-E-FIN.
           EXIT.
      *
       6010-READ-F-MVTS-E-DEB.
           READ F-MVTS-E INTO WS-ENRG-F-MVTS.
           IF NOT (WS-FS-F-MVTS-E = '00' OR '10')
             DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-MVTS-E'
             DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-MVTS-E
             PERFORM 9999-ERREUR-PROGRAMME-DEB
                THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
       6010-READ-F-MVTS-E-FIN.
           EXIT.
      *
       6020-TEST-F-MVTS-E-FIN.
           EXIT.
      *
       6030-CLOSE-F-MVTS-E-DEB.
      *
           CLOSE F-MVTS-E.
           IF NOT WS-FS-F-MVTS-E = '00'
                DISPLAY 'PROBLEME DE FERMETURE DU DOCUMENT F-MVTS-E'
                DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-F-MVTS-E
                PERFORM 9999-FIN-PROGRAMME-DEB
                   THRU 9999-FIN-PROGRAMME-FIN
           END-IF.
      *
       6030-CLOSE-F-MVTS-E-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-OPERATION-CPT-DEB.
           MOVE 0            TO WS-OBAL
                                WS-ORDAB
                                WS-ODGUI
                                WS-OCB.
           MOVE WS-MVTS-CPTE TO WS-OCPT.
       7000-OPERATION-CPT-FIN.
           EXIT.
      *
       7010-OPERATION-RETRAIT-DEB.
           ADD WS-MVTS-MT    TO WS-ORDAB.
           ADD 1             TO WS-CRET.
       7010-OPERATION-RETRAIT-FIN.
           EXIT.
      *
       7020-OPERATION-DEBIT-DEB.
           ADD WS-MVTS-MT    TO WS-ODGUI.
           ADD 1             TO WS-CDEP.
       7020-OPERATION-DEBIT-FIN.
           EXIT.
      *
       7030-OPERATION-CREDIT-DEB.
           ADD WS-MVTS-MT    TO WS-OCB.
           ADD 1             TO WS-CCB.
       7030-OPERATION-CREDIT-FIN.
           EXIT.
      *
       7040-OPERATION-ANOM-DEB.
           ADD 1             TO WS-CERR.
           MOVE WS-MVTS-CPTE TO WS-ECPT.
           MOVE WS-MVTS-CODE TO WS-EMVT.
           MOVE WS-MVTS-MT   TO WS-EMT.
       7040-OPERATION-ANOM-FIN.
           EXIT.
      *
       7050-INCR-WS-CMVT-DEB.
           ADD 1             TO WS-CMVT.
       7050-INCR-WS-CMVT-FIN.
           EXIT.
      *
       7060-OP-BAL-CLI-DEB.
           ADD 1             TO WS-CCLI.
           COMPUTE WS-OBAL = WS-ODGUI - WS-OCB - WS-ORDAB.
       7060-OP-BAL-CLI-FIN.
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
       8010-EDIT-ETAT-OP-DEB.
      *
           DISPLAY WS-LASTER.
           DISPLAY WS-LCPTE.
           DISPLAY WS-LTIRET.
           DISPLAY WS-LCB.
           DISPLAY WS-LRDAB.
           DISPLAY WS-LDGUI.
           DISPLAY WS-LTIRET.
           DISPLAY WS-LBAL.
           DISPLAY WS-LASTER.
      *
       8010-EDIT-ETAT-OP-FIN.
           EXIT.
      *
       8099-EDITION-STATISTIQUE-DEB.
      *
           DISPLAY WS-LASTER.
           DISPLAY 'NOMBRE DE CLIENTS            : ' WS-CCLI.
           DISPLAY 'NOMBRE DE MOUVEMENTS         : ' WS-CMVT.
           DISPLAY 'NOMBRE DE MOUVEMENTS ERRONES : ' WS-CERR.
           DISPLAY 'NOMBRE DE RETRAITS           : ' WS-CRET.
           DISPLAY 'NOMBRE DE CARTES BLEUES      : ' WS-CCB.
           DISPLAY 'NOMBRE DE DEPOTS             : ' WS-CDEP.
           DISPLAY WS-LASTER.
      *
       8099-EDITION-STATISTIQUE-FIN.
           EXIT.
      *
       8030-EDITION-ETAT-ERREUR-DEB.
      *
           DISPLAY WS-LASTER.
           DISPLAY WS-LECPT.
           DISPLAY WS-LEMVT.
           DISPLAY WS-LEMT.
           DISPLAY WS-LASTER.
      *
       8030-EDITION-ETAT-ERREUR-FIN.
           EXIT.
      *
      *
       9999-FIN-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO11B         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO11B        *'.
           DISPLAY '*==============================================*'.
           MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.
           