      *--------------------------------------------------------*
      * DESCRIPTION DES LIGNES D'EDITION UTILISEES DANS LES    *
      * ELEMENTS SUIVANTS:                                     *
      *  - L'ent�te �tat client                                *
      *  - Page du compte client                               *
      *  - L'ent�te anomalie                                   *
      *  - Page d'anomalie client                              *
      *  - Compte rendu d'ex�cution                            *
      *--------------------------------------------------------*
      *
      *-------------------------------------------------------------*
      * ZONES D'EDITION DE L'ENTETE DE L'ETAT CLIENT                *
      *-------------------------------------------------------------*
      *
       01  WS-ENTETE-L1.
           05 FILLER                 PIC X      VALUE  '!'.
           05 FILLER                 PIC X(76)  VALUE ALL '-'.
           05 FILLER                 PIC XX     VALUE  '! '.
      *
       01  WS-ENTETE-L2.
           05 FILLER                 PIC X      VALUE '!'.
           05 FILLER                 PIC X(76)  VALUE SPACES.
           05 FILLER                 PIC XX     VALUE '! '.
      *
       01  WS-ENTETE-L2B.
           05 FILLER                 PIC X      VALUE '!'.
           05 FILLER                 PIC X(76)  VALUE ALL '-'.
           05 FILLER                 PIC XX     VALUE '! '.
      *
       01  WS-ENTETE-L3.
           05 FILLER                 PIC X      VALUE '!'.
           05 FILLER                 PIC X(25)  VALUE SPACES.
           05 FILLER                 PIC X(11)  VALUE 'RELEVE DES '.
           05 FILLER                 PIC X(15)  VALUE
                                         'COMPTES CLIENTS'.
           05 FILLER                 PIC X(25)  VALUE SPACES.
           05 FILLER                 PIC XX     VALUE '! '.
      *
       01  WS-ENTETE-L4.
           05 FILLER                 PIC X      VALUE '!'.
           05 FILLER                 PIC X(25)  VALUE SPACES.
           05 FILLER                 PIC X(26)  VALUE ALL '='.
           05 FILLER                 PIC X(25)  VALUE SPACES.
           05 FILLER                 PIC XX     VALUE '! '.
      *
       01  WS-ENTETE-L5.
           05 FILLER                 PIC X      VALUE '!'.
           05 FILLER                 PIC X(31)  VALUE SPACES.
           05 FILLER                 PIC X(14)  VALUE 'SERVICE CLIENT'.
           05 FILLER                 PIC X(31)  VALUE SPACES.
           05 FILLER                 PIC XX     VALUE '! '.
      *
       01  WS-ENTETE-L6.
           05 FILLER                 PIC X      VALUE '!'.
           05 FILLER                 PIC X(31)  VALUE SPACES.
           05 FILLER                 PIC X(14)  VALUE ALL '='.
           05 FILLER                 PIC X(31)  VALUE SPACES.
           05 FILLER                 PIC XX     VALUE '! '.
      *
       01  WS-ENTETE-L7.
           05 FILLER                 PIC X      VALUE '!'.
           05 FILLER                 PIC X(29)  VALUE SPACES.
           05 FILLER                 PIC X(4)   VALUE 'LE  '.
           05 WS-ENTETE-L7-DATE.
              10 WS-L7-JJ-ED         PIC 9(2).
              10 FILLER              PIC X(3)   VALUE ' / '.
              10 WS-L7-MM-ED         PIC 9(2).
              10 FILLER              PIC X(3)   VALUE ' / '.
              10 WS-L7-SS-ED         PIC 9(2).
              10 WS-L7-AA-ED         PIC 9(2).
           05 FILLER                 PIC X(29)  VALUE SPACES.
           05 FILLER                 PIC XX     VALUE '! '.
      *
       01  WS-ENTETE-L8.
           05 FILLER                 PIC X      VALUE '!'.
           05 FILLER                 PIC X(29)  VALUE SPACES.
           05 FILLER                 PIC X(18)  VALUE ALL '='.
           05 FILLER                 PIC X(29)  VALUE SPACES.
           05 FILLER                 PIC XX     VALUE '! '.
      *
      *-------------------------------------------------------------*
      * ZONES D'EDITION DE L'ETAT CLIENT                            *
      *-------------------------------------------------------------*
      *
       01  WS-LETAT-DATE-PAGE.
           05 FILLER                 PIC X      VALUE '!'.
           05 FILLER                 PIC X(11)  VALUE ' RELEVE DU '.
           05 WS-LETAT-DATE-ED.
              10 WS-LETAT-JJ-ED      PIC 9(2).
              10 FILLER              PIC X      VALUE '/'.
              10 WS-LETAT-MM-ED      PIC 9(2).
              10 FILLER              PIC X(1)   VALUE '/'.
              10 WS-LETAT-SSAA-ED.
                 15 WS-LETAT-SS-ED   PIC 9(2).
                 15 WS-LETAT-AA-ED   PIC 9(2).
           05 FILLER                 PIC X(44)  VALUE SPACES.
           05 FILLER                 PIC X(7)   VALUE 'PAGE : '.
           05 WS-LETAT-PAGE-ED       PIC Z9.
           05 FILLER                 PIC X(4)   VALUE '  ! '.
      *
       01  WS-LETAT-NUMCPT.
           05 FILLER                 PIC X(2)  VALUE '! '.
           05 FILLER                 PIC X(9)  VALUE 'NUMERO DE'.
           05 FILLER                 PIC X(9)  VALUE ' COMPTE: '.
           05 WS-LETAT-NUMCPT-ED     PIC 9(10).
           05 FILLER                 PIC X(27) VALUE SPACES.
           05 WS-LETAT-OPEN-ED       PIC X(18).
           05 FILLER                 PIC X(4)  VALUE '  ! '.
      *
       01  WS-LETAT-SOLD-OP.
           05 FILLER                 PIC X(2)  VALUE '! '.
           05 WS-LETAT-LIB-ED        PIC X(20) VALUE SPACES.
           05 FILLER                 PIC X(3)  VALUE ' : '.
           05 WS-LETAT-SOLD-ED       PIC ZZZZZZZ9,99DB.
           05 FILLER                 PIC X(19) VALUE SPACES.
           05 WS-LETAT-CLOSE-ED      PIC X(19) VALUE SPACES.
           05 FILLER                 PIC X(3)  VALUE ' ! '.
      *
       01  WS-LETAT-TITRES.
           05 FILLER                 PIC X(09) VALUE '! LIBELLE'.
           05 FILLER                 PIC X(20) VALUE SPACES.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(4)  VALUE SPACES.
           05 FILLER                 PIC X(4)  VALUE 'DATE'.
           05 FILLER                 PIC X(4)  VALUE SPACES.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(5)  VALUE SPACES.
           05 FILLER                 PIC X(5)  VALUE 'DEBIT'.
           05 FILLER                 PIC X(6)  VALUE SPACES.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(5)  VALUE SPACES.
           05 FILLER                 PIC X(6)  VALUE 'CREDIT'.
           05 FILLER                 PIC X(6)  VALUE SPACES.
           05 FILLER                 PIC XX    VALUE '! '.
      *
       01  WS-LETAT-DETAIL-OP.
           05 FILLER                 PIC X(2)  VALUE '! '.
           05 WS-LETAT-OP-LIB-ED     PIC X(27).
           05 FILLER                 PIC X(2)  VALUE '! '.
           05 WS-LETAT-OP-JJ-ED      PIC 9(2).
           05 FILLER                 PIC X     VALUE '/'.
           05 WS-LETAT-OP-MM-ED      PIC 9(2).
           05 FILLER                 PIC X     VALUE '/'.
           05 WS-LETAT-OP-SS-ED      PIC 9(2).
           05 WS-LETAT-OP-AA-ED      PIC 9(2).
           05 FILLER                 PIC X(2)  VALUE ' !'.
           05 FILLER                 PIC X(4)  VALUE SPACES.
           05 WS-LETAT-OP-DEBIT-ED   PIC ZZZZZZZ9,99
                                         BLANK WHEN ZERO.
           05 FILLER                 PIC X(2)  VALUE ' !'.
           05 FILLER                 PIC X(5)  VALUE SPACES.
           05 WS-LETAT-OP-CREDIT-ED  PIC ZZZZZZZ9,99
                                         BLANK WHEN ZERO.
           05 FILLER                 PIC X(3)  VALUE ' ! '.
      *
       01  WS-LETAT-TOT-OP.
           05 FILLER                 PIC X(42) VALUE
                                         '! TOTAL DES OPERATIONS'.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(3)  VALUE SPACES.
           05 WS-LETAT-TOTDB-ED      PIC ZZZZZZZZ9,99
                                         BLANK WHEN ZERO.
           05 FILLER                 PIC X(6)  VALUE ' !    '.
           05 WS-LETAT-TOTCR-ED      PIC ZZZZZZZZ9,99
                                         BLANK WHEN ZERO.
           05 FILLER                 PIC X(2)  VALUE ' !'.
      *
      *-------------------------------------------------------------*
      * ZONES D'EDITION DE L'ETAT ANOMALIE                          *
      * LES LIGNES 1, 2, 7 ET 8 SONT COMMUNES AUX 2 ETATS, ELLES NE *
      * SONT DONC PAS DECRITES UNE SECONDE FOIS DANS L'EDITION DE   *
      * L'ETAT ANOMALIE                                             *
      *-------------------------------------------------------------*
      *
       01  WS-LANO-ENTETE-L3.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 FILLER                 PIC X(10) VALUE 'DETAIL DES'.
           05 FILLER                 PIC X(13) VALUE ' ANOMALIES DU'.
           05 FILLER                 PIC X(13) VALUE ' FICHIER DES '.
           05 FILLER                 PIC X(11) VALUE 'MOUVEMENTS '.
           05 FILLER                 PIC X(9)  VALUE 'BANCAIRES'.
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 FILLER                 PIC XX    VALUE '! '.
      *
       01  WS-LANO-ENTETE-L4.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 FILLER                 PIC X(56) VALUE ALL '='.
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 FILLER                 PIC XX    VALUE '! '.
      *
       01  WS-LANO-ENTETE-L5.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(29) VALUE SPACES.
           05 FILLER                 PIC X(8)  VALUE 'SERVICE '.
           05 FILLER                 PIC X(9)  VALUE 'COMPTABLE'.
           05 FILLER                 PIC X(30) VALUE SPACES.
           05 FILLER                 PIC XX    VALUE '! '.
      *
       01  WS-LANO-ENTETE-L6.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(29) VALUE SPACES.
           05 FILLER                 PIC X(17) VALUE ALL '='.
           05 FILLER                 PIC X(30) VALUE SPACES.
           05 FILLER                 PIC XX    VALUE '! '.
      *
       01  WS-LANO-L1.
           05 FILLER                 PIC X     VALUE '*'.
           05 FILLER                 PIC X(53) VALUE ALL '-'.
           05 FILLER                 PIC XX    VALUE '* '.
      *
       01  WS-LANO-TITRES.
           05 FILLER                 PIC X(3)  VALUE '!  '.
           05 FILLER                 PIC X(11) VALUE 'NUM COMPTE '.
           05 FILLER                 PIC X(3)  VALUE '!  '.
           05 FILLER                 PIC X(14) VALUE 'CODE MOUVEMENT'.
           05 FILLER                 PIC X(3)  VALUE '  !'.
           05 FILLER                 PIC X(6)  VALUE SPACES.
           05 FILLER                 PIC X(7)  VALUE 'MONTANT'.
           05 FILLER                 PIC X(7)  VALUE SPACES.
           05 FILLER                 PIC XX    VALUE '! '.
      *
       01  WS-LANO-L3.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(53) VALUE ALL '-'.
           05 FILLER                 PIC XX    VALUE '! '.
      *
       01  WS-LANO-DETAIL.
           05 FILLER                 PIC X(2)  VALUE '! '.
           05 WS-LANO-NUMCPT-ED      PIC 9(10).
           05 FILLER                 PIC X(3)  VALUE '  !'.
           05 FILLER                 PIC X(8)  VALUE SPACES.
           05 WS-LANO-CODEMVT-ED     PIC X.
           05 FILLER                 PIC X(9)  VALUE SPACES.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(6)  VALUE SPACES.
           05 WS-LANO-MONTANT-ED     PIC ZZZZZZZ9,99
                                         BLANK WHEN ZERO.
           05 FILLER                 PIC X(3)  VALUE SPACES.
           05 FILLER                 PIC XX    VALUE '! '.
      *
       01  WS-LANO-TOTAL.
           05 FILLER                 PIC X(2)  VALUE '! '.
           05 FILLER                 PIC X(18) VALUE
                                         'MONTANT TOTAL DES '.
           05 FILLER                 PIC X(9)  VALUE 'ANOMALIES'.
           05 FILLER                 PIC X(4)  VALUE SPACES.
           05 FILLER                 PIC X     VALUE '!'.
           05 FILLER                 PIC X(5)  VALUE SPACES.
           05 WS-LANO-TOTAL-ED       PIC ZZZZZZZZ9,99.
           05 FILLER                 PIC X(3)  VALUE SPACES.
           05 FILLER                 PIC XX     VALUE '! '.
      *
       01  WS-LANO-OK.
           05 FILLER                 PIC X(21)  VALUE ALL SPACES.
           05 FILLER                 PIC X(15)  VALUE
                                         'PAS D''ANOMALIES'.
           05 FILLER                 PIC X(16)  VALUE
                                         ' DANS LE FICHIER'.
           05 FILLER                 PIC X(4)   VALUE ' MVT'.
           05 FILLER                 PIC X(21)  VALUE ALL SPACES.
      *
      *-------------------------------------------------------------*
      * ZONES D'EDITION DE FIN D'EXECUTION                          *
      *-------------------------------------------------------------*
      *
       01  WS-LCRE-TITRE.
           05 FILLER                 PIC X(45)  VALUE
                   '*    COMPTE-RENDU D''EXECUTION (ARIO31B)     *'.
      *
       01  WS-LCRE-ASTER.
           05 FILLER                 PIC X(45)  VALUE ALL '*'.
      *
       01  WS-LCRE-DETAIL.
           05 FILLER                 PIC X(3)    VALUE '*'.
           05 WS-LCRE-DET-LIB-ED     PIC X(30)   VALUE SPACES.
           05 FILLER                 PIC X(3)    VALUE ':'.
           05 WS-LCRE-DET-TOT-ED     PIC ZZZZZ9.
           05 FILLER                 PIC X(3)    VALUE '  *'.
      *
       01  WS-LCRE-CLIENT-ED.
           05 FILLER                 PIC X(3)  VALUE '*  '.
           05 WS-LCRE-CLI-LIB-ED     PIC X(28) VALUE
                                         'NOMBRE DE CLIENTS'.
           05 FILLER                 PIC X(5)  VALUE '  :  '.
           05 WS-LANO-CLI-TOT-ED     PIC ZZZ9.
           05 FILLER                 PIC X(5)  VALUE '    *'.
      *
       01  WS-LCRE-MVTS-ED.
           05 FILLER                 PIC X(3)  VALUE '*  '.
           05 WS-LCRE-MVT-LIB-ED     PIC X(28) VALUE
                                         'NOMBRE DE MOUVEMENTS'.
           05 FILLER                 PIC X(5)  VALUE '  :  '.
           05 WS-LANO-MVT-TOT-ED     PIC ZZZ9.
           05 FILLER                 PIC X(5)  VALUE '    *'.
      *
       01  WS-LCRE-MVTE-ED.
           05 FILLER                 PIC X(3)  VALUE '*  '.
           05 WS-LCRE-MVTE-LIB-ED    PIC X(28) VALUE
                                        'NOMBRE DE MOUVEMENTS ERRONES'.
           05 FILLER                 PIC X(5)  VALUE '  :  '.
           05 WS-LANO-MVTE-TOT-ED    PIC ZZZ9.
           05 FILLER                 PIC X(5)  VALUE '    *'.
      *
       01  WS-LCRE-RET-ED.
           05 FILLER                 PIC X(3)  VALUE '*  '.
           05 WS-LCRE-RET-LIB-ED     PIC X(28) VALUE
                                         'NOMBRE DE RETRAITS'.
           05 FILLER                 PIC X(5)  VALUE '  :  '.
           05 WS-LANO-RET-TOT-ED     PIC ZZZ9.
           05 FILLER                 PIC X(5)  VALUE '    *'.
      *
       01  WS-LCRE-CBS-ED.
           05 FILLER                 PIC X(3)  VALUE '*  '.
           05 WS-LCRE-CBS-LIB-ED     PIC X(28) VALUE
                                         'NOMBRE DE CARTES BLEUES'.
           05 FILLER                 PIC X(5)  VALUE '  :  '.
           05 WS-LANO-CBS-TOT-ED     PIC ZZZ9.
           05 FILLER                 PIC X(5)  VALUE '    *'.
      *
       01  WS-LCRE-DEP-ED.
           05 FILLER                 PIC X(3)  VALUE '*  '.
           05 WS-LCRE-DEP-LIB-ED     PIC X(28) VALUE
                                         'NOMBRE DE DEPOTS'.
           05 FILLER                 PIC X(5)  VALUE '  :  '.
           05 WS-LANO-DEP-TOT-ED     PIC ZZZ9.
           05 FILLER                 PIC X(5)  VALUE '    *'.
      *
       01  WS-LCRE-CLINEWF-ED.
           05 FILLER                 PIC X(3)  VALUE '*  '.
           05 WS-LCRE-CLINEW-ED      PIC X(28) VALUE
                                         'NOMBRE DE CLIENTS NOUVEAUX'.
           05 FILLER                 PIC X(5)  VALUE '  :  '.
           05 WS-LANO-CLINEW-TOT-ED  PIC ZZZ9.
           05 FILLER                 PIC X(5)  VALUE '    *'.
      *
       01  WS-LCRE-CLISOPF-ED.
           05 FILLER                 PIC X(3)  VALUE '*  '.
           05 WS-LCRE-CLISOP-ED      PIC X(28) VALUE
                                         'NOMBRE DE CLIENTS SANS OPER'.
           05 FILLER                 PIC X(5)  VALUE '  :  '.
           05 WS-LANO-CLISOP-TOT-ED  PIC ZZZ9.
           05 FILLER                 PIC X(5)  VALUE '    *'.
      *
       01  WS-LCRE-CLISTDF-ED.
           05 FILLER                 PIC X(3)  VALUE '*  '.
           05 WS-LCRE-CLISTD-ED      PIC X(28) VALUE
                                         'NOMBRE DE CLIENTS STANDARDS'.
           05 FILLER                 PIC X(5)  VALUE '  :  '.
           05 WS-LANO-CLISTD-TOT-ED  PIC ZZZ9.
           05 FILLER                 PIC X(5)  VALUE '    *'.
