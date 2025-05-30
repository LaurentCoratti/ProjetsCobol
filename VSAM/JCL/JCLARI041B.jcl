//ARI0111E JOB (ACCT#),'ARIO41B',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=4M,TIME=(0,30)
//* **************************************************************
//* * DECLARATION DE LA BIBLIOTHEQUE DU LOAD MODULE              *
//* **************************************************************
//JOBLIB   DD  DSN=ARI01.ARI0111.LOAD,DISP=SHR
//*
//* **************************************************************
//* *                                                            *
//* *                     ESTIAC INSTITUT                        *
//* *                                                            *
//* *           UNITE DE FORMATION COBOL PROGRAMMATION           *
//* *                                                            *
//* *   EXECUTION DU PROGRAMME ARIO41B CORRESPONDANT AU TP N°4   *
//* *                                                            *
//* **************************************************************
//*
//* **************************************************************
//* * PREMIERE ETAPE : SUPPRESSION DU FICHIER CPTE-ES CREE PAR   *
//* * UNE PRECEDENTE EXECUTION DU PROGRAMME                      *
//* * SI LE FICHIER N'EXISTAIT PAS, ON MODIFIE LE RC POUR EVITER *
//* * UN ARRET DE L'EXECUTION                                    *
//* **************************************************************
//DELETE  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 DELETE ARI01.ARI0111.CPTES.KSDS
 IF LASTCC <= 8 THEN SET MAXCC = 0
/*
//* **************************************************************
//* * DEUXIEME ETAPE : DEFINITION DU CLUSTER KSDS CPTE-ES        *
//* **************************************************************
//DEFINE  EXEC  PGM=IDCAMS
//SYSPRINT  DD  SYSOUT=*
//SYSIN     DD  *
 DEFINE CLUSTER (NAME(ARI01.ARI0111.CPTES.KSDS)             -
                   VOLUME(WRK001)                           -
                   TRACKS(3 1)                              -
                   FREESPACE(20 20)                         -
                   KEYS(10 0)                               -
                   RECORDSIZE(50 50)                        -
                   INDEXED)                                 -
         DATA    (NAME(ARI01.ARI0111.CPTES.KSDS.D))         -
         INDEX   (NAME(ARI01.ARI0111.CPTES.KSDS.I))
/*
//* **************************************************************
//* * TROISIEME ETAPE : REMPLISSAGE DU FICHIER KSDS CPTE-ES      *
//* * A PARTIR DU FICHIER QSAM FCPT4                             *
//* **************************************************************
//REPRO  EXEC  PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//DDIN     DD  DSN=ARI01.ARI0111.FCPTTRI,DISP=SHR
//DDOUT    DD  DSN=ARI01.ARI0111.CPTES.KSDS,DISP=SHR
//SYSIN    DD  *
 REPRO INFILE(DDIN)   -
       OUTFILE(DDOUT)
 PRINT INDATASET(ARI01.ARI0111.CPTES.KSDS)
/*
//*
//* **************************************************************
//* * QUATRIEME ETAPE : EXECUTION DU PROGRAMME ARIO4gu           *
//* **************************************************************
//STARIO4 EXEC PGM=ARIO41B
//SYSPRINT DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*,OUTLIM=800
//* **************************************************************
//* * DECLARATION DES FICHIERS                                   *
//* **************************************************************
//INP001   DD  DSN=ARI01.ARI0111.FMVTS,DISP=SHR
//IO001    DD  DSN=ARI01.ARI0111.CPTES.KSDS,DISP=SHR
//ETATCLI  DD  SYSOUT=*,OUTLIM=500
//ETATANO  DD  SYSOUT=*,OUTLIM=500
//*
//*
//* **************************************************************
//* * CINQUIEME ETAPE : EDITION DES FICHIERS                     *
//* **************************************************************
//EDITION EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 PRINT INDATASET(ARI01.ARI0111.FCPTTRI)
 PRINT INDATASET(ARI01.ARI0111.CPTES.KSDS)
 PRINT INDATASET(ARI01.ARI0111.FMVTS)
/*
//