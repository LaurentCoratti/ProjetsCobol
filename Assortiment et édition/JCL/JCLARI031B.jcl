//ARI0111E JOB (ACCT#),'ARIO31B',MSGCLASS=H,CLASS=A,
//             REGION=4M,MSGLEVEL=(1,1),NOTIFY=&SYSUID,TIME=(0,30)
//*
//* **************************************************************
//* *                                                            *
//* *                     ESTIAC INSTITUT                        *
//* *                                                            *
//* *           UNITE DE FORMATION COBOL PROGRAMMATION           *
//* *                                                            *
//* *   EXECUTION DU PROGRAMME ARIO31B CORRESPONDANT AU TP N°3   *
//* *                                                            *
//* **************************************************************
//*
//* **************************************************************
//* * PREMIERE ETAPE : SUPPRESSION DES FICHIERS CREES PAR LE     *
//* * PROGRAMME PRINCIPAL                                        *
//* **************************************************************
//SUPFCPT EXEC PGM=IEFBR14
//FILE1    DD  DSN=ARI01.ARI0111.FCPTS,
//             UNIT=3390,VOL=SER=WRK001,DISP=(OLD,DELETE)
//FILE2    DD  DSN=ARI01.ARI0111.ETATCLI,
//             UNIT=3390,VOL=SER=WRK001,DISP=(OLD,DELETE)
//FILE3    DD  DSN=ARI01.ARI0111.ETATANO,
//             UNIT=3390,VOL=SER=WRK001,DISP=(OLD,DELETE)
//*
//* **************************************************************
//* * DEUXIEME ETAPE : EXECUTION DU PROGRAMME PRINCIPAL          *
//* **************************************************************
//STARIO3 EXEC PGM=ARIO31B
//* **************************************************************
//* * DECLARATION DE LA BIBLIOTHEQUE SUR LOAD MODULE             *
//* **************************************************************
//STEPLIB  DD  DSN=ARI01.ARI0111.LOAD,DISP=SHR
//SYSOUT   DD  SYSOUT=*,OUTLIM=800
//* **************************************************************
//* * DECLARATION DES FICHIERS EN ENTREE                         *
//* **************************************************************
//INP001   DD  DSN=ARI01.ARI0111.FMVTS,DISP=(OLD,KEEP),
//             UNIT=3390,VOL=SER=WRK001
//INP002   DD  DSN=ARI01.ARI0111.FCPTE,DISP=(OLD,KEEP),
//             UNIT=3390,VOL=SER=WRK001
//* **************************************************************
//* * DECLARATION DES FICHIERS EN SORTIE                         *
//* **************************************************************
//OUT001   DD  DSN=ARI01.ARI0111.FCPTS,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=3390,VOL=SER=WRK001,
//             SPACE=(TRK,(1,1)),
//             DCB=(RECFM=FB,LRECL=50,BLKSIZE=5000)
//ETATCLI  DD  DSN=ARI01.ARI0111.ETATCLI,UNIT=3390,
//             DISP=(NEW,CATLG,DELETE),VOL=SER=WRK001,
//             SPACE=(TRK,(1,1)),
//             DCB=(RECFM=FB,LRECL=81,BLKSIZE=810,DSORG=PS)
//ETATANO  DD  DSN=ARI01.ARI0111.ETATANO,UNIT=3390,
//             DISP=(NEW,CATLG,DELETE),VOL=SER=WRK001,
//             SPACE=(TRK,(1,1)),
//             DCB=(RECFM=FB,LRECL=81,BLKSIZE=810,DSORG=PS)
//