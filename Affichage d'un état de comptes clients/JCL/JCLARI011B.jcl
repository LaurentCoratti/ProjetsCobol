//ARI0111E JOB (ACCT#),'ARIO11B',MSGCLASS=H,CLASS=A,
//             REGION=4M,MSGLEVEL=(1,1),NOTIFY=&SYSUID,TIME=(0,30)
//*
//* **************************************************************
//* *                                                            *
//* *                     ESTIAC INSTITUT                        *
//* *                                                            *
//* *           UNITE DE FORMATION COBOL PROGRAMMATION           *
//* *                                                            *
//* *   EXECUTION DU PROGRAMME ARIO11B CORRESPONDANT AU TP N°1   *
//* *                                                            *
//* **************************************************************
//* **************************************************************
//* * PREMIERE ETAPE :                                           *
//* * EXECUTION DU PROGRAMME PRINCIPAL                           *
//* **************************************************************
//STEPTP1  EXEC PGM=ARIO11B
//SYSOUT   DD  SYSOUT=*,OUTLIM=800
//* **************************************************************
//* * DECLARATION DE LA BIBLIOTHEQUE QUI CONTIENT LE LOAD MODULE *
//* **************************************************************
//STEPLIB  DD  DSN=ARI01.ARI0111.LOAD,DISP=SHR
//* **************************************************************
//* * DECLARATION DU FICHIER DES MOUVEMENTS EN ENTREE            *
//* **************************************************************
//INP001   DD  DSN=ARI01.ARI0111.FMVTS,DISP=SHR
//