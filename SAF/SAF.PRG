************************************
* INICIO DO SISTEMA: 13/10/97
***************************************
* DESPROTEGER E VERIFICA O COMPUTADOR
***************************************

#include "inkey.ch"              // constantes de codigos das teclas
#include "Fileio.ch"             // funcoes de leitura/gravacao de arquivo
#include "set.ch"                // constantes da funcao SETKEY()
#define COM_CALE
#define COM_MAQCALC

FUNCTION saf(mdir)
******************

LOCAL men,mempresa,mtipo_cor


PUBLIC cor,cor1,mdata_sis,mversao,sistema,mfi,rma,memp,resa,mcgc_firm,minsc_firm,;
       mend_firm,mcid_firm,mfone_firm,micm_firm,muf_firm

PUBLIC mlci,mcci,mlba,mcba,mens,mlinhas,sis_cidade,mdiretorio,mdrive,maut

IF TRAVAJAN()=.F.;RETURN;ENDIF  // Verifica se ja estar em uso o sistema

REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
cor1 := SETCOLOR()
cor := SETCOLOR()
mdiretorio := SPACE(4)
mdata_sis := DATE()
SET DATE BRITI
***** NOME DA EMPRESA A COMPILAR *****
mempresa := 'NOBREGA'
mtipo_cor := '1'
**************************************

******* T I P O   D E   S I S T E M A **************
mfi := 'HRB'
rma := ' Solucoes em Informatica'
mversao := 'v6.08.03'
sistema := 'SAFE - Sistema Academico Financeiro Escolar'
****************************************************
*************************************************
mold := 'ÚÄ¿³ÙÄÀ³'      // Molduras das janelas
mol := '        '      // Molduras das janelas
*************************************************
mdrive := CURDIR()

WHILE .T.
        SET MESSAGE TO 23 CENTER
        SET CONS OFF
        SET WRAP ON
        SET TALK OFF
        SET SCOREBOARD OFF
        SET DATE BRITI
        SET DELE ON
        SET INTEN ON
        SET EPOCH TO 1911
        SET KEY 28 TO menu_cons()       // F1 MENU DE CONSULTAS
        SET KEY -41 TO maqcalc()         // F12 calculadora
        SET KEY 287 TO alt_senha()
        SET KEY 276 TO alt_data()        // ALT+T

        limpa(00,00,24,79)
        WHILE .T.
                setcor(1)
                limpa(00,00,24,79)
                num := 80
                setcor(5)
                IF mempresa = 'NOBREGA'
                        **************** NOBREGA *****************************
                        memp := 'COLEGIO'
                        resa := ' NOBREGA'
                        maut := 'NOBREGA'
                        mcgc_firm := '10.977.585/0001-58'
                        minsc_firm := ' '
                        mend_firm := 'Av.Oliveira Lima, 824'
                        mcid_firm := 'Boa Vista - Recife - PE.'
                        mfone_firm := '(081) 3423.1844'
                        sis_cidade := 'Recife, PE'
                        * DATA DE INICIO: 13/10/97
                        ******************************************************************
                        BANNER(01,'COLEGIO',setcor(5))
                        BANNER(07,'NOBREGA',setcor(5))
                        tela('>>> SAFE - Sistema Academico e Financeiro Escolar <<<')
                ELSEIF mempresa = 'SOUZA'
                        **************** NOBREGA *****************************
                        memp := 'COLEGIO '
                        resa := 'SOUZA LEAO'
                        maut := 'SOUZA'
                        mcgc_firm := '10.977.585/0001-58'
                        minsc_firm := 'P.050.015'
                        mend_firm := 'Av.Bernardo V.Melo, 6294'
                        mcid_firm := 'Candeias - Jaboatao Gurarapes - PE.'
                        mfone_firm := '(081) 469.1844'
                        sis_cidade := 'Recife, PE'
                        * DATA DE INICIO: 13/10/97
                        ******************************************************************
                        BANNER(01,'COLEGIO',setcor(5))
                        BANNER(07,'SOUZA LEAO',setcor(5))
                        tela('>>> SAFE - Sistema Academico e Financeiro Escolar <<<')
                ELSEIF mempresa = 'EAL'
                        **************** NOBREGA *****************************
                        memp := 'EDUCANDARIO'
                        resa := 'AQUARELA DAS LETRAS'
                        maut := 'EAL'
                        mcgc_firm := '                  '
                        minsc_firm := '         '
                        mend_firm := 'Rua Artur Barbosa Maciel, 40'
                        mcid_firm := 'Centro - Tacaimbo - PE.'
                        mfone_firm := '(81)3755.1209'
                        sis_cidade := 'Tacaimbo, PE'
                        * DATA DE INICIO: 20.08.2002'
                        ******************************************************************
                        BANNER(01,'EDUCANDAR',setcor(5))
                        BANNER(07,'AQUARELA',setcor(5))
                        tela('>>> SAFE - Sistema Academico e Financeiro Escolar <<<')
                ELSE
                        atencao('Firma nao foi indentificada !!!')
                        limpa(00,00,24,79)
                        QUIT
                ENDIF
                IF mdir = NIL
                        mensagem('Informa o ano que deseja:')
*                       @ 23,COL()+1 GET mdiretorio PICT '9999' VALID mdiretorio $ '1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005'
                        @ 23,COL()+1 GET mdiretorio PICT '9999'
                        READ
                        IF LASTKEY() = 27
                                limpa(00,00,24,79)
                                QUIT
                        ENDIF
                        limpa(23,00,23,79)
                ELSE
                        mdiretorio := mdir
                ENDIF
                WHILE .T.
                        IF ! CHDIR('\'+mdrive+'\'+mdiretorio)
                                mdiretorio := SPACE(4)
                                mensagem('Informa o ano que deseja:')
                                @ 23,COL()+1 GET mdiretorio PICT '9999'
                                READ
                                IF LASTKEY() = 27
                                        limpa(00,00,24,79)
                                        QUIT
                                ENDIF
                                limpa(23,00,23,79)
                        ELSE
                                CHDIR('\'+mdrive+'\')
                                EXIT
                        ENDIF
                ENDDO

*               SET DEFAULT TO ('\'+mdrive+'\'+mdiretorio)
                safdbf()
                *********************
                * ARQUIVOS
                arqsenha()
                arqtexto()
                ***********************************************************************************************************************************
                CLOSE ALL
                limpa(24,31,24,79)
                ******************************************
                setcor(1)
                *************
                IF ! abriarq('insopera','sen');RETURN NIL;ENDIF
                *************
                IF ! senha()
                        SET CURSOR ON
                        UNLOCK
                        SETCOLOR('W/N,X')
                        SET DEFAULT TO ('\'+mdrive)
                        RUN CD..
                        CLOSE ALL
                        limpa(00,00,24,79)
                        QUIT
                        RETURN NIL
                ENDIF
                *************************************************************
                CLOSE ALL
                WHILE .T.
                        limpa(24,31,24,58)
                        setcor(1)
                        DEVPOS(24,31);DEVOUT('Data do Sistema:')
                        @ 24,48 GET mdata_sis PICT '99/99/99'
                        READ
                        IF LASTKEY() = 27 .OR. EMPTY(mdata_sis)
                                LOOP
                        ELSE
                                EXIT
                        ENDIF
                ENDDO
                IF ! abriarq('safsetup','set');RETURN NIL;ENDIF
                * ERRO DO SISTEMA**********
                IF ! EMPTY(set->sysdatlm) .AND. (DATE() >= CTOD(SUBSTR(set->sysdatlm,1,2)+'/'+SUBSTR(set->sysdatlm,3,2)+'/'+SUBSTR(set->sysdatlm,5,2));
                   .OR. SUBSTR(set->sysdatlm,1,1) = '*')
                        erro()
                ENDIF
                ********************
                saf0()
        ENDDO
ENDDO
RETURN NIL
******************************** f i m **********************************
