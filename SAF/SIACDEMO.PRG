************************************
* INICIO DO SISTEMA: 14/11/94
***************************************
* DESPROTEGER E VERIFICAR O COMPUTADOR
***************************************
#include "inkey.ch"              // constantes de codigos das teclas
#include "Fileio.ch"             // funcoes de leitura/gravacao de arquivo
#include "set.ch"                // constantes da funcao SETKEY()
#define COM_CALE
#define COM_MAQCALC
//
FUNCTION sac(mdiretorio)
************
LOCAL men,mempresa,mtipo_cor,f,num,mcgc_aux,mserie:='975976'
LOCAL matsen:={},mcgc:= '11971082000138',mdata_lim
//
PUBLIC cor,mdata_sis,mfi,rma,mold,mversao,sistema,memp,resa,mcgc_firm,minsc_firm,;
       mend_firm,mcid_firm,mfone_firm,muf_firm,mdrive
//
PUBLIC mlci,mcci,mlba,mcba,mens,mcopia,mlinhas,mform,sis_cidade
//
PUBLIC tesc
tesc := CHR(27)
*************************************************
mold := 'ÚÄ¿³ÙÄÀ³'      // Molduras das janelas
* mold := '        '      // Molduras das janelas
*************************************************
//
REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
//
mdata_sis := DATE()
SET DATE BRITI

************ LIMITE DE USO ************
mdata_lim := CTOD('19/08/98')
***************************************

mdrive := CURDIR()
IF mdiretorio <> NIL
        IF ! CHDIR('\'+mdrive+'\'+mdiretorio)
                atencao('Este diretorio nao existe')
                RETURN NIL
        ENDIF
        SET DEFAULT TO ('\'+mdrive+'\'+mdiretorio)
ENDIF
******* T I P O   D E   S I S T E M A **************
mversao := 'v2.07'
sistema := 'Sistema Integrado Automacao Comercial'
mfi := 'INSE'
rma := 'RCOM Informatica Ltda. - DEMO'
****************************************************
mtipo_cor := '1'
***************************************
//
WHILE .T.
        SET MESSAGE TO 23 CENTER
        SET CONS OFF
        SET WRAP ON
*       SET TALK OFF
        SET SCOREBOARD OFF
        SET DATE BRITI
        SET DELE ON
        SET INTEN ON
        SET EPOCH TO 1960
        SET KEY 305 TO cod_nota         // ALT+N
        SET KEY 289 TO cod_fat          // ALT+F
        SET KEY 280 TO nat_op           // ALT+O
        SET KEY 287 TO alt_senha        // ALT+S
        SET KEY 276 TO alt_data         // ALT+T
        SET KEY 294 TO limite_sys       // ALT+L
        SET KEY 274 TO limite_erro      // ALT+E
        SET KEY 408 TO fim_prg          // ALT+seta p/cima
        SET KEY -40 TO maqcalc()         // F11
        SET KEY -41 TO cale()         // F12
        memp := SPACE(30)
        limpa(00,00,24,79)
        WHILE .T.
                cor := SETCOLOR()
                setcor(1)
                limpa(00,00,24,79)
                ************** TELA DE APRESENTACAO ************
                tela('"SIAC" - Sistema Integrado Automacao Comercial')
                ************************************************
                ***************************
                * ARQUIVO DE SETUP
                ***************************
                IF ! FILE('sacsetup.dbf')
                        matsen:={ }
                        AADD(matsen,{'pis','N',5,2})
                        AADD(matsen,{'confis','N',5,2})
                        AADD(matsen,{'imposto','N',5,2})
                        AADD(matsen,{'varejo','C',1,0})
                        AADD(matsen,{'fatura','N',5,2})
                        AADD(matsen,{'vlr_ent','N',6,2})
                        AADD(matsen,{'impress','C',1,0})
                        AADD(matsen,{'terminal','C',4,0})
                        AADD(matsen,{'video','C',1,0})
                        AADD(matsen,{'ped_obs','C',1,0})
                        AADD(matsen,{'impr_term','C',1,0})
                        AADD(matsen,{'copia','N',2,0})
                        AADD(matsen,{'form','C',1,0})
                        AADD(matsen,{'pagina','C',1,0})
                        AADD(matsen,{'percent','C',1,0})
                        AADD(matsen,{'relatorio','C',4,0})
                        AADD(matsen,{'nota','C',4,0})
                        AADD(matsen,{'uf','C',2,0})
                        AADD(matsen,{'icm','N',2,0})
                        AADD(matsen,{'linhas','N',3,0})
                        AADD(matsen,{'tp_comi','C',1,0})
                        AADD(matsen,{'cabecalho','C',1,0})
                        AADD(matsen,{'rodap1','C',60,0})
                        AADD(matsen,{'rodap2','C',60,0})
                        AADD(matsen,{'rodap3','C',60,0})
                        AADD(matsen,{'rodap4','C',60,0})
                        AADD(matsen,{'desc_max','N',6,2})
                        AADD(matsen,{'desc_apr','C',1,0})
                        AADD(matsen,{'desc_ped','C',1,0})
                        AADD(matsen,{'blo_vend','C',1,0})
                        AADD(matsen,{'dado_cli','C',1,0})
                        AADD(matsen,{'perc1','N',6,2})
                        AADD(matsen,{'perc2','N',6,2})
                        AADD(matsen,{'perc3','N',6,2})
                        AADD(matsen,{'perc4','N',6,2})
                        AADD(matsen,{'duplicata','C',1,0})
                        AADD(matsen,{'copia_dup','N',1,0})
                        AADD(matsen,{'dup_imp','C',1,0})
                        AADD(matsen,{'dup_port','C',4,0})
                        AADD(matsen,{'intervalo','N',3,0})
                        AADD(matsen,{'multa_dup','N',6,2})
                        AADD(matsen,{'juros_dup','N',6,2})
                        AADD(matsen,{'roda_dup1','C',60,0})
                        AADD(matsen,{'roda_dup2','C',60,0})
                        AADD(matsen,{'despesa','C',1,0})
                        AADD(matsen,{'via_desp','N',2,0})
                        AADD(matsen,{'vales','C',1,0})
                        AADD(matsen,{'via_vales','N',2,0})
                        AADD(matsen,{'deposito','C',1,0})
                        AADD(matsen,{'via_dep','N',2,0})
                        AADD(matsen,{'haver','C',1,0})
                        AADD(matsen,{'via_hv','N',2,0})
                        AADD(matsen,{'prazo1','N',2,0})
                        AADD(matsen,{'pz_perc1','N',5,2})
                        AADD(matsen,{'prazo2','N',2,0})
                        AADD(matsen,{'pz_perc2','N',5,2})
                        AADD(matsen,{'prazo3','N',2,0})
                        AADD(matsen,{'pz_perc3','N',5,2})
                        AADD(matsen,{'prazo4','N',2,0})
                        AADD(matsen,{'pz_perc4','N',5,2})
                        AADD(matsen,{'tp_lucro','C',1,0})
                        AADD(matsen,{'senha_pd','C',1,0})
                        AADD(matsen,{'nome_ven','C',1,0})
                        AADD(matsen,{'sysdatlm','C',12,0})
                        AADD(matsen,{'receber','C',1,0})
                        AADD(matsen,{'cli_def','C',5,0})
                        AADD(matsen,{'qtdchcli','N',5,0})
                        AADD(matsen,{'caixa','C',1,0})
                        AADD(matsen,{'embalag','C',1,0})
                        AADD(matsen,{'ped_ind','C',1,0})
                        AADD(matsen,{'seculo','C',1,0})
                        AADD(matsen,{'arq_cli','C',20,0})

                        AADD(matsen,{'serie','C',20,0})
                        AADD(matsen,{'apre1','C',11,0})
                        AADD(matsen,{'apre2','C',11,0})
                        AADD(matsen,{'apre3','C',50,0})
                        AADD(matsen,{'insc','C',20,0})
                        AADD(matsen,{'razao','C',35,0})
                        AADD(matsen,{'end','C',40,0})
                        AADD(matsen,{'bairro','C',20,0})
                        AADD(matsen,{'cidade','C',20,0})
                        AADD(matsen,{'fone','C',30,0})
                        AADD(matsen,{'dataini','D',8,0})
                        DBCREATE('sacsetup.dbf',matsen)
                ENDIF
                WHILE .T.
                        CLOSE ALL
                        USE sacsetup ALIAS set SHARED
                        IF ! NETERR()
                                EXIT
                        ENDIF
                ENDDO
                IF LASTREC() = 0
                        IF ! ADIREG()
                                atencao('Nao foi Possivel Acessar o Arquivo')
                                CLOSE sen
                                RETURN .T.
                        ENDIF
                        set-> pis := .65
                        set-> confis := 2
                        set-> imposto := 0
                        set-> varejo := 'N'
                        set-> fatura := 2
                        set-> vlr_ent := 0
                        set-> impress := 'M'
                        set-> terminal := 'PRN'
                        set-> video := 'N'
                        set-> ped_obs := 'N'
                        set-> impr_term := 'M'
                        set-> copia := 1
                        set-> form := 'F'
                        set-> pagina := 'M'
                        set-> percent := 'F'
                        set-> relatorio := 'LPT1'
                        set-> nota := 'LPT1'
                        set-> uf := 'PE'
                        set-> icm := 17
                        set-> linhas := 10
                        set-> tp_comi := 'V'
                        set-> cabecalho := 'S'
                        set-> rodap1 := '**** Agradecemos sua preferencia, Volte sempre ****'
                        set-> rodap2 := SPACE(60)
                        set-> rodap3 := SPACE(60)
                        set-> rodap4 := SPACE(60)
                        set-> desc_max := 0
                        set-> desc_apr := 'N'
                        set-> desc_ped := 'S'
                        set-> blo_vend := 'N'
                        set-> dado_cli := 'N'
                        set-> duplicata := 'S'
                        set-> copia_dup := 1
                        set-> dup_imp := 'V'
                        set-> dup_port := 'LPT1'
                        set-> intervalo := 30
                        set-> multa_dup := 0
                        set-> juros_dup := 0
                        set-> roda_dup1 := SPACE(60)
                        set-> roda_dup2 := SPACE(60)
                        set-> despesa := 'F'
                        set-> via_desp := 1
                        set-> vales := 'F'
                        set-> via_vales := 1
                        set-> deposito := 'F'
                        set-> via_dep := 1
                        set-> haver := 'F'
                        set-> via_hv := 1
                        set-> prazo1 := 0
                        set-> pz_perc1 := 0
                        set-> prazo2 := 0
                        set-> pz_perc2 := 0
                        set-> prazo3 := 0
                        set-> pz_perc3 := 0
                        set-> prazo4 := 0
                        set-> pz_perc4 := 0
                        set-> tp_lucro := 'R'
                        set-> senha_pd := 'N'
                        set-> nome_ven := 'N'
                        set-> receber := 'N'
                        set-> caixa := 'N'
                        set-> embalag := 'N'
                        set-> ped_ind := '1'
                        set-> seculo := 'N'
                        set->serie  :=  CRIPTO(ALLTRIM('11971082000138'),'*')+CRIPTO(ALLTRIM('975976'),'*')
                        set->apre1  :=  'INSERCOM'
                        set->apre2  :=  'DEMO'
                        set->insc   :=  '9999999999999'
                        set->razao  :=  'INSERCOM Informatica Ltda.'
                        set->end    :=  'Rua Barao da Virotia, 295 - SL.1006/1007'
                        set->bairro :=  'Sao Jose'
                        set->cidade :=  'Recife - PE.'
                        set->fone   :=  '(081) 224.3567-3623'
                        set->dataini:=  DATE()
                        DBCOMMIT()
                        DBUNLOCK()
                ENDIF
                * arqsetup()      //PRG INSERCOM
                IF EMPTY(set->serie)
                        IF ! incl_firma()       // PRG INSERCOM
                                SET CURSOR ON
                                DBUNLOCK()
                                SETCOLOR('W/N,X')
                                CLOSE ALL
                                limpa(00,00,24,79)
                                setcor(6)
                                @ 01,01 SAY 'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ '
                                @ 02,01 SAY '³ ÄÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄÄ ³Û'
                                @ 03,01 SAY '³   ÄÄÄÄÄÄÜÜÄÄÄÄÄÄ   ³Û'
                                @ 04,01 SAY '³    ÄÄÄÄÄÜÜÄÄÄÄÄ    ³Û'
                                @ 05,01 SAY '³   ÄÄÄÄÄÄÜÜÄÄÄÄÄÄ   ³Û'
                                @ 06,01 SAY '³ ÄÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄÄ ³Û'
                                @ 07,01 SAY 'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´Û'
                                @ 08,01 SAY '³Insercom Informatica³Û'
                                @ 09,01 SAY 'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ'
                                @ 10,01 SAY ' ßßßßßßßßßßßßßßßßßßßßßß'
                                SETCOLOR('W/N,X')
                                @ 01,31 SAY 'ÜÜÜ ÜÜÜ Ü ÜÜÜÜ ÜÜÜÜ ÜÜÜÜ ÜÜÜÜ ÜÜÜÜ ÜÜÜÜÜ '
                                @ 02,31 SAY ' Û  Û Û Û ÛÜÜÜ ÛÜ   ÛÜÜÛ Û    Û  Û Û Û Û '
                                @ 03,31 SAY 'ÜÛÜ Û ÛÜÛ ÜÜÜÛ ÛÜÜÜ Û ÛÜ ÛÜÜÜ ÛÜÜÛ Û Û Û '
                                @ 04,31 SAY 'ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ'
                                @ 05,31 SAY '                                         '
                                @ 06,31 SAY 'Rua Barao da Vitoria, 295 - Sl.1007      '
                                @ 07,31 SAY 'Sao Jose - Recife - PE   '
                                @ 08,31 SAY 'CEP.50020-120                            '
                                @ 09,31 SAY 'TeleFax: (081) 424.4912                  '
                                setcor(3)
                                CAIXA(mold,16,02,19,56,,,.T.)
                                setcor(1)
                                DEVPOS(17,04);DEVOUT('Nao foi possivel fazer a instalacao do sistema')
                                DEVPOS(18,04);DEVOUT('SIAC - Sistema Integrado Automacao Comercial')
                                DEVPOS(20,00);DEVOUT('')
                                SETCOLOR(cor)
                                QUIT
                                RETURN NIL
                        ENDIF
                ENDIF
                mserie := DCRIPTO(SUBSTR(set->serie,1,14))+DCRIPTO(SUBSTR(set->serie,15,6))
                IF mserie      = '11971082000138975976'  // INSERCOM
                        setcor(5)
                        limpa(00,00,24,79)
                        ************** TELA DE APRESENTACAO ************
                        tela('"SIAC" - Sistema Integrado Automacao Comercial')
                        ************************************************
                        num := 80
                        setcor(5)
                        memp := RTRIM(set->razao)
                        resa := ''
                        IF ! EMPTY(set->apre2)
                                BANNER(01,RTRIM(set->apre1),setcor(5))
                                BANNER(07,RTRIM(set->apre2),setcor(5))
                                FOR f=1 TO 161-num
                                         DEVPOS(13,00);DEVOUT(SUBSTR(SPACE(num) + PADC(RTRIM(set->apre3),80),f,num))
                                NEXT
                        ELSE
                                DEVPOS(02,00);DEVOUT(PADC(REPL('ß',LEN(RTRIM(set->apre1))*8),80))
                                BANNER(03,RTRIM(set->apre1),setcor(5))
                                DEVPOS(09,00);DEVOUT(PADC(REPL('ß',LEN(RTRIM(set->apre1))*8),80))
                                DEVPOS(10,00);DEVOUT(PADC(RTRIM(set->apre3),80))
                        ENDIF
                        mcgc_aux := DCRIPTO(set->serie)
                        mcgc_firm := SUBSTR(mcgc_aux,1,2)+'.'+SUBSTR(mcgc_aux,3,3)+'.'+SUBSTR(mcgc_aux,6,3)+'/'+SUBSTR(mcgc_aux,9,4)+'-'+SUBSTR(mcgc_aux,13,2)
                        minsc_firm := set->insc
                        mend_firm := set->end
                        mcid_firm := RTRIM(set->cidade)+'-'+set->uf
                        mfone_firm := RTRIM(set->fone)
                        sis_cidade := RTRIM(set->cidade)
                        setcor(1)
*                       setcor(6)
*                       men_giro('INSERCOM Informatica Ltda.',21,02,20)
                        men_giro('I N S E R C O M   Informatica Ltda.',17,34,35)
                        setcor(1)
                        IF set->seculo = 'S'
                                SET CENTURY ON
                        ENDIF
                        ***************************
                        * ARQUIVO DE TEXTO
                        ***************************
                        arqtexto()
                        ***************************
                        * ARQUIVO DE SENHAS
                        ***************************
                        arqsenha()
                        *************************************
                        * INICIALIZACAO DO SISTEMA POR SENHA
                        *************************************
                        *-----------------------------------------------------
                        CLOSE ALL
                        USE insopera INDEX insopera ALIAS sen SHARED NEW
                        *-----------------------------------------------------
/*
                        IF mdiretorio = NIL
                                *********************
                                inicializa(mdiretorio)
                                **********************
                        ENDIF
*/
                        setcor(5)
                        ****************************
                        * ARQUIVOS
                        sacdbf()
                        ***********************************************************************************************************************************
                        CLOSE ALL
                        setcor(1)
                        limpa(24,31,24,79)
                        *************
                        IF ! senha()
                                SET CURSOR ON
                                UNLOCK
                                SETCOLOR('W/N,X')
                                CLOSE ALL
                                limpa(00,00,24,79)
                                setcor(6)
                                @ 01,01 SAY 'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ '
                                @ 02,01 SAY '³ ÄÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄÄ ³Û'
                                @ 03,01 SAY '³   ÄÄÄÄÄÄÜÜÄÄÄÄÄÄ   ³Û'
                                @ 04,01 SAY '³    ÄÄÄÄÄÜÜÄÄÄÄÄ    ³Û'
                                @ 05,01 SAY '³   ÄÄÄÄÄÄÜÜÄÄÄÄÄÄ   ³Û'
                                @ 06,01 SAY '³ ÄÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄÄ ³Û'
                                @ 07,01 SAY 'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´Û'
                                @ 08,01 SAY '³Insercom Informatica³Û'
                                @ 09,01 SAY 'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ'
                                @ 10,01 SAY ' ßßßßßßßßßßßßßßßßßßßßßß'
                                SETCOLOR('W/N,X')
                                @ 01,31 SAY 'ÜÜÜ ÜÜÜ Ü ÜÜÜÜ ÜÜÜÜ ÜÜÜÜ ÜÜÜÜ ÜÜÜÜ ÜÜÜÜÜ '
                                @ 02,31 SAY ' Û  Û Û Û ÛÜÜÜ ÛÜ   ÛÜÜÛ Û    Û  Û Û Û Û '
                                @ 03,31 SAY 'ÜÛÜ Û ÛÜÛ ÜÜÜÛ ÛÜÜÜ Û ÛÜ ÛÜÜÜ ÛÜÜÛ Û Û Û '
                                @ 04,31 SAY 'ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ'
                                @ 05,31 SAY '                                         '
                                @ 06,31 SAY 'Rua Barao da Vitoria, 295 - Sl.1007      '
                                @ 07,31 SAY 'Sao Jose - Recife - PE   '
                                @ 08,31 SAY 'CEP.50020-120                            '
                                @ 09,31 SAY 'TeleFax: (081) 424.4912                  '
                                @ 12,02 SAY REPLI('ß',LEN('Copia Exclusiva: '+memp_resa))
                                @ 13,02 SAY 'Copia Exclusiva: '
                                setcor(4)
                                DEVPOS(13,19);DEVOUT(memp_resa)
                                SETCOLOR('W/N,X')
                                @ 14,02 SAY REPLI('Ü',LEN('Copia Exclusiva: '+memp_resa))
                                setcor(3)
                                CAIXA(mold,16,02,19,56,,,.T.)
                                setcor(1)
                                DEVPOS(17,04);DEVOUT('A tentativa de COPIA deste Sistema pode acarretar')
                                DEVPOS(18,04);DEVOUT('Problemas irrevesiveis para seu Computador')
                                DEVPOS(20,00);DEVOUT('')
                                SETCOLOR(cor)
                                QUIT
                                RETURN NIL
                        ENDIF
                        ******************************************
                        CLOSE ALL
                        IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
                        *-----------------------------------------
                        muf_firm := set->uf
                        mcopia := set->copia
                        mform := set->form
                        mn_fiscal := set->nota
                        mlinhas := set->linhas
                        **************** LIMITE DE OPERACAO ************************
                        IF DATE() > mdata_lim
                                SETCOLOR(cor)
                                DELE FILE \SIAC\SIAC.EXE
                                CLEAR
                                QUIT
                        ENDIF
                        *************************************************************
                        CLOSE ALL
                        ******************************************
                        WHILE .T.
                                limpa(24,31,24,58)
                                setcor(1)
                                DEVPOS(24,31);DEVOUT('Data do Sistema:')
                                @ 24,48 GET mdata_sis
                                READ
                                IF LASTKEY() = 27 .OR. EMPTY(mdata_sis)
                                        LOOP
                                ELSE
                                        EXIT
                                ENDIF
                        ENDDO
                        CLOSE ALL
                        USE sactmp ALIAS tmp SHARED NEW
                        **************
                        SELE('tmp')
                        ORDSETFOCUS(1)
                        GO TOP
                        **************
                        IF tmp->(DBSEEK(cod_operado))
                                atencao('Ocorreu algum problema anteriormente quando estava tirando um pedido (Consulte seu Gerente) ou existe alguem tirando pedido com sua senha')
                                LOOP
                        ENDIF
                        CLOSE ALL
                        sac0(mdiretorio)
                ELSE
                        atencao('Numero de serie nao esta correto, procure a INSERCOM')
                        BLOQREG()
                        set->serie := SPACE(19)
                        DBCOMMIT()
                        DBUNLOCK()
                        LOOP
                ENDIF
        ENDDO
ENDDO
RETURN NIL

