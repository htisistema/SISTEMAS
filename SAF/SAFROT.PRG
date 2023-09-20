*******************
* FUNCAO DE CORES
*******************
#include "inkey.ch"              // constantes de codigos das teclas

FUNCTION setcor(nTipCor,mtipo)
************************

IF nTipCor = 1 .AND. mtipo = NIL
*       SETCOLOR('n/w,w/B,,,b/w')          && cor PRINCIPAL
        SETCOLOR('n+/w,W/N,,,n/w')          && cor PRINCIPAL
ELSEIF nTipCor = 2 .AND. mtipo = NIL
*       SETCOLOR('b/w,x')                     && cor para os SAY
        SETCOLOR('n/w,x')                     && cor para os SAY
ELSEIF nTipCor = 2 .AND. mtipo <> NIL
*       SETCOLOR('b/w*,x')                     && cor para os SAY
        SETCOLOR('n/w,x')                     && cor para os SAY
ELSEIF nTipCor = 3 .AND. mtipo = NIL
*        SETCOLOR('b/w,x')                     && cor para os SAY
        SETCOLOR('n/w,w/b,x')                     && cor para os SAY
ELSEIF nTipCor = 3 .AND. mtipo <> NIL
*        SETCOLOR('b/w*,x')                     && cor para os SAY
        SETCOLOR('n/w,w/b,x')                     && cor para os SAY
ELSEIF nTipCor = 4 .AND. mtipo = NIL
*       SETCOLOR('b/w,x')                     && cor FUNDO
        SETCOLOR('n+/w,x')                     && cor FUNDO
ELSEIF nTipCor = 4 .AND. mtipo <> NIL
*       SETCOLOR('b/w*,x')                     && cor FUNDO
        SETCOLOR('n+/w,x')                     && cor FUNDO
ELSEIF nTipCor = 5 .AND. mtipo = NIL
        SETCOLOR('b/w,x')                     && cor ABRINDO ARQUIVO
ELSEIF nTipCor = 5 .AND. mtipo <> NIL
        SETCOLOR('b/w*,x')                     && cor ABRINDO ARQUIVO
ELSEIF nTipCor = 6 .AND. mtipo = NIL
*       SETCOLOR('w/B+,x')                    && cor LOGOTIPO
        SETCOLOR('GR+/B+,b/w,,,gr+/b+')          && cor PRINCIPAL
ELSEIF nTipCor = 6 .AND. mtipo <> NIL
        SETCOLOR('w/B+,x')                    && cor LOGOTIPO
ELSEIF nTipCor = 7 .AND. mtipo = NIL
        SETCOLOR('B/w,x')                    && cor LOGOTIPO
ELSEIF nTipCor = 7 .AND. mtipo <> NIL
        SETCOLOR('B/w,x')                    && cor LOGOTIPO
ENDIF
RETURN .T.
***************************** F I M ****************************************
*******************
* FUNCAO DE CORES
*******************

FUNCTION setcor1(nTipCor)
************************

IF nTipCor = 1
        SETCOLOR('gr+/b,b/bg,,,b/w')          && cor PRINCIPAL
ELSEIF nTipCor = 2
        SETCOLOR('gr+/bg,x')                   && cor para MENSAGEM
ELSEIF nTipCor = 3
        SETCOLOR('B/w,x')                     && cor para os SAY
ELSEIF nTipCor = 4
        SETCOLOR('b/w,x')                     && cor FUNDO
ELSEIF nTipCor = 5
        SETCOLOR('R/w,x')                     && cor ABRINDO ARQUIVO
ELSEIF nTipCor = 6
        SETCOLOR('w/B+,x')                    && cor LOGOTIPO
ELSE
        SETCOLOR('bg+/bg+,bg+/bg+,x')          && cor p/OPCOES
ENDIF
RETURN .T.
***************************** F I M ****************************************
*********************
* VERIFICA A TABELA DE MENSALIDADES
***********************

FUNCTION ver_tab(mgst,mparc,t)

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

***************
SELE('tabmen');ORDSETFOCUS(1)
GO TOP
***************

IF ! tabmen->(DBSEEK(mgst+mparc))
        IF t = NIL
                atencao('Tabela nao Encontrada !!!')
        ENDIF
        SELE(msele);ORDSETFOCUS(morde)
        RETURN .F.
ELSE
        SELE(msele);ORDSETFOCUS(morde)
        RETURN .T.
ENDIF
RETURN NIL
***************************** F I M ****************************************
*********************
* VER CARGA HORARIA
***********************

FUNCTION ver_carga(mgstt,mdisc)

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

***************
SELE('tbgstt');ORDSETFOCUS(1)
GO TOP
***************

IF ! tbgstt->(DBSEEK(mgstt+mdisc))
*       atencao('Tabela nao Encontrada !!!')
        SELE(msele);ORDSETFOCUS(morde)
        RETURN 0
ELSE
        SELE(msele);ORDSETFOCUS(morde)
        RETURN tbgstt->ch
ENDIF
SELE(msele);ORDSETFOCUS(morde)
RETURN tbgstt->ch
***************************** F I M ****************************************
*********************
* VERIFICA O GSTT
***********************

FUNCTION ver_gstt(mgst,mdisc,t)

LOCAL msele,morde

IF t <> NIL
        IF EMPTY(mgst)
                RETURN .T.
        ENDIF
ENDIF

msele := SELE()
morde := INDEXORD()

***************
SELE('tbgstt');ORDSETFOCUS(1)
GO TOP
***************

IF mdisc <> NIL
        IF ! tbgstt->(DBSEEK(mgst+mdisc))
                atencao('GSTT nao Encontrada !!!')
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .F.
        ELSE
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .T.
        ENDIF
ELSE
        IF ! tbgstt->(DBSEEK(mgst))
                atencao('GSTT nao Encontrada !!!')
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .F.
        ELSE
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .T.
        ENDIF
ENDIF
RETURN NIL
***************************** F I M ****************************************
*********************
* VERIFICA A FAIXA DE MENSALIDADE
***********************

FUNCTION ver_fx(mfaixa,mparc_me,mb1,mb2)

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

***************
SELE('fx');ORDSETFOCUS(1)
GO TOP
***************

IF ! fx->(DBSEEK(mfaixa))
        atencao('Faixa: '+mfaixa+' nao Encontrada !!!')
        SELE(msele);ORDSETFOCUS(morde)
        RETURN .F.
ELSE
        IF mparc_me <> NIL .AND. mb1<>mb2
                *****************
                SELE('cdmen');ORDSETFOCUS(1)
                *****************
                GO TOP
                IF cdmen->(DBSEEK(aluno->matricula+mdiretorio+mparc_me+'ME'))
                        IF ! EMPTY(cdmen->data_pag)
*                               atencao('Nao e possivel alterar esta BOLSA, ja foi PAGA')
                                SELE(msele);ORDSETFOCUS(morde)
                                RETURN .F.
                        ENDIF
                ENDIF
        ENDIF
        SELE(msele);ORDSETFOCUS(morde)
        RETURN .T.
ENDIF
RETURN NIL
***************************** F I M ****************************************
*********************
* VERIFICA A EXISTENCIA DO ALUNO
***********************

FUNCTION ver_aluno(mmat,t,li,ci,mtipo)

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

***************
SELE('aluno');ORDSETFOCUS(4)
GO TOP
***************

IF mtipo = NIL
        IF ! aluno->(DBSEEK(mmat))
                IF t = NIL
                        atencao('Matricula: '+mmat+'  nao Encontrada !!!')
                ENDIF
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .F.
        ELSE
                IF li <> NIL
                        DEVPOS(li,ci);DEVOUT(aluno->aluno)
                ENDIF
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .T.
        ENDIF
ELSE
        IF EMPTY(mmat)
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .T.
        ENDIF
        IF aluno->(DBSEEK(mmat))
                atencao('Matricula: '+mmat+'  ja existe no arquivos')
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .F.
        ELSE
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .T.
        ENDIF
ENDIF
RETURN NIL
***************************** F I M ****************************************
*********************
* VERIFICA A MATRICULA E PARCELA
***********************

FUNCTION mat_parc(mmat,mparc,t,mmens,mtp)

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

***************
SELE('cdmen');ORDSETFOCUS(1)
GO TOP
***************

IF t <> NIL
        IF mtp = NIL
                IF ! cdmen->(DBSEEK(mmat+mdiretorio+mparc))
                        IF mmens = NIL
                                atencao('Mensalidade nao PAGA !!!')
                        ENDIF
                        SELE(msele);ORDSETFOCUS(morde)
                        RETURN .F.
                ELSE
                        SELE(msele);ORDSETFOCUS(morde)
                        RETURN .T.
                ENDIF
        ELSE
                IF ! cdmen->(DBSEEK(mmat+mdiretorio+mparc+mtp))
                        IF mmens = NIL
                                atencao('Mensalidade nao PAGA !!!')
                        ENDIF
                        SELE(msele);ORDSETFOCUS(morde)
                        RETURN .F.
                ELSE
                        SELE(msele);ORDSETFOCUS(morde)
                        RETURN .T.
                ENDIF
        ENDIF
ENDIF
IF cdmen->(DBSEEK(mmat+mparc))
        IF mmens = NIL
                atencao('Mensalidade ja PAGA !!!')
        ENDIF
        SELE(msele);ORDSETFOCUS(morde)
        RETURN .F.
ELSE
        SELE(msele);ORDSETFOCUS(morde)
        RETURN .T.
ENDIF
RETURN NIL
***************************** F I M ****************************************
*********************
* VERIFICA O FABRICANTE
***********************

FUNCTION ver_fab(mcod,ld,cd,p)

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

WHILE .T.
        IF mcod = 0
                IF ld <> NIL
                        setcor(3)
                        DEVPOS(ld,cd);DEVOUT(SPACE(30))
                        setcor(1)
                ENDIF
                RETURN .T.
        ENDIF
        ***************
        SELE('forn');ORDSETFOCUS(1)
        GO TOP
        ***************

        SEEK STRZERO(mcod,4)

        IF ! FOUND()    // .OR. tipo = 'FR'
                mcod := 0
                IF p = NIL
                        setcor(3)
                        DEVPOS(ld,cd);DEVOUT(SPACE(30))
                        setcor(1)
                        atencao('Fabricante nao Cadastrado !!!')
                        SELE(msele);ORDSETFOCUS(morde)
                        RETURN .F.
                ENDIF
        ENDIF
        IF p = NIL
                muf := uf
                mfabrica := razao
        ENDIF
        setcor(3)
        DEVPOS(ld,cd);DEVOUT(razao)
        setcor(1)
        SELE(msele);ORDSETFOCUS(morde)
        RETURN .T.
ENDDO
**************** f i m **********************************
*********************
* VERIFICA O TRANSPORTADORA
***********************

FUNCTION ver_trans(mcod,ld,cd,p)

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

WHILE .T.
        IF mcod = 0
                IF ld <> NIL
                        setcor(3)
                        DEVPOS(ld,cd);DEVOUT(SPACE(30))
                        setcor(1)
                ENDIF
                RETURN .T.
        ENDIF
        ***************
        SELE('tran');ORDSETFOCUS(1)
        GO TOP
        ***************

        SEEK STRZERO(mcod,4)

        IF ! FOUND()    // .OR. tipo = 'FR'
                mcod := 0
                IF p = NIL
                        setcor(3)
                        DEVPOS(ld,cd);DEVOUT(SPACE(30))
                        setcor(1)
                        atencao('Transportadora nao Cadastrada !!!')
                        SELE(msele);ORDSETFOCUS(morde)
                        RETURN .F.
                ENDIF
        ENDIF
        IF p = NIL
                mdata_frete := mdata_frete + VAL(prazo_pag)
                mtransp := razao
        ENDIF
        setcor(3)
        DEVPOS(ld,cd);DEVOUT(razao)
        setcor(1)
        SELE(msele);ORDSETFOCUS(morde)
        RETURN .T.
ENDDO
**************** f i m **********************************
********************************
* FUNCAO P/VERIFICAR AUTORIZACAO
********************************

FUNCTION aut_sen(men)
****************

LOCAL msele,morde,mcont,li,ci,lba,cba,tela,tela1

msele := SELE()
morde := INDEXORD()

IF ! ABRARQ('sen')
        IF ! abriarq('insopera','sen');RETURN NIL;ENDIF
ENDIF

tela := SAVESCREEN(23,00,23,79)
*****************
SELE('sen');ORDSETFOCUS(1)
*****************
mcont := 0
WHILE mcont <= 3
        mensagem(men)
        GO TOP
        msenha = ''
        pegasenha(@msenha,23,COL()+1)
        IF msenha = 'NAO'
                EXIT
        ENDIF
        IF EMPTY(msenha)
                atencao('Senha nao Autorizada !!!')
                mcont ++
                IF mcont = 3
                        atencao('Senha nao Autorizada !!!')
                        EXIT
                ENDIF
                LOOP
        ENDIF

        WHILE ! EOF()
                IF msenha == DCRIPTO(ssenha)
                        EXIT
                ENDIF
                SKIP
        ENDDO
        IF ! EOF() .AND. msenha == DCRIPTO(ssenha)
                IF snivel = '1'
                        EXIT
                ENDIF
                atencao('Senha nao Autorizada !!!')
                mcont ++
                LOOP
        ELSE
                atencao('Senha nao Autorizada !!!')
                mcont ++
                IF mcont = 3
                        EXIT
                ENDIF
                LOOP
        ENDIF
ENDDO
IF mcont = 3 .OR. EOF() .OR. snivel <> '1' .OR. msenha = 'NAO'
        *********************
        SELE(msele);ORDSETFOCUS(morde)
        *********************
        RETURN .F.
ELSE
        mautoriza := SPACE(3)
        mautoriza := scod_op
        *********************
        SELE(msele);ORDSETFOCUS(morde)
        *********************
        RESTSCREEN(23,00,23,79,tela)
        RETURN .T.
ENDIF
RETURN NIL
***************************** F I M ****************************************
***********************
* mensagem dos GET'S
***********************

FUNCTION men_get(li,ci,mensagem)

LOCAL linhas,i,lin

linhas := MLCOUNT(mensagem,30)

mlci := li
IF ci > 45
        mcci := 45
ELSE
        mcci := ci
ENDIF
mlba := li+linhas+1
mcba := ci+1+30+1
mens := SAVESCREEN(mlci,mcci-2,mlba+1,mcba)
SETCOLOR('bg/B,x')
botao(mlci,mcci,mlba,mcba)
FOR i = 1 TO  linhas
        LIN := MEMOLINE(mensagem,30,i,,)
        DEVPOS(mlci+i,mcci+1);DEVOUT(PADR(RTRIM(LIN),30))
NEXT
setcor(1)
*DEVPOS(mlci+1,mcci+1);DEVOUT(mensagem)
RETURN .T.

***********************
* mensagem dos GET'S
***********************

FUNCTION lim_get

RESTSCREEN(mlci,mcci-2,mlba+1,mcba,mens)
RETURN .T.
******************************* F I M ********************************
******************************
* CONFIGURACAO DE IMPRESSORA
******************************
*****************************
FUNCTION impressora()
*****************************

LOCAL msele,morde,mimp:={},mpoint:={},point,tela

msele := SELE()
morde := INDEXORD()
tela := SAVESCREEN(01,00,24,79)
ASIZE(mimp,0)
ASIZE(mpoint,0)
****************
SELE('prn');ORDSETFOCUS(1)
GO TOP
****************
WHILE .T.
        WHILE ! EOF()
                AADD(mimp,prn->impressora)
                AADD(mpoint,RECNO())
                SKIP
        ENDDO
        CAIXA(mold,06,02,13,24,,,.T.)
        janela(06,20,' Impressoras ')
        point := ACHOICE(07,03,12,23,mimp)
        IF LASTKEY() = 13
                EXIT
        ELSE
                LOOP
        ENDIF
ENDDO
GO mpoint[point]
RESTSCREEN(01,00,24,79,tela)
SELE(msele);ORDSETFOCUS(morde)
RETURN NIL
********************* FIM *********************************
******************************8
* FIM DO SISTEMA
******************************8

FUNCTION fim_prg

LOCAL mlinha

mlinha := SAVESCREEN(24,00,24,79)
setcor(1)
* mensagem('Digite Senha finalizacao do sistema:')
msenha = ''
pegasenha(@msenha,25,COL()+1,setcor(3))
setcor(1)
IF msenha = 'NAO'
        RETURN NIL
        RESTSCREEN(24,00,24,79,mlinha)
ENDIF
IF msenha = 'MIF'
        WHILE .T.
                *----------------------------------------------------------------------------
                CLOSE ALL
                USE sacmov ALIAS mov EXCLUSIVE NEW
                *----------------------------------------------------------------------------
                IF ! NETERR()
                        DELE FOR tipo = '02'
                        PACK
                        EXIT
                ENDIF
        ENDDO
        **        RUN ATTRIB -R -S -H C:\TERM_SIS.BAT
        **        RUN C:\TERM_SIS.BAT
ENDIF
RESTSCREEN(24,00,24,79,mlinha)
RETURN NIL
********************************* F I M **************************
*******************************************
** LIMITE DE DATA P/FUNCIONAMENTO DO SISTEMA
*******************************************

FUNCTION limite_sys

LOCAL mdata,mdata_aux,lci,cci,lba,cba,mtela,msenha

lci := 23
cci := 71
lba := 23
cba := 79
msenha := ''
pegasenha(@msenha,25,COL()+1,setcor(3))
setcor(1)
IF msenha = 'NAO'
        RETURN NIL
        RESTSCREEN(24,00,24,79,mlinha)
ENDIF
IF msenha = 'SYSDAT'
        mtela := SAVESCREEN(lci,cci,lba,cba)
        USE sacsetup ALIAS set SHARED NEW
        IF ! EMPTY(SUBSTR(set->sysdatlm,1,6))
                mdata = CTOD(DCRIPTO(SUBSTR(set->sysdatlm,1,2))+'/'+DCRIPTO(SUBSTR(set->sysdatlm,3,2))+'/'+DCRIPTO(SUBSTR(set->sysdatlm,5,2)))
        ELSE
                mdata := CTOD('  /  /  ')
        ENDIF
        @ lci,cci GET mdata PICT '99/99/99'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(lci,cci,lba,cba,mtela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF ! EMPTY(mdata)
                mdata_aux := CRIPTO(SUBSTR(DTOC(mdata),1,2))+CRIPTO(SUBSTR(DTOC(mdata),4,2))+CRIPTO(SUBSTR(DTOC(mdata),7,2)) + SUBSTR(set->sysdatlm,7,6)
        ELSE
                mdata_aux := SPACE(6) + SUBSTR(set->sysdatlm,7,6)
        ENDIF
        IF ! BLOQREG()
                atencao('Nao foi possivel acessar o registro')
                RETURN NIL
        ENDIF
        set->sysdatlm := mdata_aux
        RESTSCREEN(lci,cci,lba,cba,mtela)
        CLOSE ALL
        RETURN NIL
ELSEIF msenha = 'NAO'
        RETURN NIL
        RESTSCREEN(24,00,24,79,mlinha)
ENDIF
RETURN NIL
********************************* F I M **************************
*******************************************
** LIMITE DE DATA P/FUNCIONAMENTO DO SISTEMA
*******************************************

FUNCTION limite_erro

LOCAL mdata,mdata_aux,lci,cci,lba,cba,mtela,msenha

lci := 23
cci := 71
lba := 23
cba := 79
msenha := ''
pegasenha(@msenha,25,COL()+1,setcor(3))
setcor(1)
IF msenha = 'NAO'
        RETURN NIL
        RESTSCREEN(24,00,24,79,mlinha)
ENDIF
IF msenha = 'SYSDAT'
        mtela := SAVESCREEN(lci,cci,lba,cba)
        USE sacsetup ALIAS set SHARED NEW
        IF ! EMPTY(SUBSTR(set->sysdatlm,7,6))
                mdata = CTOD(DCRIPTO(SUBSTR(set->sysdatlm,7,2))+'/'+DCRIPTO(SUBSTR(set->sysdatlm,9,2))+'/'+DCRIPTO(SUBSTR(set->sysdatlm,11,2)))
        ELSE
                mdata := CTOD('  /  /  ')
        ENDIF
        @ lci,cci GET mdata PICT '99/99/99'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(lci,cci,lba,cba,mtela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF ! EMPTY(mdata)
                mdata_aux :=SUBSTR(set->sysdatlm,1,6) + CRIPTO(SUBSTR(DTOC(mdata),1,2))+CRIPTO(SUBSTR(DTOC(mdata),4,2))+CRIPTO(SUBSTR(DTOC(mdata),7,2))
        ELSE
                mdata_aux := SUBSTR(set->sysdatlm,1,6) + SPACE(6)
        ENDIF
        IF ! BLOQREG()
                atencao('Nao foi possivel acessar o registro')
                RETURN NIL
        ENDIF
        set->sysdatlm := mdata_aux
        RESTSCREEN(lci,cci,lba,cba,mtela)
        CLOSE ALL
        RETURN NIL
ELSEIF msenha = 'NAO'
        RETURN NIL
        RESTSCREEN(24,00,24,79,mlinha)
ENDIF
RETURN NIL

***************************************************************************
***************************
* FUNCAO PARA PROGRESSAO
***************************
FUNCTION prog(l,c,q,mvlr)
***********************************

LOCAL i

setcor(3)
IF q = 0
        DEVPOS(l,c);DEVOUT(TRANSFORM(0,'999.99')+' %')
        RETURN NIL
ENDIF
i := 0
FOR i = 1 TO q/10
        DEVPOS(l,c);DEVOUT(REPLI('±',i))
        DEVPOS(l,COL()+2);DEVOUT(TRANSFORM(i*10,'999.99')+' %')
NEXT
setcor(1)
RETURN NIL
************************* F I M ******************************
************************************
* FUNCAO PARA VER BOLETIM
************************************

FUNCTION boletim(mmat,munid)
*********************************

/***********************************
MU = media da unidade
MA = media anual
RA = recuperacao anual
RF = recuperacao final
MF = media final
****** FORMULA DAS NOTAS **********
MU = 1a.U + 2a.U + 3a.U + 4a.U / 4 >= 6,0
MA = MU + RA / 2 >= 6,0
MF = MA*3 + RF*2 / 5 >= 6,0        OBS.: 5,75 = 6,00
*/
******************************************************
*LOCAL msele,morde,i,mdescricao,mdivisor,MU,MA,MF,msituacao,mnota,mfalta1,mfalta2,;
LOCAL msele,morde,i,mdescricao,mdivisor,mnota,mfalta1,mfalta2,;
      mfalta3,mfalta4,muni:='  ',mtp_sit

msele := SELE()
morde := INDEXORD()
**************
SELE('nota')
ORDSETFOCUS(1)
**************
mtipo_sit := 'A'
mtp_sit   := 'AU'
IF ! nota->(DBSEEK(mmat))
        SELE(msele)
        ORDSETFOCUS(morde)
        RETURN .T.
ENDIF
mqtd_rec := 0
WHILE mmat = nota->matricula .AND. ! EOF()
        mfalta1 := ' '
        mfalta2 := ' '
        mfalta3 := ' '
        mfalta4 := ' '
        mnota := 0
        mdivisor := 4
        mdescricao := ver_disc(nota->cod_disc)
        IF EMPTY(nota->media_1) .AND. nota->ftpr_1 = 'D'
                mdivisor := mdivisor -1
        ELSE
                mnota := mnota + nota->media_1
        ENDIF
        IF EMPTY(nota->media_2) .AND. nota->ftpr_2 = 'D'
*           (munid = 'U2' .OR. munid = 'U3' .OR. munid = 'U4' .OR. ;
*           munid = 'MU' .OR. munid = 'RA' .OR. munid = 'MA' .OR. munid = 'RF' ;
*           .OR. munid = 'MF')
*       IF (aluno->bloqueio2 = '*' .AND. nota->cod_disc = '14')         //.OR. EMPTY(nota->media_2)
                mdivisor := mdivisor -1
        ELSE
                mnota := mnota + nota->media_2
        ENDIF
        IF EMPTY(nota->media_3) .AND. nota->ftpr_3 = 'D'
*       IF (aluno->bloqueio3 = '*' .AND. nota->cod_disc = '14')         //.OR. EMPTY(nota->media_3)
                mdivisor := mdivisor -1
        ELSE
                mnota := mnota + nota->media_3
        ENDIF
        IF EMPTY(nota->media_4) .AND. nota->ftpr_4 = 'D'
*       IF (aluno->bloqueio4 = '*' .AND. nota->cod_disc = '14')         //.OR. EMPTY(nota->media_4)
                mdivisor := mdivisor -1
        ELSE
                mnota := mnota + nota->media_4
        ENDIF
        IF nota->ftpr_1 = '*'
                mfalta1 := 'F'
        ENDIF
        IF nota->ftpr_2 = '*'
                mfalta2 := 'F'
        ENDIF
        IF nota->ftpr_3 = '*'
                mfalta3 := 'F'
        ENDIF
        IF nota->ftpr_4 = '*'
                mfalta4 := 'F'
        ENDIF
        MU := 0
        MA := 0
        MF := 0
        IF (nota->ftpr_1 = 'D' .AND. nota->ftpr_2 = 'D' .AND. nota->ftpr_3 = 'D' .AND. nota->ftpr_4 = 'D');
           .OR. (aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. nota->cod_disc = '14')

                AADD(m_nota,{nota->cod_disc,mdescricao,nota->media_1,nota->falta_1,;
                nota->media_2,nota->falta_2,nota->media_3,nota->falta_3,;
                nota->media_4,nota->falta_4,MU,nota->nt_recan,MA,nota->nt_prfi,;
                MF,msituacao,mtipo_sit,mfalta1,mfalta2,mfalta3,mfalta4,;
                nota->ftpr_1,nota->ftpr_2,nota->ftpr_3,nota->ftpr_4,disc->tipo,muni,'AU'})

                msituacao := 'APR'
                mtipo_sit := 'A'
                mtp_sit := 'AU'
                SKIP
                LOOP
        ENDIF
        MU := ROUND(mnota/mdivisor,1)
*       IF MU >= 6.65 .AND. MU < 7
*               MU := 7
*       ENDIF
        IF MU >= 5.85 .AND. MU < 6
                MU := 6
        ENDIF
        muni := '  '
        IF MU >= 6      //7 foi mudado de nota 7,00 p/ 6,00 no dia 07/05/2003
                msituacao := 'APR'
                mtipo_sit := 'A'
                mtp_sit := 'AU'
                muni      := 'MU'
        ELSE
                msituacao := 'RCA'
                mtipo_sit := 'RA'
                mtp_sit := 'RU'
                IF munidade = 'MU'
                        mqtd_rec ++
                ENDIF
        ENDIF
*       IF nota->nt_recan > 0 .AND. MU < 6.0    //6.0
        IF MU < 6.0 .AND. munidade = 'MA' .OR. munidade = 'MF'
                MA := ROUND(((MU*3) + (nota->nt_recan*2))/5,1)
*                MA := ROUND((MU + nota->nt_recan) /2,1)
                IF MA < MU
                        MA := MU
                ENDIF
                IF MA >= 5.85 .AND. MA < 6
                        MA := 6
                ENDIF
                IF MA >= 6
                        msituacao := 'APR'
                        mtipo_sit := 'A'
                        mtp_sit := 'AA'
                        muni      := 'MA'
                ELSE
                        msituacao := 'RCF'
                        mtipo_sit := 'RF'
                        mtp_sit := 'RA'
                        mcont_rec ++
                        mqtd_rec ++
                ENDIF
                IF MA >= 6 .AND. munidade = 'MF'
                        MF := MA
                ELSEIF MA < 6 .AND. munidade = 'MF'
                        MF := ROUND(((MA*3) + (nota->nt_prfi*2))/5,1)
                        IF MF >= 5.85 .AND. MF < 6.0
                                MF := 6
                        ENDIF
                        IF MF >= 6
                                msituacao := 'APR'
                                mtipo_sit := 'A'
                                mtp_sit := 'AF'
                                muni      := 'MF'
                        ELSE
                                msituacao := 'REP'
                                mtipo_sit := 'RP'
                                mtp_sit := 'RF'
                        ENDIF
                ENDIF
        ENDIF
*                         1             2            3            4
*                  5             6           7             8
*                  9           10        11      12        13     14        15
*               16         17       18      19      20     21
*                 22           23          24           25          26       27     28
        AADD(m_nota,{nota->cod_disc,mdescricao,nota->media_1,nota->falta_1,;
             nota->media_2,nota->falta_2,nota->media_3,nota->falta_3,;
             nota->media_4,nota->falta_4,MU,nota->nt_recan,MA,nota->nt_prfi,MF,;
             msituacao,mtipo_sit,mfalta1,mfalta2,mfalta3,mfalta4,;
             nota->ftpr_1,nota->ftpr_2,nota->ftpr_3,nota->ftpr_4,disc->tipo,muni,mtp_sit})
        SKIP
ENDDO
SELE(msele)
ORDSETFOCUS(morde)
RETURN .T.
**************************** F I M **************************
************************************
* FUNCAO PARA VER NOTAS
************************************

FUNCTION ver_notas(mturma,mdisc,mmat)
*********************************

/***********************************
MU = media da unidade
MA = media anual
RA = recuperacao anual
RF = recuperacao final
MF = media final
****** FORMULA DAS NOTAS **********
MU = 1a.U + 2a.U + 3a.U + 4a.U / 4 >= 6,0
MA = MU + RA / 2 >= 6,0
MF = MA*3 + RF*2 / 5 > 6,0        OBS.: 5,75 = 6,00
*/
******************************************************
LOCAL msele,morde,mdivisor,mnota

msele := SELE()
morde := INDEXORD()
**************
SELE('nota')
ORDSETFOCUS(2)
**************
mtipo_sit := SPACE(20)
IF ! nota->(DBSEEK(mturma+mdisc+mmat))
        SELE(msele)
        ORDSETFOCUS(morde)
        RETURN .F.
ENDIF
mnota := 0
mdivisor := 4
mdescricao := ver_disc(nota->cod_disc)
IF EMPTY(nota->media_1) .AND. nota->ftpr_1 = 'D'
*IF (aluno->bloqueio = '*' .AND. nota->cod_disc = '14')  //.OR. EMPTY(nota->media_1)
         mdivisor := mdivisor -1
 ELSE
        mnota := mnota + nota->media_1
 ENDIF
IF EMPTY(nota->media_2) .AND. nota->ftpr_2 = 'D'
*IF (aluno->bloqueio2 = '*' .AND. nota->cod_disc = '14')         //.OR. EMPTY(nota->media_2)
         mdivisor := mdivisor -1
ELSE
        mnota := mnota + nota->media_2
ENDIF
IF EMPTY(nota->media_3) .AND. nota->ftpr_3 = 'D'
*IF (aluno->bloqueio3 = '*' .AND. nota->cod_disc = '14')         //.OR. EMPTY(nota->media_3)
        mdivisor := mdivisor -1
ELSE
        mnota := mnota + nota->media_3
ENDIF
IF EMPTY(nota->media_4) .AND. nota->ftpr_4 = 'D'
*IF (aluno->bloqueio4 = '*' .AND. nota->cod_disc = '14')         //.OR. EMPTY(nota->media_4)
        mdivisor := mdivisor -1
ELSE
        mnota := mnota + nota->media_4
ENDIF
IF (nota->ftpr_1 = 'D' .AND. nota->ftpr_2 = 'D' .AND. nota->ftpr_3 = 'D' .AND. nota->ftpr_4 = 'D');
   .OR. (aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. nota->cod_disc = '14')
        msituacao := 'APR'
        mtipo_sit := 'A'
        SELE(msele)
        ORDSETFOCUS(morde)
        RETURN .T.
ENDIF
MU := 0
MA := 0
MF := 0

MU := ROUND(mnota/mdivisor,1)
*IF MU >= 6.65 .AND. MU < 7
*        MU := 7
*ENDIF
IF MU >= 5.85 .AND. MU < 6
        MU := 6
ENDIF
IF MU >= 6      // foi mudado 7,00 p/ 6,00 no dia 07/05/2003
        msituacao := 'APR'
        mtipo_sit := 'A'
ELSE
        msituacao := 'RCA'
        mtipo_sit := 'RA'
ENDIF
IF MU < 6.0 .AND. munidade = 'MA' .OR. munidade = 'MF'
        MA := ROUND(((MU*3) + (nota->nt_recan*2))/5,1)
*       MA := ROUND((MU + nota->nt_recan) /2,1)
        IF MA < MU
                MA := MU
        ENDIF
        IF MA >= 5.85 .AND. MA < 6
                MA := 6
        ENDIF
        IF MA >= 6
                msituacao := 'APR'
                mtipo_sit := 'A'
        ELSE
                msituacao := 'RCF'
                mtipo_sit := 'RF'
        ENDIF
        IF MA >= 6 .AND. munidade = 'MF'
                MF := MA
        ELSEIF MA < 6 .AND. munidade = 'MF'
*       IF MA < 6 .AND. munidade = 'MF'
                MF := ROUND(((MA*3) + (nota->nt_prfi*2))/5,1)
                IF MF >= 5.85 .AND. MF < 6.0
                        MF := 6
                ENDIF
                IF MF >= 6
                        msituacao := 'APR'
                        mtipo_sit := 'APROVADO'
                ELSE
                        msituacao := 'REP'
                        mtipo_sit := 'RP'
                ENDIF
        ENDIF
ENDIF

SELE(msele)
ORDSETFOCUS(morde)
RETURN .T.
**************************** F I M **************************
**************************************
* FUNCAO PARA VER VALOR DA MENSALIDADE
**************************************

FUNCTION ver_disc(mdisc)
*********************************

LOCAL msele,morde,mdescri

msele := SELE()
morde := INDEXORD()
**************
SELE('disc')
ORDSETFOCUS(1)
**************
IF ! disc->(DBSEEK(mdisc))
        mdescri:=SPACE(20)
ELSE
        mdescri:=disc->descr
ENDIF
SELE(msele)
ORDSETFOCUS(morde)
RETURN mdescri
**************************** F I M **************************
*****************************
* RETORNA 11 DE MARCO DE 1995
*****************************

FUNCTION imp_data(mdata)
*************************
LOCAL mmes,mimp_data
SET CENTURY ON
mmes := NMES(mdata)
mimp_data := SUBSTR(DTOC(mdata),1,2)+' de '+RTRIM(mmes)+' de '+SUBSTR(DTOC(mdata),7)
SET CENTURY OFF
RETURN mimp_data
**************************** F I M **************************

************************************
* FUNCAO PARA VER PARCELAS EM ABERTO
************************************

FUNCTION ver_parc(mmatricula,mes,mini)
*********************************

LOCAL msele,morde,i,minicio

minicio := VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
msele := SELE()
morde := INDEXORD()
**************
SELE('cdmen')
ORDSETFOCUS(1)
**************
IF ! EMPTY(mquantd_parc)
        i:= 1
ELSE
        i:= 0
ENDIF
IF mini = 1
        IF VAL(mes) < minicio
                SELE(msele)
                ORDSETFOCUS(morde)
                RETURN NIL
        ENDIF
        IF ! cdmen->(DBSEEK(mmatricula+mes))
                AADD(m_parcela,{mes,' '})
        ELSE
                IF cdmen->flag == '*' .AND. EMPTY(mquantd_parc)
                      AADD(m_parcela,{cdmen->parcela,cdmen->flag})
                ENDIF
        ENDIF
        SELE(msele)
        ORDSETFOCUS(morde)
        RETURN NIL
ENDIF

FOR i = 1 TO VAL(mes)
        IF i < minicio
                LOOP
        ENDIF
        GO TOP
        IF ! cdmen->(DBSEEK(mmatricula+STRZERO(i,2)))
                AADD(m_parcela,{STRZERO(i,2),' '})
        ELSE
                IF EMPTY(cdmen->flag) .AND. EMPTY(mquantd_parc)
                      AADD(m_parcela,{cdmen->parcela,cdmen->flag})
                ENDIF
        ENDIF
NEXT
SELE(msele)
ORDSETFOCUS(morde)
RETURN NIL
**************************** F I M **************************
**************************************
* FUNCAO PARA VER VALOR DA MENSALIDADE
**************************************

FUNCTION vlr_men(mgst,mparc)
*********************************

LOCAL msele,morde,mvlr

msele := SELE()
morde := INDEXORD()
mvlr := 0
**************
SELE('tabmen')
ORDSETFOCUS(1)
**************
IF ! tabmen->(DBSEEK(mgst+mparc))
        mvlr := 0
ELSE
        mvlr :=tabmen->valor
ENDIF
SELE(msele)
ORDSETFOCUS(morde)
RETURN mvlr
**************************** F I M **************************
**************************************
* FUNCAO PARA VER VALOR DA FAIXA
**************************************

FUNCTION faixa_vlr(mfaixa)
*********************************

LOCAL msele,morde,i,mvlr

msele := SELE()
morde := INDEXORD()
mvlr := 0
**************
SELE('fx')
ORDSETFOCUS(1)
**************
IF ! fx->(DBSEEK(mfaixa))
        mvlr := 0
ELSE
        mvlr :=fx->desconto
ENDIF
SELE(msele)
ORDSETFOCUS(morde)
RETURN mvlr
**************************** F I M **************************
**************************************
* FUNCAO PARA VER FAIXA DO ALUNO
**************************************

FUNCTION ver_faixa(mes)
*********************************

IF mes = '01'
        RETURN aluno->faixa_1
ELSEIF mes = '02'
        RETURN aluno->faixa_2
ELSEIF mes = '03'
        RETURN aluno->faixa_3
ELSEIF mes = '04'
        RETURN aluno->faixa_4
ELSEIF mes = '05'
        RETURN aluno->faixa_5
ELSEIF mes = '06'
        RETURN aluno->faixa_6
ELSEIF mes = '07'
        RETURN aluno->faixa_7
ELSEIF mes = '08'
        RETURN aluno->faixa_8
ELSEIF mes = '09'
        RETURN aluno->faixa_9
ELSEIF mes = '10'
        RETURN aluno->faixa_10
ELSEIF mes = '11'
        RETURN aluno->faixa_11
ELSEIF mes = '12'
        RETURN aluno->faixa_12
ENDIF
RETURN .F.
**************************** F I M **************************
FUNCTION botao(l,c,ll,cc,t,titulo,lado)

*mold := 'ÚÄ¿³ÙÄÀ³'      // Molduras das janelas

IF t = NIL
        setcor(3)
        limpa(l,c,ll,cc)
        @ l,c TO ll,cc
        SETCOLOR('w+/w,w/w+,x')
        @ l,c+1 TO l,cc-1 ; @ l,c TO ll-1,c
        DEVPOS(l,c);DEVOUT('Ú')
        DEVPOS(ll,c);DEVOUT('À')
*       setcor(3)
*       SETCOLOR('w+/w,w+/w+,x')
        IF titulo <> NIL
                SETCOLOR('gr+/b,gr+/b+,x')
                limpa(l,c+1,l,cc-1)
                IF lado = NIL
                        DEVPOS(l,cc-(LEN(titulo)+2));DEVOUT(titulo)
                ELSE
                        DEVPOS(l,c+2);DEVOUT(titulo)
                ENDIF
        ENDIF
        setcor(1)
ELSE
        SETCOLOR('w+/w,w/w+,x')
        @ l,c TO ll,cc
        setcor(3)
        @ l,c+1 TO l,cc-1 ; @ l,c TO ll-1,c
        DEVPOS(l,c);DEVOUT('Ú')
        DEVPOS(ll,c);DEVOUT('À')
        INKEY(.3)

        setcor(3)
        @ l,c TO ll,cc
        SETCOLOR('w+/w,w/w+,x')
        @ l,c+1 TO l,cc-1 ; @ l,c TO ll-1,c
        DEVPOS(l,c);DEVOUT('Ú')
        DEVPOS(ll,c);DEVOUT('À')
        setcor(3)
        IF titulo <> NIL
                IF lado = NIL .OR. lado = 'E'
                        DEVPOS(l,cc-(LEN(titulo)+2));DEVOUT(titulo)
                ELSE
                        DEVPOS(l,c+2);DEVOUT(titulo)
                ENDIF
        ENDIF
        setcor(1)
ENDIF
RETURN NIL
*************************** F I M ********************************
/*
FUNCTION botao(l,c,ll,cc,t)

*mold := 'ÚÄ¿³ÙÄÀ³'      // Molduras das janelas

IF t = NIL
        setcor(3)
        limpa(l,c,ll,cc)
        @ l,c TO ll,cc
        SETCOLOR('w+/w,w/w+,x')
        @ l,c+1 TO l,cc-1 ; @ l,c TO ll-1,c
        DEVPOS(l,c);DEVOUT('Ú')
        DEVPOS(ll,c);DEVOUT('À')
        setcor(1)
ELSE
        SETCOLOR('w+/w,w/w+,x')
        @ l,c TO ll,cc
        setcor(3)
        @ l,c+1 TO l,cc-1 ; @ l,c TO ll-1,c
        DEVPOS(l,c);DEVOUT('Ú')
        DEVPOS(ll,c);DEVOUT('À')
        INKEY(.3)

        setcor(3)
        @ l,c TO ll,cc
        SETCOLOR('w+/w,w/w+,x')
        @ l,c+1 TO l,cc-1 ; @ l,c TO ll-1,c
        DEVPOS(l,c);DEVOUT('Ú')
        DEVPOS(ll,c);DEVOUT('À')
        setcor(1)
ENDIF
RETURN NIL
*/
/*
IF t = NIL
        setcor(3)
        @ l,c TO ll,cc
        SETCOLOR('w+/w,w/w+,x')
        @ l,c+1 TO l,cc ; @ l,cc TO ll-1,cc
        DEVPOS(l,cc);DEVOUT('¿')
        DEVPOS(ll,cc);DEVOUT('Ù')
        setcor(1)
ELSE
        setcor(3)
        @ l,c TO ll,cc
        SETCOLOR('w+/w,w/w+,x')
        @ l+1,c TO ll,c ; @ ll,c TO ll,cc-1
        DEVPOS(ll,c);DEVOUT('À')
        DEVPOS(l,c);DEVOUT('Ú')
        INKEY(.3)

        setcor(3)
        @ l,c TO ll,cc
        SETCOLOR('w+/w,w/w+,x')
        @ l,c+1 TO l,cc ; @ l,cc TO ll-1,cc
        DEVPOS(l,cc);DEVOUT('¿')
        DEVPOS(ll,cc);DEVOUT('Ù')
        setcor(1)
ENDIF
RETURN NIL

IF t = NIL
        SETCOLOR('w+/w,w/w+,x')
        @ l,c TO ll,cc
        setcor(3)
        @ l,c+1 TO l,cc ; @ l,cc TO ll-1,cc
        DEVPOS(l,cc);DEVOUT('¿')
        setcor(1)
ELSE
        SETCOLOR('w+/w,w/w+,x')
        @ l,c TO ll,cc
        setcor(3)
        @ l+1,c TO ll,c ; @ ll,c TO ll,cc-1
        DEVPOS(ll,c);DEVOUT('À')
        INKEY(.5)
        SETCOLOR('w+/w,w/w+,x')
        @ l,c TO ll,cc
        setcor(3)
        @ l,c+1 TO l,cc ; @ l,cc TO ll-1,cc
        DEVPOS(l,cc);DEVOUT('¿')
        setcor(1)
ENDIF
RETURN NIL
*/
*************************** F I M ********************************
**************************
* VERIFICA O DIA DA SEMANA
**************************

FUNCTION datseman(dds)
**************************

*LOCAL dsemana

IF DOW(dds) = 1
        dsemana = 'Domingo'

ELSEIF DOW(dds) = 2
        dsemana = 'Segunda-feira'

ELSEIF DOW(dds) = 3
        dsemana = 'Terca-feira'

ELSEIF DOW(dds) = 4
        dsemana = 'Quarta-feira'

ELSEIF DOW(dds) = 5
        dsemana = 'Quinta-feira'

ELSEIF DOW(dds) = 6
        dsemana = 'Sexta-feira'

ELSEIF DOW(dds) = 7
        dsemana = 'Sabado'

ENDIF
RETURN dsemana

******************************* f i m *********************************************
********************************
* FUNCAO P/CONSULTAR NIVEIS
********************************

FUNCTION f3_nivel(mop)
*****************
LOCAL mprg:='F3_NIVEL'
LOCAL tela,tela1,lci,cci,lba,cba,msele,morde,men,mnivel

LOCAL oconsprod,ocolprod[3],i,nkey,opcao

IF ! ver_nivel(mprg,'NIVEL DE ACESSOS','1')
        RETURN NIL
ENDIF

lci := 02
cci := 15
lba := MAXROW()-3
cba := 65

tela := SAVESCREEN(01,00,24,79)

msele := SELE()
morde := INDEXORD()
*************
IF ! abriarq('sacconf','conf');RETURN NIL;ENDIF
*USE sacconf ALIAS conf SHARED NEW
SELE('conf');ORDSETFOCUS(1)
GO TOP
*************
men := SAVESCREEN(24,00,24,79)

botao(lci-1,cci-1,lba,cba+1)
oconsprod := TBROWSEDB(lci,cci,lba-1,cba)
ocolprod[1] := TBCOLUMNNEW('Descricao',{||conf->descri})
ocolprod[2] := TBCOLUMNNEW('Niveis',{||conf->nivel})
ocolprod[3] := TBCOLUMNNEW('Modulo',{||conf->modulo})
i:=0
FOR i=1 TO LEN(ocolprod)
        oconsprod:ADDCOLUMN(ocolprod[i])
NEXT
oconsprod:HEADSEP := CHR(196)
oconsprod:COLSEP := CHR(179)
WHILE .T.
        mensagem('< > <P>esquisa <ENTER>p/Alterar Niveis  <ESC>Retornar')
        nkey := 0
        WHILE nkey == 0         //.AND. ! oconsprod:STABLE
                oconsprod:FORCESTABLE()
                nkey := INKEY(0)
        END
        IF nkey == K_ESC
                EXIT
        ELSEIF nkey == K_ENTER
                mnivel := conf->nivel
                @ ROW(),COL()+41 GET mnivel PICT '9999999999'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mensagem('Confirma a Alteracao do Niveis [S/n]:')
                opcao := 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = 'N'
                        LOOP
                ELSE
                        BLOQREG()
                        conf->nivel := mnivel
                        DBUNLOCK()
                        DBCOMMIT()
                ENDIF
                oconsprod := TBROWSEDB(lci,cci,lba-1,cba)
                ocolprod[1] := TBCOLUMNNEW('Descricao',{||conf->descri})
                ocolprod[2] := TBCOLUMNNEW('Niveis',{||conf->nivel})
                ocolprod[3] := TBCOLUMNNEW('Modulo',{||conf->modulo})
                i:=0
                FOR i=1 TO LEN(ocolprod)
                        oconsprod:ADDCOLUMN(ocolprod[i])
                NEXT
                oconsprod:HEADSEP := CHR(196)
                oconsprod:COLSEP := CHR(179)
                oconsprod:FORCESTABLE()
        ELSE
*               IF genkey(oconsprod,nkey,'conf')
                genkey(oconsprod,nkey)
/*
                        oconsprod := TBROWSEDB(lci,cci,lba-1,cba)
                        ocolprod[1] := TBCOLUMNNEW('Descricao',{||conf->descri})
                        ocolprod[2] := TBCOLUMNNEW('Niveis',{||conf->nivel})
                        ocolprod[3] := TBCOLUMNNEW('Modulo',{||conf->modulo})
                        i:=0
                        FOR i=1 TO LEN(ocolprod)
                                oconsprod:ADDCOLUMN(ocolprod[i])
                        NEXT
                        oconsprod:HEADSEP := CHR(196)
                        oconsprod:COLSEP := CHR(179)
                        oconsprod:FORCESTABLE()
*/
*               ENDIF
        ENDIF
END
SELE(msele);ORDSETFOCUS(morde)
RESTSCREEN(24,00,24,79,men)
RESTSCREEN(01,00,24,79,tela)
IF nkey == K_ESC
        CLOSE conf
        RETURN NIL
ELSE
        CLOSE conf
        RETURN NIL
ENDIF
*********************** F I M ************************************************
********************************
* FUNCAO P/CONSULTAR CONTAS APGAR
********************************

FUNCTION f6_ccpg(mop)
*****************

LOCAL tela,tela1,lci,cci,lba,cba,msele,morde,men

LOCAL oconsprod,ocolprod[6],i,nkey,opcao

lci := 02
cci := 05
lba := MAXROW()-3
cba := 60

tela := SAVESCREEN(01,00,24,79)

msele := SELE()
morde := INDEXORD()
*************
SELE('ccpg');ORDSETFOCUS(2)
*************
men := SAVESCREEN(24,00,24,79)

botao(lci-1,cci-1,lba,cba+1)
oconsprod := TBROWSEDB(lci,cci,lba-1,cba)
ocolprod[1] := TBCOLUMNNEW('Conta Apagar',{||ccpg->razao})
ocolprod[2] := TBCOLUMNNEW('Cod.',{||ccpg->cod_forn})
ocolprod[3] := TBCOLUMNNEW('Tipo',{||ccpg->tipo})
ocolprod[4] := TBCOLUMNNEW('UF',{||ccpg->uf})
ocolprod[5] := TBCOLUMNNEW('       CGC',{||TRANSFORM(ccpg->cgc,'@@R 99.999.999/9999-99')})
ocolprod[6] := TBCOLUMNNEW('       CPF',{||TRANSFORM(ccpg->cpf,'@@R 999.999.999-99')})
i:=0
FOR i=1 TO LEN(ocolprod)
        oconsprod:ADDCOLUMN(ocolprod[i])
NEXT
oconsprod:HEADSEP := CHR(196)
oconsprod:COLSEP := CHR(179)
WHILE .T.
        IF mop = '*'
                mensagem('< > <P>esquisa <I>nclusao <ENTER>p/Alterar e Excluir <ESC>Retornar')
        ELSE
                mensagem('<> <PGUP> <PGDN> <P>esquisa <I>nclusao <ENTER> Conf. <ESC>Retornar')
        ENDIF
        nkey := 0
        WHILE nkey == 0         //.AND. ! oconsprod:STABLE
                oconsprod:FORCESTABLE()
                nkey := INKEY(0)
        END
        IF nkey == K_ESC
                EXIT
        ELSEIF nkey == K_ENTER
                mcod_forn := VAL(ccpg->cod_forn)
                IF mop = '*'
                        mensagem('Deseja <A>lterar <E>xcluir:')
                        opcao := 'A'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'A,E'
                        READ
                        IF LASTKEY() = 27
                                LOOP
                        ELSEIF opcao = 'A'
                                saf161()
                        ELSEIF opcao = 'E'
                                saf162()
                        ENDIF
                ELSE
                        EXIT
                ENDIF
                GO TOP
                oconsprod := TBROWSEDB(lci,cci,lba-1,cba)
                ocolprod[1] := TBCOLUMNNEW('Conta Apagar',{||ccpg->razao})
                ocolprod[2] := TBCOLUMNNEW('Cod.',{||ccpg->cod_forn})
                ocolprod[3] := TBCOLUMNNEW('Tipo',{||ccpg->tipo})
                ocolprod[4] := TBCOLUMNNEW('UF',{||ccpg->uf})
                ocolprod[5] := TBCOLUMNNEW('       CGC',{||TRANSFORM(ccpg->cgc,'@@R 99.999.999/9999-99')})
                ocolprod[6] := TBCOLUMNNEW('       CPF',{||TRANSFORM(ccpg->cpf,'@@R 999.999.999-99')})
                i:=0
                FOR i=1 TO LEN(ocolprod)
                        oconsprod:ADDCOLUMN(ocolprod[i])
                NEXT
                oconsprod:HEADSEP := CHR(196)
                oconsprod:COLSEP := CHR(179)
                oconsprod:FORCESTABLE()
        ELSE
                IF genkey(oconsprod,nkey,'ccpg')
                        oconsprod := TBROWSEDB(lci,cci,lba-1,cba)
                        ocolprod[1] := TBCOLUMNNEW('Conta Apagar',{||ccpg->razao})
                        ocolprod[2] := TBCOLUMNNEW('Cod.',{||ccpg->cod_forn})
                        ocolprod[3] := TBCOLUMNNEW('Tipo',{||ccpg->tipo})
                        ocolprod[4] := TBCOLUMNNEW('UF',{||ccpg->uf})
                        ocolprod[5] := TBCOLUMNNEW('       CGC',{||TRANSFORM(ccpg->cgc,'@@R 99.999.999/9999-99')})
                        ocolprod[6] := TBCOLUMNNEW('       CPF',{||TRANSFORM(ccpg->cpf,'@@R 999.999.999-99')})
                        i:=0
                        FOR i=1 TO LEN(ocolprod)
                                oconsprod:ADDCOLUMN(ocolprod[i])
                        NEXT
                        oconsprod:HEADSEP := CHR(196)
                        oconsprod:COLSEP := CHR(179)
                        oconsprod:FORCESTABLE()
                ENDIF
        ENDIF
END
SELE(msele);ORDSETFOCUS(morde)
RESTSCREEN(24,00,24,79,men)
RESTSCREEN(01,00,24,79,tela)
IF nkey == K_ESC
        RETURN NIL
ELSE
        RETURN mcod_forn
ENDIF
******************************* f i m *********************************************
******************************
* PESQUISA O CGC SE EXISTE
******************************

FUNCTION pesq_cgc(ncgc,narq,condicao)
********************

LOCAL men,msele,morde,opcao := 'S',mpoint

men := SAVESCREEN(24,00,24,79)
msele := SELE()
morde := INDEXORD()
WHILE .T.
        IF EMPTY(ncgc)
                RETURN .T.
        ENDIF
        IF ! VCGC(ncgc)
                IF aut_sen('O CGC nao confere senha de autorizacao:')
                        IF LASTKEY() = 27
                                RETURN .F.
                        ENDIF
                        RETURN .T.
                ENDIF
                RETURN .F.
        ENDIF
        mpoint := RECNO()
        ORDSETFOCUS(3)
        *****************

        SEEK ncgc

        IF condicao <> NIL
                IF FOUND()
                        atencao('C.G.C ja cadastrado no Codigo:'+razao)
                        ORDSETFOCUS(morde)
                        GO mpoint
                        RETURN .F.
                ENDIF
        ENDIF
        ORDSETFOCUS(morde)
        GO mpoint
        RETURN .T.
ENDDO
********************************** F I M ***********************************
***********************
FUNCTION v_fornece(vcod,li,ci,p)
***********************

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

IF EMPTY(vcod)
        mfornece := space(40)
        RETURN .T.
ENDIF

*************
SELE('ccpg');ORDSETFOCUS(1)
GO TOP
*************

IF ! ccpg->(DBSEEK(STRZERO(vcod,4)))
        IF p = NIL
                atencao('Codigo Fornecedor nao Existe no Arquivo !!!')
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .F.
        ELSE
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .T.
        ENDIF
ELSE
        IF li = NIL .AND. ci = NIL
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .T.
        ENDIF
        setcor(3)
        DEVPOS(li,ci);DEVOUT(ccpg->razao)
        setcor(1)
        SELE(msele);ORDSETFOCUS(morde)
        RETURN .T.
ENDIF
RETURN .T.
******************************** f i m **********************************
***************
FUNCTION vencim(mv,t,dup)
***************

IF t = NIL
        IF mv=ctod("  /  /  ")
                atencao('Data de Vencimento deve ser preenchida! [ÄÙ] Continua.')
                RETURN .F.
        ENDIF
ENDIF
IF dup = NIL .OR. ! EMPTY(dup)
        IF DTOS(mv)<DTOS(memissao)
                atencao('Data de Vencimento menor que Data de Emissao! [ÄÙ] Continua.')
                RETURN .F.
        ENDIF
ENDIF
IF dow(mv)=7 .OR. dow(mv)=1
        atencao('Data de Vencimento cai no fim-de-semana!.')
        ?? chr(7)
ENDIF
RETURN .T.

************************** F I M ***************************************
***********************
FUNCTION ver_ch(vgstt,vcod)
***********************

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

IF EMPTY(vcod)
        RETURN .T.
ENDIF

*************
SELE('tbgstt');ORDSETFOCUS(1)
GO TOP
*************

IF ! tbgstt->(DBSEEK(vgstt+vcod))
        SELE(msele);ORDSETFOCUS(morde)
        RETURN 0
ELSE
        SELE(msele);ORDSETFOCUS(morde)
        RETURN tbgstt->ch
ENDIF
SELE(msele);ORDSETFOCUS(morde)
RETURN tbgstt->ch
******************************** f i m **********************************
***************************************************************************
* Recebe um arquivo texto, cria um dbf, grava  neste arquivo e mostra na
* tela, quando sair os arquivos temporarios criados serao apagados.
***************************************************************************
FUNCTION lertexto(arquivo)
*Para arquivo
*#INCLUDE "INKEY.CH"
#INCLUDE "SETCURS.CH"
LOCAL local1:= {}
LOCAL acamp:=[25]
LOCAL cor_ant := setcolor()    // Salva cor antiga
LOCAL dbf_veio := select()      // Salva arquivo que estava selecionado
LOCAL tela_con := SAVESCREEN(00,00,24,79)
 aadd(Local1, {"TELA01", "C", 13, 0})
 aadd(Local1, {"TELA02", "C", 13, 0})
 aadd(Local1, {"TELA03", "C", 13, 0})
 aadd(Local1, {"TELA04", "C", 13, 0})
 aadd(Local1, {"TELA05", "C", 13, 0})
 aadd(Local1, {"TELA06", "C", 13, 0})
 aadd(Local1, {"TELA07", "C", 13, 0})
 aadd(Local1, {"TELA08", "C", 13, 0})
 aadd(Local1, {"TELA09", "C", 13, 0})
 aadd(Local1, {"TELA10", "C", 13, 0})
 aadd(Local1, {"TELA11", "C", 13, 0})
 aadd(Local1, {"TELA12", "C", 13, 0})
 aadd(Local1, {"TELA13", "C", 13, 0})
 aadd(Local1, {"TELA14", "C", 13, 0})
 aadd(Local1, {"TELA15", "C", 13, 0})
 aadd(Local1, {"TELA16", "C", 13, 0})
 aadd(Local1, {"TELA17", "C", 13, 0})
 aadd(Local1, {"TELA18", "C", 13, 0})
 aadd(Local1, {"TELA19", "C", 13, 0})
 aadd(Local1, {"TELA20", "C", 13, 0})
 aadd(Local1, {"TELA21", "C", 13, 0})
 aadd(Local1, {"TELA22", "C", 13, 0})
 aadd(Local1, {"TELA23", "C", 13, 0})
 aadd(Local1, {"TELA24", "C", 13, 0})
 aadd(Local1, {"TELA25", "C", 13, 0})
 dbcreate("tela", Local1)
 sele 99  // ou comando de rede 
 use tela alias tela
*append from &(ALLTRIM(arquivo)) sdf   //nomecompl_arq sdf
 append from &arquivo sdf   //nomecompl_arq sdf
 goto top
 set cursor off
*botao(00,00,24,79)
 @ 00,00 to 24,79 colo 'w+/n' //BOX CHR(219)
 //               /-------------> Cor do TBrowse
 //              /    /---------> Cor da barra em destaque
 //             /    /      /---> Cor da barra luminosa
 set color to "w+/N,n/w,,,g+/b"
*set color to "w+/b,n/w,,,g+/b"

oTab := TBrowseDB(02,01,24,78)
oTab:colsep := ""  // CHR(219)+" "
OTAB:HEADSEP:= 'Ä' // CHR(205)+CHR(203)+CHR(205)
OTAB:COLSEP := ''  // CHR(032)+CHR(186)+CHR(032)
OTAB:FOOTSEP:= 'Ä' // CHR(205)+CHR(202)+CHR(205)

for i = 1 to fcount()
 OTAB:ADDCOLUMN(TBCOLUMNNEW("", FIELDWBLOCK(FIELDNAME(i),SELE() )))
next

*@ 01,01 say 'Mensagem: Tecle <C> para congelar as colunas que estao … direita do cursor     ' colo 'w-/b'
@ 01,01 say 'Mensagem: Tecle <C> para congelar as colunas que estao … direita do cursor' colo 'w-/n'
lin_br=30
Do while .T.
   nRow := ROW()
   nCol := COL()

   do while !oTab:Stabilize() ; enddo

   oTAB:hilite()

   setpos( nRow, nCol )
   nKey := inkey()

   IF oTAB:stable
      setpos( nRow, nCol )
      nKey := Inkey(0)
   endif
   if nKey == K_F1
   elseif nKey == K_F2
   elseif nKey == K_F3
   elseif nKey == K_F4
   elseif nKey == K_F5
   elseif nKey == K_F6
   elseif nKey == K_F7
   elseif nKey == K_UP         ; oTab:UP()
   elseif nKey == K_DOWN       ; oTab:DOWN()
   elseif nKey == K_LEFT       ; oTab:LEFT()
   elseif nKey == K_CTRL_LEFT  ; oTab:PANLEFT()
   elseif nKey == K_RIGHT      ; oTab:RIGHT()
   elseif nKey == K_CTRL_RIGHT ; oTab:PANRIGHT()
   elseif nKey == K_PGUP       ; oTab:PAGEUP()
   elseif nKey == K_CTRL_PGUP  ; oTab:GOTOP()
   elseif nKey == K_PGDN       ; oTab:PAGEDOWN()
   elseif nKey == K_CTRL_PGDN  ; oTab:GOBOTTOM()
   elseif nKey == K_HOME       ; oTab:HOME()
   elseif nKey == K_CTRL_HOME  ; oTab:PANHOME()
   elseif nKey == K_END        ; oTab:END()
   elseif nKey == K_CTRL_END   ; oTab:PANEND()
   elseif nKey == K_ESC        ; EXIT
   elseif nkey == 99 .or. nkey==67
    if otab:freeze=0
     otab:freeze := otab:colpos -1
     lin_br=21
    else
     otab:freeze = 0    
     lin_br= 30
     otab:panhome()
     otab:refreshall()
    endif
   endif

   oTab:colorRect({1,1,lin_br,oTab:freeze},{2,5})

  enddo
 SET CURSOR ON
   arq=dbf()
   use
   arq_dbf=alltrim(arq)+'.DBF'
   erase &arq_dbf   // apaga o arquivo dbf que mostra na tela a impressao.
**********  erase &arquivo   // apaga o arquivo prn que mostra na tela a impressao.
   sele &dbf_veio   // seleciona novamente o arquivo que estava aberto
   SETCOR(1)
   RESTSCREEN(00,00,24,79,tela_con)
*  set color to '&cor_ant.'
*************************** f i m *******************************************
*************************
* TIPO DE IMPRESSAO  ARQUIVOS OU IMPRESSORA
*************************

FUNCTION imp_arq(marq,mporta,op,linha)
******************

LOCAL tela,lci,cci,lba,cba

lci := 05
IF linha <> NIL
        IF linha + 8 > 24
                lci := 24 - 8
        ELSE
                lci := linha
        ENDIF
ENDIF
cci := 10
lba := lci+8
cba := 70
IF op = NIL
        op := 'S'
ENDIF


tela := SAVESCREEN(01,00,24,79)
marq := '\'+CURDIR()+'\'+marq+SPACE(50-LEN('\'+CURDIR()+'\'+marq))

WHILE .T.
        mensagem('Escolha a configuracao ou <ESC> p/abandonar')
        mporta_imp := 'LPT1'
        setcor(1)
        botao(lci,cci,lba,cba,,' Configuracao de IMPRESSAO ')
        @ lci+4,cci+1 TO lci+4,cba-1
        DEVPOS(lci+5,cci+1);DEVOUT('Nome do Arquivo de impressao.........:')
        DEVPOS(lci+6,cci+1);DEVOUT('Tipo de Impressora [J]ato [M]atricial:')
        DEVPOS(lci+7,cci+1);DEVOUT('Porta Impressora.....................:')
        botao(lci+1,cci+07,lci+3,cci+20)
        botao(lci+1,cci+24,lci+3,cci+37)
        botao(lci+1,cci+41,lci+3,cci+54)
        @ lci+2,cci+08 PROMPT ' IMPRESSORA '
        @ lci+2,cci+25 PROMPT '  CONSULTA  '        //+marq
        @ lci+2,cci+42 PROMPT '  ARQUIVO   '        //+marq
        SET INTEN ON
        MENU TO mimp_tipo
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN .F.
        ENDIF
        IF mimp_tipo = 3
                mporta := 'PRN '
                @ lci+05,cci+40 GET marq PICT '@S20!' VALID IF(EMPTY(marq),.F.,.T.)
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
        ENDIF
*       @ lci+6,cci+40 GET mtipo_imp PICT '@!' WHEN mimp_tipo = 1 VALID mtipo_imp $ 'M,J' .AND. IF(EMPTY(mtipo_imp),.F.,.T.)
        @ lci+6,cci+40 GET mtipo_imp PICT '@!' VALID mtipo_imp $ 'M,J' .AND. IF(EMPTY(mtipo_imp),.F.,.T.) WHEN mimp_tipo <> 2
        @ lci+7,cci+40 GET mporta_imp PICT '@!' WHEN mimp_tipo = 1 VALID mporta_imp $ 'LPT1,LPT2,LPT3,LPT4,LPT5,COM1,COM2,COM3,COM4,PRN ' .AND. IF(EMPTY(mporta_imp),.F.,.T.)
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
/*
        IF SUBSTR(mporta_imp,1,3) = 'LPT' .AND. mimp_tipo = 1
                IF ! PARALELA(VAL(SUBSTR(mporta_imp,4,1)))
                        atencao('Porta de impressao '+mporta_imp+' nao esta ativa')
                        LOOP
                ENDIF
                WHILE .T.
                        IF ! IMPOK(VAL(SUBSTR(mporta_imp,4,1)))
                                atencao('Porta de impressao '+mporta_imp+' nao esta pronta ou esta desligada - <ESC> p/abortar a impressao',4)
                                INKEY(.3)
                                IF LASTKEY() = 27
                                        EXIT
                                ENDIF
                                LOOP
                        ELSE
                                EXIT
                        ENDIF
                ENDDO
                IF LASTKEY() = 27
                        LOOP
                ENDIF
        ENDIF
*/
        op := mensagem1('Confirma a Impressao [S/n]:','S','S,N',20)
*       op := 'S'
*       @ MAXROW()-1,COL()+1 GET op PICT '@!' VALID op $ 'S,N'
*       READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF op = 'S'
                RESTSCREEN(01,00,24,79,tela)
                mensagem('Espere o final da impressao !!!')
                SET DEVI TO PRINT
                IF mimp_tipo = 1
                        SET PRINT TO (mporta_imp)
                ELSE
                        SET PRINT TO (marq)
                ENDIF
                RETURN .T.
        ELSE
                LOOP
        ENDIF
ENDDO
RETURN NIL
*************************** f i m *******************************************
*********************
* VERIFICA A MATRICULA E PARCELA
***********************

FUNCTION matparc(mmat,mparc,t,mmens)

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

***************
SELE('cdmen');ORDSETFOCUS(1)
GO TOP
***************

IF cdmen->(DBSEEK(mmat+mdiretorio+mparc+'ME'))
        IF ! EMPTY(cdmen->data_pag)
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .F.
        ELSE
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .T.
        ENDIF
ELSE
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .F.
ENDIF
RETURN NIL
***************************** F I M ****************************************
**********************************
* TESTE DE EXISTENCIA DE DESPESAS
**********************************

FUNCTION ver_desp(mcod_des,li,ci)

LOCAL msele,morde

IF EMPTY(mcod_des); RETURN .T.;END

msele := SELE()
morde := INDEXORD()
mnome_ven = SPACE(30)

WHILE .T.

        **************
        SELECT('desp');ORDSETFOCUS(1)
        GO TOP
        **************

        SEEK STRZERO(mcod_des,3)

        IF .NOT. FOUND()
                atencao('Despesa nao Cadastrada !!!')
                mcod_des = 0
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .F.
        ELSE
                IF li = NIL
                        mdescri1 = desc1
                        mdescri2 = desc2
                        SELE(msele);ORDSETFOCUS(morde)
                        RETURN .T.
                ELSE
                        mdescri1 = desc1
                        mdescri2 = desc2
                        setcor(3)
                        DEVPOS(li,ci);DEVOUT(mdescri1)
                        DEVPOS(li+1,ci);DEVOUT(mdescri2)
                        setcor(1)
                        SELE(msele);ORDSETFOCUS(morde)
                        RETURN .T.
                ENDIF
        ENDIF
ENDDO
*************************** f i m *********************************************
**********************************
* TESTE DE EXISTENCIA DE VENDEDOR
**********************************

FUNCTION ven(mcod_ven,li,ci)

LOCAL msele,morde

IF EMPTY(mcod_ven); RETURN .T.;END

msele := SELE()
morde := INDEXORD()

WHILE .T.
        **************
        SELECT('sen');ORDSETFOCUS(2)
        GO TOP
        **************

        SEEK STRZERO(mcod_ven,3)

        IF ! FOUND()
                atencao('Vendedor nao Cadastrado !!!')
                mcod_ven = 0
                SELE(msele);ORDSETFOCUS(morde)
                RETURN .F.
        ELSE
                IF li = NIL
                        SELE(msele);ORDSETFOCUS(morde)
                        RETURN .T.
                ELSE
                        setcor(3)
                        DEVPOS(li,ci);DEVOUT(sen->snome)
                        setcor(1)
                        SELE(msele);ORDSETFOCUS(morde)
                        RETURN .T.
                ENDIF
        ENDIF
ENDDO
*************************** f i m *********************************************
**********************************
* VER TABELA DO IGPM
**********************************

FUNCTION ver_igpm(mmes_igpm)

LOCAL msele,morde

msele := SELE()
morde := INDEXORD()

WHILE .T.
        **************
        SELECT('igpm');ORDSETFOCUS(1)
        GO TOP
        **************

        IF ! igpm->(DBSEEK(mmes_igpm))
                SELE(msele);ORDSETFOCUS(morde)
                RETURN 0
        ELSE
                SELE(msele);ORDSETFOCUS(morde)
                RETURN igpm->vlr_igpm
        ENDIF
ENDDO
************************** F I M ***********************************

*-----------------* // Esta funcao verifica se o sistema ja esta sendo 
 FUNCTION TRAVAJAN  // Utilizado na estacao corrente
*-----------------* // Desta forma, nao permite 2 Janelas.
                    // Verifica ainda se esta estabelecida a variavel de
                    // ambiente INSERCOM e se ela e valida
                    // Retorna .T. se tudo ok e .F. se houver problemas
LOCAL tela
tela := SAVESCREEN(00,00,24,79)
IF LEN(ALLTRIM(GETENV("INSERCOM")))=0
        CLS
        botao(10,05,17,75,,' MENSAGEM DE ALERTA ')
        DEVPOS(11,06);DEVOUT('      Estacao nao configurada no AUTOEXEC.BAT. Verifique !!!')
        DEVPOS(12,06);DEVOUT('********************************************************************')
        DEVPOS(13,06);DEVOUT('>>> Deve ser incluido no AUTOEXEC.BAT a Linha "SET INSERCOM=001" <<<')
        DEVPOS(14,06);DEVOUT('          Entre em contato com a HRB Informatica Ltda.         ')
        DEVPOS(15,06);DEVOUT(' Recife..:(81)3325.6663 - Roberta: 8835.1751 - Helio: 9975.5923    ')
        DEVPOS(16,06);DEVOUT(' Tacaimbo:(81)3755.1489 - Edson...: 8835.1752    ')
        INKEY(9)
        RESTSCREEN(00,00,24,79,tela)
        RETURN(.F.)
ENDIF
IF VAL(GETENV("INSERCOM"))<1 .OR. VAL(GETENV("INSERCOM"))>999 .OR.;
   LEN(ALLTRIM(GETENV("INSERCOM")))<>3
        CLS
        botao(10,05,18,75,,' MENSAGEM DE ALERTA ')
        DEVPOS(11,06);DEVOUT('Valor Invalido - INSERCOM no AUTOEXEC.BAT. Verifique !!!')
        DEVPOS(12,06);DEVOUT('Valor INSERCOM -> '+GETENV("INSERCOM"))
        DEVPOS(13,06);DEVOUT('********************************************************************')
        DEVPOS(14,06);DEVOUT('>>> Deve ser incluido no AUTOEXEC.BAT a Linha "SET INSERCOM=001" <<<')
        DEVPOS(15,06);DEVOUT('          Entre em contato com a HRB Informatica Ltda.         ')
        DEVPOS(16,06);DEVOUT(' Recife..:(81)3462.1866 - Tarcisio: 9964.8264 - Helio: 9975.5923    ')
        DEVPOS(17,06);DEVOUT(' Tacaimbo:(81)3755.1489 - Edson...: 9944.9475 - Luiz.: 9601.5896    ')
        INKEY(9)
        RESTSCREEN(00,00,24,79,tela)
        RETURN(.F.)
ENDIF

ESTACAO=GETENV("INSERCOM")
FERASE("CONTROLE."+ESTACAO)
VA=FCREATE("CONTROLE."+ESTACAO,0)
FWRITE(VA,"[ Arquivo de Controle do Sistema ]")
FCLOSE(VA)
VB=FOPEN("CONTROLE."+ESTACAO,50)

IF FERROR()<>0
        CLS
        botao(10,05,14,75,,' MENSAGEM DE ALERTA ')
        DEVPOS(11,06);DEVOUT('     O Sistema ja esta sendo executado nesta estacao - '+ESTACAO)
        DEVPOS(12,06);DEVOUT('********************************************************************')
        DEVPOS(13,06);DEVOUT(' >>> Verifique se ja tem alguma janela aberta este COMPUTADOR <<<')
*       @ 02,00 SAY "O Sistema ja esta sendo executado nesta estacao - "+ESTACAO')
*       @ 03,00 SAY STRZERO(FERROR(),3)
*       @ 04,00 SAY STRZERO(VA,3)
*       @ 05,00 SAY REPLICATE("=",50)
        INKEY(9)
        RESTSCREEN(00,00,24,79,tela)
        RETURN(.F.)
ENDIF
RETURN(.T.)

**---------------------- Fim da funcao TRAVAJAN

