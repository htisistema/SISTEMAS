************************************************
* EMISSAO DE CARNES DO MES
************************************************

FUNCTION saf5252
***************
LOCAL mprg:='SAF5252'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mcont_parc,mdesconto,mvlr_men,mgstt,mtot_men,mtot_ger,mindex,minicio,;
      tela,mgst,mmatricula,mop,maluno

PRIVATE m_parcela:={},mquantd_parc

IF ! ver_nivel(mprg,'EMISSAO DE CARNES DO MES (MENSALIDADES)','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
lci := 06
cci := 05
lba := 14
cba := 75
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mop := 0
        mgst := SPACE(4)
        mmatricula := SPACE(6)
        maluno     := SPACE(40)
        mquantd_parc := 0
        mindex := 0
        mtot_men := 0
        mtot_ger := 0
        mcont_aluno := 0
        mcont_parc := 0
        mmes := SPACE(2)
        mquantd = 1
        ***************
        SELE('aluno');ORDSETFOCUS(1)
        GO TOP
        ***************
        setcor(3)
        botao(lci,cci,lba,cba)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        setcor(1)
        DEVPOS(lci+5,cci+1);DEVOUT('Digite a Parcela que deseja ..........................: ')
        @ lci+1,cci+1 PROMPT ' P/GSTT ' MESSAGE '** Relacao de debitos por GSTT - <ESC> Retornar **'
        @ lci+1,cci+11 PROMPT ' INDIVIDUAL  ' MESSAGE '** Emissao de carne INDIVIDUAL - <ESC> Retornar **'
        @ lci+1,cci+26 PROMPT ' GERAL ' MESSAGE '** Relatorio de DEBITO geral - <ESC> p/retornar **'
        SET INTEN ON
        MENU TO mop
        IF LASTKEY() = 27
                CLOSE ALL
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF mop = 1
                DEVPOS(lci+3,cci+1);DEVOUT('Digite a GSTT : ')
                @ lci+3,cci+17 GET mgst PICT '@!'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                ***************
                IF ! aluno->(DBSEEK(mgst))
                        atencao('Nao foi encontrado esta GSTT')
                        LOOP
                ENDIF
        ELSEIF mop = 2
                DEVPOS(lci+3,cci+1);DEVOUT('Digite a MATRICULA : ')
                @ lci+3,cci+22 GET mmatricula PICT '999999'
                @ lci+3,cci+29 GET maluno PICT '@!' WHEN EMPTY(mmatricula)
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF ! EMPTY(mmatricularicula)
                        ORDSETFOCUS(4)
                        IF ! aluno->(DBSEEK(mmatricula))
                                atencao('Matricula nao encontrada no arquivo')
                                LOOP
                        ENDIF
                ELSEIF ! EMPTY(maluno)
                        ORDSETFOCUS(3)
                        IF ! aluno->(DBSEEK(RTRIM(maluno)))
                                atencao('Aluno nao encontrada no arquivo')
                                LOOP
                        ENDIF
                        mmatricula := f7_aluno()
                ELSE
                        LOOP
                ENDIF
        ENDIF
        setcor(3)
        DEVPOS(lci+3,cci+22);DEVOUT(aluno->matricula)
        DEVPOS(lci+3,cci+29);DEVOUT(aluno->aluno)
        setcor(1)
        @ lci+5,cci+57 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF mop = 3
                mensagem('Confirma a Impressao [S/n]:')
                opcao = 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF opcao = 'N'
                        LOOP
                ENDIF
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                SET DEVI TO PRINT
*               SET PRINT TO 'LPT1'
                i := 0
                WHILE ! EOF()
                        IF mmes < SUBSTR(DTOC(aluno->data_ing),4,2)
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        mvalor := 0
                        mdesconto := faixa_vlr(ver_faixa(mmes))
/*
                        IF mmes = '01'
                                mdesconto := faixa_vlr(aluno->faixa_1)
                        ELSEIF mmes = '02'
                                mdesconto := faixa_vlr(aluno->faixa_2)
                        ELSEIF mmes = '03'
                                mdesconto := faixa_vlr(aluno->faixa_3)
                        ELSEIF mmes = '04'
                                mdesconto := faixa_vlr(aluno->faixa_4)
                        ELSEIF mmes = '05'
                                mdesconto := faixa_vlr(aluno->faixa_5)
                        ELSEIF mmes = '06'
                                mdesconto := faixa_vlr(aluno->faixa_6)
                        ELSEIF mmes = '07'
                                mdesconto := faixa_vlr(aluno->faixa_7)
                        ELSEIF mmes = '08'
                                mdesconto := faixa_vlr(aluno->faixa_8)
                        ELSEIF mmes = '09'
                                mdesconto := faixa_vlr(aluno->faixa_9)
                        ELSEIF mmes = '10'
                                mdesconto := faixa_vlr(aluno->faixa_10)
                        ELSEIF mmes = '11'
                                mdesconto := faixa_vlr(aluno->faixa_11)
                        ELSEIF mmes = '12'
                                mdesconto := faixa_vlr(aluno->faixa_12)
                        ENDIF
*/
                        IF mdesconto == 100
                                SKIP
                                LOOP
                        ENDIF
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := vlr_men(mgstt,mmes)
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),mmes,'*')
                        IF maut = 'NOBREGA'
                                imprt('M','N',4)
                                DEVPOS(PROW(),00);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                                DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                                DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                                DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                                DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                                DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                                DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                                DEVPOS(PROW(),33);DEVOUT(mmes+'a.Parcela')
                                DEVPOS(PROW(),51);DEVOUT(mmes)
                                DEVPOS(PROW(),57);DEVOUT(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
*                                DEVPOS(PROW(),57);DEVOUT('05/'+mmes+'/'+SUBSTR(mdiretorio,3,2))
                                DEVPOS(PROW(),76);DEVOUT(mmes)
                                DEVPOS(PROW(),81);DEVOUT(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
*                                DEVPOS(PROW(),81);DEVOUT('05/'+mmes+'/'+SUBSTR(mdiretorio,3,2))
                                DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                                DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                                DEVPOS(PROW()+4,00);DEVOUT(' ')
                        ELSE
                                imprt('M','N')
                                DEVPOS(PROW()+1,00);DEVOUT(memp+resa)
                                DEVPOS(PROW(),00);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                                DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                                DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                                DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                                DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                                DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                                DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                                DEVPOS(PROW(),33);DEVOUT(mmes+'a.Parcela')
                                DEVPOS(PROW(),51);DEVOUT(mmes)
                                DEVPOS(PROW(),57);DEVOUT(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
*                               DEVPOS(PROW(),57);DEVOUT('05/'+mmes+'/'+SUBSTR(mdiretorio,3,2))
                                DEVPOS(PROW(),76);DEVOUT(mmes)
                                DEVPOS(PROW(),81);DEVOUT(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
*                               DEVPOS(PROW(),81);DEVOUT('05/'+mmes+'/'+SUBSTR(mdiretorio,3,2))
                                DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                                DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                                DEVPOS(PROW()+4,00);DEVOUT(' ')
                        ENDIF
                        SKIP
                ENDDO
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ELSEIF mop = 1
                mensagem('Confirma a Impressao [S/n]:')
                opcao = 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF opcao = 'N'
                        LOOP
                ENDIF
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                SET DEVI TO PRINT
*               * SET PRINT TO LPT1
                i := 0
                WHILE mgst = aluno->gstt .AND. ! EOF()
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        mdesconto := faixa_vlr(ver_faixa(mmes))
/*
                        IF mmes = '01'
                                mdesconto := faixa_vlr(aluno->faixa_1)
                        ELSEIF mmes = '02'
                                mdesconto := faixa_vlr(aluno->faixa_2)
                        ELSEIF mmes = '03'
                                mdesconto := faixa_vlr(aluno->faixa_3)
                        ELSEIF mmes = '04'
                                mdesconto := faixa_vlr(aluno->faixa_4)
                        ELSEIF mmes = '05'
                                mdesconto := faixa_vlr(aluno->faixa_5)
                        ELSEIF mmes = '06'
                                mdesconto := faixa_vlr(aluno->faixa_6)
                        ELSEIF mmes = '07'
                                mdesconto := faixa_vlr(aluno->faixa_7)
                        ELSEIF mmes = '08'
                                mdesconto := faixa_vlr(aluno->faixa_8)
                        ELSEIF mmes = '09'
                                mdesconto := faixa_vlr(aluno->faixa_9)
                        ELSEIF mmes = '10'
                                mdesconto := faixa_vlr(aluno->faixa_10)
                        ELSEIF mmes = '11'
                                mdesconto := faixa_vlr(aluno->faixa_11)
                        ELSEIF mmes = '12'
                                mdesconto := faixa_vlr(aluno->faixa_12)
                        ENDIF
*/
                        IF mdesconto == 100
                                SKIP
                                LOOP
                        ENDIF
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := vlr_men(mgstt,mmes)
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),mmes,'*')
                        imprt('M','N',4)
                        DEVPOS(PROW(),00);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                        DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                        DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW(),33);DEVOUT(mmes+'a.Parcela')
                        DEVPOS(PROW(),51);DEVOUT(mmes)
                        DEVPOS(PROW(),57);DEVOUT(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
*                       DEVPOS(PROW(),57);DEVOUT('05/'+mmes+'/'+SUBSTR(mdiretorio,3,2))
                        DEVPOS(PROW(),76);DEVOUT(mmes)
                        DEVPOS(PROW(),81);DEVOUT(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
*                       DEVPOS(PROW(),81);DEVOUT('05/'+mmes+'/'+SUBSTR(mdiretorio,3,2))
                        DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                        DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                        DEVPOS(PROW()+4,00);DEVOUT(' ')
                        SKIP
                ENDDO
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ELSEIF mop = 2
                mensagem('Confirma a Impressao [S/n]:')
                opcao = 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF opcao = 'N'
                        LOOP
                ENDIF
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                SET DEVI TO PRINT
*               SET PRINT TO 'LPT1'
                mtot_men := 0
                mdesconto := 0
                mvlr_men := 0
                mdesconto := faixa_vlr(ver_faixa(mmes))
                IF mdesconto == 100
                        SKIP
                        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                        LOOP
                ENDIF
                mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                mvlr_men := vlr_men(mgstt,mmes)
                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),mmes,'*')
                imprt('M','N',4)
                DEVPOS(PROW(),00);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                DEVPOS(PROW(),33);DEVOUT(mmes+'a.Parcela')
                DEVPOS(PROW(),51);DEVOUT(mmes)
                DEVPOS(PROW(),57);DEVOUT(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
*               DEVPOS(PROW(),57);DEVOUT('05/'+mmes+'/'+SUBSTR(mdiretorio,3,2))
                DEVPOS(PROW(),76);DEVOUT(mmes)
                DEVPOS(PROW(),81);DEVOUT(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
*               DEVPOS(PROW(),81);DEVOUT('05/'+mmes+'/'+SUBSTR(mdiretorio,3,2))
                DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                DEVPOS(PROW()+4,00);DEVOUT(' ')
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ENDIF
ENDDO
RETURN NIL
**************************** F I M **************************

