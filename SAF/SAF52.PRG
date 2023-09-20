************************************************
* RELATORIO DE ALUNOS EM DEBITOS
************************************************

FUNCTION saf52
***************
LOCAL mprg:='SAF52'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mcont_parc,mdesconto,mvlr_men,mgstt,mtot_men,mtot_ger,mindex,minicio,;
      tela,mgst,mmat,mop

PRIVATE m_parcela:={},mquantd_parc

IF ! ver_nivel(mprg,'RELATORIO DE ALUNOS EM DEBITOS','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
lci := 06
cci := 10
lba := 14
cba := 70
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
        mmat := SPACE(6)
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
        CAIXA(mold,lci,cci,lba,cba,,,.T.)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        setcor(1)
        DEVPOS(lci+5,cci+1);DEVOUT('Digite ate que MES deseja.............................: ')
        DEVPOS(lci+6,cci+1);DEVOUT('Por ordem de:[1] p/Resp.[2]GSTT [3]Aluno [4] Matricula: ')
        DEVPOS(lci+7,cci+1);DEVOUT('Digite quantidade de parcelas devidas.................: ')
        @ lci+1,cci+1 PROMPT ' P/GSTT ' MESSAGE '** Relacao de debitos por GSTT - <ESC> Retornar **'
        @ lci+1,cci+11 PROMPT ' P/MATRICULA ' MESSAGE '** Relacao de DEBITO por matricula - <ESC> Retornar **'
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
                @ lci+3,cci+22 GET mmat PICT '999999'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                ***************
                SELE('aluno');ORDSETFOCUS(4)
                GO TOP
                ***************
                IF ! aluno->(DBSEEK(mmat))
                        atencao('Nao foi encontrado esta MATRICULA')
                        LOOP
                ENDIF
        ENDIF
        @ lci+5,cci+57 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        @ lci+6,cci+57 GET mindex PICT '9' VALID IF(mindex<1 .OR. mindex > 4,.F.,.T.) WHEN mop = 3
        @ lci+7,cci+57 GET mquantd_parc PICT '99'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF mop = 3
                IF mindex = 1
                        ***************
                        SELE('aluno');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                ELSEIF mindex = 2
                        ***************
                        SELE('aluno');ORDSETFOCUS(2)
                        GO TOP
                        ***************
                ELSEIF mindex = 3
                        ***************
                        SELE('aluno');ORDSETFOCUS(3)
                        GO TOP
                        ***************
                ELSE
                        ***************
                        SELE('aluno');ORDSETFOCUS(4)
                        GO TOP
                        ***************
                ENDIF
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
                mpag := 1
                mtit := 'Relacao de Alunos em DEBITOS'
                mtipo := 'por ordem de Responsavel'
                cabecalho(mpag,mtit,mtipo)
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT('Responsavel')
                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                DEVPOS(PROW(),51);DEVOUT('Aluno')
                DEVPOS(PROW(),93);DEVOUT('Fone')
                DEVPOS(PROW(),103);DEVOUT('GSTT')
                DEVPOS(PROW(),109);DEVOUT('Fx')
                DEVPOS(PROW(),113);DEVOUT('Parc.')
                DEVPOS(PROW(),120);DEVOUT('Valor')
                DEVPOS(PROW()+1,00);DEVOUT('Endereco')
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT(mtraco)
                imprt('M','C')
                i := 0
                WHILE ! EOF()
                        INKEY(.1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        ASIZE(m_parcela,0)
                        ver_parc(aluno->matricula,mmes,minicio)
                        IF LEN(m_parcela) == 0
                                SKIP
                                LOOP
                        ENDIF
                        IF LEN(m_parcela) < mquantd_parc
                                SKIP
                                LOOP
                        ENDIF
                        mdesconto := faixa_vlr(aluno->faixa_1)
                        IF mdesconto == 100
                                SKIP
                                LOOP
                        ENDIF
                        imprt('M','C',1)
                        DEVPOS(PROW(),00);DEVOUT(aluno->responsa)
                        DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                        DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                        DEVPOS(PROW(),109);DEVOUT(aluno->faixa_1)
                        mcont_aluno ++
                        mcont_parc := mcont_parc + LEN(m_parcela)
                        DEVPOS(PROW(),113);DEVOUT(m_parcela[1,1]+' '+m_parcela[1,2])
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                        mtot_men := mtot_men + mvlr_men
                        mtot_ger := mtot_ger + mvlr_men
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mvlr_men := 0
                                DEVPOS(PROW(),113);DEVOUT(m_parcela[i,1]+' '+m_parcela[i,2])
                                mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                mtot_men := mtot_men + mvlr_men
                                mtot_ger := mtot_ger + mvlr_men
                                IF PROW() >= 59
                                        EJECT
                                        mpag ++
                                        cabecalho(mpag,mtit,mtipo)
                                        imprt('M','C',1)
                                        DEVPOS(PROW(),00);DEVOUT('Responsavel')
                                        DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                        DEVPOS(PROW(),51);DEVOUT('Aluno')
                                        DEVPOS(PROW(),93);DEVOUT('Fone')
                                        DEVPOS(PROW(),103);DEVOUT('GSTT')
                                        DEVPOS(PROW(),109);DEVOUT('Parc.')
                                        imprt('M','N',1)
                                        DEVPOS(PROW(),00);DEVOUT(mtraco)
                                        IF LEN(m_parcela) > i
                                                imprt('M','C',1)
                                                DEVPOS(PROW(),00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                                DEVPOS(PROW(),109);DEVOUT(aluno->faixa_1)
                                        ENDIF
                                        LOOP
                                ELSEIF LEN(m_parcela) > i
                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                ENDIF
                        NEXT
                        DEVPOS(PROW()+1,111);DEVOUT('----')
                        DEVPOS(PROW(),120);DEVOUT('-----------')
                        DEVPOS(PROW()+1,89);DEVOUT('T O T A I S: ')
                        DEVPOS(PROW(),113);DEVOUT(STRZERO(LEN(m_parcela),2))
                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM(mtot_men,'99,999.99'))
                        DEVPOS(PROW()+1,89);DEVOUT('TOTAL A PAGAR (+2%): ')
                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mtot_men*1.02),'99,999.99'))
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT(REPLI('-',80))
                        SKIP
                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C',1)
                                DEVPOS(PROW(),00);DEVOUT('Responsavel')
                                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                DEVPOS(PROW(),51);DEVOUT('Aluno')
                                DEVPOS(PROW(),93);DEVOUT('Fone')
                                DEVPOS(PROW(),103);DEVOUT('GSTT')
                                DEVPOS(PROW(),109);DEVOUT('Parc.')
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT('Total de ALUNOS em Debito....: '+STRZERO(mcont_aluno,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total de PARCELAS em Debito: '+STRZERO(mcont_parc,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total em VALOR mensalidade.: '+TRANSFORM(mtot_ger,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR A PAGAR.....: '+TRANSFORM((mtot_ger*1.02),'9,999,999.99'))
                EJECT
                SETPRC(00,00)
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
                mpag := 1
                mtit := 'Relacao de Alunos em DEBITOS'
                mtipo := 'por ordem de Responsavel'
                cabecalho(mpag,mtit,mtipo)
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT('Responsavel')
                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                DEVPOS(PROW(),51);DEVOUT('Aluno')
                DEVPOS(PROW(),93);DEVOUT('Fone')
                DEVPOS(PROW(),103);DEVOUT('GSTT')
                DEVPOS(PROW(),109);DEVOUT('Fx')
                DEVPOS(PROW(),113);DEVOUT('Parc.')
                DEVPOS(PROW(),120);DEVOUT('Valor')
                DEVPOS(PROW()+1,00);DEVOUT('Endereco')
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT(mtraco)
                imprt('M','C')
                i := 0
                WHILE mgst = aluno->gstt .AND. ! EOF()
                        INKEY(.1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        ASIZE(m_parcela,0)
                        ver_parc(aluno->matricula,mmes,minicio)
                        IF LEN(m_parcela) == 0
                                SKIP
                                LOOP
                        ENDIF
                        IF LEN(m_parcela) < mquantd_parc
                                SKIP
                                LOOP
                        ENDIF
                        mdesconto := faixa_vlr(aluno->faixa_1)
                        IF mdesconto == 100
                                SKIP
                                LOOP
                        ENDIF
                        imprt('M','C',1)
                        DEVPOS(PROW(),00);DEVOUT(aluno->responsa)
                        DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                        DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                        DEVPOS(PROW(),109);DEVOUT(aluno->faixa_1)
                        mcont_aluno ++
                        mcont_parc := mcont_parc + LEN(m_parcela)
                        DEVPOS(PROW(),113);DEVOUT(m_parcela[1,1]+' '+m_parcela[1,2])
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                        mtot_men := mtot_men + mvlr_men
                        mtot_ger := mtot_ger + mvlr_men
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mvlr_men := 0
                                DEVPOS(PROW(),113);DEVOUT(m_parcela[i,1]+' '+m_parcela[i,2])
                                mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                mtot_men := mtot_men + mvlr_men
                                mtot_ger := mtot_ger + mvlr_men
                                IF PROW() >= 59
                                        EJECT
                                        mpag ++
                                        cabecalho(mpag,mtit,mtipo)
                                        imprt('M','C',1)
                                        DEVPOS(PROW(),00);DEVOUT('Responsavel')
                                        DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                        DEVPOS(PROW(),51);DEVOUT('Aluno')
                                        DEVPOS(PROW(),93);DEVOUT('Fone')
                                        DEVPOS(PROW(),103);DEVOUT('GSTT')
                                        DEVPOS(PROW(),109);DEVOUT('Parc.')
                                        imprt('M','N',1)
                                        DEVPOS(PROW(),00);DEVOUT(mtraco)
                                        IF LEN(m_parcela) > i
                                                imprt('M','C',1)
                                                DEVPOS(PROW(),00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                                DEVPOS(PROW(),109);DEVOUT(aluno->faixa_1)
                                        ENDIF
                                        LOOP
                                ELSEIF LEN(m_parcela) > i
                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                ENDIF
                        NEXT
                        DEVPOS(PROW()+1,111);DEVOUT('----')
                        DEVPOS(PROW(),120);DEVOUT('-----------')
                        DEVPOS(PROW()+1,89);DEVOUT('T O T A I S: ')
                        DEVPOS(PROW(),113);DEVOUT(STRZERO(LEN(m_parcela),2))
                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM(mtot_men,'99,999.99'))
                        DEVPOS(PROW()+1,89);DEVOUT('TOTAL A PAGAR (+2%): ')
                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mtot_men*1.02),'99,999.99'))
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT(REPLI('-',80))
                        SKIP
                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C',1)
                                DEVPOS(PROW(),00);DEVOUT('Responsavel')
                                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                DEVPOS(PROW(),51);DEVOUT('Aluno')
                                DEVPOS(PROW(),93);DEVOUT('Fone')
                                DEVPOS(PROW(),103);DEVOUT('GSTT')
                                DEVPOS(PROW(),109);DEVOUT('Parc.')
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT('Total de ALUNOS em Debito....: '+STRZERO(mcont_aluno,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total de PARCELAS em Debito: '+STRZERO(mcont_parc,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total em VALOR mensalidade.: '+TRANSFORM(mtot_ger,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR A PAGAR.....: '+TRANSFORM((mtot_ger*1.02),'9,999,999.99'))
                EJECT
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
*               * SET PRINT TO LPT1
                mpag := 1
                mtit := 'Relacao de Alunos em DEBITOS'
                mtipo := 'por ordem de Responsavel'
                cabecalho(mpag,mtit,mtipo)
                imprt('M','C',1)
                DEVPOS(PROW(),00);DEVOUT('Responsavel')
                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                DEVPOS(PROW(),51);DEVOUT('Aluno')
                DEVPOS(PROW(),93);DEVOUT('Fone')
                DEVPOS(PROW(),103);DEVOUT('GSTT')
                DEVPOS(PROW(),109);DEVOUT('Fx')
                DEVPOS(PROW(),113);DEVOUT('Parc.')
                DEVPOS(PROW(),120);DEVOUT('Valor')
                DEVPOS(PROW()+1,00);DEVOUT('Endereco')
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT(mtraco)
                imprt('M','C')
                i := 0
                WHILE mmat = aluno->matricula .AND. ! EOF()
                        INKEY(.1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        ASIZE(m_parcela,0)
                        ver_parc(aluno->matricula,mmes,minicio)
                        IF LEN(m_parcela) == 0
                                SKIP
                                LOOP
                        ENDIF
                        IF LEN(m_parcela) < mquantd_parc
                                SKIP
                                LOOP
                        ENDIF
                        mdesconto := faixa_vlr(aluno->faixa_1)
                        IF mdesconto == 100
                                SKIP
                                LOOP
                        ENDIF
                        imprt('M','C',1)
                        DEVPOS(PROW(),00);DEVOUT(aluno->responsa)
                        DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                        DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                        DEVPOS(PROW(),109);DEVOUT(aluno->faixa_1)
                        mcont_aluno ++
                        mcont_parc := mcont_parc + LEN(m_parcela)
                        DEVPOS(PROW(),113);DEVOUT(m_parcela[1,1]+' '+m_parcela[1,2])
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                        mtot_men := mtot_men + mvlr_men
                        mtot_ger := mtot_ger + mvlr_men
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mvlr_men := 0
                                DEVPOS(PROW(),113);DEVOUT(m_parcela[i,1]+' '+m_parcela[i,2])
                                mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                mtot_men := mtot_men + mvlr_men
                                mtot_ger := mtot_ger + mvlr_men
                                IF PROW() >= 59
                                        EJECT
                                        mpag ++
                                        cabecalho(mpag,mtit,mtipo)
                                        imprt('M','C',1)
                                        DEVPOS(PROW(),00);DEVOUT('Responsavel')
                                        DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                        DEVPOS(PROW(),51);DEVOUT('Aluno')
                                        DEVPOS(PROW(),93);DEVOUT('Fone')
                                        DEVPOS(PROW(),103);DEVOUT('GSTT')
                                        DEVPOS(PROW(),109);DEVOUT('Parc.')
                                        imprt('M','N',1)
                                        DEVPOS(PROW(),00);DEVOUT(mtraco)
                                        IF LEN(m_parcela) > i
                                                imprt('M','C',1)
                                                DEVPOS(PROW(),00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                                DEVPOS(PROW(),109);DEVOUT(aluno->faixa_1)
                                        ENDIF
                                        LOOP
                                ELSEIF LEN(m_parcela) > i
                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                ENDIF
                        NEXT
                        DEVPOS(PROW()+1,111);DEVOUT('----')
                        DEVPOS(PROW(),120);DEVOUT('-----------')
                        DEVPOS(PROW()+1,89);DEVOUT('T O T A I S: ')
                        DEVPOS(PROW(),113);DEVOUT(STRZERO(LEN(m_parcela),2))
                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM(mtot_men,'99,999.99'))
                        DEVPOS(PROW()+1,89);DEVOUT('TOTAL A PAGAR (+2%): ')
                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mtot_men*1.02),'99,999.99'))
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT(REPLI('-',80))
                        SKIP
                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C',1)
                                DEVPOS(PROW(),00);DEVOUT('Responsavel')
                                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                DEVPOS(PROW(),51);DEVOUT('Aluno')
                                DEVPOS(PROW(),93);DEVOUT('Fone')
                                DEVPOS(PROW(),103);DEVOUT('GSTT')
                                DEVPOS(PROW(),109);DEVOUT('Parc.')
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT('Total de ALUNOS em Debito....: '+STRZERO(mcont_aluno,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total de PARCELAS em Debito: '+STRZERO(mcont_parc,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total em VALOR mensalidade.: '+TRANSFORM(mtot_ger,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR A PAGAR.....: '+TRANSFORM((mtot_ger*1.02),'9,999,999.99'))
                EJECT
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ENDIF
ENDDO
RETURN NIL
**************************** F I M **************************
************************************
* FUNCAO PARA VER PARCELAS EM ABERTO
************************************

FUNCTION ver_parc(mmatricula,mes)
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
FOR i = 1 TO VAL(mes)
        IF i < minicio
                LOOP
        ENDIF
        GO TOP
        IF ! cdmen->(DBSEEK(mmatricula+STRZERO(i,2)))
                AADD(m_parcela,{STRZERO(i,2),' '})
        ELSE
                IF cdmen->flag == '*' .AND. EMPTY(mquantd_parc)
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
SELE('faixa')
ORDSETFOCUS(1)
**************
IF ! faixa->(DBSEEK(mfaixa))
        mvlr := 0
ELSE
        mvlr :=faixa->desconto
ENDIF
SELE(msele)
ORDSETFOCUS(morde)
RETURN mvlr

