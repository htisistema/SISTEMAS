************************************************
* RELATORIO DE ALUNOS EM DEBITOS
************************************************

FUNCTION saf520
***************
LOCAL mprg:='SAF520'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mcont_parc,mdesconto,mvlr_men,mgstt,mtot_men,mtot_ger,mindex,;
      tela,mgst,mmat,mop,mfaixa,mflag,mimp_resp:=' ',mcont_ind:=0,mqtd_parc:=0,mtip_mes:=0,;
      mqtd_dias:=0
PRIVATE m_parcela:={},mquantd_parc
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DE ALUNOS EM DEBITOS','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',132)
lci := 06
cci := 10
lba := 15
cba := 75
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('safsetup','set');RETURN NIL;ENDIF
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
        mtip_mes := 0
        mquantd = 1
        mtit := 'Relacao de Alunos em DEBITOS do ano '+mdiretorio
        ***************
        SELE('aluno');ORDSETFOCUS(1)
        GO TOP
        ***************
        setcor(3)
        botao(lci,cci,lba,cba)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        setcor(1)
        DEVPOS(lci+5,cci+1);DEVOUT('Digite ate que MES deseja.............................: ')
        DEVPOS(lci+6,cci+1);DEVOUT('Por ordem de:[1] p/Resp.[2]GSTT [3]Aluno [4] Matricula: ')
        DEVPOS(lci+7,cci+1);DEVOUT('Digite o Mes que deseja ser impresso..................: ')
        DEVPOS(lci+8,cci+1);DEVOUT('Titulo do Relatorio: ')
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
        @ lci+7,cci+57 GET mtip_mes PICT '99'
        @ lci+8,cci+22 GET mtit
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF

        IF ! imp_arq('RELA_DEB.REL')
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
                mflag := ' '
                mpag := 1
                mtipo := 'por ordem de Responsavel'
                cabecalho(mpag,mtit,mtipo)
                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                DEVPOS(PROW(),51);DEVOUT('Aluno')
                DEVPOS(PROW(),93);DEVOUT('Fone')
                DEVPOS(PROW(),103);DEVOUT('GSTT')
                DEVPOS(PROW(),109);DEVOUT('Fx')
                DEVPOS(PROW(),113);DEVOUT('Parc.')
                DEVPOS(PROW(),120);DEVOUT('Valor')
                DEVPOS(PROW()+1,00);DEVOUT('Endereco')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                imprt('M','C')
                i := 0
                WHILE ! EOF()
                        mflag := ' '
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        mqtd_parc := 0
                        ASIZE(m_parcela,0)

                        ***************
                        SELE('cdmen');ORDSETFOCUS(8)
                        GO TOP
                        ***************
                        IF cdmen->(DBSEEK(aluno->matricula))
                                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. VAL(SUBSTR(DTOC(cdmen->data_venc),4,2)) <= VAL(mmes)
                                        IF EMPTY(cdmen->data_pag)
                                                IF mtip_mes > 0
                                                        IF VAL(cdmen->parcela) <> mtip_mes
                                                                SKIP
                                                                LOOP
                                                        ENDIF
                                                ENDIF
                                                IF mdata_sis > cdmen->data_venc
                                                        mqtd_dias := mqtd_dias + (mdata_sis - cdmen->data_venc)
                                                ENDIF
                                                AADD(m_parcela,{cdmen->parcela,cdmen->vlr_parc,cdmen->conta,cdmen->data_venc})
                                        ENDIF
                                        SKIP
                                ENDDO
                        ENDIF

                        IF mindex = 1
                                ***************
                                SELE('aluno');ORDSETFOCUS(1)
                                ***************
                        ELSEIF mindex = 2
                                ***************
                                SELE('aluno');ORDSETFOCUS(2)
                                ***************
                        ELSEIF mindex = 3
                                ***************
                                SELE('aluno');ORDSETFOCUS(3)
                                ***************
                        ELSE
                                ***************
                                SELE('aluno');ORDSETFOCUS(4)
                                ***************
                        ENDIF

                        IF LEN(m_parcela) == 0
                                SKIP
                                LOOP
                        ENDIF
                        IF LEN(m_parcela) < mquantd_parc
                                SKIP
                                LOOP
                        ENDIF
                        mfaixa := ver_faixa(m_parcela[1,1])
                        mdesconto := faixa_vlr(mfaixa)
*                       IF mfaixa = 'S'
*                               SKIP
*                               LOOP
*                       ENDIF
                        IF VAL(m_parcela[1,1]) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                SKIP
                                LOOP
                        ENDIF
                        IF mtip_mes > 0
                                IF VAL(m_parcela[1,1]) <> mtip_mes
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                IF ! VAL(m_parcela[1,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        IF mdesconto < 100
                                                mflag := '*'
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                                mcont_aluno ++
                                                DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                                mcont_parc := mcont_parc + LEN(m_parcela)
                                                DEVPOS(PROW(),113);DEVOUT(m_parcela[1,1])
                                                IF m_parcela[1,3] <> '*'
                                                        mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                                                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                ELSE
                                                        mvlr_men := m_parcela[1,2]
                                                ENDIF
                                                DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                                DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                                mtot_men := mtot_men + mvlr_men
                                                mtot_ger := mtot_ger + mvlr_men
                                                mqtd_parc ++
                                        ENDIF
                                ENDIF
                        ELSE
                                IF mdesconto < 100
                                        mflag := '*'
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                        DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                        DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                        DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                        mcont_aluno ++
                                        DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                        mcont_parc := mcont_parc + LEN(m_parcela)
                                        DEVPOS(PROW(),113);DEVOUT(m_parcela[1,1])
                                        IF m_parcela[1,3] <> '*'
                                                mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                        ELSE
                                                mvlr_men := m_parcela[1,2]
                                        ENDIF
                                        DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                        DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mqtd_parc ++
                                ENDIF
                        ENDIF
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mfaixa := ver_faixa(m_parcela[i,1])
*                               IF mfaixa = 'S'
*                                       SKIP
*                                       LOOP
*                               ENDIF
                                IF mtip_mes > 0
                                        IF VAL(m_parcela[i,1]) <> mtip_mes
*                                               SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                mdesconto := faixa_vlr(mfaixa)
                                IF VAL(m_parcela[i,1]) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
*                                       SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(m_parcela[i,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                LOOP
                                        ENDIF
                                ENDIF
                                IF mdesconto < 100
                                        IF mflag = ' '
*                                               mflag := '*'
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                                mcont_aluno ++
                                        ENDIF
                                        mvlr_men := 0
                                        DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                        DEVPOS(PROW(),113);DEVOUT(m_parcela[i,1])
*                                       mcont_parc := mcont_parc + LEN(m_parcela)
                                        IF m_parcela[i,3] <> '*'
                                                mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                        ELSE
                                                mvlr_men := m_parcela[i,2]
                                        ENDIF
                                        DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[i,3])
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mqtd_parc ++
                                        IF mflag = ' '
                                                DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                                mflag := '*'
                                        ENDIF
                                ELSE
                                        LOOP
                                ENDIF
                                IF PROW() >= 59
                                        EJECT
                                        mpag ++
                                        cabecalho(mpag,mtit,mtipo)
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                                        DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                        DEVPOS(PROW(),51);DEVOUT('Aluno')
                                        DEVPOS(PROW(),93);DEVOUT('Fone')
                                        DEVPOS(PROW(),103);DEVOUT('GSTT')
                                        DEVPOS(PROW(),109);DEVOUT('Parc.')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                        imprt('M','C')
                                        IF LEN(m_parcela) > i
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                                DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                        ENDIF
                                        LOOP
                                ELSEIF LEN(m_parcela) > i
                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                ENDIF
                        NEXT
                        IF ! EMPTY(mtot_men)
                                DEVPOS(PROW()+1,111);DEVOUT('----')
                                DEVPOS(PROW(),120);DEVOUT('-----------')
                                DEVPOS(PROW()+1,89);DEVOUT('T O T A I S: ')
                                DEVPOS(PROW(),113);DEVOUT(TRANSFORM(mqtd_parc,'99'))
                                DEVPOS(PROW(),122);DEVOUT(TRANSFORM(mtot_men,'99,999.99'))
                                DEVPOS(PROW()+1,89);DEVOUT('TOTAL MULTA: ')
                                DEVPOS(PROW(),113);DEVOUT(TRANSFORM(mqtd_dias,'9999'))
                                DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mqtd_dias*set->vlr_multa),'99,999.99'))
                                DEVPOS(PROW()+1,89);DEVOUT('TOTAL A PAGAR (+2%): ')
                                DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mtot_men*1.02)+(mqtd_dias*set->vlr_multa),'99,999.99'))
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                        ENDIF
                        SKIP
                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                DEVPOS(PROW(),51);DEVOUT('Aluno')
                                DEVPOS(PROW(),93);DEVOUT('Fone')
                                DEVPOS(PROW(),103);DEVOUT('GSTT')
                                DEVPOS(PROW(),109);DEVOUT('Parc.')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                IF ! EMPTY(mtot_men)
                        DEVPOS(PROW()+1,111);DEVOUT('----')
                        DEVPOS(PROW(),120);DEVOUT('-----------')
                        DEVPOS(PROW()+1,89);DEVOUT('T O T A I S: ')
                        DEVPOS(PROW(),113);DEVOUT(TRANSFORM(mqtd_parc,'99'))
                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM(mtot_men,'99,999.99'))
                        DEVPOS(PROW()+1,89);DEVOUT('TOTAL MULTA: ')
                        DEVPOS(PROW(),113);DEVOUT(TRANSFORM(mqtd_dias,'9999'))
                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mqtd_dias*set->vlr_multa),'99,999.99'))
                        DEVPOS(PROW()+1,89);DEVOUT('TOTAL A PAGAR (+2%): ')
                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mtot_men*1.02)+(mqtd_dias*set->vlr_multa),'99,999.99'))
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                ENDIF
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT('Total de ALUNOS em Debito....: '+STRZERO(mcont_aluno,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total de PARCELAS em Debito: '+STRZERO(mcont_parc,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total em VALOR mensalidade.: '+TRANSFORM(mtot_ger,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR A PAGAR.....: '+TRANSFORM((mtot_ger*1.02),'9,999,999.99'))
        ELSEIF mop = 1
                mflag := ' '
                mpag := 1
                mtipo := 'por ordem de Responsavel'
                cabecalho(mpag,mtit,mtipo)
                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                DEVPOS(PROW(),51);DEVOUT('Aluno')
                DEVPOS(PROW(),93);DEVOUT('Fone')
                DEVPOS(PROW(),103);DEVOUT('GSTT')
                DEVPOS(PROW(),109);DEVOUT('Fx')
                DEVPOS(PROW(),113);DEVOUT('Parc.')
                DEVPOS(PROW(),120);DEVOUT('Valor')
                DEVPOS(PROW()+1,00);DEVOUT('Endereco')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                imprt('M','C')
                i := 0
                WHILE mgst = aluno->gstt .AND. ! EOF()
                        INKEY(.1)
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        ASIZE(m_parcela,0)

                        ***************
                        SELE('cdmen');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                        IF cdmen->(DBSEEK(aluno->matricula))
                                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. SUBSTR(DTOC(cdmen->data_venc),4,2) <= mmes
                                        IF EMPTY(cdmen->data_pag)
                                                AADD(m_parcela,{cdmen->parcela,cdmen->vlr_parc,cdmen->conta})
                                        ENDIF
                                        SKIP
                                ENDDO
                        ENDIF

                        ***************
                        SELE('aluno');ORDSETFOCUS(2)
                        ***************

                        IF LEN(m_parcela) == 0
                                SKIP
                                LOOP
                        ENDIF
                        IF LEN(m_parcela) < mquantd_parc
                                SKIP
                                LOOP
                        ENDIF
                        mfaixa := ver_faixa(m_parcela[1,1])
                        IF mfaixa = 'S'
                                SKIP
                                LOOP
                        ENDIF
                        mdesconto := faixa_vlr(mfaixa)
                        IF mdesconto < 100
                                mflag := '*'
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                mcont_aluno ++
                                DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                mcont_parc := mcont_parc + LEN(m_parcela)
                                DEVPOS(PROW(),113);DEVOUT(m_parcela[1,1])
                                IF m_parcela[1,3] <> '*'
                                        mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                ELSE
                                        mvlr_men := m_parcela[1,2]
                                ENDIF
                                DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                mtot_men := mtot_men + mvlr_men
                                mtot_ger := mtot_ger + mvlr_men
                                mqtd_parc ++
                        ENDIF
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mfaixa := ver_faixa(m_parcela[i,1])
                                IF mfaixa = 'S'
                                        SKIP
                                        LOOP
                                ENDIF
                                mdesconto := faixa_vlr(mfaixa)
                                IF VAL(m_parcela[i,1]) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(m_parcela[i,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                LOOP
                                        ENDIF
                                ENDIF
                                IF mdesconto < 100
                                        IF mflag = ' '
*                                               mflag := '*'
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                                mcont_aluno ++
                                        ENDIF
                                        mvlr_men := 0
                                        DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                        DEVPOS(PROW(),113);DEVOUT(m_parcela[1,1])
*                                       DEVPOS(PROW(),113);DEVOUT(m_parcela[i,1]+' '+m_parcela[i,2])
                                        mcont_parc := mcont_parc + LEN(m_parcela)
                                        IF m_parcela[i,3] <> '*'
                                                mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                        ELSE
                                                mvlr_men := m_parcela[i,2]
                                        ENDIF
                                        DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[i,3])
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mqtd_parc ++
                                        IF mflag = ' '
                                                DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                                mflag := '*'
                                        ENDIF
                                ELSE
                                        LOOP
                                ENDIF
                                IF PROW() >= 59
                                        EJECT
                                        mpag ++
                                        cabecalho(mpag,mtit,mtipo)
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                                        DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                        DEVPOS(PROW(),51);DEVOUT('Aluno')
                                        DEVPOS(PROW(),93);DEVOUT('Fone')
                                        DEVPOS(PROW(),103);DEVOUT('GSTT')
                                        DEVPOS(PROW(),109);DEVOUT('Parc.')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                        IF LEN(m_parcela) > i
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                                DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                        ENDIF
                                        LOOP
                                ELSEIF LEN(m_parcela) > i
                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                ENDIF
                        NEXT
                        IF ! EMPTY(mtot_men)
                                DEVPOS(PROW()+1,111);DEVOUT('----')
                                DEVPOS(PROW(),120);DEVOUT('-----------')
                                DEVPOS(PROW()+1,89);DEVOUT('T O T A I S: ')
                                DEVPOS(PROW(),113);DEVOUT(STRZERO(LEN(m_parcela),2))
                                DEVPOS(PROW(),122);DEVOUT(TRANSFORM(mtot_men,'99,999.99'))
                                DEVPOS(PROW()+1,89);DEVOUT('TOTAL A PAGAR (+2%): ')
                                DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mtot_men*1.02),'99,999.99'))
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                        ENDIF
                        SKIP
                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                DEVPOS(PROW(),51);DEVOUT('Aluno')
                                DEVPOS(PROW(),93);DEVOUT('Fone')
                                DEVPOS(PROW(),103);DEVOUT('GSTT')
                                DEVPOS(PROW(),109);DEVOUT('Parc.')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT('Total de ALUNOS em Debito....: '+STRZERO(mcont_aluno,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total de PARCELAS em Debito: '+STRZERO(mcont_parc,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total em VALOR mensalidade.: '+TRANSFORM(mtot_ger,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR A PAGAR.....: '+TRANSFORM((mtot_ger*1.02),'9,999,999.99'))
        ELSEIF mop = 2
                mpag := 1
                mtipo := 'por ordem de Responsavel'
                cabecalho(mpag,mtit,mtipo)
                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                DEVPOS(PROW(),51);DEVOUT('Aluno')
                DEVPOS(PROW(),93);DEVOUT('Fone')
                DEVPOS(PROW(),103);DEVOUT('GSTT')
                DEVPOS(PROW(),109);DEVOUT('Fx')
                DEVPOS(PROW(),113);DEVOUT('Parc.')
                DEVPOS(PROW(),120);DEVOUT('Valor')
                DEVPOS(PROW()+1,00);DEVOUT('Endereco')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                imprt('M','C')
                i := 0
                ***************
                SELE('aluno');ORDSETFOCUS(4)
                GO TOP
                ***************
                IF ! aluno->(DBSEEK(mmat))
                        atencao('Nao foi encontrado esta MATRICULA')
                        LOOP
                ENDIF
                mcont_parc := 0
                mqtd_parc := 0
                mflag := ' '
                WHILE mmat = aluno->matricula .AND. ! EOF()
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        INKEY(.1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        ASIZE(m_parcela,0)

                        ***************
                        SELE('cdmen');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                        IF cdmen->(DBSEEK(aluno->matricula))
                                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. SUBSTR(DTOC(cdmen->data_venc),4,2) <= mmes
                                        IF EMPTY(cdmen->data_pag)
                                                AADD(m_parcela,{cdmen->parcela,cdmen->vlr_parc,cdmen->conta})
                                        ENDIF
                                        SKIP
                                ENDDO
                        ENDIF

                        ***************
                        SELE('aluno');ORDSETFOCUS(4)
                        ***************

                        IF LEN(m_parcela) == 0
                                SKIP
                                LOOP
                        ENDIF
                        IF LEN(m_parcela) < mquantd_parc
                                SKIP
                                LOOP
                        ENDIF
                        mfaixa := ver_faixa(m_parcela[1,1])
                        mdesconto := faixa_vlr(mfaixa)
                        IF VAL(m_parcela[1,1]) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                IF ! VAL(m_parcela[1,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        IF mdesconto < 100
                                                mflag := '*'
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                                mcont_aluno ++
                                                DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                                mcont_parc ++
                                                DEVPOS(PROW(),113);DEVOUT(m_parcela[1,1])
                                                IF m_parcela[1,3] <> '*'
                                                        mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                                                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                ELSE
                                                        mvlr_men := m_parcela[1,2]
                                                ENDIF
                                                DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                                DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                                mtot_men := mtot_men + mvlr_men
                                                mtot_ger := mtot_ger + mvlr_men
                                                mqtd_parc ++
                                        ENDIF
                                ENDIF
                        ELSE
                                IF mdesconto < 100
                                        mflag := '*'
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                        DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                        DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                        DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                        mcont_aluno ++
                                        DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                        mcont_parc ++
                                        DEVPOS(PROW(),113);DEVOUT(m_parcela[1,1])
                                        IF m_parcela[1,3] <> '*'
                                                mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                        ELSE
                                                mvlr_men := m_parcela[1,2]
                                        ENDIF
                                        DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                        DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mqtd_parc ++
                                ENDIF
                        ENDIF
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mfaixa := ver_faixa(m_parcela[i,1])
                                mdesconto := faixa_vlr(mfaixa)
                                IF VAL(m_parcela[i,1]) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(m_parcela[i,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                LOOP
                                        ENDIF
                                ENDIF
                                IF mdesconto < 100
                                        IF mflag = ' '
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                        ENDIF
                                        mvlr_men := 0
                                        DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                        DEVPOS(PROW(),113);DEVOUT(m_parcela[i,1])
                                        IF m_parcela[i,3] <> '*'
                                                mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                        ELSE
                                                mvlr_men := m_parcela[i,2]
                                        ENDIF
                                        DEVPOS(PROW(),123);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[i,3])
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mcont_parc ++
                                        mqtd_parc ++
                                        IF mflag = ' '
                                                DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                                mflag := '*'
                                        ENDIF
                                ELSE
                                        LOOP
                                ENDIF
                                IF PROW() >= 59
                                        EJECT
                                        mpag ++
                                        cabecalho(mpag,mtit,mtipo)
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                                        DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                        DEVPOS(PROW(),51);DEVOUT('Aluno')
                                        DEVPOS(PROW(),93);DEVOUT('Fone')
                                        DEVPOS(PROW(),103);DEVOUT('GSTT')
                                        DEVPOS(PROW(),109);DEVOUT('Parc.')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                        IF LEN(m_parcela) > i
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),51);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),93);DEVOUT(aluno->r_fone)
                                                DEVPOS(PROW(),103);DEVOUT(aluno->gstt)
                                                DEVPOS(PROW(),109);DEVOUT(mfaixa)
                                        ENDIF
                                        LOOP
                                ELSEIF LEN(m_parcela) > i
                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                ENDIF
                        NEXT
                        IF ! EMPTY(mtot_men)
                                DEVPOS(PROW()+1,111);DEVOUT('----')
                                DEVPOS(PROW(),120);DEVOUT('-----------')
                                DEVPOS(PROW()+1,89);DEVOUT('T O T A I S: ')
                                DEVPOS(PROW(),113);DEVOUT(STRZERO(mcont_parc,2))
                                DEVPOS(PROW(),122);DEVOUT(TRANSFORM(mtot_men,'99,999.99'))
                                DEVPOS(PROW()+1,89);DEVOUT('TOTAL A PAGAR (+2%): ')
                                DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mtot_men*1.02),'99,999.99'))
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                        ENDIF
                        SKIP
                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                                DEVPOS(PROW(),42);DEVOUT(' Matric.')
                                DEVPOS(PROW(),51);DEVOUT('Aluno')
                                DEVPOS(PROW(),93);DEVOUT('Fone')
                                DEVPOS(PROW(),103);DEVOUT('GSTT')
                                DEVPOS(PROW(),109);DEVOUT('Parc.')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT('Total de ALUNOS em Debito....: '+STRZERO(1,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total de PARCELAS em Debito: '+STRZERO(mcont_parc,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total em VALOR mensalidade.: '+TRANSFORM(mtot_ger,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR A PAGAR.....: '+TRANSFORM((mtot_ger*1.02),'9,999,999.99'))
        ENDIF
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('RELA_DEB.REL')
        ENDIF
ENDDO
RETURN NIL

