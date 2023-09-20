************************************************
* RELATORIO DE ALUNOS EM DEBITOS
************************************************

FUNCTION saf520
***************
LOCAL mprg:='SAF520'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mcont_parc,mdesconto,mvlr_men,mgstt,mtot_men,mtot_ger,mindex,;
      tela,mgst,mmat,mop,mfaixa,mflag,mimp_resp:=' ',mcont_ind:=0,mqtd_parc:=0,mtip_mes:=0,;
      mqtd_dias:=0,mtot_dias:=0,mdias:=0,mtot_multa:=0,mvlr_multa:=0,mcomp:='    ',;
      mdata1,mdata2,mtipo_men,mtot_juros:=0,mvlr_juros:=0,mtot_igpm:=0,mvlr_igpm:=0
PRIVATE m_parcela:={},mquantd_parc
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DE ALUNOS EM DEBITOS','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',132)
lci := 06
cci := 10
lba := 16
cba := 75
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('safigpm','igpm');RETURN NIL;ENDIF
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
        mcomp := '    '
        mdata1 := CTOD('  /  /  ')
        mdata2 := mdata_sis
        mtipo_men := 'ME'
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
        DEVPOS(lci+5,cci+1);DEVOUT('Digite o Periodo  Data Incial.:          a          Data Final')
        DEVPOS(lci+6,cci+1);DEVOUT('Por ordem de:[1] p/Resp.[2]GSTT [3]Aluno [4] Matricula: ')
        DEVPOS(lci+7,cci+1);DEVOUT('Digite o Mes que deseja ser impresso..................: ')
        DEVPOS(lci+8,cci+1);DEVOUT('Digite o Tipo de mensalidade que deseja...............: ')
        DEVPOS(lci+9,cci+1);DEVOUT('Titulo do Relatorio: ')
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
*       @ lci+5,cci+57 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        @ lci+5,cci+33 GET mdata1
        @ lci+5,cci+44 GET mdata2 VALID IF(mdata2 < mdata1,.F.,.T.)
        @ lci+6,cci+57 GET mindex PICT '9' VALID IF(mindex<1 .OR. mindex > 4,.F.,.T.) WHEN mop = 3
        @ lci+7,cci+57 GET mtip_mes PICT '99'
        @ lci+8,cci+57 GET mtipo_men PICT '@!'
        @ lci+9,cci+22 GET mtit
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
                DEVPOS(PROW(),60);DEVOUT('Fx')
                DEVPOS(PROW(),63);DEVOUT('Parc.')
                DEVPOS(PROW(),69);DEVOUT('Vlr.Parc')
                DEVPOS(PROW(),79);DEVOUT('Vl.Multa')
                DEVPOS(PROW(),90);DEVOUT('Dias')
                DEVPOS(PROW(),95);DEVOUT('Vl.Juros')
                DEVPOS(PROW(),104);DEVOUT('Vlr.IGPM')
                DEVPOS(PROW(),117);DEVOUT('Vl.Total')
                DEVPOS(PROW(),127);DEVOUT('ANO')
                DEVPOS(PROW()+1,00);DEVOUT('Matric.')
                DEVPOS(PROW(),07);DEVOUT('Aluno')
                DEVPOS(PROW(),50);DEVOUT('GSTT')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                imprt('M','C')
                i := 0
                mtot_dias := 0
                mtot_multa := 0
                mvlr_multa := 0
                mvlr_juros := 0
                mtot_juros := 0
                mvlr_igpm  := 0
                mtot_igpm  := 0
                WHILE ! EOF()
                        mflag := ' '
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mdesconto := 0
                        mvlr_men := 0
                        mqtd_parc := 0
                        mqtd_dias := 0
                        mtot_men := 0
                        mvlr_multa := 0
                        mvlr_juros := 0
                        mvlr_igpm  := 0
                        ASIZE(m_parcela,0)

                        ***************
                        SELE('cdmen');ORDSETFOCUS(8)
                        GO TOP
                        ***************
                        IF cdmen->(DBSEEK(aluno->matricula))
*                               WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. VAL(SUBSTR(DTOC(cdmen->data_venc),4,2)) <= VAL(mmes)
                                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. cdmen->data_venc <= mdata2
                                        mdesconto := 0
                                        IF ! EMPTY(mtipo_men)
                                                IF mtipo_men <> cdmen->tipo
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDIF
                                        IF EMPTY(cdmen->data_pag)
                                                mdesconto := faixa_vlr(cdmen->bolsa)
                                                IF mdesconto = 100 .AND. cdmen->tipo = 'ME'
                                                        SKIP;LOOP
                                                ENDIF
                                                IF cdmen->data_venc < mdata1
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF mtip_mes > 0
                                                        IF VAL(cdmen->parcela) <> mtip_mes
                                                                SKIP
                                                                LOOP
                                                        ENDIF
                                                ENDIF
*                                               IF mdata_sis > cdmen->data_venc
*                                                       mqtd_dias := mqtd_dias + (mdata_sis - cdmen->data_venc)
*                                                       mtot_dias := mtot_dias + (mdata_sis - cdmen->data_venc)
*                                               ENDIF
*                                                                     1              2                3            4              5            6               7             8
                                                AADD(m_parcela,{cdmen->parcela,cdmen->vlr_parc,cdmen->conta,cdmen->data_venc,cdmen->ano,cdmen->desc_bolsa,cdmen->bolsa,cdmen->tipo})
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
*                       mfaixa := ver_faixa(m_parcela[1,1])
*                       mfaixa := cdmen->bolsa
*                       mdesconto := faixa_vlr(mfaixa)
                        mdesconto := 0
                        mfaixa := m_parcela[1,7]
                        mdesconto := faixa_vlr(m_parcela[1,7])
                        IF mtip_mes > 0 .AND. VAL(m_parcela[1,1]) <> mtip_mes
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                IF ! VAL(m_parcela[1,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        IF mdesconto < 100 .OR. m_parcela[1,8] <> 'ME'
                                                mflag := '*'
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Fone: '+aluno->r_fone)
                                                mcont_aluno ++
                                                DEVPOS(PROW(),60);DEVOUT(m_parcela[1,7])
                                                mcont_parc := mcont_parc + LEN(m_parcela)
                                                DEVPOS(PROW(),64);DEVOUT(m_parcela[1,1])
                                                IF m_parcela[1,3] <> '*'
*                                                       mvlr_men := vlr_men(mgstt,m_parcela[1,1])
*                                                       mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                        mvlr_men := m_parcela[1,2] - m_parcela[1,6]
                                                ELSE
                                                        mvlr_men := m_parcela[1,2]
                                                ENDIF
                                                DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                                IF mdata_sis > m_parcela[1,4]
                                                        mdias := (mdata_sis - m_parcela[1,4])
                                                        DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_men*(set->vlr_multa/100),'9,999.99'))
                                                        mvlr_multa := mvlr_multa + mvlr_men*(set->vlr_multa/100)
                                                        mtot_multa := mtot_multa + mvlr_men*(set->vlr_multa/100)
                                                ELSE
                                                        DEVPOS(PROW(),79);DEVOUT(TRANSFORM(0,'9,999.99'))
                                                        mdias := 0
                                                ENDIF
                                                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mdias,'9,999'))
                                                DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mdias * (set->vlr_juros * mvlr_men),'9,999.99'))
                                                IF mdias > 30
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(mvlr_men * (ver_igpm(m_parcela[1,1])/100),'999.99'))
                                                        mvlr_igpm := mvlr_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        mtot_igpm := mtot_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[1,4],(set->vlr_multa/100),0))+(mvlr_men*(ver_igpm(m_parcela[1,1])/100)),'9,999.99'))
                                                ELSE
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(0,'999.99'))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[1,4],(set->vlr_multa/100),0)),'9,999.99'))
                                                ENDIF
*                                               DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[1,4],(set->vlr_multa/100),0))+(mvlr_men*(ver_igpm(m_parcela[1,1])/100)),'9,999.99'))
                                                DEVPOS(PROW(),127);DEVOUT(m_parcela[1,5])
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->gstt)
                                                mvlr_juros := mvlr_juros + mdias * (set->vlr_juros * mvlr_men)
                                                mtot_juros := mtot_juros + mdias * (set->vlr_juros * mvlr_men)
                                                mqtd_dias := mqtd_dias + mdias
                                                mtot_men := mtot_men + mvlr_men
                                                mtot_ger := mtot_ger + mvlr_men
                                                mcont_parc ++
                                                mqtd_parc ++
                                        ENDIF
                                ENDIF
                        ELSE
                                IF mdesconto < 100 .OR. m_parcela[1,8] <> 'ME'
                                        mflag := '*'
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Fone: '+aluno->r_fone)
                                        mcont_aluno ++
                                        DEVPOS(PROW(),60);DEVOUT(m_parcela[1,7])
                                        DEVPOS(PROW(),64);DEVOUT(m_parcela[1,1])
                                        IF m_parcela[1,3] <> '*'
*                                               mvlr_men := vlr_men(mgstt,m_parcela[1,1])
*                                               mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                mvlr_men := m_parcela[1,2] - m_parcela[1,6]
                                        ELSE
                                                mvlr_men := m_parcela[1,2]
                                        ENDIF
                                        DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                        IF mdata_sis > m_parcela[1,4]
                                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_men*(set->vlr_multa/100),'9,999.99'))
                                                mvlr_multa := mvlr_multa + mvlr_men*(set->vlr_multa/100)
                                                mtot_multa := mtot_multa + mvlr_men*(set->vlr_multa/100)
                                                mdias := (mdata_sis - m_parcela[1,4])
                                        ELSE
                                                mdias := 0
                                        ENDIF
                                        DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mdias,'9,999'))
                                        DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mdias * (set->vlr_juros * mvlr_men),'9,999.99'))
                                                IF mdias > 30
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(mvlr_men * (ver_igpm(m_parcela[1,1])/100),'999.99'))
                                                        mvlr_igpm := mvlr_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        mtot_igpm := mtot_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[1,4],(set->vlr_multa/100),0))+(mvlr_men*(ver_igpm(m_parcela[1,1])/100)),'9,999.99'))
                                                ELSE
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(0,'999.99'))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[1,4],(set->vlr_multa/100),0)),'9,999.99'))
                                                ENDIF
*                                       DEVPOS(PROW(),115);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*(set->vlr_multa/100)),'9,999.99'))
                                        DEVPOS(PROW(),127);DEVOUT(m_parcela[1,5])
                                        DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->aluno)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->gstt)
                                        mvlr_juros := mvlr_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mtot_juros := mtot_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mqtd_dias := mqtd_dias + mdias
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mcont_parc ++
                                        mqtd_parc ++
                                ENDIF
                        ENDIF
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mdesconto := 0
                                mfaixa := m_parcela[i,7]
*                               mfaixa := cdmen->bolsa
*                               mfaixa := ver_faixa(m_parcela[i,1])
*                               IF mfaixa = 'S'
*                                       SKIP
*                                       LOOP
*                               ENDIF
                                IF mtip_mes > 0
                                        IF VAL(m_parcela[i,1]) <> mtip_mes
                                                LOOP
                                        ENDIF
                                ENDIF
                                mdesconto := faixa_vlr(m_parcela[i,7])
*                                mdesconto := faixa_vlr(mfaixa)
                                IF mdesconto = 100 .AND. m_parcela[i,8] = 'ME'
                                        LOOP
                                ENDIF
                                IF VAL(m_parcela[i,1]) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                        LOOP
                                ENDIF
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(m_parcela[i,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                LOOP
                                        ENDIF
                                ENDIF
                                IF mdesconto < 100 .OR. m_parcela[i,8] <> 'ME'
                                        IF mflag = ' '
*                                               mflag := '*'
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('Fone: '+aluno->r_fone)
                                                mcont_aluno ++
                                        ENDIF
                                        mvlr_men := 0
                                        DEVPOS(PROW(),60);DEVOUT(m_parcela[i,7])
                                        DEVPOS(PROW(),64);DEVOUT(m_parcela[i,1])
*                                       mcont_parc := mcont_parc + LEN(m_parcela)
                                        IF m_parcela[i,3] <> '*'
*                                               mvlr_men := vlr_men(mgstt,m_parcela[1,1])
*                                               mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                mvlr_men := m_parcela[i,2] - m_parcela[i,6]
                                        ELSE
                                                mvlr_men := m_parcela[i,2]
                                        ENDIF
                                        DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[i,3])
                                        IF mdata_sis > m_parcela[i,4]
                                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_men*(set->vlr_multa/100),'9,999.99'))
                                                mvlr_multa := mvlr_multa + mvlr_men*(set->vlr_multa/100)
                                                mtot_multa := mtot_multa + mvlr_men*(set->vlr_multa/100)
                                                mdias := (mdata_sis - m_parcela[i,4])
                                        ELSE
                                                mdias := 0
                                        ENDIF
                                        DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mdias,'9,999'))
                                        DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mdias * (set->vlr_juros * mvlr_men),'9,999.99'))
                                                IF mdias > 30
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(mvlr_men * (ver_igpm(m_parcela[i,1])/100),'999.99'))
                                                        mvlr_igpm := mvlr_igpm + (mvlr_men*(ver_igpm(m_parcela[i,1])/100))
                                                        mtot_igpm := mtot_igpm + (mvlr_men*(ver_igpm(m_parcela[i,1])/100))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[i,4],(set->vlr_multa/100),0))+(mvlr_men*(ver_igpm(m_parcela[i,1])/100)),'9,999.99'))
                                                ELSE
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(0,'999.99'))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[i,4],(set->vlr_multa/100),0)),'9,999.99'))
                                                ENDIF
*                                       DEVPOS(PROW(),115);DEVOUT(TRANSFORM(mvlr_men+(mdias * set->vlr_juros)+(mvlr_men*(set->vlr_multa/100)),'9,999.99'))
                                        DEVPOS(PROW(),127);DEVOUT(m_parcela[i,5])
                                        mvlr_juros := mvlr_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mtot_juros := mtot_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mqtd_dias := mqtd_dias + mdias
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mqtd_parc ++
                                        mcont_parc ++
                                        IF mflag = ' '
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->gstt)
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
                                        DEVPOS(PROW(),60);DEVOUT('Fx')
                                        DEVPOS(PROW(),63);DEVOUT('Parc.')
                                        DEVPOS(PROW(),69);DEVOUT('Vlr.Parc')
                                        DEVPOS(PROW(),79);DEVOUT('Vl.Multa')
                                        DEVPOS(PROW(),90);DEVOUT('Dias')
                                        DEVPOS(PROW(),95);DEVOUT('Vl.Juros')
                                        DEVPOS(PROW(),104);DEVOUT('Vlr.IGPM')
                                        DEVPOS(PROW(),117);DEVOUT('Vl.Total')
                                        DEVPOS(PROW(),127);DEVOUT('ANO')
                                        DEVPOS(PROW()+1,00);DEVOUT('Matric.')
                                        DEVPOS(PROW(),07);DEVOUT('Aluno')
                                        DEVPOS(PROW(),50);DEVOUT('GSTT')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                        imprt('M','C')
                                        IF LEN(m_parcela) > i
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('Fone: '+aluno->r_fone)
                                        ENDIF
                                        LOOP
                                ELSEIF LEN(m_parcela) > i
                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                ENDIF
                        NEXT
                        IF ! EMPTY(mtot_men)
                                DEVPOS(PROW()+1,64);DEVOUT('--')
                                DEVPOS(PROW(),69);DEVOUT('--------')
                                DEVPOS(PROW(),79);DEVOUT('--------')
                                DEVPOS(PROW(),89);DEVOUT('-----')
                                DEVPOS(PROW(),95);DEVOUT('--------')
                                DEVPOS(PROW(),104);DEVOUT('--------')
                                DEVPOS(PROW(),115);DEVOUT('----------')
                                DEVPOS(PROW()+1,00);DEVOUT('T O T A I S...................................................: ')
                                DEVPOS(PROW(),64);DEVOUT(TRANSFORM(mqtd_parc,'99'))
                                DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mtot_men,'9,999.99'))
                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_multa,'9,999.99'))
                                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mqtd_dias/mqtd_parc,'9,999'))
                                DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mvlr_juros,'9,999.99'))
                                DEVPOS(PROW(),104);DEVOUT(TRANSFORM(mvlr_igpm,'9,999.99'))
                                DEVPOS(PROW(),115);DEVOUT(TRANSFORM(mtot_men + mvlr_juros + mvlr_multa+mvlr_igpm,'999,999.99'))
                                DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                DEVPOS(PROW()+1,00);DEVOUT('Filiacao: Pai: '+aluno->pai+' -  Mae: '+aluno->mae)
                                DEVPOS(PROW()+1,00);DEVOUT('OBS.: '+aluno->obs)
                                mtot_dias := mtot_dias + mqtd_dias
                                mqtd_dias := 0
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                        ENDIF
                        SKIP
                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                                DEVPOS(PROW(),60);DEVOUT('Fx')
                                DEVPOS(PROW(),63);DEVOUT('Parc.')
                                DEVPOS(PROW(),69);DEVOUT('Vlr.Parc')
                                DEVPOS(PROW(),79);DEVOUT('Vl.Multa')
                                DEVPOS(PROW(),90);DEVOUT('Dias')
                                DEVPOS(PROW(),95);DEVOUT('Vl.Juros')
                                DEVPOS(PROW(),104);DEVOUT('Vlr.IGPM')
                                DEVPOS(PROW(),117);DEVOUT('Vl.Total')
                                DEVPOS(PROW(),127);DEVOUT('ANO')
                                DEVPOS(PROW()+1,00);DEVOUT('Matric.')
                                DEVPOS(PROW(),07);DEVOUT('Aluno')
                                DEVPOS(PROW(),50);DEVOUT('GSTT')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT('Total de ALUNOS em Debito....: '+STRZERO(mcont_aluno,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total de PARCELAS em Debito: '+STRZERO(mcont_parc,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total em VALOR mensalidade.: '+TRANSFORM(mtot_ger,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR MULTA 2%....: '+TRANSFORM(mtot_multa,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR Dias Atrasos: '+TRANSFORM(mtot_juros,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR IGPM R$.....: '+TRANSFORM(mtot_igpm,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('                             ------------')
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR GERAL.......: '+TRANSFORM(mtot_ger+mtot_multa+mtot_juros+mtot_igpm,'9,999,999.99'))
        ELSEIF mop = 1
                mflag := ' '
                mpag := 1
                mtipo := 'por ordem de Responsavel'
                cabecalho(mpag,mtit,mtipo)
                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                DEVPOS(PROW(),60);DEVOUT('Fx')
                DEVPOS(PROW(),63);DEVOUT('Parc.')
                DEVPOS(PROW(),69);DEVOUT('Vlr.Parc')
                DEVPOS(PROW(),79);DEVOUT('Vl.Multa')
                DEVPOS(PROW(),90);DEVOUT('Dias')
                DEVPOS(PROW(),95);DEVOUT('Vl.Juros')
                DEVPOS(PROW(),104);DEVOUT('Vlr.IGPM')
                DEVPOS(PROW(),117);DEVOUT('Vl.Total')
                DEVPOS(PROW(),127);DEVOUT('ANO')
                DEVPOS(PROW()+1,00);DEVOUT('Matric.')
                DEVPOS(PROW(),07);DEVOUT('Aluno')
                DEVPOS(PROW(),50);DEVOUT('GSTT')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                imprt('M','C')
                i := 0
                mtot_dias := 0
                mtot_multa := 0
                mvlr_multa := 0
                mtot_juros := 0
                mvlr_juros := 0
                mtot_igpm  := 0
                mvlr_igpm  := 0
                WHILE mgst = aluno->gstt .AND. ! EOF()
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        mqtd_parc := 0
                        mqtd_dias := 0
                        mvlr_multa := 0
                        mvlr_juros := 0
                        mvlr_igpm  := 0
                        ASIZE(m_parcela,0)

                        ***************
                        SELE('cdmen');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                        IF cdmen->(DBSEEK(aluno->matricula))
*                               WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. SUBSTR(DTOC(cdmen->data_venc),4,2) <= mmes
                                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. cdmen->data_venc <= mdata2
                                        mdesconto := 0
                                        IF ! EMPTY(mtipo_men)
                                                IF mtipo_men <> cdmen->tipo
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDIF
                                        IF EMPTY(cdmen->data_pag)
                                                IF cdmen->data_venc < mdata1
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                mdesconto := faixa_vlr(cdmen->bolsa)
                                                IF mdesconto = 100 .AND. cdmen->tipo = 'ME'
                                                        SKIP;LOOP
                                                ENDIF
                                                IF mtip_mes > 0
                                                        IF VAL(cdmen->parcela) <> mtip_mes
                                                                SKIP
                                                                LOOP
                                                        ENDIF
                                                ENDIF
*                                               IF mdata2 > cdmen->data_venc
*                                                       mqtd_dias := mqtd_dias + (mdata2 - cdmen->data_venc)
*                                               ENDIF
                                                AADD(m_parcela,{cdmen->parcela,cdmen->vlr_parc,cdmen->conta,cdmen->data_venc,cdmen->ano,cdmen->desc_bolsa,cdmen->bolsa,cdmen->tipo})
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
                        mdesconto := 0
                        mfaixa := m_parcela[1,7]
*                       mfaixa := cdmen->bolsa
*                       mfaixa := ver_faixa(m_parcela[1,1])
*                        IF mfaixa = 'S'
*                                SKIP
*                                LOOP
*                        ENDIF
*                       mdesconto := faixa_vlr(mfaixa)
*                       IF mdesconto = 100 .AND. EMPTY(mtipo_men)
*                               SKIP;LOOP
*                       ENDIF
                        mdesconto := faixa_vlr(m_parcela[1,7])
                        IF ! EMPTY(aluno->dt_tranf)
                                IF ! VAL(m_parcela[1,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        IF mdesconto < 100 .OR. m_parcela[1,8] <> 'ME'
                                                mflag := '*'
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Fone: '+aluno->r_fone)
                                                mcont_aluno ++
                                                DEVPOS(PROW(),60);DEVOUT(m_parcela[1,7])
                                                DEVPOS(PROW(),64);DEVOUT(m_parcela[1,1])
                                                IF m_parcela[1,3] <> '*'
*                                                       mvlr_men := vlr_men(mgstt,m_parcela[1,1])
*                                                       mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                        mvlr_men := m_parcela[1,2] - m_parcela[1,6]
                                                ELSE
                                                        mvlr_men := m_parcela[1,2]
                                                ENDIF
                                                DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                                IF mdata2 > m_parcela[1,4]
                                                        mdias := (mdata2 - m_parcela[1,4])
                                                        DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_men*(set->vlr_multa/100),'9,999.99'))
                                                        mvlr_multa := mvlr_multa + mvlr_men*(set->vlr_multa/100)
                                                        mtot_multa := mtot_multa + mvlr_men*(set->vlr_multa/100)
                                                ELSE
                                                        DEVPOS(PROW(),79);DEVOUT(TRANSFORM(0,'9,999.99'))
                                                        mdias := 0
                                                ENDIF
                                                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mdias,'9,999'))
                                                DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mdias * (set->vlr_juros * mvlr_men),'9,999.99'))
                                                IF mdias > 30
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(mvlr_men * (ver_igpm(m_parcela[1,1])/100),'999.99'))
                                                        mvlr_igpm := mvlr_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        mtot_igpm := mtot_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata2 > m_parcela[1,4],(set->vlr_multa/100),0))+(mvlr_men*(ver_igpm(m_parcela[1,1])/100)),'9,999.99'))
                                                ELSE
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(0,'999.99'))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata2 > m_parcela[1,4],(set->vlr_multa/100),0)),'9,999.99'))
                                                ENDIF
*                                               DEVPOS(PROW(),115);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*(set->vlr_multa/100),'9,999.99'),'9,999.99'))
                                                DEVPOS(PROW(),127);DEVOUT(m_parcela[1,5])
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->gstt)
                                                mvlr_juros := mvlr_juros + mdias * (set->vlr_juros * mvlr_men)
                                                mtot_juros := mtot_juros + mdias * (set->vlr_juros * mvlr_men)
                                                mqtd_dias := mqtd_dias + mdias
                                                mtot_men := mtot_men + mvlr_men
                                                mtot_ger := mtot_ger + mvlr_men
                                                mcont_parc ++
                                                mqtd_parc ++
                                        ENDIF
                                ENDIF
                        ELSE
                                IF mdesconto < 100 .OR. m_parcela[1,8] <> 'ME'
                                        mflag := '*'
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Fone: '+aluno->r_fone)
                                        mcont_aluno ++
                                        DEVPOS(PROW(),60);DEVOUT(m_parcela[1,7])
                                        DEVPOS(PROW(),64);DEVOUT(m_parcela[1,1])
                                        IF m_parcela[1,3] <> '*'
*                                               mvlr_men := vlr_men(mgstt,m_parcela[1,1])
*                                               mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                mvlr_men := m_parcela[1,2] - m_parcela[1,6]
                                        ELSE
                                                mvlr_men := m_parcela[1,2]
                                        ENDIF
                                        DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                        IF mdata2 > m_parcela[1,4]
                                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_men*(set->vlr_multa/100),'9,999.99'))
                                                mvlr_multa := mvlr_multa + mvlr_men*(set->vlr_multa/100)
                                                mtot_multa := mtot_multa + mvlr_men*(set->vlr_multa/100)
                                                mdias := (mdata2 - m_parcela[1,4])
                                        ELSE
                                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(0,'9,999.99'))
                                                mdias := 0
                                        ENDIF
                                        DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mdias,'9,999'))
                                        DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mdias * (set->vlr_juros * mvlr_men),'9,999.99'))
                                                IF mdias > 30
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(mvlr_men * (ver_igpm(m_parcela[1,1])/100),'999.99'))
                                                        mvlr_igpm := mvlr_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        mtot_igpm := mtot_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata2 > m_parcela[1,4],(set->vlr_multa/100),0))+(mvlr_men*(ver_igpm(m_parcela[1,1])/100)),'9,999.99'))
                                                ELSE
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(0,'999.99'))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata2 > m_parcela[1,4],(set->vlr_multa/100),0)),'9,999.99'))
                                                ENDIF
*                                       DEVPOS(PROW(),115);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*(set->vlr_multa/100)),'9,999.99'))
                                        DEVPOS(PROW(),127);DEVOUT(m_parcela[1,5])
                                        DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->aluno)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->gstt)
                                        mvlr_juros := mvlr_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mtot_juros := mtot_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mqtd_dias := mqtd_dias + mdias
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mqtd_parc ++
                                        mcont_parc ++
                                ENDIF
                        ENDIF
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mdesconto := 0
                                mfaixa := m_parcela[i,7]
*                               mfaixa := cdmen->bolsa
*                               mfaixa := ver_faixa(m_parcela[i,1])
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
                                mdesconto := faixa_vlr(m_parcela[i,7])
*                                mdesconto := faixa_vlr(mfaixa)
                                IF mdesconto = 100 .AND. m_parcela[i,8] = 'ME'
                                        LOOP
                                ENDIF
                                IF VAL(m_parcela[i,1]) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                        LOOP
                                ENDIF
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(m_parcela[i,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                LOOP
                                        ENDIF
                                ENDIF
                                IF mdesconto < 100 .OR. m_parcela[i,8] <> 'ME'
                                        IF mflag = ' '
*                                               mflag := '*'
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('Fone: '+aluno->r_fone)
                                                mcont_aluno ++
                                        ENDIF
                                        mvlr_men := 0
                                        DEVPOS(PROW(),60);DEVOUT(m_parcela[i,7])
                                        DEVPOS(PROW(),64);DEVOUT(m_parcela[i,1])
                                        IF m_parcela[i,3] <> '*'
*                                               mvlr_men := vlr_men(mgstt,m_parcela[i,1])
*                                               mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                mvlr_men := m_parcela[i,2] - m_parcela[i,6]
                                        ELSE
                                                mvlr_men := m_parcela[i,2]
                                        ENDIF
                                        DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[i,3])
                                        IF mdata2 > m_parcela[i,4]
                                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_men*(set->vlr_multa/100),'9,999.99'))
                                                mvlr_multa := mvlr_multa + mvlr_men*(set->vlr_multa/100)
                                                mtot_multa := mtot_multa + mvlr_men*(set->vlr_multa/100)
                                                mdias := (mdata2 - m_parcela[i,4])
                                        ELSE
                                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(0,'9,999.99'))
                                                mdias := 0
                                        ENDIF
                                        DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mdias,'9,999'))
                                        DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mdias * (set->vlr_juros * mvlr_men),'9,999.99'))
                                                IF mdias > 30
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(mvlr_men * (ver_igpm(m_parcela[i,1])/100),'999.99'))
                                                        mvlr_igpm := mvlr_igpm + (mvlr_men*(ver_igpm(m_parcela[i,1])/100))
                                                        mtot_igpm := mtot_igpm + (mvlr_men*(ver_igpm(m_parcela[i,1])/100))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata2 > m_parcela[i,4],(set->vlr_multa/100),0))+(mvlr_men*(ver_igpm(m_parcela[i,1])/100)),'9,999.99'))
                                                ELSE
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(0,'999.99'))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata2 > m_parcela[i,4],(set->vlr_multa/100),0)),'9,999.99'))
                                                ENDIF
*                                       DEVPOS(PROW(),115);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*(set->vlr_multa/100)),'9,999.99'))
                                        DEVPOS(PROW(),127);DEVOUT(m_parcela[i,5])
                                        mvlr_juros := mvlr_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mtot_juros := mtot_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mqtd_dias := mqtd_dias + mdias
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mqtd_parc ++
                                        mcont_parc ++
                                        IF mflag = ' '
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->gstt)
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
                                        DEVPOS(PROW(),60);DEVOUT('Fx')
                                        DEVPOS(PROW(),63);DEVOUT('Parc.')
                                        DEVPOS(PROW(),69);DEVOUT('Vlr.Parc')
                                        DEVPOS(PROW(),79);DEVOUT('Vl.Multa')
                                        DEVPOS(PROW(),90);DEVOUT('Dias')
                                        DEVPOS(PROW(),95);DEVOUT('Vl.Juros')
                                        DEVPOS(PROW(),104);DEVOUT('Vlr.IGPM')
                                        DEVPOS(PROW(),117);DEVOUT('Vl.Total')
                                        DEVPOS(PROW(),127);DEVOUT('ANO')
                                        DEVPOS(PROW()+1,00);DEVOUT('Matric.')
                                        DEVPOS(PROW(),07);DEVOUT('Aluno')
                                        DEVPOS(PROW(),50);DEVOUT('GSTT')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                        imprt('M','C')
                                        IF LEN(m_parcela) > i
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),PCOL()+3);DEVOUT('Fone: '+aluno->r_fone)
                                        ENDIF
                                        LOOP
                                ELSEIF LEN(m_parcela) > i
                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                ENDIF
                        NEXT
                        IF ! EMPTY(mtot_men)
                                DEVPOS(PROW()+1,64);DEVOUT('--')
                                DEVPOS(PROW(),69);DEVOUT('--------')
                                DEVPOS(PROW(),79);DEVOUT('--------')
                                DEVPOS(PROW(),89);DEVOUT('-----')
                                DEVPOS(PROW(),95);DEVOUT('--------')
                                DEVPOS(PROW(),104);DEVOUT('--------')
                                DEVPOS(PROW(),115);DEVOUT('----------')
                                DEVPOS(PROW()+1,00);DEVOUT('T O T A I S...................................................: ')
                                DEVPOS(PROW(),64);DEVOUT(TRANSFORM(mqtd_parc,'99'))
                                DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mtot_men,'9,999.99'))
                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_multa,'9,999.99'))
                                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mqtd_dias/mqtd_parc,'9,999'))
                                DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mvlr_juros,'9,999.99'))
                                DEVPOS(PROW(),104);DEVOUT(TRANSFORM(mvlr_igpm,'9,999.99'))
                                DEVPOS(PROW(),113);DEVOUT(TRANSFORM(mtot_men + mvlr_juros + mvlr_multa + mvlr_igpm,'999,999.99'))
                                DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                DEVPOS(PROW()+1,00);DEVOUT('Filiacao: Pai: '+aluno->pai+' -  Mae: '+aluno->mae)
                                DEVPOS(PROW()+1,00);DEVOUT('OBS.: '+aluno->obs)
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                                mtot_dias := mtot_dias + mqtd_dias
                                mqtd_dias := 0
                        ENDIF
                        SKIP
                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                                DEVPOS(PROW(),60);DEVOUT('Fx')
                                DEVPOS(PROW(),63);DEVOUT('Parc.')
                                DEVPOS(PROW(),69);DEVOUT('Vlr.Parc')
                                DEVPOS(PROW(),79);DEVOUT('Vl.Multa')
                                DEVPOS(PROW(),90);DEVOUT('Dias')
                                DEVPOS(PROW(),95);DEVOUT('Vl.Juros')
                                DEVPOS(PROW(),104);DEVOUT('Vlr.IGPM')
                                DEVPOS(PROW(),117);DEVOUT('Vl.Total')
                                DEVPOS(PROW(),127);DEVOUT('ANO')
                                DEVPOS(PROW()+1,00);DEVOUT('Matric.')
                                DEVPOS(PROW(),07);DEVOUT('Aluno')
                                DEVPOS(PROW(),50);DEVOUT('GSTT')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT('Total de ALUNOS em Debito....: '+STRZERO(mcont_aluno,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total de PARCELAS em Debito: '+STRZERO(mcont_parc,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total em VALOR mensalidade.: '+TRANSFORM(mtot_ger,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR MULTA 2%....: '+TRANSFORM(mtot_multa,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR Dias Atrasos: '+TRANSFORM(mtot_juros,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR IGPM R$.....: '+TRANSFORM(mtot_igpm,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('                             ------------')
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR GERAL.......: '+TRANSFORM(mtot_ger+mtot_multa+mtot_juros+mtot_igpm,'9,999,999.99'))
        ELSEIF mop = 2
                mpag := 1
                mtipo := 'por ordem de Responsavel'
                cabecalho(mpag,mtit,mtipo)
                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                DEVPOS(PROW(),60);DEVOUT('Fx')
                DEVPOS(PROW(),63);DEVOUT('Parc.')
                DEVPOS(PROW(),69);DEVOUT('Vlr.Parc')
                DEVPOS(PROW(),79);DEVOUT('Vl.Multa')
                DEVPOS(PROW(),90);DEVOUT('Dias')
                DEVPOS(PROW(),95);DEVOUT('Vl.Juros')
                DEVPOS(PROW(),104);DEVOUT('Vlr.IGPM')
                DEVPOS(PROW(),117);DEVOUT('Vl.Total')
                DEVPOS(PROW(),127);DEVOUT('ANO')
                DEVPOS(PROW()+1,00);DEVOUT('Matric.')
                DEVPOS(PROW(),07);DEVOUT('Aluno')
                DEVPOS(PROW(),50);DEVOUT('GSTT')
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
                mtot_dias := 0
                mtot_multa := 0
                mvlr_multa := 0
                mtot_juros := 0
                mvlr_juros := 0
                mtot_igpm  := 0
                mvlr_igpm  := 0
                WHILE mmat = aluno->matricula .AND. ! EOF()
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        mqtd_dias := 0
                        mvlr_multa := 0
                        mvlr_juros := 0
                        mvlr_igpm  := 0
                        ASIZE(m_parcela,0)

                        ***************
                        SELE('cdmen');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                        IF cdmen->(DBSEEK(aluno->matricula))
*                               WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. SUBSTR(DTOC(cdmen->data_venc),4,2) <= mmes
                                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. cdmen->data_venc <= mdata2
                                        mdesconto := 0
                                        IF ! EMPTY(mtipo_men) .AND. mtipo_men <> cdmen->tipo
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF EMPTY(cdmen->data_pag)
                                                mdesconto := faixa_vlr(cdmen->bolsa)
                                                IF mdesconto = 100 .AND. EMPTY(mtipo_men)
                                                        SKIP;LOOP
                                                ENDIF
                                                IF cdmen->data_venc < mdata1
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF mtip_mes > 0 .AND. VAL(cdmen->parcela) <> mtip_mes
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                AADD(m_parcela,{cdmen->parcela,cdmen->vlr_parc,cdmen->conta,cdmen->data_venc,cdmen->ano,cdmen->desc_bolsa,cdmen->bolsa})
                                        ENDIF
                                        SKIP
                                ENDDO
                        ENDIF

                        ***************
                        SELE('aluno');ORDSETFOCUS(4)
                        ***************

                        IF LEN(m_parcela) = 0
                                SKIP
                                LOOP
                        ENDIF
                        IF LEN(m_parcela) < mquantd_parc
                                SKIP
                                LOOP
                        ENDIF
                        mdesconto := 0
                        mfaixa := m_parcela[1,7]
                        mdesconto := faixa_vlr(m_parcela[1,7])
                        IF mdesconto = 100 .AND. EMPTY(mtipo_men)
                                SKIP;LOOP
                        ENDIF
                        IF VAL(m_parcela[1,1]) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2)) .AND. (aluno->data_ing < mdata1)
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                IF ! VAL(m_parcela[1,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        IF mdesconto < 100
                                                mflag := '*'
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Fone: '+aluno->r_fone)
                                                mcont_aluno ++
                                                DEVPOS(PROW(),60);DEVOUT(m_parcela[1,7])
                                                DEVPOS(PROW(),64);DEVOUT(m_parcela[1,1])
                                                IF m_parcela[1,3] <> '*'
*                                                       mvlr_men := vlr_men(mgstt,m_parcela[1,1])
*                                                       mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                        mvlr_men := m_parcela[1,2] - m_parcela[1,6]
                                                ELSE
                                                        mvlr_men := m_parcela[1,2]
                                                ENDIF
                                                DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                                IF mdata_sis > m_parcela[1,4]
                                                        mdias := (mdata_sis - m_parcela[1,4])
                                                        DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_men*(set->vlr_multa/100),'9,999.99'))
                                                        mvlr_multa := mvlr_multa + mvlr_men*(set->vlr_multa/100)
                                                        mtot_multa := mtot_multa + mvlr_men*(set->vlr_multa/100)
                                                ELSE
                                                        DEVPOS(PROW(),79);DEVOUT(TRANSFORM(0,'9,999.99'))
                                                        mdias := 0
                                                ENDIF
                                                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mdias,'9,999'))
                                                DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mdias * (set->vlr_juros * mvlr_men),'9,999.99'))
                                                IF mdias > 30
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(mvlr_men * (ver_igpm(m_parcela[1,1])/100),'999.99'))
                                                        mvlr_igpm := mvlr_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        mtot_igpm := mtot_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[1,4],(set->vlr_multa/100),0))+(mvlr_men*(ver_igpm(m_parcela[1,1])/100)),'9,999.99'))
                                                ELSE
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(0,'999.99'))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[1,4],(set->vlr_multa/100),0)),'9,999.99'))
                                                ENDIF
*                                                DEVPOS(PROW(),115);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*(set->vlr_multa/100)),'9,999.99'))
                                                DEVPOS(PROW(),127);DEVOUT(m_parcela[1,5])
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->gstt)
                                                mvlr_juros := mvlr_juros + mdias * (set->vlr_juros * mvlr_men)
                                                mtot_juros := mtot_juros + mdias * (set->vlr_juros * mvlr_men)
                                                mqtd_dias := mqtd_dias + mdias
                                                mtot_men := mtot_men + mvlr_men
                                                mtot_ger := mtot_ger + mvlr_men
                                                mqtd_parc ++
                                                mcont_parc ++
                                        ENDIF
                                ENDIF
                        ELSE
                                IF mdesconto < 100
                                        mflag := '*'
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Fone: '+aluno->r_fone)
                                        mcont_Aluno ++
                                        DEVPOS(PROW(),60);DEVOUT(m_parcela[1,7])
                                        mcont_parc ++
                                        DEVPOS(PROW(),64);DEVOUT(m_parcela[1,1])
                                        IF m_parcela[1,3] <> '*'
*                                               mvlr_men := vlr_men(mgstt,m_parcela[1,1])
*                                               mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                mvlr_men := m_parcela[1,2] - m_parcela[1,6]
                                        ELSE
                                                mvlr_men := m_parcela[1,2]
                                        ENDIF
                                        DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[1,3])
                                        IF mdata_sis > m_parcela[1,4]
                                                mdias := (mdata_sis - m_parcela[1,4])
                                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_men*(set->vlr_multa/100),'9,999.99'))
                                                mvlr_multa := mvlr_multa + mvlr_men*(set->vlr_multa/100)
                                                mtot_multa := mtot_multa + mvlr_men*(set->vlr_multa/100)
                                        ELSE
                                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(0,'9,999.99'))
                                                mdias := 0
                                        ENDIF
                                        DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mdias,'9,999'))
                                        DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mdias * (set->vlr_juros * mvlr_men),'9,999.99'))
                                                IF mdias > 30
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(mvlr_men * (ver_igpm(m_parcela[1,1])/100),'999.99'))
                                                        mvlr_igpm := mvlr_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        mtot_igpm := mtot_igpm + (mvlr_men*(ver_igpm(m_parcela[1,1])/100))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[1,4],(set->vlr_multa/100),0))+(mvlr_men*(ver_igpm(m_parcela[1,1])/100)),'9,999.99'))
                                                ELSE
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(0,'999.99'))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[1,4],(set->vlr_multa/100),0)),'9,999.99'))
                                                ENDIF
*                                       DEVPOS(PROW(),115);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*(set->vlr_multa/100)),'9,999.99'))
                                        DEVPOS(PROW(),127);DEVOUT(m_parcela[1,5])
                                        DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->aluno)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->gstt)
                                        mvlr_juros := mvlr_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mtot_juros := mtot_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mqtd_dias := mqtd_dias + mdias
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mqtd_parc ++
                                ENDIF
                        ENDIF
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mdesconto := 0
                                mfaixa := m_parcela[i,7]
*                               mfaixa := cdmen->bolsa
*                               mfaixa := ver_faixa(m_parcela[i,1])
*                               mdesconto := faixa_vlr(mfaixa)
                                mdesconto := faixa_vlr(m_parcela[i,7])
                                IF mdesconto = 100 .AND. EMPTY(mtipo_men)
                                        LOOP
                                ENDIF
                                IF VAL(m_parcela[i,1]) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
*                                       SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(aluno->dt_tranf) .AND. VAL(m_parcela[i,1]) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        LOOP
                                ENDIF
                                IF mdesconto < 100
                                        IF mflag = ' '
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Fone: '+aluno->r_fone)
                                        ENDIF
                                        mvlr_men := 0
                                        DEVPOS(PROW(),60);DEVOUT(m_parcela[i,7])
                                        DEVPOS(PROW(),64);DEVOUT(m_parcela[i,1])
                                        IF m_parcela[i,3] <> '*'
*                                               mvlr_men := vlr_men(mgstt,m_parcela[i,1])
*                                               mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                                mvlr_men := m_parcela[i,2] - m_parcela[i,6]
                                        ELSE
                                                mvlr_men := m_parcela[i,2]
                                        ENDIF
                                        DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mvlr_men,'9,999.99')+m_parcela[i,3])
                                        IF mdata_sis > m_parcela[i,4]
                                                mdias := (mdata_sis - m_parcela[i,4])
                                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_men*(set->vlr_multa/100),'9,999.99'))
                                                mvlr_multa := mvlr_multa + mvlr_men*(set->vlr_multa/100)
                                                mtot_multa := mtot_multa + mvlr_men*(set->vlr_multa/100)
                                        ELSE
                                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(0,'9,999.99'))
                                                mdias := 0
                                        ENDIF
                                        DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mdias,'9,999'))
                                        DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mdias * (set->vlr_juros * mvlr_men),'9,999.99'))
                                                IF mdias > 30
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(mvlr_men * (ver_igpm(m_parcela[i,1])/100),'999.99'))
                                                        mvlr_igpm := mvlr_igpm + (mvlr_men*(ver_igpm(m_parcela[i,1])/100))
                                                        mtot_igpm := mtot_igpm + (mvlr_men*(ver_igpm(m_parcela[i,1])/100))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[i,4],(set->vlr_multa/100),0))+(mvlr_men*(ver_igpm(m_parcela[i,1])/100)),'9,999.99'))
                                                ELSE
                                                        DEVPOS(PROW(),106);DEVOUT(TRANSFORM(0,'999.99'))
                                                        DEVPOS(PROW(),117);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*IF(mdata_sis > m_parcela[i,4],(set->vlr_multa/100),0)),'9,999.99'))
                                                ENDIF
*                                       DEVPOS(PROW(),115);DEVOUT(TRANSFORM(mvlr_men+(mdias * (set->vlr_juros * mvlr_men))+(mvlr_men*(set->vlr_multa/100)),'9,999.99'))
                                        DEVPOS(PROW(),127);DEVOUT(m_parcela[i,5])
                                        mvlr_juros := mvlr_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mtot_juros := mtot_juros + mdias * (set->vlr_juros * mvlr_men)
                                        mqtd_dias := mqtd_dias + mdias
                                        mtot_men := mtot_men + mvlr_men
                                        mtot_ger := mtot_ger + mvlr_men
                                        mcont_parc ++
                                        mqtd_parc ++
                                        IF mflag = ' '
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->aluno)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(aluno->gstt)
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
                                        DEVPOS(PROW(),60);DEVOUT('Fx')
                                        DEVPOS(PROW(),63);DEVOUT('Parc.')
                                        DEVPOS(PROW(),69);DEVOUT('Vlr.Parc')
                                        DEVPOS(PROW(),79);DEVOUT('Vl.Multa')
                                        DEVPOS(PROW(),90);DEVOUT('Dias')
                                        DEVPOS(PROW(),95);DEVOUT('Vl.Juros')
                                        DEVPOS(PROW(),104);DEVOUT('Vlr.IGPM')
                                        DEVPOS(PROW(),117);DEVOUT('Vl.Total')
                                        DEVPOS(PROW(),127);DEVOUT('ANO')
                                        DEVPOS(PROW()+1,00);DEVOUT('Matric.')
                                        DEVPOS(PROW(),07);DEVOUT('Aluno')
                                        DEVPOS(PROW(),50);DEVOUT('GSTT')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                        IF LEN(m_parcela) > i
                                                imprt('M','C')
                                                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Fone: '+aluno->r_fone)
                                        ENDIF
                                        LOOP
                                ELSEIF LEN(m_parcela) > i
                                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                                ENDIF
                        NEXT
                        IF ! EMPTY(mtot_men)
                                DEVPOS(PROW()+1,64);DEVOUT('--')
                                DEVPOS(PROW(),69);DEVOUT('--------')
                                DEVPOS(PROW(),79);DEVOUT('--------')
                                DEVPOS(PROW(),89);DEVOUT('-----')
                                DEVPOS(PROW(),95);DEVOUT('--------')
                                DEVPOS(PROW(),104);DEVOUT('--------')
                                DEVPOS(PROW(),115);DEVOUT('----------')
                                DEVPOS(PROW()+1,00);DEVOUT('T O T A I S...................................................: ')
                                DEVPOS(PROW(),64);DEVOUT(TRANSFORM(mqtd_parc,'99'))
                                DEVPOS(PROW(),69);DEVOUT(TRANSFORM(mtot_men,'9,999.99'))
                                DEVPOS(PROW(),79);DEVOUT(TRANSFORM(mvlr_multa,'9,999.99'))
                                DEVPOS(PROW(),89);DEVOUT(TRANSFORM(mqtd_dias/mqtd_parc,'9,999'))
                                DEVPOS(PROW(),95);DEVOUT(TRANSFORM(mvlr_juros,'9,999.99'))
                                DEVPOS(PROW(),104);DEVOUT(TRANSFORM(mvlr_igpm,'9,999.99'))
                                DEVPOS(PROW(),115);DEVOUT(TRANSFORM(mtot_men + mvlr_juros + mvlr_multa + mvlr_igpm,'999,999.99'))
                                DEVPOS(PROW()+1,00);DEVOUT('End.: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - Apto.:'+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)+'- CEP.:'+RTRIM(aluno->r_cep)+'- CPF.:'+RTRIM(aluno->r_cpf))
                                DEVPOS(PROW()+1,00);DEVOUT('Filiacao: Pai: '+aluno->pai+' -  Mae: '+aluno->mae)
                                DEVPOS(PROW()+1,00);DEVOUT('OBS.: '+aluno->obs)
                                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                                mtot_dias := mtot_dias + mqtd_dias
                                mqtd_dias := 0
                        ENDIF
                        SKIP
                        IF PROW() >= 59
                                EJECT
                                mpag ++
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('Responsavel')
                                DEVPOS(PROW(),60);DEVOUT('Fx')
                                DEVPOS(PROW(),63);DEVOUT('Parc.')
                                DEVPOS(PROW(),69);DEVOUT('Vlr.Parc')
                                DEVPOS(PROW(),79);DEVOUT('Vl.Multa')
                                DEVPOS(PROW(),90);DEVOUT('Dias')
                                DEVPOS(PROW(),95);DEVOUT('Vl.Juros')
                                DEVPOS(PROW(),104);DEVOUT('Vlr.IGPM')
                                DEVPOS(PROW(),117);DEVOUT('Vl.Total')
                                DEVPOS(PROW(),127);DEVOUT('ANO')
                                DEVPOS(PROW()+1,00);DEVOUT('Matric.')
                                DEVPOS(PROW(),07);DEVOUT('Aluno')
                                DEVPOS(PROW(),50);DEVOUT('GSTT')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                imprt('M','C')
                                LOOP
                        ENDIF
                ENDDO
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT('Total de ALUNOS em Debito....: '+STRZERO(mcont_aluno,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total de PARCELAS em Debito: '+STRZERO(mcont_parc,5))
                DEVPOS(PROW()+1,00);DEVOUT('Total em VALOR mensalidade.: '+TRANSFORM(mtot_ger,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR MULTA 2%....: '+TRANSFORM(mtot_multa,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR Dias Atrasos: '+TRANSFORM(mtot_juros,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR IGPM R$.....: '+TRANSFORM(mtot_igpm,'9,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('                             ------------')
                DEVPOS(PROW()+1,00);DEVOUT('Total do VALOR GERAL.......: '+TRANSFORM(mtot_ger+mtot_multa+mtot_juros+mtot_igpm,'9,999,999.99'))
        ENDIF
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('RELA_DEB.REL')
        ENDIF
ENDDO
RETURN NIL

