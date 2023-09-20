************************************************
* EMISSAO DE CARNES EM DEBITOS
************************************************

FUNCTION saf5251
***************
LOCAL mprg:='SAF5251'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mcont_parc,mdesconto,mvlr_men,mgstt,mtot_men,mtot_ger,mindex,minicio,;
      tela,mgst,mmat,mop,mano:=SPACE(4),mcontador:=0,mgst_aux
PRIVATE m_parcela:={},mquantd_parc
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO EMISSAO DE CARNES EM DEBITOS (MENSALIDADES)','125')
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
        mano := SPACE(4)
        ***************
        SELE('aluno');ORDSETFOCUS(1)
        GO TOP
        ***************
        setcor(3)
        botao(lci,cci,lba,cba)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        setcor(1)
        DEVPOS(lci+5,cci+1);DEVOUT('Digite o ANO que deseja...............................: ')
        DEVPOS(lci+6,cci+1);DEVOUT('Digite ate que MES deseja.............................: ')
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
        ELSEIF mop = 3
                DEVPOS(lci+3,cci+1);DEVOUT('Por ordem de:[1] p/Resp.[2]GSTT [3]Aluno [4] Matricula: ')
                @ lci+3,cci+57 GET mindex PICT '9' VALID IF(mindex<1 .OR. mindex > 4,.F.,.T.) WHEN mop = 3
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                ***************
                SELE('aluno');ORDSETFOCUS(mindex)
                GO TOP
                ***************
        ENDIF
        @ lci+5,cci+57 GET mano PICT '9999' VALID IF(EMPTY(mano),.F.,.T.)
        @ lci+6,cci+57 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
*       @ lci+7,cci+57 GET mquantd_parc PICT '99'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF

        IF ! imp_arq('CARN_DEB.REL')
                LOOP
        ENDIF

        IF mop = 3
                mcontador := 0
                i := 0
                WHILE ! EOF()
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
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
                                        IF mano <> cdmen->ano;SKIP;LOOP;ENDIF
                                        IF EMPTY(cdmen->data_pag)
                                                AADD(m_parcela,{cdmen->parcela,TRANSFORM(cdmen->vlr_parc,'9,999.99')})
                                        ENDIF
                                        SKIP
                                ENDDO
                        ENDIF

                        ***************
                        SELE('aluno');ORDSETFOCUS(mindex)
                        ***************
                        IF LEN(m_parcela) == 0
                                SKIP
                                LOOP
                        ENDIF
                        IF LEN(m_parcela) < mquantd_parc
                                SKIP
                                LOOP
                        ENDIF
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
/*
                        mdesconto := faixa_vlr(ver_faixa(m_parcela[1,1]))
                        IF mdesconto == 100
                                LOOP
                        ENDIF
                        mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        IF maut = 'NOBREGA'
                        imprt('M','N')
                        DEVPOS(PROW()+4,00);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                        DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                        DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW(),33);DEVOUT(m_parcela[1,1]+'a.Parcela')
                        DEVPOS(PROW(),51);DEVOUT(m_parcela[1,1])
                        DEVPOS(PROW(),57);DEVOUT('05/'+m_parcela[1,1]+'/'+SUBSTR(mdiretorio,3,2))
                        DEVPOS(PROW(),76);DEVOUT(m_parcela[1,1])
                        DEVPOS(PROW(),81);DEVOUT('05/'+m_parcela[1,1]+'/'+SUBSTR(mdiretorio,3,2))
                        DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                        DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                        DEVPOS(PROW()+4,00);DEVOUT(' ')
                        mcont_aluno ++
                        mcont_parc := mcont_parc + LEN(m_parcela)
                        mtot_men := mtot_men + mvlr_men
                        mtot_ger := mtot_ger + mvlr_men
*/
                        i := 0
                        FOR i=1 TO LEN(m_parcela)
                                mdesconto := faixa_vlr(ver_faixa(m_parcela[i,1]))
                                IF mdesconto = 100
                                        LOOP
                                ENDIF
                                mvlr_men := 0
                                mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                mgst_aux := SUBSTR(aluno->gstt,1,1)+SUBSTR(aluno->gstt,2,1)+SUBSTR(aluno->gstt,4,1)
                                ver_tab(mgst_aux,mmes,'*')
                                IF maut = 'NOBREGA'
                                        imprt('M','N')
                                        DEVPOS(PROW()+4,00);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                                        DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                                        DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                                        DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                                        DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                                        DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                                        DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                                        DEVPOS(PROW(),33);DEVOUT(m_parcela[i,1]+'a.Parcela')
                                        DEVPOS(PROW(),51);DEVOUT(m_parcela[i,1])
                                        DEVPOS(PROW(),57);DEVOUT('05/'+m_parcela[i,1]+'/'+SUBSTR(mdiretorio,3,2))
                                        DEVPOS(PROW(),76);DEVOUT(m_parcela[i,1])
                                        DEVPOS(PROW(),81);DEVOUT('05/'+m_parcela[i,1]+'/'+SUBSTR(mdiretorio,3,2))
                                        DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                        DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                        DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                        DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                                        DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                                        DEVPOS(PROW()+4,00);DEVOUT(' ')
                                ELSE
                                        imprt('M','N')
                                        DEVPOS(PROW(),00);DEVOUT(mtraco)
                                        DEVPOS(PROW()+1,00);DEVOUT(memp+resa)
                                        DEVPOS(PROW(),40);DEVOUT(memp+resa)
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT('CGC: '+mcgc_firm+' - Inscricao: '+minsc_firm)
                                        DEVPOS(PROW(),69);DEVOUT('CGC: '+mcgc_firm+' - Inscricao: '+minsc_firm)
                                        DEVPOS(PROW()+1,00);DEVOUT('Endereco: '+mend_firm)
                                        DEVPOS(PROW(),69);DEVOUT('Endereco: '+mend_firm)
                                        DEVPOS(PROW()+1,00);DEVOUT('Bairro..: '+mcid_firm)
                                        DEVPOS(PROW(),69);DEVOUT('Bairro..: '+mcid_firm)
                                        DEVPOS(PROW()+1,00);DEVOUT('Aluno: '+aluno->matricula)
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - '+aluno->aluno)
                                        DEVPOS(PROW(),69);DEVOUT('Aluno: '+aluno->matricula)
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - '+aluno->aluno)
                                        DEVPOS(PROW()+1,00);DEVOUT('GSTT: '+aluno->gstt)
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Parcela: '+m_parcela[i,1]+'a.Parcela')
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('Data Vencimento: '+tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
                                        DEVPOS(PROW(),69);DEVOUT('GSTT: '+aluno->gstt)
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Parcela: '+m_parcela[i,1]+'a.Parcela')
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('Data Vencimento: '+tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
                                        DEVPOS(PROW()+1,00);DEVOUT('Valor da Parcela: '+TRANSFORM(mvlr_men,'9,999.99'))
                                        DEVPOS(PROW(),69);DEVOUT('Valor da Parcela: '+TRANSFORM(mvlr_men,'9,999.99'))
                                        DEVPOS(PROW()+2,00);DEVOUT('Data Pagamento:......./......./.......')
                                        DEVPOS(PROW(),69);DEVOUT('Data Pagamento:......./......./.......')
                                        DEVPOS(PROW()+2,00);DEVOUT('TESOURARIA DO '+memp+resa)
                                        DEVPOS(PROW(),69);DEVOUT('TESOURARIA DO '+memp+resa)
                                        imprt('M','N')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                        DEVPOS(PROW()+2,00);DEVOUT(REPLI('-',80))
                                        DEVPOS(PROW()+2,00);DEVOUT('')
                                        mcontador ++
                                        IF mcontador = 4
                                                EJECT
                                                mcontador := 0
                                        ENDIF
                                ENDIF
                                mcont_aluno ++
                                mcont_parc := mcont_parc + LEN(m_parcela)
                                mtot_men := mtot_men + mvlr_men
                                mtot_ger := mtot_ger + mvlr_men
                        NEXT
                        SKIP
                ENDDO
        ELSEIF mop = 1
                i := 0
                WHILE mgst = aluno->gstt .AND. ! EOF()
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
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
                                        IF mano <> cdmen->ano;SKIP;LOOP;ENDIF
                                        IF EMPTY(cdmen->data_pag)
                                                AADD(m_parcela,{cdmen->parcela,TRANSFORM(cdmen->vlr_parc,'9,999.99')})
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
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
/*
                        mdesconto := faixa_vlr(ver_faixa(m_parcela[1,1]))
                        IF mdesconto == 100
                                SKIP
                                LOOP
                        ENDIF
                        mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        imprt('M','N')
                        DEVPOS(PROW()+4,00);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                        DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                        DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW(),33);DEVOUT(m_parcela[1,1]+'a.Parcela')
                        DEVPOS(PROW(),51);DEVOUT(m_parcela[1,1])
                        DEVPOS(PROW(),57);DEVOUT('05/'+m_parcela[1,1]+'/'+SUBSTR(mdiretorio,3,2))
                        DEVPOS(PROW(),76);DEVOUT(m_parcela[1,1])
                        DEVPOS(PROW(),81);DEVOUT('05/'+m_parcela[1,1]+'/'+SUBSTR(mdiretorio,3,2))
                        DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                        DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                        DEVPOS(PROW()+4,00);DEVOUT(' ')
                        mcont_aluno ++
                        mcont_parc := mcont_parc + LEN(m_parcela)
                        mtot_men := mtot_men + mvlr_men
                        mtot_ger := mtot_ger + mvlr_men
*/
                        i := 0
                        FOR i=1 TO LEN(m_parcela)
                                mdesconto := faixa_vlr(ver_faixa(STRZERO(i,2)))
                                IF mdesconto = 100
                                        LOOP
                                ENDIF
                                mvlr_men := 0
                                mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                mgst_aux := SUBSTR(aluno->gstt,1,1)+SUBSTR(aluno->gstt,2,1)+SUBSTR(aluno->gstt,4,1)
                                ver_tab(mgst_aux,mmes,'*')
                                IF maut = 'NOBREGA'
                                        imprt('M','N')
                                        DEVPOS(PROW()+4,00);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                                        DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                                        DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                                        DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                                        DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                                        DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                                        DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                                        DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                                        DEVPOS(PROW(),33);DEVOUT(m_parcela[i,1]+'a.Parcela')
                                        DEVPOS(PROW(),51);DEVOUT(m_parcela[i,1])
                                        DEVPOS(PROW(),57);DEVOUT('05/'+m_parcela[i,1]+'/'+SUBSTR(mdiretorio,3,2))
                                        DEVPOS(PROW(),76);DEVOUT(m_parcela[i,1])
                                        DEVPOS(PROW(),81);DEVOUT('05/'+m_parcela[i,1]+'/'+SUBSTR(mdiretorio,3,2))
                                        DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                        DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                        DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                        DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                                        DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                                        DEVPOS(PROW()+4,00);DEVOUT(' ')
                                ELSE
                                        imprt('M','N')
                                        DEVPOS(PROW(),00);DEVOUT(mtraco)
                                        DEVPOS(PROW()+1,00);DEVOUT(memp+resa)
                                        DEVPOS(PROW(),40);DEVOUT(memp+resa)
                                        imprt('M','C')
                                        DEVPOS(PROW()+1,00);DEVOUT('CGC: '+mcgc_firm+' - Inscricao: '+minsc_firm)
                                        DEVPOS(PROW(),69);DEVOUT('CGC: '+mcgc_firm+' - Inscricao: '+minsc_firm)
                                        DEVPOS(PROW()+1,00);DEVOUT('Endereco: '+mend_firm)
                                        DEVPOS(PROW(),69);DEVOUT('Endereco: '+mend_firm)
                                        DEVPOS(PROW()+1,00);DEVOUT('Bairro..: '+mcid_firm)
                                        DEVPOS(PROW(),69);DEVOUT('Bairro..: '+mcid_firm)
                                        DEVPOS(PROW()+1,00);DEVOUT('Aluno: '+aluno->matricula)
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - '+aluno->aluno)
                                        DEVPOS(PROW(),69);DEVOUT('Aluno: '+aluno->matricula)
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - '+aluno->aluno)
                                        DEVPOS(PROW()+1,00);DEVOUT('GSTT: '+aluno->gstt)
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Parcela: '+m_parcela[i,1]+'a.Parcela')
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('Data Vencimento: '+'05/'+m_parcela[i,1]+'/'+SUBSTR(mdiretorio,3,2))
                                        DEVPOS(PROW(),69);DEVOUT('GSTT: '+aluno->gstt)
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Parcela: '+m_parcela[i,1]+'a.Parcela')
                                        DEVPOS(PROW(),PCOL()+3);DEVOUT('Data Vencimento: '+'05/'+m_parcela[i,1]+'/'+SUBSTR(mdiretorio,3,2))
                                        DEVPOS(PROW()+1,00);DEVOUT('Valor da Parcela: '+TRANSFORM(mvlr_men,'9,999.99'))
                                        DEVPOS(PROW(),69);DEVOUT('Valor da Parcela: '+TRANSFORM(mvlr_men,'9,999.99'))
                                        DEVPOS(PROW()+2,00);DEVOUT('Data Pagamento:......./......./.......')
                                        DEVPOS(PROW(),69);DEVOUT('Data Pagamento:......./......./.......')
                                        DEVPOS(PROW()+2,00);DEVOUT('TESOURARIA DO '+memp+resa)
                                        DEVPOS(PROW(),69);DEVOUT('TESOURARIA DO '+memp+resa)
                                        imprt('M','N')
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                        DEVPOS(PROW()+2,00);DEVOUT(REPLI('-',80))
                                        DEVPOS(PROW()+2,00);DEVOUT('')
                                        mcontador ++
                                        IF mcontador = 4
                                                EJECT
                                                mcontador := 0
                                        ENDIF
                                ENDIF
                                mcont_aluno ++
                                mcont_parc := mcont_parc + LEN(m_parcela)
                                mtot_men := mtot_men + mvlr_men
                                mtot_ger := mtot_ger + mvlr_men
                        NEXT
                        SKIP
                ENDDO
        ELSEIF mop = 2 .AND. EMPTY(aluno->dt_tranf)
                i := 0
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
                                IF mano <> cdmen->ano;SKIP;LOOP;ENDIF
                                IF EMPTY(cdmen->data_pag)
                                        AADD(m_parcela,{cdmen->parcela,TRANSFORM(cdmen->vlr_parc,'9,999.99')})
                                ENDIF
                                SKIP
                        ENDDO
                ENDIF

                ***************
                SELE('aluno');ORDSETFOCUS(4)
                ***************

                IF LEN(m_parcela) == 0
                        LOOP
                ENDIF
                IF LEN(m_parcela) < mquantd_parc
                        LOOP
                ENDIF
                mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
/*
                mdesconto := faixa_vlr(ver_faixa(m_parcela[1,1]))
                IF mdesconto == 100
                        LOOP
                ENDIF
                mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                imprt('M','N')
                DEVPOS(PROW()+4,00);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                DEVPOS(PROW(),33);DEVOUT(m_parcela[1,1]+'a.Parcela')
                DEVPOS(PROW(),51);DEVOUT(m_parcela[1,1])
                DEVPOS(PROW(),57);DEVOUT('05/'+m_parcela[1,1]+'/'+SUBSTR(mdiretorio,3,2))
                DEVPOS(PROW(),76);DEVOUT(m_parcela[1,1])
                DEVPOS(PROW(),81);DEVOUT('05/'+m_parcela[1,1]+'/'+SUBSTR(mdiretorio,3,2))
                DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                DEVPOS(PROW()+4,00);DEVOUT(' ')
                mcont_aluno ++
                mcont_parc := mcont_parc + LEN(m_parcela)
                mtot_men := mtot_men + mvlr_men
                mtot_ger := mtot_ger + mvlr_men
*/
                i := 0
                FOR i=1 TO LEN(m_parcela)
                        mdesconto := faixa_vlr(ver_faixa(m_parcela[i,1]))
                        IF mdesconto == 100
                                LOOP
                        ENDIF
                        mvlr_men := 0
                        mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        mgst_aux := SUBSTR(aluno->gstt,1,1)+SUBSTR(aluno->gstt,2,1)+SUBSTR(aluno->gstt,4,1)
                        ver_tab(mgst_aux,mmes,'*')
                        IF maut = 'NOBREGA'
                                imprt('M','N')
                                DEVPOS(PROW()+4,00);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                                DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                                DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                                DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                                DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                                DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                                DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                                DEVPOS(PROW(),33);DEVOUT(m_parcela[i,1]+'a.Parcela')
                                DEVPOS(PROW(),51);DEVOUT(m_parcela[i,1])
                                DEVPOS(PROW(),57);DEVOUT('05/'+m_parcela[i,1]+'/'+SUBSTR(mdiretorio,3,2))
                                DEVPOS(PROW(),76);DEVOUT(m_parcela[i,1])
                                DEVPOS(PROW(),81);DEVOUT('05/'+m_parcela[i,1]+'/'+SUBSTR(mdiretorio,3,2))
                                DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                                DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                                DEVPOS(PROW()+4,00);DEVOUT(' ')
                        ELSE
                                imprt('M','N')
                                DEVPOS(PROW(),00);DEVOUT(mtraco)
                                DEVPOS(PROW()+1,00);DEVOUT(memp+resa)
                                DEVPOS(PROW(),40);DEVOUT(memp+resa)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('CGC: '+mcgc_firm+' - Inscricao: '+minsc_firm)
                                DEVPOS(PROW(),69);DEVOUT('CGC: '+mcgc_firm+' - Inscricao: '+minsc_firm)
                                DEVPOS(PROW()+1,00);DEVOUT('Endereco: '+mend_firm)
                                DEVPOS(PROW(),69);DEVOUT('Endereco: '+mend_firm)
                                DEVPOS(PROW()+1,00);DEVOUT('Bairro..: '+mcid_firm)
                                DEVPOS(PROW(),69);DEVOUT('Bairro..: '+mcid_firm)
                                DEVPOS(PROW()+1,00);DEVOUT('Aluno: '+aluno->matricula)
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(' - '+aluno->aluno)
                                DEVPOS(PROW(),69);DEVOUT('Aluno: '+aluno->matricula)
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(' - '+aluno->aluno)
                                DEVPOS(PROW()+1,00);DEVOUT('GSTT: '+aluno->gstt)
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Parcela: '+m_parcela[i,1]+'a.Parcela')
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('Data Vencimento: '+'05/'+m_parcela[i,1]+'/'+SUBSTR(mdiretorio,3,2))
                                DEVPOS(PROW(),69);DEVOUT('GSTT: '+aluno->gstt)
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Parcela: '+m_parcela[i,1]+'a.Parcela')
                                DEVPOS(PROW(),PCOL()+3);DEVOUT('Data Vencimento: '+'05/'+m_parcela[i,1]+'/'+SUBSTR(mdiretorio,3,2))
                                DEVPOS(PROW()+1,00);DEVOUT('Valor da Parcela: '+TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW(),69);DEVOUT('Valor da Parcela: '+TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW()+2,00);DEVOUT('Data Pagamento:......./......./.......')
                                DEVPOS(PROW(),69);DEVOUT('Data Pagamento:......./......./.......')
                                DEVPOS(PROW()+2,00);DEVOUT('TESOURARIA DO '+memp+resa)
                                DEVPOS(PROW(),69);DEVOUT('TESOURARIA DO '+memp+resa)
                                imprt('M','N')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                DEVPOS(PROW()+2,00);DEVOUT(REPLI('-',80))
                                DEVPOS(PROW()+2,00);DEVOUT('')
                                mcontador ++
                                IF mcontador = 4
                                        EJECT
                                        mcontador := 0
                                ENDIF
                        ENDIF
                        mcont_aluno ++
                        mcont_parc := mcont_parc + LEN(m_parcela)
                        mtot_men := mtot_men + mvlr_men
                        mtot_ger := mtot_ger + mvlr_men
                NEXT
        ENDIF
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('CARN_DEB.REL')
        ENDIF
ENDDO
RETURN NIL
**************************** F I M **************************

