************************************************
* CARTA DE COBRANCA DE ALUNOS EM DEBITOS
************************************************

FUNCTION saf524
***************
LOCAL mprg:='SAF524'
LOCAL lci,cci,lba,cba,tela,mmatricula,mgst,mtipo_con,li,ci,lb,cb,tela1,;
      opcao,mquantd,mtraco,mmes,i,mcont_aluno,mcont_parc,mdesconto,;
      mvlr_men,mtot_men,mtot_ger,mgstt,mindex,mdata,mes

LOCAL l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23,l24
PRIVATE m_parcela:={},mquantd_pa
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'CARTA DE COBRANCA P/ALUNOS EM DEBITOS','125')
        RETURN NIL
ENDIF

mtraco:=REPLI(CHR(223),80)
*mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cartacob','ctcob');RETURN NIL;ENDIF
IF ! abriarq('safprn','prn');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
lci := 10
cci := 08
lba := 16
cba := 70
li := 00
ci := 01
lb := 24
cb := 78
tela := SAVESCREEN(00,00,24,79)
WHILE .T.
        l1:=l2:=l3:=l4:=l5:=l6:=l7:=l8:=l9:=l10:=l11:=l12:=l13:=l14:=l15:=l16:=;
        l17:=l18:=l19:=l20:=l21:=l22:=l23:=l24:= SPACE(73)
        mquantd_pa := 0
        mdata      := mdata_sis
        mmatricula := SPACE(6)
        mtipo_con  := 0
        mgst       := SPACE(4)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Data da Carta:')
        @ lci+1,COL()+1 GET mdata
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        @ lci+3,cci+4 PROMPT ' Individual ' MESSAGE 'Emissao de Boletim INDIVIDUAL'
        @ lci+3,COL()+4 PROMPT '    GSTT    ' MESSAGE 'Emissao de Boletim por Grau, Serie, Turma e turno'
        @ lci+3,COL()+4 PROMPT '    Geral   ' MESSAGE 'Emissao de Boletim em GERAL'
        SET INTEN ON
        MENU TO mtipo_con

        IF LASTKEY() = 27
                LOOP
        ENDIF
        *****************
        SELE('ctcob')
        GO TOP
        *****************
        l1  := ctcob->lin1
        l2  := ctcob->lin2
        l3  := ctcob->lin3
        l4  := ctcob->lin4
        l5  := ctcob->lin5
        l6  := ctcob->lin6
        l7  := ctcob->lin7
        l8  := ctcob->lin8
        l9  := ctcob->lin9
        l10 := ctcob->lin10
        l11 := ctcob->lin11
        l12 := ctcob->lin12
        l13 := ctcob->lin13
        l14 := ctcob->lin14
        l15 := ctcob->lin15
        l16 := ctcob->lin16
        l17 := ctcob->lin17
        l18 := ctcob->lin18
        l19 := ctcob->lin19
        l20 := ctcob->lin20
        l21 := ctcob->lin21
        l22 := ctcob->lin22
        l23 := ctcob->lin23
        l24 := ctcob->lin24
        tela1 := SAVESCREEN(00,00,24,79)
        botao(li,ci,lb,cb)
        @ li+1,ci+1 GET l1
        @ li+2,ci+1 GET l2
        @ li+3,ci+1 GET l3
        @ li+4,ci+1 GET l4
        @ li+5,ci+1 GET l5
        @ li+6,ci+1 GET l6
        @ li+7,ci+1 GET l7
        @ li+8,ci+1 GET l8
        @ li+9,ci+1 GET l9
        @ li+10,ci+1 GET l10
        @ li+11,ci+1 GET l11
        @ li+12,ci+1 GET l12
        @ li+13,ci+1 GET l13
        @ li+14,ci+1 GET l14
        @ li+15,ci+1 GET l15
        @ li+16,ci+1 GET l16
        @ li+17,ci+1 GET l17
        @ li+18,ci+1 GET l18
        @ li+19,ci+1 GET l19
        @ li+20,ci+1 GET l20
        @ li+21,ci+1 GET l21
        @ li+22,ci+1 GET l22
        @ li+23,ci+1 GET l23
        @ li+24,ci+1 GET l24
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela1)
                LOOP
        ENDIF
        BLOQREG()
        ctcob->lin1 :=l1
        ctcob->lin2 :=l2
        ctcob->lin3 :=l3
        ctcob->lin4 :=l4
        ctcob->lin5 :=l5
        ctcob->lin6 :=l6
        ctcob->lin7 :=l7
        ctcob->lin8 :=l8
        ctcob->lin9 :=l9
        ctcob->lin10:=l10
        ctcob->lin11:=l11
        ctcob->lin12:=l12
        ctcob->lin13:=l13
        ctcob->lin14:=l14
        ctcob->lin15:=l15
        ctcob->lin16:=l16
        ctcob->lin17:=l17
        ctcob->lin18:=l18
        ctcob->lin20:=l20
        ctcob->lin21:=l21
        ctcob->lin22:=l22
        ctcob->lin23:=l23
        ctcob->lin24:=l24
        RESTSCREEN(00,00,24,79,tela1)
        IF mtipo_con = 1
                *****************
                SELE('aluno');ORDSETFOCUS(4)
                GO TOP
                *****************
                DEVPOS(lci+5,cci+1);DEVOUT('Matricula:')
                @ lci+5,cci+12 GET mmatricula PICT '999999'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF ! aluno->(DBSEEK(mmatricula))
                        atencao('Nao foi possivel encontrar esta matricula')
                        LOOP
                ENDIF
                setcor(3)
                DEVPOS(lci+5,cci+22);DEVOUT(aluno->aluno)
                setcor(1)
                IF ! EMPTY(aluno->dt_tranf)
                        atencao('Este Aluno foi Transferido')
                        LOOP
                ENDIF
                mtot_men := 0
                mtot_ger := 0
                mcont_aluno := 0
                mcont_parc := 0
                mmes := SPACE(2)
                mquantd = 1
                mensagem('Digite ate que MES deseja: ')
                @ 23,COL()+1 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN NIL
                ENDIF
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
                                IF EMPTY(cdmen->data_pag) .AND. VAL(cdmen->parcela) >= VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                        AADD(m_parcela,{cdmen->parcela,TRANSFORM(cdmen->vlr_parc,'9,999.99')})
                                ENDIF
                                SKIP
                        ENDDO
                ENDIF

                ***************
                SELE('aluno');ORDSETFOCUS(4)
                ***************

                IF LEN(m_parcela) == 0
                        atencao('Este aluno nao esta em DEBITO com o Colegio')
                        LOOP
                ENDIF
                mdesconto := faixa_vlr(aluno->faixa_1)
                IF mdesconto == 100
                        atencao('Este aluno e BOLSISTA INTEGRAL')
                        LOOP
                ENDIF
                mes := NMES(mdata)
                IF ! imp_arq('CART_DEB.REL')
                       LOOP
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->a_expande)))
                DEVPOS(PROW(),00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                DEVPOS(PROW(),00);DEVOUT('COLEGIO NOBREGA')
                DEVPOS(PROW(),00);DEVOUT(&(ALLTRIM(prn->d_expande)))
                DEVPOS(PROW(),00);DEVOUT((ALLTRIM(prn->a_italico)))
                DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                DEVPOS(PROW(),00);DEVOUT('Av.Oliveira Lima, 824 - Boa Vista - Recife - PE.')
                DEVPOS(PROW()+1,00);DEVOUT('TeleFax: (081) 423.1844')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                DEVPOS(PROW(),00);DEVOUT('Responsavel: '+aluno->responsa)
                DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->d_negrito)))
                DEVPOS(PROW(),00);DEVOUT('CPF........: '+aluno->r_cpf+'  -  Fone: '+aluno->r_fone)
                DEVPOS(PROW()+1,00);DEVOUT('Endereco...: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - '+RTRIM(aluno->r_apto))
                DEVPOS(PROW()+1,00);DEVOUT('Bairro.....: '+RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+RTRIM(aluno->r_estado))
                DEVPOS(PROW()+1,00);DEVOUT('CEP........: '+RTRIM(aluno->r_cep))
                DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                DEVPOS(PROW(),00);DEVOUT('Aluno......: '+aluno->matricula+' - '+aluno->aluno)
                DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->d_negrito)))
                DEVPOS(PROW(),00);DEVOUT('Grau.......: '+SUBSTR(aluno->gstt,1,1))
                DEVPOS(PROW(),PCOL()+3);DEVOUT('Serie: '+SUBSTR(aluno->gstt,2,1))
                DEVPOS(PROW(),PCOL()+3);DEVOUT('Turma: '+SUBSTR(aluno->gstt,3,1))
                DEVPOS(PROW(),PCOL()+3);DEVOUT('Turno: '+SUBSTR(aluno->gstt,4,1))
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                DEVPOS(PROW()+2,03);DEVOUT('Recife, '+SUBSTR(DTOC(mdata),1,2)+' de '+mes+' de 20'+SUBSTR(DTOC(mdata),7,2))
                DEVPOS(PROW()+3,03);DEVOUT('Prezado Senhor(a)')
                DEVPOS(PROW()+2,03);DEVOUT(l1)
                DEVPOS(PROW()+1,03);DEVOUT(l2)
                DEVPOS(PROW()+1,03);DEVOUT(l3)
                DEVPOS(PROW()+1,03);DEVOUT(l4)
                DEVPOS(PROW()+1,03);DEVOUT(l5)
                DEVPOS(PROW()+1,03);DEVOUT(l6)
                DEVPOS(PROW()+1,03);DEVOUT(l7)
                DEVPOS(PROW()+1,03);DEVOUT(l8)
                DEVPOS(PROW()+1,03);DEVOUT(l9)
                DEVPOS(PROW()+1,03);DEVOUT(l10)
                DEVPOS(PROW()+1,03);DEVOUT(l11)
                DEVPOS(PROW()+1,03);DEVOUT(l12)
                DEVPOS(PROW()+1,03);DEVOUT(l13)
                DEVPOS(PROW()+1,03);DEVOUT(l14)
                DEVPOS(PROW()+1,03);DEVOUT(l15)
                DEVPOS(PROW()+1,03);DEVOUT(l16)
                DEVPOS(PROW()+1,03);DEVOUT(l17)
                DEVPOS(PROW()+1,03);DEVOUT(l18)
                DEVPOS(PROW()+1,03);DEVOUT(l19)
                DEVPOS(PROW()+1,03);DEVOUT(l20)
                DEVPOS(PROW()+1,03);DEVOUT(l21)
                DEVPOS(PROW()+3,03);DEVOUT(l22)
                DEVPOS(PROW()+3,03);DEVOUT(l23)
                DEVPOS(PROW()+1,03);DEVOUT(l24)
/*
                DEVPOS(PROW()+2,03);DEVOUT('          Todo negocio exige a entrega de um produto  em  contraprestacao')
                DEVPOS(PROW()+1,03);DEVOUT('de uma remuneracao para que as partes sobrevivam harmoniozamente.')
                DEVPOS(PROW()+1,03);DEVOUT('          Nossa prestacao de servicos escolares esta sendo feita  desde o')
                DEVPOS(PROW()+1,03);DEVOUT('inicio do ano letivo, por isso aguardavamos a remuneracao dos servicos em')
                DEVPOS(PROW()+1,03);DEVOUT('seus vencimentos.')
                DEVPOS(PROW()+1,03);DEVOUT('          Para o nosso descontentamento, alguns  responsaveis  por alunos')
                DEVPOS(PROW()+1,03);DEVOUT('matriculados  em  nosso  Colegio  nao  estao  cumprindo  com  sua  parte,')
                DEVPOS(PROW()+1,03);DEVOUT('aumentando assim o numero de inadimplentes.')
                DEVPOS(PROW()+1,03);DEVOUT('          Para evitar o desgaste no contato  com  responsaveis  em  nossa')
                DEVPOS(PROW()+1,03);DEVOUT('secretaria, contratamos os servicos de  cobrancas  de  um  escritorio  de')
                DEVPOS(PROW()+1,03);DEVOUT('Assessoria  especializado  para   onde   serao  remetidas  as  prestacoes')
                DEVPOS(PROW()+1,03);DEVOUT('(mensalidades escolares) que estiverem em atraso.')
                DEVPOS(PROW()+1,03);DEVOUT('          Todas as mensalidades  vencidas, depois do dia 16  de  novembro')
                DEVPOS(PROW()+1,03);DEVOUT('de 20'+SUBSTR(DTOC(mdata),7,2)+', somente serao recebidas pelo  escritorio  de  cobrancas  com  os')
                DEVPOS(PROW()+1,03);DEVOUT('acrescimos legais e honorarios.')
                DEVPOS(PROW()+1,03);DEVOUT('          Se porventura V.Sa. tiver debito em atraso  e  nao solucionar a')
                DEVPOS(PROW()+1,03);DEVOUT('pendencia  ate  o  final  do  mes  de  outubro,  no Colegio, devera estar')
                DEVPOS(PROW()+1,03);DEVOUT('recebendo comunicacao a respeito, diretamente do escritorio de cobrancas.')
                DEVPOS(PROW()+1,03);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                DEVPOS(PROW(),03);DEVOUT('          CASO  V.Sa.  NAO  ESTEJA  MAIS INADIMPLENTE,  FAVOR PROCURAR  A')
                DEVPOS(PROW()+1,03);DEVOUT('TESOURARIA E TORNAR A PRESENTE SEM EFEITO.')
                DEVPOS(PROW()+1,03);DEVOUT(&(ALLTRIM(prn->d_negrito)))
                DEVPOS(PROW()+2,03);DEVOUT('Atenciosamente,')
                DEVPOS(PROW()+3,03);DEVOUT('Pe. Luiz Gonzaga de Almeida S.J.')
                DEVPOS(PROW()+1,03);DEVOUT('      Diretor Administrativo')
                DEVPOS(PROW()+1,03);DEVOUT(&(ALLTRIM(prn->a_negrito)))
*/
                DEVPOS(PROW()+1,03);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                DEVPOS(PROW()+1,03);DEVOUT('*** MESES EM ATRASO ***')
                DEVPOS(PROW()+1,03);DEVOUT('Parcelas       Valor')
                mcont_aluno ++
                mcont_parc := mcont_parc + LEN(m_parcela)
                DEVPOS(PROW()+1,06);DEVOUT(m_parcela[1,1]+' '+m_parcela[1,2])
                mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                DEVPOS(PROW(),15);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                mtot_men := mtot_men + mvlr_men
                mtot_ger := mtot_ger + mvlr_men
                i := 0
                FOR i=2 TO LEN(m_parcela)
                        mvlr_men := 0
                        DEVPOS(PROW()+1,06);DEVOUT(m_parcela[i,1]+' '+m_parcela[i,2])
                        mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        DEVPOS(PROW(),15);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        mtot_men := mtot_men + mvlr_men
                        mtot_ger := mtot_ger + mvlr_men
                NEXT
                DEVPOS(PROW()+1,05);DEVOUT('----')
                DEVPOS(PROW(),15);DEVOUT('-----------')
                DEVPOS(PROW()+1,06);DEVOUT(STRZERO(LEN(m_parcela),2))
                DEVPOS(PROW(),14);DEVOUT(TRANSFORM(mtot_men,'99,999.99'))
*                DEVPOS(PROW()+1,89);DEVOUT('TOTAL A PAGAR (+2%): ')
*                DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mtot_men*1.02),'99,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->d_negrito)))
                DEVPOS(PROW(),00);DEVOUT((ALLTRIM(prn->d_italico)))
                EJECT
        ELSEIF mtipo_con = 2
                *****************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                *****************
                DEVPOS(lci+5,cci+1);DEVOUT('GSTT:')
                @ lci+5,COL()+2 GET mgst PICT '@!'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF ! aluno->(DBSEEK(mgst))
                        atencao('Nao foi possivel encontrar esta TURMA')
                        LOOP
                ENDIF
                mtot_men := 0
                mtot_ger := 0
                mcont_aluno := 0
                mcont_parc := 0
                mmes := SPACE(2)
                mquantd = 1
                mensagem('Digite ate que MES deseja: ')
                @ 23,COL()+1 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN NIL
                ENDIF
                IF ! imp_arq('CART_DEB.REL')
                        LOOP
                ENDIF

                mes := NMES(mdata)
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

                        ***************
                        SELE('cdmen');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                        IF cdmen->(DBSEEK(aluno->matricula))
                                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. SUBSTR(DTOC(cdmen->data_venc),4,2) <= mmes
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
                        mdesconto := faixa_vlr(aluno->faixa_1)
                        IF mdesconto == 100
                                SKIP
                                LOOP
                        ENDIF
                        DEVPOS(PROW(),00);DEVOUT(&(ALLTRIM(prn->a_expande)))
                        DEVPOS(PROW(),00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('COLEGIO NOBREGA')
                        DEVPOS(PROW(),00);DEVOUT(&(ALLTRIM(prn->d_expande)))
                        DEVPOS(PROW(),00);DEVOUT((ALLTRIM(prn->a_italico)))
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('Av.Oliveira Lima, 824 - Boa Vista - Recife - PE.')
                        DEVPOS(PROW()+1,00);DEVOUT('TeleFax: (081) 423.1844')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('Responsavel: '+aluno->responsa)
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->d_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('CPF........: '+aluno->r_cpf+'  -  Fone: '+aluno->r_fone)
                        DEVPOS(PROW()+1,00);DEVOUT('Endereco...: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - '+RTRIM(aluno->r_apto))
                        DEVPOS(PROW()+1,00);DEVOUT('Bairro.....: '+RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+RTRIM(aluno->r_estado))
                        DEVPOS(PROW()+1,00);DEVOUT('CEP........: '+RTRIM(aluno->r_cep))
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('Aluno......: '+aluno->matricula+' - '+aluno->aluno)
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->d_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('Grau.......: '+SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),PCOL()+3);DEVOUT('Serie: '+SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),PCOL()+3);DEVOUT('Turma: '+SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW(),PCOL()+3);DEVOUT('Turno: '+SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        DEVPOS(PROW()+2,03);DEVOUT('Recife, '+SUBSTR(DTOC(mdata),1,2)+' de '+mes+' de 20'+SUBSTR(DTOC(mdata),7,2))
                        DEVPOS(PROW()+3,03);DEVOUT('Prezado Senhor(a)')
                        DEVPOS(PROW()+2,03);DEVOUT(l1)
                        DEVPOS(PROW()+1,03);DEVOUT(l2)
                        DEVPOS(PROW()+1,03);DEVOUT(l3)
                        DEVPOS(PROW()+1,03);DEVOUT(l4)
                        DEVPOS(PROW()+1,03);DEVOUT(l5)
                        DEVPOS(PROW()+1,03);DEVOUT(l6)
                        DEVPOS(PROW()+1,03);DEVOUT(l7)
                        DEVPOS(PROW()+1,03);DEVOUT(l8)
                        DEVPOS(PROW()+1,03);DEVOUT(l9)
                        DEVPOS(PROW()+1,03);DEVOUT(l10)
                        DEVPOS(PROW()+1,03);DEVOUT(l11)
                        DEVPOS(PROW()+1,03);DEVOUT(l12)
                        DEVPOS(PROW()+1,03);DEVOUT(l13)
                        DEVPOS(PROW()+1,03);DEVOUT(l14)
                        DEVPOS(PROW()+1,03);DEVOUT(l15)
                        DEVPOS(PROW()+1,03);DEVOUT(l16)
                        DEVPOS(PROW()+1,03);DEVOUT(l17)
                        DEVPOS(PROW()+1,03);DEVOUT(l18)
                        DEVPOS(PROW()+1,03);DEVOUT(l19)
                        DEVPOS(PROW()+1,03);DEVOUT(l20)
                        DEVPOS(PROW()+1,03);DEVOUT(l21)
                        DEVPOS(PROW()+3,03);DEVOUT(l22)
                        DEVPOS(PROW()+3,03);DEVOUT(l23)
                        DEVPOS(PROW()+1,03);DEVOUT(l24)
                        DEVPOS(PROW()+1,03);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                        DEVPOS(PROW()+1,03);DEVOUT('*** MESES EM ATRASO ***')
                        DEVPOS(PROW()+1,03);DEVOUT('Parcelas       Valor')
                        mcont_aluno ++
                        mcont_parc := mcont_parc + LEN(m_parcela)
                        DEVPOS(PROW()+1,06);DEVOUT(m_parcela[1,1]+' '+m_parcela[1,2])
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        DEVPOS(PROW(),15);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        mtot_men := mtot_men + mvlr_men
                        mtot_ger := mtot_ger + mvlr_men
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mvlr_men := 0
                                DEVPOS(PROW()+1,06);DEVOUT(m_parcela[i,1]+' '+m_parcela[i,2])
                                mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                DEVPOS(PROW(),15);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                mtot_men := mtot_men + mvlr_men
                                mtot_ger := mtot_ger + mvlr_men
                        NEXT
                        DEVPOS(PROW()+1,05);DEVOUT('----')
                        DEVPOS(PROW(),15);DEVOUT('-----------')
                        DEVPOS(PROW()+1,06);DEVOUT(STRZERO(LEN(m_parcela),2))
                        DEVPOS(PROW(),14);DEVOUT(TRANSFORM(mtot_men,'99,999.99'))
*                        DEVPOS(PROW()+1,89);DEVOUT('TOTAL A PAGAR (+2%): ')
*                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mtot_men*1.02),'99,999.99'))
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->d_negrito)))
                        DEVPOS(PROW(),00);DEVOUT((ALLTRIM(prn->d_italico)))
                        SKIP
                        EJECT
                ENDDO
        ELSEIF mtipo_con = 3
                mindex := 0
                mtot_men := 0
                mtot_ger := 0
                mcont_aluno := 0
                mcont_parc := 0
                mmes := SPACE(2)
                mquantd = 1
                mensagem('Digite ate que MES deseja: ')
                @ 23,COL()+1 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN NIL
                ENDIF
                mensagem('Por ordem de:[1] p/Resp.[2]GSTT [3]Aluno [4] Matricula: ')
                @ 23,COL()+1 GET mindex PICT '9' VALID IF(mindex<1 .OR. mindex > 4,.F.,.T.)
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN NIL
                ENDIF
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

                IF ! imp_arq('CART_DEB.REL')
                       LOOP
                ENDIF
                mes := NMES(mdata)
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

                        ***************
                        SELE('cdmen');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                        IF cdmen->(DBSEEK(aluno->matricula))
                                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. SUBSTR(DTOC(cdmen->data_venc),4,2) <= mmes
                                        IF EMPTY(cdmen->data_pag)
                                                AADD(m_parcela,{cdmen->parcela,TRANSFORM(cdmen->vlr_parc,'9,999.99')})
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
                        mdesconto := faixa_vlr(aluno->faixa_1)
                        IF mdesconto == 100
                                SKIP
                                LOOP
                        ENDIF
                        DEVPOS(PROW(),00);DEVOUT(&(ALLTRIM(prn->a_expande)))
                        DEVPOS(PROW(),00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('COLEGIO NOBREGA')
                        DEVPOS(PROW(),00);DEVOUT(&(ALLTRIM(prn->d_expande)))
                        DEVPOS(PROW(),00);DEVOUT((ALLTRIM(prn->a_italico)))
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('Av.Oliveira Lima, 824 - Boa Vista - Recife - PE.')
                        DEVPOS(PROW()+1,00);DEVOUT('TeleFax: (081) 423.1844')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        DEVPOS(PROW()+2,03);DEVOUT('Recife, '+SUBSTR(DTOC(mdata),1,2)+' de '+mes+' de 20'+SUBSTR(DTOC(mdata),7,2))
                        DEVPOS(PROW()+3,03);DEVOUT('Prezado Senhor(a)')
                        DEVPOS(PROW()+2,03);DEVOUT(l1)
                        DEVPOS(PROW()+1,03);DEVOUT(l2)
                        DEVPOS(PROW()+1,03);DEVOUT(l3)
                        DEVPOS(PROW()+1,03);DEVOUT(l4)
                        DEVPOS(PROW()+1,03);DEVOUT(l5)
                        DEVPOS(PROW()+1,03);DEVOUT(l6)
                        DEVPOS(PROW()+1,03);DEVOUT(l7)
                        DEVPOS(PROW()+1,03);DEVOUT(l8)
                        DEVPOS(PROW()+1,03);DEVOUT(l9)
                        DEVPOS(PROW()+1,03);DEVOUT(l10)
                        DEVPOS(PROW()+1,03);DEVOUT(l11)
                        DEVPOS(PROW()+1,03);DEVOUT(l12)
                        DEVPOS(PROW()+1,03);DEVOUT(l13)
                        DEVPOS(PROW()+1,03);DEVOUT(l14)
                        DEVPOS(PROW()+1,03);DEVOUT(l15)
                        DEVPOS(PROW()+1,03);DEVOUT(l16)
                        DEVPOS(PROW()+1,03);DEVOUT(l17)
                        DEVPOS(PROW()+1,03);DEVOUT(l18)
                        DEVPOS(PROW()+1,03);DEVOUT(l19)
                        DEVPOS(PROW()+1,03);DEVOUT(l20)
                        DEVPOS(PROW()+1,03);DEVOUT(l21)
                        DEVPOS(PROW()+3,03);DEVOUT(l22)
                        DEVPOS(PROW()+3,03);DEVOUT(l23)
                        DEVPOS(PROW()+1,03);DEVOUT(l24)
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('Responsavel: '+aluno->responsa)
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->d_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('CPF........: '+aluno->r_cpf+'  -  Fone: '+aluno->r_fone)
                        DEVPOS(PROW()+1,00);DEVOUT('Endereco...: '+RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - '+RTRIM(aluno->r_apto))
                        DEVPOS(PROW()+1,00);DEVOUT('Bairro.....: '+RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+RTRIM(aluno->r_estado))
                        DEVPOS(PROW()+1,00);DEVOUT('CEP........: '+RTRIM(aluno->r_cep))
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('Aluno......: '+aluno->matricula+' - '+aluno->aluno)
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->d_negrito)))
                        DEVPOS(PROW(),00);DEVOUT('Grau.......: '+SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),PCOL()+3);DEVOUT('Serie: '+SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),PCOL()+3);DEVOUT('Turma: '+SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW(),PCOL()+3);DEVOUT('Turno: '+SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        DEVPOS(PROW()+1,03);DEVOUT(&(ALLTRIM(prn->a_negrito)))
                        DEVPOS(PROW()+1,03);DEVOUT('*** MESES EM ATRASO ***')
                        DEVPOS(PROW()+1,03);DEVOUT('Parcelas       Valor')
                        mcont_aluno ++
                        mcont_parc := mcont_parc + LEN(m_parcela)
                        DEVPOS(PROW()+1,06);DEVOUT(m_parcela[1,1]+' '+m_parcela[1,2])
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := vlr_men(mgstt,m_parcela[1,1])
                        mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                        DEVPOS(PROW(),15);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        mtot_men := mtot_men + mvlr_men
                        mtot_ger := mtot_ger + mvlr_men
                        i := 0
                        FOR i=2 TO LEN(m_parcela)
                                mvlr_men := 0
                                DEVPOS(PROW()+1,06);DEVOUT(m_parcela[i,1]+' '+m_parcela[i,2])
                                mvlr_men := vlr_men(mgstt,m_parcela[i,1])
                                mvlr_men := mvlr_men - (mvlr_men * (mdesconto/100))
                                DEVPOS(PROW(),15);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                mtot_men := mtot_men + mvlr_men
                                mtot_ger := mtot_ger + mvlr_men
                        NEXT
                        DEVPOS(PROW()+1,05);DEVOUT('----')
                        DEVPOS(PROW(),15);DEVOUT('-----------')
                        DEVPOS(PROW()+1,06);DEVOUT(STRZERO(LEN(m_parcela),2))
                        DEVPOS(PROW(),14);DEVOUT(TRANSFORM(mtot_men,'99,999.99'))
*                        DEVPOS(PROW()+1,89);DEVOUT('TOTAL A PAGAR (+2%): ')
*                        DEVPOS(PROW(),122);DEVOUT(TRANSFORM((mtot_men*1.02),'99,999.99'))
                        DEVPOS(PROW()+1,00);DEVOUT(&(ALLTRIM(prn->d_negrito)))
                        DEVPOS(PROW(),00);DEVOUT((ALLTRIM(prn->d_italico)))
                        SKIP
                        EJECT
                ENDDO
        ENDIF
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('CART_DEB.REL')
        ENDIF
ENDDO
**************************** F I M **************************

