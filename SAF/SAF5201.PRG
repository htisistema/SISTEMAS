************************************************
* RELATORIO DE ALUNOS EM DIA
************************************************

FUNCTION saf5201
***************
LOCAL mprg:='SAF5201'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mcont_parc,mdesconto,mvlr_men,mgstt,mtot_men,mtot_ger,mindex,;
      tela,mgst,mmat,mop,mfaixa,mflag,mtp,msele,morde,mcont,mtipo_rel:=SPACE(1)

LOCAL mpag:=0
LOCAL li:=00,ci:=01,lb:=24,cb:=78
LOCAL l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23,l24
PRIVATE m_parcela:={},mquantd_parc
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DE ALUNOS EM DIA C/MENSALIDADES','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco:=REPLI(CHR(223),80)
*mtraco := REPLI('=',80)
lci := 06
cci := 05
lba := 18
cba := 75
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cartadia','ctdia');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('discalu','dalu','E');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
**************
SELE('dalu')
ZAP
*************
WHILE .T.
        l1:=l2:=l3:=l4:=l5:=l6:=l7:=l8:=l9:=l10:=l11:=l12:=l13:=l14:=l15:=l16:=;
        l17:=l18:=l19:=l20:=l21:=l22:=l23:=l24:= SPACE(73)
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
        mtp := 'D'
        ***************
        SELE('aluno');ORDSETFOCUS(1)
        GO TOP
        ***************
        setcor(3)
        botao(lci,cci,lba,cba)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        @ lci+10,cci+1 TO lci+10,cba-1
        setcor(1)
        limpa(23,00,23,79)
        DEVPOS(lci+5,cci+1);DEVOUT('Digite ate que MES deseja.............................: ')
        DEVPOS(lci+6,cci+1);DEVOUT('Por ordem de:[1] p/Resp.[2]GSTT [3]Aluno [4] Matricula: ')
        DEVPOS(lci+7,cci+1);DEVOUT('Digite quantidade de parcelas devidas.................: ')
        DEVPOS(lci+8,cci+1);DEVOUT('Tipo de Relatorio [C]arta [S]o nome aluno.............: ')
        DEVPOS(lci+9,cci+1);DEVOUT('<I>mpressao ou Gerar <D>isco..........................: ')
        DEVPOS(lci+11,cci+1);DEVOUT('Registro Processados: ')
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
        mtipo_rel := 'C'
        @ lci+5,cci+57 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        @ lci+6,cci+57 GET mindex PICT '9' VALID IF(mindex<1 .OR. mindex > 4,.F.,.T.) WHEN mop = 3
        @ lci+7,cci+57 GET mquantd_parc PICT '99'
        @ lci+8,cci+57 GET mtipo_rel PICT '@!' VALID mtipo_rel $ 'C,S' WHEN mop = 3
        @ lci+9,cci+57 GET mtp PICT '@!' VALID mtp $ 'I,D'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF mtipo_rel = 'C'
                *****************
                SELE('ctdia')
                GO TOP
                *****************
                l1  := ctdia->lin1
                l2  := ctdia->lin2
                l3  := ctdia->lin3
                l4  := ctdia->lin4
                l5  := ctdia->lin5
                l6  := ctdia->lin6
                l7  := ctdia->lin7
                l8  := ctdia->lin8
                l9  := ctdia->lin9
                l10 := ctdia->lin10
                l11 := ctdia->lin11
                l12 := ctdia->lin12
                l13 := ctdia->lin13
                l14 := ctdia->lin14
                l15 := ctdia->lin15
                l16 := ctdia->lin16
                l17 := ctdia->lin17
                l18 := ctdia->lin18
                l19 := ctdia->lin19
                l20 := ctdia->lin20
                l21 := ctdia->lin21
                l22 := ctdia->lin22
                l23 := ctdia->lin23
                l24 := ctdia->lin24
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
                ctdia->lin1 :=l1
                ctdia->lin2 :=l2
                ctdia->lin3 :=l3
                ctdia->lin4 :=l4
                ctdia->lin5 :=l5
                ctdia->lin6 :=l6
                ctdia->lin7 :=l7
                ctdia->lin8 :=l8
                ctdia->lin9 :=l9
                ctdia->lin10:=l10
                ctdia->lin11:=l11
                ctdia->lin12:=l12
                ctdia->lin13:=l13
                ctdia->lin14:=l14
                ctdia->lin15:=l15
                ctdia->lin16:=l16
                ctdia->lin17:=l17
                ctdia->lin18:=l18
                ctdia->lin20:=l20
                ctdia->lin21:=l21
                ctdia->lin22:=l22
                ctdia->lin23:=l23
                ctdia->lin24:=l24
                RESTSCREEN(00,00,24,79,tela1)
        ENDIF
        IF mtp = 'I'
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
                        IF ! imp_arq('CARTA_OK.REL')
                                LOOP
                        ENDIF
                        mflag := ' '
                        i := 0
                        mpag :=0
                        WHILE ! EOF()
                                mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                mtot_men := 0
                                mdesconto := 0
                                mvlr_men := 0
                                ASIZE(m_parcela,0)
                                ver_parc(aluno->matricula,mmes,mquantd_parc)
                                IF LEN(m_parcela) > 0
                                        SKIP
                                        LOOP
                                ENDIF
                                IF mtipo_rel = 'C'
                                        imprt('M','E',1)
                                        DEVPOS(PROW(),00);DEVOUT(memp+resa)
                                        imprt('M','N',1)
                                        DEVPOS(PROW(),00);DEVOUT(mend_firm+' - '+mcid_firm)
                                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                        DEVPOS(PROW()+6,00);DEVOUT(PADL(sis_cidade+' '+imp_data(DATE()),80))
                                        DEVPOS(PROW()+6,00);DEVOUT('Do: '+memp+resa)
                                        DEVPOS(PROW()+1,00);DEVOUT('Aos: Srs. Pais e/ou Responsavel: '+aluno->responsa)
                                        DEVPOS(PROW()+1,00);DEVOUT('Aluno(a): '+aluno->matricula+'-'+aluno->aluno)
                                        DEVPOS(PROW()+3,03);DEVOUT(l1)
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
                                        DEVPOS(PROW()+1,03);DEVOUT(l22)
                                        DEVPOS(PROW()+1,03);DEVOUT(l23)
                                        DEVPOS(PROW()+1,03);DEVOUT(l24)
/*
                                        DEVPOS(PROW()+6,20);DEVOUT('Estamos  proximo  do  encerramento  do ano letivo  de '+mdiretorio+' e')
                                        DEVPOS(PROW()+2,00);DEVOUT('observando  nosso  arquivo financeiro constatamos o rigoroso pagamento de  todas')
                                        DEVPOS(PROW()+2,00);DEVOUT('as parcelas ate '+mmes+'/'+mdiretorio+'.')
                                        DEVPOS(PROW()+2,20);DEVOUT('Agradecemos o empenho  realizado para o correto cumprimento')
                                        DEVPOS(PROW()+2,00);DEVOUT('do acordo firmado conosco.')
                                        DEVPOS(PROW()+2,20);DEVOUT('Pessoa como  o  Sr.(a)  contribui de forma  concreta para a')
                                        DEVPOS(PROW()+2,00);DEVOUT('continuidade do trabalho serio e responsavel que ja realizamos ha 82 anos.')
                                        DEVPOS(PROW()+2,20);DEVOUT('Esperamos continuar mantendo da melhor forma  possivel essa')
                                        DEVPOS(PROW()+2,00);DEVOUT('nossa parceria em beneficio do(a) seu filho(a).')
                                        DEVPOS(PROW()+6,20);DEVOUT('Atenciosamente,')
                                        DEVPOS(PROW()+6,40);DEVOUT('A Direcao')
*/
                                        EJECT
                                ELSE
                                        mtit:='Relatorio de Alunos em DIA com MENSALIDADES'
                                        IF PROW() = 59 .OR. mpag = 0
                                                mpag ++
                                                IF mpag > 1
                                                        EJECT
                                                ENDIF
                                                cabecalho(mpag,mtit)
                                        ENDIF
                                        DEVPOS(PROW()+1,00);DEVOUT('Aluno(a): '+aluno->matricula+'-'+aluno->aluno)
                                ENDIF
                                SKIP
                        ENDDO
                ELSEIF mop = 1
                        IF ! imp_arq('CARTA_OK.REL')
                                LOOP
                        ENDIF
                        mflag := ' '
                        i := 0
                        WHILE mgst = aluno->gstt .AND. ! EOF()
                                mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                mtot_men := 0
                                mdesconto := 0
                                mvlr_men := 0
                                ASIZE(m_parcela,0)
                                ver_parc(aluno->matricula,mmes,mquantd_parc)
                                IF LEN(m_parcela) > 0
                                        SKIP
                                        LOOP
                                ENDIF
                                imprt('M','E',1)
                                DEVPOS(PROW(),00);DEVOUT(memp+resa)
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT(mend_firm+' - '+mcid_firm)
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                DEVPOS(PROW()+6,00);DEVOUT(PADL(sis_cidade+' '+imp_data(DATE()),80))
                                DEVPOS(PROW()+6,00);DEVOUT('Do: '+memp+resa)
                                DEVPOS(PROW()+1,00);DEVOUT('Aos: Srs. Pais e/ou Responsavel: '+aluno->responsa)
                                DEVPOS(PROW()+1,00);DEVOUT('Aluno(a): '+aluno->matricula+'-'+aluno->aluno)
                                DEVPOS(PROW()+3,03);DEVOUT(l1)
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
                                DEVPOS(PROW()+1,03);DEVOUT(l22)
                                DEVPOS(PROW()+1,03);DEVOUT(l23)
                                DEVPOS(PROW()+1,03);DEVOUT(l24)
/*
                                DEVPOS(PROW()+6,20);DEVOUT('Estamos  proximo  do  encerramento  do ano letivo  de '+mdiretorio+' e')
                                DEVPOS(PROW()+2,00);DEVOUT('observando  nosso  arquivo financeiro constatamos o rigoroso pagamento de  todas')
                                DEVPOS(PROW()+2,00);DEVOUT('as parcelas ate '+mmes+'/'+mdiretorio+'.')
                                DEVPOS(PROW()+2,20);DEVOUT('Agradecemos o empenho  realizado para o correto cumprimento')
                                DEVPOS(PROW()+2,00);DEVOUT('do acordo firmado conosco.')
                                DEVPOS(PROW()+2,20);DEVOUT('Pessoa como  o  Sr.(a)  contribui de forma  concreta para a')
                                DEVPOS(PROW()+2,00);DEVOUT('continuidade do trabalho serio e responsavel que ja realizamos ha 82 anos.')
                                DEVPOS(PROW()+2,20);DEVOUT('Esperamos continuar mantendo da melhor forma  possivel essa')
                                DEVPOS(PROW()+2,00);DEVOUT('nossa parceria em beneficio do(a) seu filho(a).')
                                DEVPOS(PROW()+6,20);DEVOUT('Atenciosamente,')
                                DEVPOS(PROW()+6,40);DEVOUT('A Direcao')
*/
                                SKIP
                                EJECT
                        ENDDO
                ELSEIF mop = 2
                        IF ! imp_arq('CARTA_OK.REL')
                                LOOP
                        ENDIF
*                        SET DEVI TO PRINT
                        mpag := 1
                        i := 0
                        ***************
                        SELE('aluno');ORDSETFOCUS(4)
                        GO TOP
                        ***************
                        IF ! aluno->(DBSEEK(mmat))
                                atencao('Nao foi encontrado esta MATRICULA')
                                LOOP
                        ENDIF
                        WHILE mmat = aluno->matricula .AND. ! EOF()
                                mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                mtot_men := 0
                                mdesconto := 0
                                mvlr_men := 0
                                ASIZE(m_parcela,0)
                                ver_parc(aluno->matricula,mmes,mquantd_parc)
                                IF LEN(m_parcela) > 0
                                        SKIP
                                        LOOP
                                ENDIF
                                imprt('M','E',1)
                                DEVPOS(PROW(),00);DEVOUT(memp+resa)
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT(mend_firm+' - '+mcid_firm)
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                DEVPOS(PROW()+6,00);DEVOUT(PADL(sis_cidade+' '+imp_data(DATE()),80))
                                DEVPOS(PROW()+6,00);DEVOUT('Do: '+memp+resa)
                                DEVPOS(PROW()+1,00);DEVOUT('Aos: Srs. Pais e/ou Responsavel: '+aluno->responsa)
                                DEVPOS(PROW()+1,00);DEVOUT('Aluno(a): '+aluno->matricula+'-'+aluno->aluno)
                                DEVPOS(PROW()+1,03);DEVOUT(l1)
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
                                DEVPOS(PROW()+1,03);DEVOUT(l22)
                                DEVPOS(PROW()+1,03);DEVOUT(l23)
                                DEVPOS(PROW()+1,03);DEVOUT(l24)
/*
                                DEVPOS(PROW()+6,20);DEVOUT('Estamos  proximo  do  encerramento  do ano letivo  de '+mdiretorio+' e')
                                DEVPOS(PROW()+2,00);DEVOUT('observando  nosso  arquivo financeiro constatamos o rigoroso pagamento de  todas')
                                DEVPOS(PROW()+2,00);DEVOUT('as parcelas ate '+mmes+'/'+mdiretorio+'.')
                                DEVPOS(PROW()+2,20);DEVOUT('Agradecemos o empenho  realizado para o correto cumprimento')
                                DEVPOS(PROW()+2,00);DEVOUT('do acordo firmado conosco.')
                                DEVPOS(PROW()+2,20);DEVOUT('Pessoa como  o  Sr.(a)  contribui de forma  concreta para a')
                                DEVPOS(PROW()+2,00);DEVOUT('continuidade do trabalho serio e responsavel que ja realizamos ha 82 anos.')
                                DEVPOS(PROW()+2,20);DEVOUT('Esperamos continuar mantendo da melhor forma  possivel essa')
                                DEVPOS(PROW()+2,00);DEVOUT('nossa parceria em beneficio do(a) seu filho(a).')
                                DEVPOS(PROW()+6,20);DEVOUT('Atenciosamente,')
                                DEVPOS(PROW()+6,40);DEVOUT('A Direcao')
*/
                                SKIP
                                EJECT
                        ENDDO
                ENDIF
                imprt('M','N',1)
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                IF mimp_tipo = 2
                        lertexto('CARTA_OK.REL')
                ENDIF
                LOOP
        ELSE
                mensagem('Confirma o processo [S/n]:')
                mcont := 0
                opcao := 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = 'N'
                        LOOP
                ENDIF
                mensagem('Aguarde o final do processo...')
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
                        msele := SELE()
                        morde := INDEXORD()
                        mflag := ' '
                        i := 0
                        WHILE ! EOF()
                                mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                mtot_men := 0
                                mdesconto := 0
                                mvlr_men := 0
                                ASIZE(m_parcela,0)
                                ver_parc(aluno->matricula,mmes,mquantd_parc)
                                IF LEN(m_parcela) > 0
                                        SKIP
                                        LOOP
                                ENDIF
                                DEVPOS(lci+11,cci+23);DEVOUT(STRZERO(mcont++,5)+'-'+aluno->responsa)
                                SELE('dalu')
                                ADIREG()
                                dalu->responsa := aluno->responsa
                                dalu->aluno    := aluno->matricula+'-'+aluno->aluno
                                dalu->gstt     := aluno->gstt
                                dalu->rua      := aluno->r_rua
                                dalu->num      := aluno->r_num
                                dalu->apto     := aluno->r_apto
                                dalu->bairro   := aluno->r_bairro
                                dalu->cidade   := aluno->r_cidade
                                dalu->estado   := aluno->r_estado
                                dalu->cep      := aluno->r_cep
                                dalu->mes_ano  := mmes+'/'+mdiretorio
                                dalu->ano      := mdiretorio
                                SELE(msele);ORDSETFOCUS(morde)
                                SKIP
                        ENDDO
                ELSEIF mop = 1
                        mflag := ' '
                        i := 0
                        msele := SELE()
                        morde := INDEXORD()
                        WHILE mgst = aluno->gstt .AND. ! EOF()
                                mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                mtot_men := 0
                                mdesconto := 0
                                mvlr_men := 0
                                ASIZE(m_parcela,0)
                                ver_parc(aluno->matricula,mmes,mquantd_parc)
                                IF LEN(m_parcela) > 0
                                        SKIP
                                        LOOP
                                ENDIF
                                DEVPOS(lci+11,cci+23);DEVOUT(STRZERO(mcont++,5)+'-'+aluno->responsa)
                                SELE('dalu')
                                ADIREG()
                                dalu->responsa := aluno->responsa
                                dalu->aluno    := aluno->matricula+'-'+aluno->aluno
                                dalu->gstt     := aluno->gstt
                                dalu->rua      := aluno->r_rua
                                dalu->num      := aluno->r_num
                                dalu->apto     := aluno->r_apto
                                dalu->bairro   := aluno->r_bairro
                                dalu->cidade   := aluno->r_cidade
                                dalu->estado   := aluno->r_estado
                                dalu->cep      := aluno->r_cep
                                dalu->mes_ano  := mmes+'/'+mdiretorio
                                dalu->ano      := mdiretorio
                                SELE(msele);ORDSETFOCUS(morde)
                                SKIP
                        ENDDO
                ELSEIF mop = 2
                        mpag := 1
                        i := 0
                        ***************
                        SELE('aluno');ORDSETFOCUS(4)
                        GO TOP
                        ***************
                        IF ! aluno->(DBSEEK(mmat))
                                atencao('Nao foi encontrado esta MATRICULA')
                                LOOP
                        ENDIF
                        msele := SELE()
                        morde := INDEXORD()
                        WHILE mmat = aluno->matricula .AND. ! EOF()
                                mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                mtot_men := 0
                                mdesconto := 0
                                mvlr_men := 0
                                ASIZE(m_parcela,0)
                                ver_parc(aluno->matricula,mmes,mquantd_parc)
                                IF LEN(m_parcela) > 0
                                        SKIP
                                        LOOP
                                ENDIF
                                DEVPOS(lci+11,cci+23);DEVOUT(STRZERO(mcont++,5)+'-'+aluno->responsa)
                                SELE('dalu')
                                ADIREG()
                                dalu->responsa := aluno->responsa
                                dalu->aluno    := aluno->matricula+'-'+aluno->aluno
                                dalu->gstt     := aluno->gstt
                                dalu->rua      := aluno->r_rua
                                dalu->num      := aluno->r_num
                                dalu->apto     := aluno->r_apto
                                dalu->bairro   := aluno->r_bairro
                                dalu->cidade   := aluno->r_cidade
                                dalu->estado   := aluno->r_estado
                                dalu->cep      := aluno->r_cep
                                dalu->mes_ano  := mmes+'/'+mdiretorio
                                dalu->ano      := mdiretorio
                                SELE(msele);ORDSETFOCUS(morde)
                                SKIP
                        ENDDO
                        DBCOMMITALL()
                        DBUNLOCKALL()
                ENDIF
                limpa(23,00,23,79)
                mensagem1('Insira o disco no drive A: p/fazer a copia p/o disco OK e pressione <ENTER> p/continuar')
                COPY FILE ('\SAF\'+mdiretorio+'\DISCALU.DBF') TO A:DISCALU.DBF
                mensagem1('Fim da copia, pressione <ENTER> p/continuar')
        ENDIF
ENDDO
RETURN NIL
**************************** F I M **************************

