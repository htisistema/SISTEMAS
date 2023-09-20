*********************************************
* FUNCAO P/GERAR DISCO P/IMPRESSAO DE CARNES ELOGICA
*********************************************

FUNCTION safdcar1
*****************
LOCAL mprg:='SAFDCAR1',;
      tela,opcao,lci,cci,lba,cba,mparcela,mparcela1,msele,morde,mseq,mlin,mvalor,mfaixa,;
      mbanlinh1,mbanlinh2,mbanlinh3,mbanlinh4,mbanlinh5,mvlr_bolsa:=0,i:=0,mdata_venc,;
      msoma,mresto,mdv,mnosso_no,mop_bandepe,mqtd_tit:=0,mvalor_tit:=0,mmatricula:=SPACE(6),;
      m_matricula:={},mtela_mat

PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35),mnum_conta:=SPACE(7),mnum_banco:=SPACE(3),mnum_ag:=SPACE(4)

IF ! ver_nivel(mprg,'GERAR DISCO P/IMPRESSAO DE CARNES','125')
        RETURN NIL
ENDIF

lci := 03
cci := 03
lba := 13
cba := 79
tela := SAVESCREEN(00,00,24,79)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('nobrega','nobre');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
**************
SELE('nobre')
**************
mparcela  := SPACE(2)
mparcela1 := SPACE(2)
mbanlinh1 := '- Ate o vencimento receber o valor R$:'+TRANSFORM(mvlr_bolsa,'999.99')
mbanlinh2 := '- Ate 30 dias apos vencimento receber R$:'+TRANSFORM(mvlr_bolsa*1.02,'999.99')
mbanlinh3 := '- Apos 30 dias do vencimento receber o valor  do  documento'
mbanlinh4 := '  mais multa de 2% ao mes e juros de 1% ao mes   '
mbanlinh5 := '- Apos 60 dias sera encaminhado para protesto  '
/*
mbanlinh1 := nobre->banlinh1
mbanlinh2 := nobre->banlinh2
mbanlinh3 := nobre->banlinh3
mbanlinh4 := nobre->banlinh4
mbanlinh5 := nobre->banlinh5
*/
mnum_banco := '   '
mnum_ag := SPACE(4)
mnum_conta := SPACE(7)
mop_bandepe := ' '
@ 23,00 CLEAR TO 23,79
setcor(3)
botao(lci,cci,lba,cba)
setcor(1)
DEVPOS(lci+1,cci+1);DEVOUT('Digite Parcela que deseja [01...12]:    a ')
DEVPOS(lci+2,cci+1);DEVOUT('Instrucoes:')
DEVPOS(lci+7,cci+1);DEVOUT('No.Banco:     No.Agencia:      No.Conta:')
DEVPOS(lci+8,cci+1);DEVOUT('Deseja gerar arquivo Remessa [S/N]:')
DEVPOS(lci+9,cci+1);DEVOUT('Digite a Matricula dos Alunos.....:')
@ lci+1,cci+38 GET mparcela PICT '99' VALID mparcela $ '01,02,03,04,05,06,07,08,09,10,11,12'
@ lci+1,cci+44 GET mparcela1 PICT '99' VALID IF(VAL(mparcela)>VAL(mparcela1),.F.,mparcela $ '01,02,03,04,05,06,07,08,09,10,11,12')
@ lci+2,cci+13 GET mbanlinh1 PICT '@!'
@ lci+3,cci+13 GET mbanlinh2 PICT '@!'
@ lci+4,cci+13 GET mbanlinh3 PICT '@!'
@ lci+5,cci+13 GET mbanlinh4 PICT '@!'
@ lci+6,cci+13 GET mbanlinh5 PICT '@!'
@ lci+7,cci+10 GET mnum_banco PICT '999'
@ lci+7,cci+26 GET mnum_ag PICT '@!'
@ lci+7,cci+41 GET mnum_conta PICT '@!'
@ lci+8,cci+37 GET mop_bandepe PICT '@!' VALID mop_bandepe $ 'S,N'
@ lci+9,cci+37 GET mmatricula PICT '999999' VALID IF(EMPTY(mmatricula),.T.,ver_aluno(mmatricula,,lci+9,cci+45))
READ
IF LASTKEY() = 27
        RESTSCREEN(00,00,24,79,tela)
        RETURN NIL
ENDIF
ASIZE(m_matricula,0)
IF ! EMPTY(mmatricula)
        mensagem('Confirma a inclusao da Matricula [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF opcao = 'S'
                AADD(m_matricula,mmatricula)
        ENDIF
        WHILE LASTKEY() <> 27
                mensagem('Digite a Matricula que deseja Incluir ou <ESC> p/Continuar')
                @ lci+9,cci+37 GET mmatricula PICT '999999' VALID IF(EMPTY(mmatricula),.T.,ver_aluno(mmatricula,,lci+9,cci+45))
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF ASCAN(m_matricula,mmatricula) > 0
                        atencao('Esta Matricula ja foi solicitada')
                        LOOP
                ENDIF
                mensagem('Confirma a inclusao da Matricula [S/n]:')
                opcao := 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF opcao = 'S'
                        AADD(m_matricula,mmatricula)
                ENDIF
        ENDDO
ENDIF
opcao := ' '
opcao := mensagem1('Confirma a Operacao:','S','S,N')
IF LASTKEY() = 27 .OR. opcao = 'N'
        RETURN NIL
ENDIF
********************************
SELE('aluno');ORDSETFOCUS(6)
GO TOP
********************************
msele := SELE()
morde := INDEXORD()
mseq := 1
mlin := SAVESCREEN(24,00,24,79)
i:= 0
*i := VAL(mparcela)
SET CENTURY ON
IF ! imp_arq('NOBREGA.TXT')
        RESTSCREEN(00,00,24,79,tela)
        RETURN NIL
ENDIF
WHILE ! EOF()
        limpa(24,00,24,79)
        SETPOS(24,00);DISPOUT('Esta Gerando o carne em arquivo, aguarde processamento MAT: '+aluno->matricula)
        IF LEN(m_matricula) > 0 .AND. ASCAN(m_matricula,aluno->matricula) = 0
                SKIP
                LOOP
        ENDIF
        IF aluno->gstt = 'DEBI'
                SKIP
                LOOP
        ENDIF
        i:= 0
        FOR i = VAL(mparcela) TO VAL(mparcela1)         //-VAL(mparcela)
                IF STRZERO(i,2) = '01'
                        mfaixa := aluno->faixa_1
                ELSEIF STRZERO(i,2) = '02'
                        mfaixa := aluno->faixa_2
                ELSEIF STRZERO(i,2) = '03'
                        mfaixa := aluno->faixa_3
                ELSEIF STRZERO(i,2) = '04'
                        mfaixa := aluno->faixa_4
                ELSEIF STRZERO(i,2) = '05'
                        mfaixa := aluno->faixa_5
                ELSEIF STRZERO(i,2) = '06'
                        mfaixa := aluno->faixa_6
                ELSEIF STRZERO(i,2) = '07'
                        mfaixa := aluno->faixa_7
                ELSEIF STRZERO(i,2) = '08'
                        mfaixa := aluno->faixa_8
                ELSEIF STRZERO(i,2) = '09'
                        mfaixa := aluno->faixa_9
                ELSEIF STRZERO(i,2) = '10'
                        mfaixa := aluno->faixa_10
                ELSEIF STRZERO(i,2) = '11'
                        mfaixa := aluno->faixa_11
                ELSEIF STRZERO(i,2) = '12'
                        mfaixa := aluno->faixa_12
                ENDIF
                IF ! EMPTY(aluno->dt_tranf)
                        LOOP
                ENDIF
                IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),STRZERO(i,2))
                        LOOP
                ENDIF
                mvalor := 0
                mvlr_bolsa := 0
                IF ver_fx(mfaixa)
                        mvlr_bolsa := tabmen->valor - (tabmen->valor*(fx->desconto/100))
                ENDIF
                mvalor := tabmen->valor
                IF fx->desconto = 100 .OR. mfaixa = 'S'
                        LOOP
                ENDIF
                IF ! matparc(aluno->matricula,STRZERO(i,2))
                        LOOP
                ENDIF
                mnosso_no := STRZERO(i,2)+SUBSTR(aluno->contrato,1,5)+mnum_ag+mnum_conta
                msoma := VAL(SUBSTR(mnosso_no,1,1)) * 1
                msoma := msoma + (VAL(SUBSTR(mnosso_no,2,1)) * 2)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,3,1)) * 1)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,4,1)) * 2)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,5,1)) * 1)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,6,1)) * 2)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,7,1)) * 1)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,8,1)) * 2)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,9,1)) * 1)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,10,1)) * 2)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,11,1)) * 1)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,12,1)) * 2)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,13,1)) * 1)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,14,1)) * 2)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,15,1)) * 1)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,16,1)) * 2)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,17,1)) * 1)
                msoma := msoma + (VAL(SUBSTR(mnosso_no,18,1)) * 2)
                mresto := VAL(SUBSTR(STRZERO(msoma /10,4,2),3,1))
                mdv := 10 - mresto
                IF mdv = 10
                        mdv := 0
                ENDIF
                mdata_venc := CTOD(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3))
                DEVPOS(PROW(),00);DEVOUT(STRZERO(mseq,6))
                DEVPOS(PROW(),PCOL());DEVOUT(aluno->matricula+STRZERO(i,2))
                DEVPOS(PROW(),PCOL());DEVOUT(LEFT(memp+resa+SPACE(30),30))
                DEVPOS(PROW(),PCOL());DEVOUT(SUBSTR(STRZERO(mvalor,14,2),1,11)+SUBSTR(STRZERO(mvalor,14,2),13))
                DEVPOS(PROW(),PCOL());DEVOUT(SUBSTR(DTOC(mdata_venc),1,2)+SUBSTR(DTOC(mdata_venc),4,2)+SUBSTR(DTOC(mdata_venc),7))
                DEVPOS(PROW(),PCOL());DEVOUT('- Ate o vencimento receber o valor R$:'+TRANSFORM(mvlr_bolsa,'999.99')+'                ')
                DEVPOS(PROW(),PCOL());DEVOUT('- Ate o ultimo dia util do mes vigente receber R$:'+TRANSFORM(mvlr_bolsa*1.02,'999.99')+SPACE(4))
                DEVPOS(PROW(),PCOL());DEVOUT('- Apos o ultimo dia util do mes  vigente  receber  valor do ')
                DEVPOS(PROW(),PCOL());DEVOUT('  documento mais multa de 2% ao mes e juros de 1% ao mes    ')
                DEVPOS(PROW(),PCOL());DEVOUT('- Apos 60 dias sera encaminhado para protesto               ')
                DEVPOS(PROW(),PCOL());DEVOUT(aluno->gstt)
                DEVPOS(PROW(),PCOL());DEVOUT(aluno->aluno)
                DEVPOS(PROW(),PCOL());DEVOUT(aluno->responsa+'- CPF: '+aluno->r_cpf)
                DEVPOS(PROW(),PCOL());DEVOUT(aluno->r_rua+aluno->r_num+aluno->r_apto+aluno->r_bairro+aluno->r_cidade+aluno->r_estado)
                DEVPOS(PROW(),PCOL());DEVOUT(mnum_banco+mnum_ag+mnum_conta)
                DEVPOS(PROW(),PCOL());DEVOUT(STRZERO(i,2)+SUBSTR(aluno->contrato,1,5)+STRZERO(mdv,1))
                DEVPOS(PROW()+1,00);DEVOUT('')
                mseq ++
        NEXT
        SELE(msele);ORDSETFOCUS(morde)
        SKIP
ENDDO
RESTSCREEN(24,00,24,79,mlin)
SETPRC(00,00)
SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
atencao('Processamento concluido com sucesso',3)
IF mimp_tipo = 2
        lertexto('NOBREGA.TXT')
ENDIF
SET CENTURY OFF
IF mop_bandepe = 'S'
        ********************************
        SELE('aluno');ORDSETFOCUS(6)
        GO TOP
        ********************************
        msele := SELE()
        morde := INDEXORD()
        mseq := 1
        mlin := SAVESCREEN(24,00,24,79)
        i:= 0
        *i := VAL(mparcela)
        IF ! imp_arq('BANDEPE.TXT')
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        DEVPOS(PROW(),00);DEVOUT('0')
        DEVPOS(PROW(),01);DEVOUT('1REMESSA01COBRANCA      ')
        DEVPOS(PROW(),26);DEVOUT('0')
        DEVPOS(PROW(),27);DEVOUT(mnum_ag)
        DEVPOS(PROW(),31);DEVOUT('0')
        DEVPOS(PROW(),32);DEVOUT(mnum_conta)
        DEVPOS(PROW(),39);DEVOUT(SPACE(7))
        DEVPOS(PROW(),46);DEVOUT('COLEGIO NOBREGA               ')
        DEVPOS(PROW(),76);DEVOUT(mnum_banco)
        DEVPOS(PROW(),79);DEVOUT('BANDEPE        ')
        DEVPOS(PROW(),94);DEVOUT(SUBSTR(DTOC(mdata_sis),1,2)+SUBSTR(DTOC(mdata_sis),4,2)+SUBSTR(DTOC(mdata_sis),7,2))
        DEVPOS(PROW(),100);DEVOUT('01600BPI')
        DEVPOS(PROW(),108);DEVOUT(SPACE(286))
        DEVPOS(PROW(),394);DEVOUT('000001')
        mseq := 2
        WHILE ! EOF()
                limpa(24,00,24,79)
                SETPOS(24,00);DISPOUT('Esta Gerando o ARQUIVO REMESSA, aguarde processamento MAT: '+aluno->matricula)
                IF LEN(m_matricula) > 0 .AND. ASCAN(m_matricula,aluno->matricula) = 0
                        SKIP
                        LOOP
                ENDIF
                IF aluno->gstt = 'DEBI'
                        SKIP
                        LOOP
                ENDIF
                i:= 0
                FOR i = VAL(mparcela) TO VAL(mparcela1)         //-VAL(mparcela)
                        IF STRZERO(i,2) = '01'
                                mfaixa := aluno->faixa_1
                        ELSEIF STRZERO(i,2) = '02'
                                mfaixa := aluno->faixa_2
                        ELSEIF STRZERO(i,2) = '03'
                                mfaixa := aluno->faixa_3
                        ELSEIF STRZERO(i,2) = '04'
                                mfaixa := aluno->faixa_4
                        ELSEIF STRZERO(i,2) = '05'
                                mfaixa := aluno->faixa_5
                        ELSEIF STRZERO(i,2) = '06'
                                mfaixa := aluno->faixa_6
                        ELSEIF STRZERO(i,2) = '07'
                                mfaixa := aluno->faixa_7
                        ELSEIF STRZERO(i,2) = '08'
                                mfaixa := aluno->faixa_8
                        ELSEIF STRZERO(i,2) = '09'
                                mfaixa := aluno->faixa_9
                        ELSEIF STRZERO(i,2) = '10'
                                mfaixa := aluno->faixa_10
                        ELSEIF STRZERO(i,2) = '11'
                                mfaixa := aluno->faixa_11
                        ELSEIF STRZERO(i,2) = '12'
                                mfaixa := aluno->faixa_12
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                LOOP
                        ENDIF
                        IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),STRZERO(i,2))
                                LOOP
                        ENDIF
                        mvalor := 0
                        mvlr_bolsa := 0
                        IF ver_fx(mfaixa)
                                mvlr_bolsa := tabmen->valor - (tabmen->valor*(fx->desconto/100))
                        ENDIF
                        mvalor := tabmen->valor
                        IF fx->desconto = 100 .OR. mfaixa = 'S'
                                LOOP
                        ENDIF
                        IF ! matparc(aluno->matricula,STRZERO(i,2))
                                LOOP
                        ENDIF
                        mnosso_no := STRZERO(i,2)+SUBSTR(aluno->contrato,1,5)+mnum_ag+mnum_conta
                        msoma := VAL(SUBSTR(mnosso_no,1,1)) * 1
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,2,1)) * 2)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,3,1)) * 1)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,4,1)) * 2)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,5,1)) * 1)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,6,1)) * 2)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,7,1)) * 1)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,8,1)) * 2)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,9,1)) * 1)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,10,1)) * 2)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,11,1)) * 1)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,12,1)) * 2)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,13,1)) * 1)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,14,1)) * 2)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,15,1)) * 1)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,16,1)) * 2)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,17,1)) * 1)
                        msoma := msoma + (VAL(SUBSTR(mnosso_no,18,1)) * 2)
                        mresto := VAL(SUBSTR(STRZERO(msoma /10,4,2),3,1))
                        mdv := 10 - mresto
                        IF mdv = 10
                                mdv := 0
                        ENDIF
                        mdata_venc := CTOD(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3))
                        DEVPOS(PROW()+1,00);DEVOUT('1')
                        DEVPOS(PROW(),1);DEVOUT('02')
                        DEVPOS(PROW(),3);DEVOUT(SUBSTR(mcgc_firm,1,2)+SUBSTR(mcgc_firm,4,3)+SUBSTR(mcgc_firm,8,3))
                        DEVPOS(PROW(),11);DEVOUT(SUBSTR(mcgc_firm,12,4))
                        DEVPOS(PROW(),15);DEVOUT(SUBSTR(mcgc_firm,17,2))
                        DEVPOS(PROW(),17);DEVOUT('0')
                        DEVPOS(PROW(),18);DEVOUT(mnum_ag)
                        DEVPOS(PROW(),22);DEVOUT('0')
                        DEVPOS(PROW(),23);DEVOUT(mnum_conta)
                        DEVPOS(PROW(),30);DEVOUT(SPACE(7))
                        DEVPOS(PROW(),37);DEVOUT(SPACE(25))
                        DEVPOS(PROW(),62);DEVOUT('00')
                        DEVPOS(PROW(),64);DEVOUT(STRZERO(i,2)+SUBSTR(aluno->contrato,1,5))
                        DEVPOS(PROW(),71);DEVOUT('0')
                        DEVPOS(PROW(),72);DEVOUT('00')
                        DEVPOS(PROW(),74);DEVOUT('1')
                        DEVPOS(PROW(),75);DEVOUT('0000000000000')
                        DEVPOS(PROW(),88);DEVOUT(SPACE(7))
                        DEVPOS(PROW(),95);DEVOUT('000000000')
                        DEVPOS(PROW(),104);DEVOUT(SPACE(3))
                        DEVPOS(PROW(),107);DEVOUT('5')
                        DEVPOS(PROW(),108);DEVOUT('01')
                        DEVPOS(PROW(),110);DEVOUT(aluno->matricula+STRZERO(i,2)+SPACE(2))
                        DEVPOS(PROW(),120);DEVOUT(SUBSTR(DTOC(mdata_venc),1,2)+SUBSTR(DTOC(mdata_venc),4,2)+SUBSTR(DTOC(mdata_venc),7,2))
                        DEVPOS(PROW(),126);DEVOUT(SUBSTR(STRZERO(mvalor,14,2),1,11)+SUBSTR(STRZERO(mvalor,14,2),13))
                        DEVPOS(PROW(),139);DEVOUT(mnum_banco)
                        DEVPOS(PROW(),142);DEVOUT('00000')
                        DEVPOS(PROW(),147);DEVOUT('01')
                        DEVPOS(PROW(),149);DEVOUT('A')
                        DEVPOS(PROW(),150);DEVOUT(SUBSTR(DTOC(mdata_sis),1,2)+SUBSTR(DTOC(mdata_sis),4,2)+SUBSTR(DTOC(mdata_sis),7,2))
                        DEVPOS(PROW(),156);DEVOUT('00')
                        DEVPOS(PROW(),158);DEVOUT('  ')
                        DEVPOS(PROW(),160);DEVOUT('1')
                        DEVPOS(PROW(),161);DEVOUT('000000000000')
                        DEVPOS(PROW(),173);DEVOUT('888888')
                        DEVPOS(PROW(),179);DEVOUT('0000000000000')
                        DEVPOS(PROW(),192);DEVOUT('0000000000000')
                        DEVPOS(PROW(),205);DEVOUT('0000000000000')
                        DEVPOS(PROW(),218);DEVOUT('01')
                        DEVPOS(PROW(),220);DEVOUT(SUBSTR(aluno->r_cpf,1,9))
                        DEVPOS(PROW(),229);DEVOUT('000')
                        DEVPOS(PROW(),232);DEVOUT(SUBSTR(aluno->r_cpf,11,2))
                        DEVPOS(PROW(),234);DEVOUT(aluno->responsa)
                        DEVPOS(PROW(),274);DEVOUT(LEFT(ALLTRIM(aluno->r_rua)+' No '+ALLTRIM(aluno->r_num)+IF(! EMPTY(aluno->r_apto),' Apto '+ALLTRIM(aluno->r_apto),''),40))
                        DEVPOS(PROW(),314);DEVOUT(LEFT(aluno->r_bairro,12))
                        DEVPOS(PROW(),326);DEVOUT(SUBSTR(aluno->r_cep,1,5))
                        DEVPOS(PROW(),331);DEVOUT(SUBSTR(aluno->r_cep,6,3))
                        DEVPOS(PROW(),334);DEVOUT(LEFT(aluno->r_cidade,15))
                        DEVPOS(PROW(),349);DEVOUT(aluno->r_estado,)
                        DEVPOS(PROW(),351);DEVOUT(aluno->responsa)
                        DEVPOS(PROW(),391);DEVOUT('0')
                        DEVPOS(PROW(),392);DEVOUT('07')
                        DEVPOS(PROW(),394);DEVOUT(STRZERO(mseq,6))
                        mqtd_tit ++
                        mvalor_tit := mvalor_tit + mvalor
                        mseq ++
                        DEVPOS(PROW()+1,00);DEVOUT('8')
                        DEVPOS(PROW(),1);DEVOUT('1')
                        DEVPOS(PROW(),2);DEVOUT('0')
                        DEVPOS(PROW(),3);DEVOUT(mnum_ag)
                        DEVPOS(PROW(),7);DEVOUT('0')
                        DEVPOS(PROW(),8);DEVOUT(mnum_conta)
                        DEVPOS(PROW(),15);DEVOUT(aluno->matricula+STRZERO(i,2)+SPACE(2))
                        DEVPOS(PROW(),25);DEVOUT('Ate o vencimento receber o valor R$:'+TRANSFORM(mvlr_bolsa,'999.99')+'                           ')
                        DEVPOS(PROW(),94);DEVOUT('3')
                        DEVPOS(PROW(),95);DEVOUT('Ate o ultimo dia util do mes vigente receber R$:'+TRANSFORM(mvlr_bolsa*1.02,'999.99')+'               ')
                        DEVPOS(PROW(),164);DEVOUT('3')
                        DEVPOS(PROW(),165);DEVOUT('Apos o ultimo dia util do mes  vigente  receber  valor do            ')
                        DEVPOS(PROW(),234);DEVOUT('3')
                        DEVPOS(PROW(),235);DEVOUT('documento mais multa de 2% ao mes e juros de 1% ao mes               ')
                        DEVPOS(PROW(),304);DEVOUT('3')
                        DEVPOS(PROW(),305);DEVOUT('Apos 60 dias sera encaminhado para protesto                          ')
                        DEVPOS(PROW(),374);DEVOUT('3')
                        DEVPOS(PROW(),375);DEVOUT(SPACE(19))
                        DEVPOS(PROW(),394);DEVOUT(STRZERO(mseq,6))
                        mseq ++
                NEXT
                SELE(msele);ORDSETFOCUS(morde)
                SKIP
        ENDDO
        DEVPOS(PROW()+1,00);DEVOUT('9')
        DEVPOS(PROW(),1);DEVOUT(STRZERO(mqtd_tit,6))
        DEVPOS(PROW(),7);DEVOUT(SUBSTR(STRZERO(mvalor_tit,14,2),1,11)+SUBSTR(STRZERO(mvalor_tit,14,2),13))
        DEVPOS(PROW(),20);DEVOUT(SPACE(374))
        DEVPOS(PROW(),394);DEVOUT(STRZERO(mseq,6))
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(24,00,24,79,mlin)
        atencao('Processamento concluido com sucesso',3)
        IF mimp_tipo = 2
                lertexto('BANDEPE.TXT')
        ENDIF
ENDIF
RETURN NIL

