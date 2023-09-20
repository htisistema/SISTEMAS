********************
* FECHAMENTO DO CAIXA
********************

FUNCTION saf33
**************
LOCAL MPRG:='SAF33'
LOCAL tela,telaprin,lci,cci,lba,cba,li,ci,la,ca,lc,cc,lb,cb,mopcao,opcao,;
      mtot_din,mtot_ch,mtot_ch_p,mtot_vale,mtot_dup,mtot_cart,mtot_av,mtot_fi,;
      mtot_ap,mtot_re,mtot_dp,mdata,mtraco,mfundo,msangria,mquantd,mtot_des,;
      mdescri2,tela1,tela2,mnumero1,mnumero2,mcod_vend,mcod_op,mtip_ordem,mecf,;
      mdata_ped,m_ano:={},m_anotot:={},m_multa:={},mpont:=0,mtot_div:=0,mtot_doc:=0

LOCAL mtot_vol,mtot_dia,mtot_ped,mtot_geral,mtipo_rel,i,mtot_mul:=0,;
      mfin_aux,misento,mn_isento,mdup_rec,mtipo_form:=' ',mnum_cx,mponto,mprocura

PRIVATE mnome_ven,mcom_ven,mdescri1,menv
PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)
IF ! ver_nivel(mprg,'FECHAMENTO DO CAIXA','1345',nivel_acess)
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)

lci := 04
cci := 11
lba := 16
cba := 59

li := 10
ci := 64
la := 13
ca := 76

lc := 16
cc := 45
lb := 19
cb := 79

tela := SAVESCREEN(00,00,24,79)
*tela1 := SAVESCREEN(li,ci-1,la+1,ca)
*--------------------------------------------------------------------------
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('safcaixa','caix');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        ASIZE(m_ano,0)
        ASIZE(m_anotot,0)
        ASIZE(m_multa,0)
        mtipo_imp := 'M'
        mtot_des := 0
        mtot_din := 0
        mtot_ch := 0
        mtot_ch_p := 0
        mtot_vale := 0
        mtot_dup := 0
        mtot_cart := 0
        mtot_av := 0
        mtot_ap := 0
        mtot_re := 0
        mtot_dp := 0
        mtot_fi := 0
        mdup_rec:= 0
        mtot_div:= 0
        mtot_mul:= 0
        mtot_doc:= 0
        mfundo := 0
        msangria := 0
        mquantd := 0
        mdata := mdata_sis
        mdescri1 := SPACE(40)
        mdescri2 := SPACE(40)
        mnumero1 := 0
        mnumero2 := 0
        mcod_vend := 0
        mcod_op := 0
        mtip_ordem := '1'
        mecf := ' '
        botao(lci,cci,lba,cba)
        DEVPOS(lci,cci+2);DEVOUT('Resumo do Dia: ')

        DEVPOS(lci+1,cci+1);DEVOUT('Fundo de Caixa.............:')
        DEVPOS(lci+2,cci+1);DEVOUT('Total em Mensalidade.......:')
        DEVPOS(lci+3,cci+1);DEVOUT('Total Recebimentos Diversos:')
        @ lci+4,cci+1 TO lci+4,cba-1
        DEVPOS(lci+5,cci+1);DEVOUT('Total de Vales.............:')
        DEVPOS(lci+6,cci+1);DEVOUT('Total de Despesas..........:')
        DEVPOS(lci+7,cci+1);DEVOUT('Retirada p/Deposito........:')
        setcor(5)
        DEVPOS(lci+9,cci+1);DEVOUT('TOTAL DO RECEBIMENTO (A)...:')
        DEVPOS(lci+10,cci+1);DEVOUT('TOTAL DE DEBITOS (B).......:')
        DEVPOS(lci+11,cci+1);DEVOUT('Somatorio (A) + (B)........:')
        setcor(1)
        @ lci,cci+17 GET mdata
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        ****************
        SELE('caix');ORDSETFOCUS(1)
        GO TOP
        ****************

        SEEK DTOS(mdata)

        IF ! FOUND()
                atencao('Nenhum movimento neste Dia !!!')
                RESTSCREEN(00,00,24,79,tela)
                CLOSE ALL
                RELEASE mnome_ven,mcom_ven,mdescri1
                RETURN NIL
        ENDIF
        mponto := RECNO()
        mensagem('Aguarde um Momento OK !!!')
        limpa(23,00,23,79)
        botao(li,ci,la,ca,,' Opcoes ')
        opcao := 0
        @ li+1,ci+1 PROMPT ' Geral     ' MESSAGE 'Fechamento Geral - <ESC> Abandonar'
        @ li+2,ci+1 PROMPT ' Operador  ' MESSAGE 'Fechamento por Operador - <ESC> Abandonar'
        SET INTEN ON
        MENU TO opcao
        IF LASTKEY() = 27
                LOOP
        ENDIF

*       IF opcao = 1
*               IF ! ver_nivel(mprg+'GE','FECHAMENTO DO CAIXA (GERAL)','135',nivel_acess)
*                       LOOP
*               ENDIF
        IF opcao = 2
*               IF ! ver_nivel(mprg+'OP','FECHAMENTO DO CAIXA (POR OPERADOR)','135',nivel_acess)
*                       LOOP
*               ENDIF
                tela2 := SAVESCREEN(01,00,24,79)
                botao(lc,cc,lb,cb,,' Fechamento p/Operador ')
                DEVPOS(lc+1,cc+2);DEVOUT('Codigo do Operador:')
                @ lc+1,cc+22 GET mcod_op PICT '999' VALID ven(mcod_op,lc+2,cc+1)
                READ
                IF LASTKEY() = 27
                        RESTSCREEN(00,00,24,79,tela)
                        LOOP
                ENDIF
                INKEY(2)
                RESTSCREEN(01,00,24,79,tela2)
        ENDIF
        WHILE caix->data = mdata .AND. ! EOF()
                IF ! EMPTY(caix->ano)
                        mpont := 0
                        mpont := ASCAN(m_ano,caix->ano)
                        IF mpont > 0
                                m_anotot[mpont,1] := m_anotot[mpont,1] + caix->valor
                                m_multa[mpont,1] := m_multa[mpont,1] + caix->valor_com
                                mtot_mul := mtot_mul + caix->valor_com
                                mtot_doc := mtot_doc + 1
                        ELSE
                                AADD(m_ano,caix->ano)
                                AADD(m_anotot,{caix->valor,caix->ano})
                                AADD(m_multa,{caix->valor_com,caix->ano})
                                mtot_mul := mtot_mul + caix->valor_com
                                mtot_doc := mtot_doc + 1
                        ENDIF
                ENDIF
                IF opcao = 2
                        IF caix->cod_opera <> STRZERO(mcod_op,3)
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                IF tipo = 'FD'
                        mfundo += valor
                        SKIP
                        LOOP
                ENDIF
                IF tipo = 'SG'
                        msangria += valor
                        SKIP
                        LOOP
                ENDIF
                IF tipo = 'VL' .AND. pg = '*'
                        mtot_re += valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'VL'
                        mtot_vale += valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'ME'
                        mtot_din += valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'DE'
                        mtot_des += valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'RE'
                        mtot_re += valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'DP'
                        mtot_dp += valor
                        SKIP
                        LOOP
                ELSE
                        mtot_re += caix->valor
                        SKIP
                        LOOP
                ENDIF
        ENDDO
        DEVPOS(lci+1,cci+30);DEVOUTPICT(mfundo,'99,999,999.99')
        DEVPOS(lci+2,cci+30);DEVOUTPICT(mtot_din,'99,999,999.99')
        DEVPOS(lci+3,cci+30);DEVOUTPICT(mtot_re,'99,999,999.99')

        DEVPOS(lci+5,cci+30);DEVOUTPICT(mtot_vale,'99,999,999.99')
        DEVPOS(lci+6,cci+30);DEVOUTPICT(mtot_des,'99,999,999.99')
        DEVPOS(lci+7,cci+30);DEVOUTPICT(mtot_dp,'99,999,999.99')
        setcor(5)
        DEVPOS(lci+9,cci+30);DEVOUTPICT(mfundo + mtot_din + mtot_ch + mtot_cart + mtot_dup + mtot_ch_p + mtot_re + mdup_rec,'99,999,999.99')
        DEVPOS(lci+10,cci+30);DEVOUTPICT(mtot_des + mtot_dp - mtot_vale,'99,999,999.99')
        DEVPOS(lci+11,cci+30);DEVOUTPICT(mfundo + mtot_din + mtot_ch + mtot_cart + mtot_dup + mtot_ch_p + mtot_re + mtot_fi +mdup_rec + mtot_des + mtot_dp - mtot_vale,'99,999,999.99')
        setcor(1)
        mtipo_rel := mensagem1('Informe o tipo de Fechamento [R]esumo [G]eral: ','G','G,R',20)
        IF LASTKEY() = 27
                LOOP
        ENDIF
        menv := 'N'
*        mtip_ordem := 8         //mensagem1('Escolha o tipo de ordem que deseja [1]Pedido [8]tipo de documento:','1','1,8',20)
        mtipo_form := 'F'
        mtipo_form:= mensagem1('Tipo de Relatorio [F]ormulario [B]obina: ','B','F,B',20)
        ********************
        ORDSETFOCUS(8)
        GO TOP
        ********************

        SEEK DTOS(mdata)

        IF ! FOUND()
                atencao('Nao existe Movimento nesta Data !!!')
                LOOP
        ENDIF
        mensagem('Aguarde o final da Impressao - [ESC]Abandonar')
        mpag := 1
        IF ! imp_arq('CX'+SUBSTR(DTOC(mdata),1,2)+SUBSTR(DTOC(mdata),4,2)+SUBSTR(DTOC(mdata),7)+'.REL','R','S',20)
                LOOP
        ENDIF
        mtraco := REPLI('=',80)
        mtit := 'Fechamento do Caixa'
        mtipo := 'Data do Fechamento: '+DTOC(mdata)
        IF mtipo_form = 'F'

                imprt(mtipo_imp,'C',0,17)

                cabecalho(mpag,mtit,mtipo)

                DEVPOS(PROW()+1,02);DEVOUT('TP/NF')
                DEVPOS(PROW(),09);DEVOUT('Descricao')
                DEVPOS(PROW(),75);DEVOUT('Valor')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)

                WHILE data = mdata .AND. ! EOF()
                        IF opcao = 2
                                IF caix->cod_opera <> STRZERO(mcod_op,3)
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF tipo = 'DF'
                                SKIP
                                LOOP
                        ENDIF
                        IF descri1 <> SPACE(40) .OR. descri2 <> SPACE(40)
                                mdescri1 := descri1
                                mdescri2 := descri2+'  Vend.:'+cod_vend+' Op:'+cod_opera
                        ELSE
                                IF tipo = 'FD'
                                        mdescri1 := 'Fundo de Caixa  -  Operador:'+cod_opera
                                        mdescri2 := SPACE(40)
                                ENDIF
                                IF tipo = 'SG'
                                        mdescri1 = 'Sangria do Caixa  -  Operador:'+cod_opera
                                        mdescri2 := SPACE(40)
                                ENDIF
                                IF tipo = 'TR'
                                        mdescri1 = 'TRANSFERENCIA DE MERCADORIA        No.: '+num_dup+'   Venc.: '+DTOC(venci)
                                        mdescri2 := '   Cod.Cli.: '+cod_cli+'  Vend:'+cod_vend+' Op:'+cod_opera
                                ELSEIF tipo = 'ME'
                                        mdescri1 = 'Mensalidade do ANO: '+caix->ano+'                                    '
                                        mdescri2 := '                                                      Cod.Cli.: '+cod_cli+'  Vend:'+cod_vend+' Op:'+cod_opera
                                ELSE
                                        mdescri1 = 'Outros Recebimentos: '+caix->ano+'                                   '
                                        mdescri2 := '                                                      Cod.Cli.: '+cod_cli+'  Vend:'+cod_vend+' Op:'+cod_opera
                                ENDIF
                        ENDIF
                        IF mtipo_rel = 'G'
*                                IF mopcao = 'I'
                                IF mimp_tipo = 1
                                        DEVPOS(PROW()+1,00);DEVOUT(num_dup)
                                        imprt(mtipo_imp,'C',0,17)
                                        DEVPOS(PROW(),17);DEVOUT(RTRIM(mdescri1)+RTRIM(mdescri2))
                                        DEVPOS(PROW(),121);DEVOUTPICT(valor,'9,999,999.99')
                                ELSE
                                        DEVPOS(PROW()+1,00);DEVOUT(num_dup)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(RTRIM(mdescri1)+RTRIM(mdescri2))
                                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(valor,'9,999,999.99')
                                ENDIF
                        ENDIF
                        IF PROW() >= 59
                                EJECT
                                mpag ++

                                cabecalho(mpag,mtit,mtipo)

                                DEVPOS(PROW()+1,00);DEVOUT('TP/NF')
                                DEVPOS(PROW(),09);DEVOUT('Descricao')
                                DEVPOS(PROW(),75);DEVOUT('Valor')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        ENDIF
                        SKIP
                ENDDO
                IF PROW() >= 44
                        EJECT
                        mpag ++

                        cabecalho(mpag,mtit,mtipo)

                        DEVPOS(PROW()+1,00);DEVOUT('TP/NF')
                        DEVPOS(PROW(),09);DEVOUT('Descricao')
                        DEVPOS(PROW(),75);DEVOUT('Valor')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                ENDIF
                DEVPOS(PROW()+3,00);DEVOUT('*****************  RESUMO  *********************')
                i := 0
                FOR i = 1 TO LEN(m_anotot)
                        DEVPOS(PROW()+1,00);DEVOUT('Total em Mensalidades ANO: '+m_anotot[i,2]+':')
                        DEVPOS(PROW(),35);DEVOUTPICT(m_anotot[i,1],'99,999,999.99')
                NEXT

                DEVPOS(PROW()+1,35);DEVOUT('-------------')
                DEVPOS(PROW()+1,00);DEVOUT('Total Geral das Mensalidades...:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_din,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('Total Fundo de Caixa...........:')
                DEVPOS(PROW(),35);DEVOUTPICT(mfundo,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('Total Recebimentos Diversos....:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_re,'99,999,999.99')
                DEVPOS(PROW()+1,35);DEVOUT('-------------')
                DEVPOS(PROW()+1,00);DEVOUT('Total Recebimentos (A).........:')
                DEVPOS(PROW(),35);DEVOUTPICT(mfundo + mtot_din + mtot_re,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('------------------------------------------------')

                DEVPOS(PROW()+1,00);DEVOUT('Total de Vales.................:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_vale,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('Total de Despesas..............:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_des,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('Total de Retirada p/Deposito...:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_dp,'99,999,999.99')
                DEVPOS(PROW()+1,35);DEVOUT('-------------')

                DEVPOS(PROW()+1,00);DEVOUT('Total de Debitos (B)...........:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_des + mtot_dp - mtot_vale,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('------------------------------------------------')

                DEVPOS(PROW()+1,00);DEVOUT('Somatorio (A) + (B)............:')
                DEVPOS(PROW(),35);DEVOUTPICT(mfundo + mtot_din + mtot_re + mtot_des + mtot_dp - mtot_vale,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('------------------------------------------------')


                DEVPOS(PROW()+2,00);DEVOUT(TIME())

                IF mimp_tipo = 2
                        DEVPOS(PROW()+2,00);DEVOUT(' ')
                        SETPRC(00,00)
                ELSE
                        EJECT
                ENDIF

        ELSE            // BOBINA
                imprt(mtipo_imp,'C',0,17)

                imprt(mtipo_imp,'C')
                DEVPOS(PROW()+1,00);DEVOUT(PADC(ALLTRIM(memp+resa),54))
                DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM(mend_firm),54))
                DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM(mcid_firm),54))
                DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM('Fone: '+mfone_firm),54))
                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',54))
                DEVPOS(PROW()+1,00);DEVOUT('FECHAMENTO DO CAIXA: '+DTOC(mdata_sis))
                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',54))
                DEVPOS(PROW()+1,02);DEVOUT('TP/NF')
                DEVPOS(PROW(),09);DEVOUT('Descricao')
                DEVPOS(PROW(),49);DEVOUT('Valor')
                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',54))

                WHILE data = mdata .AND. ! EOF()
                        IF opcao = 2
                                IF caix->cod_opera <> STRZERO(mcod_op,3)
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF tipo = 'DF'
                                SKIP
                                LOOP
                        ENDIF
                        IF descri1 <> SPACE(40) .OR. descri2 <> SPACE(40)
                                IF tipo = 'DE'
                                        mdescri1 := 'DESPESA.....: '+descri1
                                        mdescri2 := '              '+descri2
                                ELSEIF tipo = 'RE'
                                        mdescri1 := 'REC.DIVERSOS: '+descri1
                                        mdescri2 := '              '+descri2
                                ELSEIF tipo = 'DP'
                                        mdescri1 := 'RET.DEPOSITO: '+descri1
                                        mdescri2 := '              '+descri2
                                ELSEIF tipo = 'FD'
                                        mdescri1 := 'FUND.CAIXA..: '+descri1
                                        mdescri2 := '              '+descri2
                                ELSE
                                        mdescri1 := '         '+descri1
                                        mdescri2 := '         '+descri2
                                ENDIF
                                mdescri3 := 'Op: '+cod_opera+'........................Valor R$: '
                        ELSE
                                IF tipo = 'FD'
                                        mdescri1 := 'Fundo de Caixa  - Op.:'+cod_opera
                                        mdescri2 := SPACE(40)
                                        mdescri3 := SPACE(40)
                                ENDIF
                                IF tipo = 'SG'
                                        mdescri1 = 'Sangria do Caixa  - Op.:'+cod_opera
                                        mdescri2 := SPACE(40)
                                        mdescri3 := SPACE(40)
                                ENDIF
                                IF tipo = 'TR'
                                        mdescri1 = 'TRANSF.MERCADORIA - Op.:'+cod_opera
                                        mdescri2 := SPACE(40)
                                        mdescri3 := SPACE(40)
                                ELSEIF tipo = 'ME'
                                        mdescri1 = 'Mensalidade ANO: '+caix->ano+'  '+cod_opera
                                        mdescri2 := SPACE(40)
                                        mdescri3 := SPACE(40)
                                ELSE
                                        mdescri1 = 'Outros Recebim.: '+caix->ano+'  '+cod_opera
                                        mdescri2 := SPACE(40)
                                        mdescri3 := SPACE(40)
                                ENDIF
                        ENDIF
                        IF mtipo_rel = 'G'
                                IF mimp_tipo = 1
                                        IF EMPTY(num_dup)
                                                DEVPOS(PROW()+1,00);DEVOUT(mdescri1)
                                                IF ! EMPTY(mdescri2)
                                                        DEVPOS(PROW()+1,00);DEVOUT(mdescri2)
                                                ENDIF
                                                DEVPOS(PROW()+1,00);DEVOUT(mdescri3)
                                                DEVPOS(PROW(),44);DEVOUTPICT(valor,'999,999.99')
                                        ELSE
                                                DEVPOS(PROW()+1,00);DEVOUT(num_dup)
                                                DEVPOS(PROW(),17);DEVOUT(RTRIM(mdescri1))
                                                DEVPOS(PROW(),44);DEVOUTPICT(valor,'999,999.99')
                                        ENDIF
                                ELSE
                                        IF EMPTY(num_dup)
                                                DEVPOS(PROW()+1,00);DEVOUT(mdescri1)
                                                IF ! EMPTY(mdescri2)
                                                        DEVPOS(PROW()+1,00);DEVOUT(mdescri2)
                                                ENDIF
                                                DEVPOS(PROW()+1,00);DEVOUT(mdescri3)
                                                DEVPOS(PROW(),44);DEVOUTPICT(valor,'999,999.99')
                                        ELSE
                                                DEVPOS(PROW()+1,00);DEVOUT(num_dup)
                                                DEVPOS(PROW(),PCOL()+2);DEVOUT(RTRIM(mdescri1))
                                                DEVPOS(PROW(),44);DEVOUTPICT(valor,'999,999.99')
                                        ENDIF
                                ENDIF
                        ENDIF
                        SKIP
                ENDDO
                DEVPOS(PROW()+3,00);DEVOUT('*********  RESUMO DOS MENSALIDADES/ANO  ********')
                i := 0
                FOR i = 1 TO LEN(m_anotot)
                        DEVPOS(PROW()+1,00);DEVOUT('Total Mensalidades ANO R$: '+m_anotot[i,2]+':')
                        DEVPOS(PROW(),35);DEVOUTPICT(m_anotot[i,1] - m_multa[i,1],'99,999,999.99')
                        DEVPOS(PROW()+1,00);DEVOUT('Total de Multa do ANO R$.: '+m_anotot[i,2]+':')
                        DEVPOS(PROW(),35);DEVOUTPICT(m_multa[i,1],'99,999,999.99')
                        DEVPOS(PROW()+1,35);DEVOUT('-------------')
                        DEVPOS(PROW()+1,00);DEVOUT('Total em GERAL ANO R$....: '+m_anotot[i,2]+':')
                        DEVPOS(PROW(),35);DEVOUTPICT(m_anotot[i,1],'99,999,999.99')
                NEXT

                DEVPOS(PROW()+2,00);DEVOUT('***************  RESUMO GERAL  *****************')
                DEVPOS(PROW()+1,00);DEVOUT('Total de Mensalidades Recebidas:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_doc,'99,999')
                DEVPOS(PROW()+1,00);DEVOUT('Total Geral das Mensalidades R$:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_din-mtot_mul,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('Total Geral de Multas R$.......:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_mul,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('Total Fundo de Caixa R$........:')
                DEVPOS(PROW(),35);DEVOUTPICT(mfundo,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('Total Recebimentos Diversos R$.:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_re,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('Total de Vales.................:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_vale,'99,999,999.99')
                DEVPOS(PROW()+1,35);DEVOUT('-------------')
                DEVPOS(PROW()+1,00);DEVOUT('Total Recebimentos (A).........:')
                DEVPOS(PROW(),35);DEVOUTPICT(mfundo + mtot_din + mtot_ch + mtot_cart + mtot_dup + mtot_ch_p + mtot_re + mtot_fi + mdup_rec + mtot_vale,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('------------------------------------------------')

                DEVPOS(PROW()+1,00);DEVOUT('Total de Despesas..............:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_des,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('Total de Retirada p/Deposito...:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_dp,'99,999,999.99')
                DEVPOS(PROW()+1,35);DEVOUT('-------------')

                DEVPOS(PROW()+1,00);DEVOUT('Total de Debitos (B)...........:')
                DEVPOS(PROW(),35);DEVOUTPICT(mtot_des + mtot_dp,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('------------------------------------------------')

                DEVPOS(PROW()+1,00);DEVOUT('Somatorio (A) + (B)............:')
                DEVPOS(PROW(),35);DEVOUTPICT(mfundo + mtot_din + mtot_ch + mtot_cart + mtot_dup + mtot_ch_p + mtot_fi + mdup_rec + mtot_re + mtot_vale + mtot_des + mtot_dp,'99,999,999.99')
                DEVPOS(PROW()+1,00);DEVOUT('------------------------------------------------')
                DEVPOS(PROW()+11,00);DEVOUT(' ')

        ENDIF
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(00,00,24,79,tela)
        IF mimp_tipo = 2
                lertexto('CX'+SUBSTR(DTOC(mdata),1,2)+SUBSTR(DTOC(mdata),4,2)+SUBSTR(DTOC(mdata),7)+'.REL')
        ENDIF
        CLOSE ALL
        RETURN NIL
ENDDO

**************************  F I M  **********************************8
********************
* CONSULTA DO CAIXA P/PERIODO
********************

FUNCTION sac33_1
**************

LOCAL tela,telaprin,lci,cci,lba,cba,li,ci,la,ca,lc,cc,lb,cb,mopcao,opcao,;
      mtot_din,mtot_ch,mtot_ch_p,mtot_vale,mtot_dup,mtot_cart,mtot_av,mtot_fi,;
      mtot_ap,mtot_re,mtot_dp,mdata1,mdata2,mtraco,mfundo,msangria,mquantd,mtot_des,;
      mdescri2,tela1,tela2,mnumero1,mnumero2,mcod_vend,mcod_op,mtip_ordem

LOCAL mtot_vol,mtot_dia,mtot_ped,mtot_geral,mtipo_rel,m_cartao:={},m_finan:={},i,;
      mfin_aux

PRIVATE mnome_ven,mcom_ven,mdescri1,mtipo_imp

mtraco := REPLI('=',80)

lci := 01
cci := 11
lba := 21
cba := 59

li := 10
ci := 64
la := 15
ca := 76

lc := 16
cc := 45
lb := 19
cb := 79

tela := SAVESCREEN(00,00,24,79)
*tela1 := SAVESCREEN(li,ci-1,la+1,ca)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('saccaixa','caix');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mtipo_imp := 'M'
        mtot_des := 0
        mtot_din := 0
        mtot_ch := 0
        mtot_ch_p := 0
        mtot_vale := 0
        mtot_dup := 0
        mtot_cart := 0
        mtot_av := 0
        mtot_ap := 0
        mtot_re := 0
        mtot_dp := 0
        mtot_fi := 0
        mfundo := 0
        msangria := 0
        mquantd := 0
        mdata1 := mdata_sis
        mdata2 := CTOD('  /  /  ')
        mdescri1 := SPACE(40)
        mdescri2 := SPACE(40)
        mnumero1 := 0
        mnumero2 := 0
        mcod_vend := 0
        mcod_op := 0
        mtip_ordem := '1'
        botao(lci,cci,lba,cba)
        @ lci+1,cci+1 TO lci+1,cba-1
        @ lci+13,cci+1 TO lci+13,cba-1
        @ lci+16,cci+1 TO lci+16,cba-1
        DEVPOS(lci,cci+2);DEVOUT('Resumo do Periodo:            a ')

        DEVPOS(lci+2,cci+1);DEVOUT('Fundo de Caixa.............:')
        DEVPOS(lci+3,cci+1);DEVOUT('Total em Dinheiro..........:')
        DEVPOS(lci+4,cci+1);DEVOUT('Total em Cheques...........:')
        DEVPOS(lci+5,cci+1);DEVOUT('Total em Cartao............:')
        DEVPOS(lci+6,cci+1);DEVOUT('Total Duplicata a Receber..:')
        DEVPOS(lci+7,cci+1);DEVOUT('Total em Cheques pre-datado:')
        DEVPOS(lci+8,cci+1);DEVOUT('Total de Vales.............:')
        DEVPOS(lci+9,cci+1);DEVOUT('Total de Despesas..........:')
        DEVPOS(lci+10,cci+1);DEVOUT('Total Recebimentos Diversos:')
        DEVPOS(lci+11,cci+1);DEVOUT('Retirada p/Deposito........:')
        DEVPOS(lci+12,cci+1);DEVOUT('Total de Financiamento.....:')
        DEVPOS(lci+14,cci+1);DEVOUT('TOTAL DO RECEBIMENTO.......:')
        DEVPOS(lci+15,cci+1);DEVOUT('RECEBIMENTO - (VALES+DESP.):')
        DEVPOS(lci+17,cci+1);DEVOUT('Total de Vendas a vista....:')
        DEVPOS(lci+18,cci+1);DEVOUT('Total de Vendas a prazo....:')
        DEVPOS(lci+19,cci+1);DEVOUT('Outros Recebimentos........:')
        @ lci,cci+21 GET mdata1
        @ lci,cci+34 GET mdata2 VALID IF(mdata2 < mdata1,.F.,.T.)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        ****************
        SELE('caix');ORDSETFOCUS(1)
        GO TOP
        ****************

        caix->(DBSEEK(DTOS(mdata1),.T.))

        IF EOF()
                atencao('Nenhum movimento neste Periodo !!!')
                RESTSCREEN(00,00,24,79,tela)
                CLOSE ALL
                RELEASE mnome_ven,mcom_ven,mdescri1
                RETURN NIL
        ENDIF
        mensagem('Aguarde um Momento OK !!!')
        limpa(23,00,23,79)
        botao(li,ci,la,ca,,' Opcoes ')
        opcao := 0
        @ li+1,ci+1 PROMPT '   Geral   ' MESSAGE 'Fechamento Geral - <ESC> Abandonar'
        @ li+2,ci+1 PROMPT ' No.Pedido ' MESSAGE 'Fechamento por No.Pedido - <ESC> Abandonar'
        @ li+3,ci+1 PROMPT ' Vendedor  ' MESSAGE 'Fechamento por Vendedor - <ESC> Abandonar'
        @ li+4,ci+1 PROMPT ' Operador  ' MESSAGE 'Fechamento por Operador - <ESC> Abandonar'
        SET INTEN ON
        MENU TO opcao
        IF LASTKEY() = 27
                LOOP
        ENDIF

        IF opcao = 2
                tela2 := SAVESCREEN(01,00,24,79)
                botao(lc,cc,lb,cb,,' Fechamento No.Pedido ')
                DEVPOS(lc+1,cc+2);DEVOUT('No.Pedido Inicial:')
                DEVPOS(lc+2,cc+2);DEVOUT('No.Pedido Final..:')
                @ lc+1,cc+21 GET mnumero1 PICT '999999'
                @ lc+2,cc+21 GET mnumero2 PICT '999999' VALID IF(mnumero2 < mnumero1,.F.,.T.)
                READ
                IF LASTKEY() = 27
                        RESTSCREEN(00,00,24,79,tela)
                        LOOP
                ENDIF
                RESTSCREEN(01,00,24,79,tela2)
        ELSEIF opcao = 3
                tela2 := SAVESCREEN(01,00,24,79)
                botao(lc,cc,lb,cb,,' Fechamento Vendedor ')
                DEVPOS(lc+1,cc+2);DEVOUT('Codigo do Vendedor:')
                @ lc+1,cc+22 GET mcod_vend PICT '999' VALID ven(mcod_vend,lc+2,cc+1)
                READ
                IF LASTKEY() = 27
                        RESTSCREEN(00,00,24,79,tela)
                        LOOP
                ENDIF
                INKEY(2)
                RESTSCREEN(01,00,24,79,tela2)
        ELSEIF opcao = 4
                tela2 := SAVESCREEN(01,00,24,79)
                botao(lc,cc,lb,cb,,' Fechamento p/Operador ')
                DEVPOS(lc+1,cc+2);DEVOUT('Codigo do Operador:')
                @ lc+1,cc+22 GET mcod_op PICT '999' VALID ven(mcod_op,lc+2,cc+1)
                READ
                IF LASTKEY() = 27
                        RESTSCREEN(00,00,24,79,tela)
                        LOOP
                ENDIF
                INKEY(2)
                RESTSCREEN(01,00,24,79,tela2)
        ENDIF
        WHILE caix->data <= mdata2 .AND. ! EOF()
                IF opcao = 2
                        IF VAL(SUBSTR(nota,3,6)) < mnumero1 .OR. VAL(SUBSTR(nota,3,6)) > mnumero2
                                SKIP
                                LOOP
                        ENDIF
                ELSEIF opcao = 3
                        IF cod_vend <> STRZERO(mcod_vend,3)
                                SKIP
                                LOOP
                        ENDIF
                ELSEIF opcao = 4
                        IF caix->cod_opera <> STRZERO(mcod_op,3)
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                IF tipo_comp = 'AV' .AND. tipo <> 'RE'
                        mtot_av = mtot_av + valor
                ENDIF
                IF tipo_comp = 'AP'
                        IF caix->data <= venci .AND. tipo = 'CT'
                                mtot_ap = mtot_ap + valor
*                       ELSEIF caix->data = venci .AND. tipo <> 'RE'
*                               mtot_av = mtot_av + valor
                        ELSEIF tipo = 'CH'.AND. venci > caix->data
                                mtot_ap = mtot_ap + valor
                        ELSEIF tipo = 'DU' .AND. pg = ' '
                                mtot_ap = mtot_ap + valor
                        ELSEIF tipo = 'FI'
                                mtot_ap = mtot_ap + valor
                        ENDIF
                ENDIF
                IF tipo = 'FD'
                        mfundo = mfundo + valor
                        SKIP
                        LOOP
                ENDIF
                IF tipo = 'SG'
                        msangria = msangria + valor
                        SKIP
                        LOOP
                ENDIF
                IF tipo = 'CH'.AND. venci > caix->data
                        mtot_ch_p = mtot_ch_p + valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'CH' .AND. venci = caix->data
                        mtot_ch = mtot_ch + valor
                        SKIP
                        LOOP
                 ELSEIF tipo = 'DU' .AND. pg = '*'
                         mtot_re = mtot_re + valor
                         SKIP
                         LOOP
                ELSEIF tipo = 'DU' .AND. pg = ' '
                        mtot_dup = mtot_dup + valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'CT'
                        mtot_cart = mtot_cart + valor
                        SKIP
                        LOOP
*               ELSEIF tipo = 'VL' .AND. pg = '*'
*                       mtot_re = mtot_re + valor
*                       SKIP
*                       LOOP
                ELSEIF tipo = 'VL'
                        mtot_vale = mtot_vale - valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'DN'
                        mtot_din = mtot_din + valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'DE'
                        mtot_des = mtot_des + valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'RE'
                        mtot_re = mtot_re + valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'DP'
                        mtot_dp = mtot_dp + valor
                        SKIP
                        LOOP
                ELSEIF tipo = 'FI'
                        mtot_fi = mtot_fi + valor
                        SKIP
                        LOOP
                ELSE
                        SKIP
                        LOOP
                ENDIF
        ENDDO
        DEVPOS(lci+2,cci+30);DEVOUTPICT(mfundo,'99,999,999.99')
        DEVPOS(lci+3,cci+30);DEVOUTPICT(mtot_din,'99,999,999.99')
        DEVPOS(lci+4,cci+30);DEVOUTPICT(mtot_ch,'99,999,999.99')
        DEVPOS(lci+5,cci+30);DEVOUTPICT(mtot_cart,'99,999,999.99')
        DEVPOS(lci+6,cci+30);DEVOUTPICT(mtot_dup,'99,999,999.99')
        DEVPOS(lci+7,cci+30);DEVOUTPICT(mtot_ch_p,'99,999,999.99')
        DEVPOS(lci+8,cci+30);DEVOUTPICT(mtot_vale,'99,999,999.99')
        DEVPOS(lci+9,cci+30);DEVOUTPICT(mtot_des,'99,999,999.99')
        DEVPOS(lci+10,cci+30);DEVOUTPICT(mtot_re,'99,999,999.99')
        DEVPOS(lci+11,cci+30);DEVOUTPICT(mtot_dp,'99,999,999.99')
        DEVPOS(lci+12,cci+30);DEVOUTPICT(mtot_fi,'99,999,999.99')
        DEVPOS(lci+14,cci+30);DEVOUTPICT(mfundo + mtot_din + mtot_ch + mtot_cart + mtot_dup + mtot_ch_p + mtot_re + mtot_fi,'99,999,999.99')
        DEVPOS(lci+15,cci+30);DEVOUTPICT(mfundo + mtot_din + mtot_ch + mtot_cart + mtot_dup + mtot_ch_p + mtot_re + mtot_vale + mtot_des + mtot_fi,'99,999,999.99')
        DEVPOS(lci+17,cci+30);DEVOUTPICT(mtot_av,'99,999,999.99')
        DEVPOS(lci+18,cci+30);DEVOUTPICT(mtot_ap,'99,999,999.99')
        DEVPOS(lci+19,cci+30);DEVOUTPICT(mtot_re,'99,999,999.99')

        mensagem('Escolha sua opcao: [I]mpressao [V]ideo [ESC] abandonar')
        mopcao = 'I'
        @ 23,COL()+1 GET mopcao PICT '@!' VALID mopcao $ 'I,V'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        mtipo_rel := mensagem1('Informe o tipo de Fechamento [R]esumo [G]eral: ','G','G,R')
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Escolha o tipo de ordem que deseja [1]Pedido [8]tipo de documento:')
        mtip_ordem := '1'
        @ 23,COL()+1 GET mtip_ordem PICT '9' VALID mtip_ordem $ '1,8'
        READ
        ********************
        ORDSETFOCUS(VAL(mtip_ordem))
        GO TOP
        ********************

        caix->(DBSEEK(DTOS(mdata1),.T.))

        IF EOF()
                atencao('Nao existe Movimento neste Perido !!!')
                LOOP
        ENDIF
        mensagem('Aguarde o final da Impressao - [ESC]Abandonar')
        mpag := 1
        IF mopcao = 'I'
                IF ! imp_arq('CONSULTA.REL','R')
                        LOOP
                ENDIF
                mtraco := REPLI('=',80)
        ELSE
                SET DEVI TO PRINT
                SET PRINT TO consulta.rel
                mtraco := REPLI('=',130)
        ENDIF
        mtit := 'Fechamento do Caixa'
        mtipo := 'Data do Fechamento: '+DTOC(mdata1)+' a '+DTOC(mdata2)

        cabecalho(mpag,mtit,mtipo)

        imprt(mtipo_imp,'C')
        DEVPOS(PROW()+1,00);DEVOUT('  Data  ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('   TP/NF')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('Descricao'+SPACE(91))
        DEVPOS(PROW(),PCOL()+1);DEVOUT('        Valor')
        imprt(mtipo_imp,'N')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)

        WHILE caix->data <= mdata2 .AND. ! EOF()
                IF opcao = 2
                        IF VAL(SUBSTR(nota,3,6)) < mnumero1 .OR. VAL(SUBSTR(nota,3,6)) > mnumero2
                                SKIP
                                LOOP
                        ENDIF
                ELSEIF opcao = 3
                        IF cod_vend <> STRZERO(mcod_vend,3)
                                SKIP
                                LOOP
                        ENDIF
                ELSEIF opcao = 4
                        IF caix->cod_opera <> STRZERO(mcod_op,3)
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                IF tipo = 'DF'
                        SKIP
                        LOOP
                ENDIF
                IF descri1 <> SPACE(40) .OR. descri2 <> SPACE(40)
                        mdescri1 := descri1
                        mdescri2 := descri2+'   Cod.Vend.: '+cod_vend
                ELSE
                        IF tipo = 'FD'
                                mdescri1 := 'Fundo de Caixa'
                                mdescri2 := SPACE(40)
                        ENDIF
                        IF tipo = 'SG'
                                mdescri1 = 'Sangria do Caixa'
                                mdescri2 := SPACE(40)
                        ENDIF
                        IF tipo = 'CH'.AND. venci > caix->data
                                mdescri1 = 'Cheque pre-datado   No.Bco.: '+num_ban+'   No.: '+DOCUMENTO+'   Venc.: '+DTOC(venci)
                                mdescri2 := '   Cod.Cli.: '+cod_cli+'   Cod.Vend.:'+cod_vend
                        ELSEIF tipo = 'CH' .AND. venci = caix->data
                                mdescri1 = 'Cheque Avista       No.Bco.: '+num_ban+'   No.: '+documento
                                mdescri2 := '                         Cod.Cli.: '+cod_cli+'   Cod.Vend.:'+cod_vend
                        ELSEIF tipo = 'DU' .AND. pg = '*'
                                mdescri1 = 'Duplicatas Pagas                   No.: '+num_dup+'   Venc.: '+DTOC(venci)
                                mdescri2 := '   Cod.Cli.: '+cod_cli+'   Cod.Vend.:'+cod_vend
                        ELSEIF tipo = 'DU'
                                mdescri1 = 'Duplicata Receber                  No.: '+num_dup+'   Venc.: '+DTOC(venci)
                                mdescri2 := '   Cod.Cli.: '+cod_cli+'   Cod.Vend.:'+cod_vend
                        ELSEIF tipo = 'DN'
                                mdescri1 = 'Vendas a Dinheiro                                                        '
                                mdescri2 := '                                                      Cod.Cli.: '+cod_cli+'   Cod.Vend.:'+cod_vend
                        ELSEIF tipo = 'FI'
                                mdescri1 = 'Financiamento                      No.: '+documento+'   Venc.: '+DTOC(venci)
                                mdescri2 := '   Cod.Cli.: '+cod_cli+'   Cod.Vend.:'+cod_vend
                                i := 0
                                FOR i = 1 TO LEN(m_finan)
                                        IF caix->cod_ct = m_finan[i,1]
                                                m_finan[i,3] := m_finan[i,3] + caix->valor
                                                EXIT
                                        ENDIF
                                NEXT
                        ENDIF
                ENDIF
                IF mtipo_rel = 'G'
                        imprt(mtipo_imp,'C')
                        IF mopcao = 'I'
                                DEVPOS(PROW()+1,00);DEVOUT(caix->data)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(nota)
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(RTRIM(mdescri1)+RTRIM(mdescri2))
                                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(valor,'99,999,999.99')
                        ELSE
                                DEVPOS(PROW()+1,00);DEVOUT(caix->data)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(nota)
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(RTRIM(mdescri1)+RTRIM(mdescri2))
                                DEVPOS(PROW(),PCOL()+1);DEVOUTPICT(valor,'99,999,999.99')
                        ENDIF
                        imprt(mtipo_imp,'N')
                ENDIF
                IF PROW() >= 59
                        EJECT
                        mpag ++

                        cabecalho(mpag,mtit,mtipo)

                        imprt(mtipo_imp,'C')
                        DEVPOS(PROW()+1,00);DEVOUT('  Data  ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('   TP/NF')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('Descricao'+SPACE(91))
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('        Valor')
                        imprt(mtipo_imp,'N')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                ENDIF
                SKIP
        ENDDO
        IF PROW() >= 44
                EJECT
                mpag ++

                cabecalho(mpag,mtit,mtipo)

                imprt(mtipo_imp,'C')
                DEVPOS(PROW()+1,00);DEVOUT('  Data  ')
                DEVPOS(PROW(),PCOL()+2);DEVOUT('   TP/NF')
                DEVPOS(PROW(),PCOL()+1);DEVOUT('Descricao'+SPACE(91))
                DEVPOS(PROW(),PCOL()+1);DEVOUT('        Valor')
                imprt(mtipo_imp,'N')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        ENDIF
        DEVPOS(PROW()+3,00);DEVOUT('*****************  RESUMO  ************************')
        DEVPOS(PROW()+1,00);DEVOUT('Total Fundo de Caixa...........:')
        DEVPOS(PROW(),35);DEVOUTPICT(mfundo,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total em Dinheiro..............:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_din,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total em Cheques...............:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_ch,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total em Cartao................:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_cart,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total Duplicata a Receber......:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_dup,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total Recebimentos Diversos....:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_re,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total em Cheques pre-datado....:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_ch_p,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total Financiamento............:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_fi,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total de Vales.................:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_vale,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total de Despesas..............:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_des,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total de Retirada p/Deposito...:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_dp,'99,999,999.99')

        DEVPOS(PROW()+2,00);DEVOUT('Total Recebimentos.............:')
        DEVPOS(PROW(),35);DEVOUTPICT(mfundo + mtot_din + mtot_ch + mtot_cart + mtot_dup + mtot_ch_p + mtot_re + mtot_fi,'99,999,999.99')
        DEVPOS(PROW(),35);DEVOUTPICT(mfundo + mtot_din + mtot_ch + mtot_cart + mtot_dup + mtot_ch_p + mtot_re + mtot_fi,'99,999,999.99')

        DEVPOS(PROW()+2,00);DEVOUT('Recebimentos - (Vales+Despesas):')
        DEVPOS(PROW(),35);DEVOUTPICT(mfundo + mtot_din + mtot_ch + mtot_cart + mtot_dup + mtot_ch_p + mtot_re + mtot_vale + mtot_des + mtot_fi,'99,999,999.99')
        DEVPOS(PROW(),35);DEVOUTPICT(mfundo + mtot_din + mtot_ch + mtot_cart + mtot_dup + mtot_ch_p + mtot_re + mtot_vale + mtot_des + mtot_fi,'99,999,999.99')

        DEVPOS(PROW()+2,00);DEVOUT('Total de Vendas a vista........:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_av,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Total de Vendas a prazo........:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_ap,'99,999,999.99')
        DEVPOS(PROW()+1,00);DEVOUT('Outros recebimentos............:')
        DEVPOS(PROW(),35);DEVOUTPICT(mtot_re,'99,999,999.99')
        DEVPOS(PROW()+2,00);DEVOUT(TIME())
        IF mopcao = 'V'
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                SETMODE(50,80)
                botao(02,00,MAXROW()-2,79,,' Consulta Fechamento do CAIXA ')
                mensagem('<TAB>p/Direita - <HOME>p/Esquerda - <ESC>p/abandonar')
                mconsulta := MEMOREAD('consulta.rel')
                MEMOEDIT(mconsulta,03,01,MAXROW()-3,78,.F.,,160,80)
                SETMODE(25,80)
                RESTSCREEN(00,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(00,00,24,79,tela)
        CLOSE ALL
        RETURN NIL
ENDDO







