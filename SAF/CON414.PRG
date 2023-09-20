**** CON414.PRG: Relatorio -> Duplicatas Pagas
***
*************************************************

FUNCTION con414
****************
LOCAL MPRG:='CON414'
LOCAL tela,t_ini,t_fim,l_mes,td_mes,tp_mes,pag,vencer,vencidas,tot_dup,tot_jur,;
      tot_des,vlr_dup,vlr_jur,vlr_des,mtot_dup,mtot_jur,mtot_des,mvlr_dup,;
      mvlr_jur,mvlr_des,f,mquantd,mtit,mtipo,opcao,mtipo_doc
LOCAL mfo,mfv,mvv,mad,mtr,mfi,mdv,mtipo_rel,mtipo_con

PRIVATE mcod_for,bank,mfornece,muf,mtipo_imp

IF ! ver_nivel(mprg,'RELATORIO DOCUMENTO PAGOS (C.APAGAR)','15',nivel_acess)
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
*------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('safccpg','ccpg');RETURN NIL;ENDIF
IF ! abriarq('sacdupp','dupp');RETURN NIL;ENDIF
*------------------------------------------------------------------
***************
SELE('ccpg');ORDSETFOCUS(3)
GO TOP
***************

botao(9, 07, 16, 78)

DEVPOS(09,30);DEVOUT(" Documento Pagos ")
DEVPOS(10,09);DEVOUT("Fornecedor.:")
DEVPOS(11,09);DEVOUT("Inicio.....:")
DEVPOS(12,09);DEVOUT("Final......:")
DEVPOS(13,09);DEVOUT("Tipo Doc...:")
DEVPOS(14,09);DEVOUT('Tipo Conta..: FO:    FV:    VV:    TR:    AD:    FI:    DV:')
DEVPOS(15,09);DEVOUT("Resumo/Geral:")
mtipo_doc := SPACE(2)
mfornece := SPACE(40)
muf := SPACE(2)
mfo := SPACE(1)
mfv := SPACE(1)
mvv := SPACE(1)
mad := SPACE(1)
mtr := SPACE(1)
mfi := SPACE(1)
mdv := SPACE(1)
mcod_for=0
mtipo_rel := 'G'
t_ini=mdata_sis
t_fim=mdata_sis+30
bank=SPACE(3)
@ 10,22 GET mcod_for PICT "9999" VALID v_fornece(mcod_for,10,27)
@ 11,22 GET t_ini
@ 12,22 GET t_fim
@ 13,22 GET mtipo_doc PICT '@!' VALID lim_get() WHEN men_get(14,35,'Informe o tipo de DOCUMENTO que deseja <PA>pagamento <DU>duplicata <CH>cheques <FR>fretes ou NAO informe nenhum tipo de DOCUMENTO')
@ 14,27 GET mfo PICT '@!' VALID mfo $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,35,'Marque com um "X" p/consulta Conta de Fornecedor ou deixe em branco')
@ 14,34 GET mfv PICT '@!' VALID mfv $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,42,'Marque com um "X" p/consulta Conta de Fixa de Venda ou deixe em branco')
@ 14,41 GET mvv PICT '@!' VALID mvv $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,49,'Marque com um "X" p/consulta Conta de Variavel de Venda ou deixe em branco')
@ 14,48 GET mtr PICT '@!' VALID mtr $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,56,'Marque com um "X" p/consulta Conta de Tributaria ou deixe em branco')
@ 14,55 GET mad PICT '@!' VALID mad $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,63,'Marque com um "X" p/consulta Conta de Administrativa ou deixe em branco')
@ 14,62 GET mfi PICT '@!' VALID mfi $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,70,'Marque com um "X" p/consulta Conta de Financeira ou deixe em branco')
@ 14,69 GET mdv PICT '@!' VALID mfi $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,70,'Marque com um "X" p/consulta Conta de Financeira ou deixe em branco')
@ 15,22 GET mtipo_rel PICT '@!' VALID lim_get() .AND. mtipo_rel $ 'R,G'  WHEN men_get(17,35,'Informe o [R] para resumo dos dias [G] para todas as duplicatas')
READ
IF LASTKEY()=27
        RESTSCREEN(01,00,24,79,tela)
        CLOSE ALL
        RETURN
ENDIF
IF EMPTY(mcod_for)
        mtipo_con := mensagem1('Escolha o tipo de consulta [A]NALITICO [S]INTETICO:','A','A,S')
ELSE
        mtipo_con := 'A'
ENDIF
IF t_ini=CTOD("  /  /  ")
        t_ini=CTOD("11/11/11")
ENDIF
IF t_fim=CTOD("  /  /  ")
        t_fim=CTOD("31/12/99")
ENDIF
RESTSCREEN(01,00,24,79,tela)
l_mes=" "
td_mes=0    && tot.dup no mes
tp_mes=0    && tot. pagar no mes
mtit := "DOCUMENTOS PAGOS (Contas a Pagar)"
mensagem('Confirma a Impressao [S/n]:')
opcao := 'S'
@ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
READ
IF LASTKEY() = 27 .OR. opcao = 'N'
        RESTSCREEN(01,00,24,79,tela)
        CLOSE ALL
        RETURN
ENDIF
IF mtipo_con = 'A'
        mtit := mtit+' ANALITICO'
        ********************
        SELE('dupp')
        ORDSETFOCUS(3)
        GO TOP
        ********************
        dupp->(DBSEEK(DTOS(t_ini),.T.))
        IF EOF()
                atencao('Nao existe movimento neste periodo !!!')
                CLOSE ALL
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        mensagem('Aguarde o final da Impressao - [ESC] Abandona.')
        SET DEVI TO PRINT
        SET PRINT on
        pag=0
        vencer=0
        vencidas=0
        SET DATE brit
        tot_dup=0
        tot_jur=0
        tot_des=0
        vlr_dup=0
        vlr_jur=0
        vlr_des=0
        mtot_dup=0
        mtot_jur=0
        mtot_des=0
        mvlr_dup=0
        mvlr_jur=0
        mvlr_des=0
        mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
        WHILE dupp->datpag <= t_fim .AND. ! EOF()
                IF pag=0 .OR. PROW()>=57
                        pag=pag+1
                        IF pag>1
                                EJECT
                        ENDIF
                        cabecalho(pag,mtit,mtipo)
                        imprt(mtipo_imp,'C',1)
                        ?? "Cod.  Fornecedor                               Tipo Duplicata      Vencim.   D.Pagam     Valor Dup.     Valor Pago   Dias"
                        ? REPLICATE("=",137)
                ENDIF
                IF dupp->pago <> 'B'
                        SKIP
                        LOOP
                ENDIF
                IF ! EMPTY(mcod_for)
                        IF dupp->fornec <> STRZERO(mcod_for,4)
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                IF ! EMPTY(mtipo_doc)
                        IF mtipo_doc <> tipo
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                WHILE dupp->datpag <= t_fim .AND. ! EOF() .AND. EMPTY(mcod_for)
                        IF dupp->pago <> 'B'
                                SKIP
                                LOOP
                        ENDIF
                        IF mfo = 'X' .AND. 'FO' = tip_for
                                EXIT
                        ELSEIF mfv = 'X' .AND. 'FV' = tip_for
                                EXIT
                        ELSEIF mvv = 'X' .AND. 'VV' = tip_for
                                EXIT
                        ELSEIF mtr = 'X' .AND. 'TR' = tip_for
                                EXIT
                        ELSEIF mad = 'X' .AND. 'AD' = tip_for
                                EXIT
                        ELSEIF mfi = 'X' .AND. 'FI' = tip_for
                                EXIT
                        ELSEIF mdv = 'X' .AND. 'DV' = tip_for
                                EXIT
                        ELSE
                                SKIP
                                LOOP
                        ENDIF
                ENDDO
                IF EOF()
                        EXIT
                ENDIF
                IF l_mes<>SUBSTR(DTOC(datpag),4,5)
                        l_mes=SUBSTR(DTOC(datpag),4,5)
                        ? l_mes
                        ?
                ENDIF
                ? dupp->fornec+"  "
                ?? dupp->cliente+"  "
                ?? dupp->tip_for+"  "
                ?? dupp->tipo+dupp->duplicata+"  "
                ?? DTOC(dupp->venc)+"  "
                ?? DTOC(dupp->datpag)+"  "
                ?? TRANSFORM(dupp->valor,"@E 99,999,999.99")+"  "
                ?? TRANSFORM(dupp->vlpago,"@E 99,999,999.99")+"  "
                ?? TRANSFORM(dupp->venc-mdata_sis,"99999")
                tot_dup=tot_dup+1
                vlr_dup=vlr_dup+vlpago
                mtot_dup=mtot_dup+1
                mvlr_dup=mvlr_dup+vlpago
                IF vlpago>valor
                        vlr_jur=vlr_jur+(vlpago-valor)
                        tot_jur=tot_jur+1
                        mvlr_jur=mvlr_jur+(vlpago-valor)
                        mtot_jur=mtot_jur+1
                ENDIF
                IF vlpago<valor
                        vlr_des=vlr_des+(valor-vlpago)
                        tot_des=tot_des+1
                        mvlr_des=mvlr_des+(valor-vlpago)
                        mtot_des=mtot_des+1
                ENDIF
                SKIP
                IF l_mes<>SUBSTR(DTOC(datpag),4,5)
                        IF PROW()>54
                                EJECT
                                pag=pag+1
                                cabecalho(pag,mtit,mtipo)
                                ?
                        ENDIF
                        imprt(mtipo_imp,'C',1)
                        ? "** RESUMO DO MES **"
                        ?
                        ? "Total de Juros.....: "+TRANSFORM(tot_jur,"9999")+" - "+TRANSFORM(vlr_jur,"@E 999,999,999,999.99")
                        ? "Total de Descontos.: "+TRANSFORM(tot_des,"9999")+" - "+TRANSFORM(vlr_des,"@E 999,999,999,999.99")
                        ? "Total de Duplicatas: "+TRANSFORM(tot_dup,"9999")+" - "+TRANSFORM(vlr_dup,"@E 999,999,999,999.99")
                        tot_dup=0
                        tot_jur=0
                        tot_des=0
                        vlr_dup=0
                        vlr_jur=0
                        vlr_des=0
                ENDIF
        ENDDO
        IF PROW() > 55
                EJECT
                pag=pag+1
                cabecalho(pag,mtit,mtipo)
        ENDIF
        ?
        imprt(mtipo_imp,'N',1)
        ? "** RESUMO FINAL **"
        ?
        ? "Total de Juros.....: "+TRANSFORM(mtot_jur,"9999")+" - "+TRANSFORM(mvlr_jur,"@E 999,999,999,999.99")
        ? "Total de Descontos.: "+TRANSFORM(mtot_des,"9999")+" - "+TRANSFORM(mvlr_des,"@E 999,999,999,999.99")
        ? "Total de Documentos: "+TRANSFORM(mtot_dup,"9999")+" - "+TRANSFORM(mvlr_dup,"@E 999,999,999,999.99")
        ? TIME()
        EJECT
ELSE
        mtit := mtit+' SINTETICO'
        ********************
        SELE('dupp')
        ORDSETFOCUS(6)
        GO TOP
        ********************
        SET DEVI TO PRINT
        SET PRINT on
        pag=0
        vencer=0
        vencidas=0
        SET DATE brit
        tot_dup=0
        tot_jur=0
        tot_des=0
        vlr_dup=0
        vlr_jur=0
        vlr_des=0
        mtot_dup=0
        mtot_jur=0
        mtot_des=0
        mvlr_dup=0
        mvlr_jur=0
        mvlr_des=0
        mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
        WHILE ! EOF()
                mcod_aux := dupp->fornec
                mcliente := dupp->cliente
                mtip_for := dupp->tip_for
                WHILE mcod_aux = dupp->fornec .AND. ! EOF()
                        IF pag=0 .OR. PROW()>=57
                                pag=pag+1
                                IF pag>1
                                        EJECT
                                ENDIF
                                cabecalho(pag,mtit,mtipo)
                                imprt(mtipo_imp,'C',1)
                                ?? "Cod.  Fornecedor                               Tipo Duplicata      Vencim.   D.Pagam     Valor Dup.     Valor Pago   Dias"
                                ? REPLICATE("=",137)
                        ENDIF
                        IF dupp->datpag > t_fim .OR. dupp->datpag < t_ini
                                SKIP
                                LOOP
                        ENDIF
                        IF dupp->pago <> 'B'
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(mcod_for)
                                IF dupp->fornec <> STRZERO(mcod_for,4)
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF ! EMPTY(mtipo_doc)
                                IF mtipo_doc <> tipo
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        WHILE dupp->datpag <= t_fim .AND. ! EOF() .AND. EMPTY(mcod_for)
*                       WHILE ! EOF()
                                IF dupp->pago <> 'B'
                                        SKIP
                                        LOOP
                                ENDIF
                                IF mfo = 'X' .AND. 'FO' = tip_for
                                        EXIT
                                ELSEIF mfv = 'X' .AND. 'FV' = tip_for
                                        EXIT
                                ELSEIF mvv = 'X' .AND. 'VV' = tip_for
                                        EXIT
                                ELSEIF mtr = 'X' .AND. 'TR' = tip_for
                                        EXIT
                                ELSEIF mad = 'X' .AND. 'AD' = tip_for
                                        EXIT
                                ELSEIF mfi = 'X' .AND. 'FI' = tip_for
                                        EXIT
                                ELSEIF mdv = 'X' .AND. 'DV' = tip_for
                                        EXIT
                                ELSE
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDDO
                        IF EOF()
                                EXIT
                        ENDIF
                        IF mtipo_rel = 'G'
                                ? dupp->fornec+"  "
                                ?? dupp->cliente+"  "
                                ?? dupp->tip_for+"  "
                                ?? dupp->tipo+dupp->duplicata+"  "
                                ?? DTOC(dupp->venc)+"  "
                                ?? DTOC(dupp->datpag)+"  "
                                ?? TRANSFORM(dupp->valor,"@E 99,999,999.99")+"  "
                                ?? TRANSFORM(dupp->vlpago,"@E 99,999,999.99")+"  "
                                ?? TRANSFORM(dupp->venc-mdata_sis,"99999")
                        ENDIF
                        tot_dup=tot_dup+1
                        vlr_dup=vlr_dup+vlpago
                        mtot_dup=mtot_dup+1
                        mvlr_dup=mvlr_dup+vlpago
                        IF vlpago>valor
                                vlr_jur=vlr_jur+(vlpago-valor)
                                tot_jur=tot_jur+1
                                mvlr_jur=mvlr_jur+(vlpago-valor)
                                mtot_jur=mtot_jur+1
                        ENDIF
                        IF vlpago<valor
                                vlr_des=vlr_des+(valor-vlpago)
                                tot_des=tot_des+1
                                mvlr_des=mvlr_des+(valor-vlpago)
                                mtot_des=mtot_des+1
                        ENDIF
                        SKIP
                ENDDO
                IF ! EMPTY(vlr_dup)
                        IF PROW() > 55
                                EJECT
                                pag=pag+1
                                cabecalho(pag,mtit,mtipo)
                                imprt(mtipo_imp,'C',1)
                                ?? "Cod.  Fornecedor                               Tipo Duplicata      Vencim.   D.Pagam     Valor Dup.     Valor Pago   Dias"
                                ? REPLICATE("=",137)
                        ENDIF
                        IF mtipo_rel = 'G'
                                ? REPLICATE("-",137)
                                ? "Total de Juros: "+TRANSFORM(tot_jur,"9999")+" - "+TRANSFORM(vlr_jur,"@E 9,999,999.99")+'  '
                                ?? "Total de Descontos.: "+TRANSFORM(tot_des,"9999")+" - "+TRANSFORM(vlr_des,"@E 9,999,999.99")+'  '
                                ?? "Total de Duplicatas: "+TRANSFORM(tot_dup,"9999")+" - "+TRANSFORM(vlr_dup,"@E 9,999,999.99")
                                ? REPLICATE("-",137)
                                tot_dup=0
                                tot_jur=0
                                tot_des=0
                                vlr_dup=0
                                vlr_jur=0
                                vlr_des=0
                        ELSE
                                ? mcod_aux+' '
                                ?? mcliente+' '+mtip_for+' '
                                ?? 'Totais: Juros:'+TRANSFORM(tot_jur,'9999')+' -'+TRANSFORM(vlr_jur,'@E 999,999.99')+'  '
                                ?? 'Desc.:'+TRANSFORM(tot_des,'9999')+' -'+TRANSFORM(vlr_des,'@E 999,999.99')+'  '
                                ?? 'Documento:'+TRANSFORM(tot_dup,'9999')+' -'+TRANSFORM(vlr_dup,'@E 9,999,999.99')
                                ? REPLICATE("-",137)
                                tot_dup=0
                                tot_jur=0
                                tot_des=0
                                vlr_dup=0
                                vlr_jur=0
                                vlr_des=0
                        ENDIF
                ENDIF
        ENDDO
        IF PROW() > 55
                EJECT
                pag=pag+1
                cabecalho(pag,mtit,mtipo)
        ENDIF
        ?
        imprt(mtipo_imp,'N',1)
        ? "** RESUMO FINAL **"
        ?
        ? "Total de Juros.....: "+TRANSFORM(mtot_jur,"9999")+" - "+TRANSFORM(mvlr_jur,"@E 999,999,999,999.99")
        ? "Total de Descontos.: "+TRANSFORM(mtot_des,"9999")+" - "+TRANSFORM(mvlr_des,"@E 999,999,999,999.99")
        ? "Total de Documentos: "+TRANSFORM(mtot_dup,"9999")+" - "+TRANSFORM(mvlr_dup,"@E 999,999,999,999.99")
        ? TIME()
        EJECT
ENDIF
SETPRC(00,00)
SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
RESTSCREEN(01,00,24,79,tela)
CLOSE ALL
RETURN




