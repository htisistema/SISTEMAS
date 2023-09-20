**** CON413.PRG: Relatorio -> Duplicatas Vencer/Vencidas
***  CONTAS A PAGAR
*********************************************************

FUNCTION con413
***************
LOCAL MPRG:='CON413'
LOCAL tela,t_ini,t_fim,tp_mes,td_mes,u_dat,pag,vencer,;
      vencidas,t_vencer,t_vencidas,l_mes,tot,tot_g,tot_din,tot_gin,mquantd,;
      mtit,mtipo,opcao,mtipo_doc,mt_dia,mdia,mtipo_rel,mtipo_conta

LOCAL mfo,mfv,mvv,mad,mtr,mfi,mdv,mtipo_con,mcliente,mtip_for

PRIVATE bank,mcod_for,mtipo_imp

IF ! ver_nivel(mprg,'RELATORIO DOCUMENTOS VENCER/VENCIDAS (C.APAGAR)','15',nivel_acess)
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)

*------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('safccpg','ccpg');RETURN NIL;ENDIF
IF ! abriarq('sacdupp','dupp');RETURN NIL;ENDIF
*------------------------------------------------------------------
******************
SELE('ccpg');ORDSETFOCUS(1)
******************
botao(9, 07, 17, 78)
DEVPOS(09,10);DEVOUT(" Vencer/Vencidas ")
DEVPOS(10,09);DEVOUT("Tipo.......:")
DEVPOS(11,09);DEVOUT("Fornecedor.:")
DEVPOS(12,09);DEVOUT("Inicio.....:")
DEVPOS(13,09);DEVOUT("Final......:")
DEVPOS(14,09);DEVOUT("Tipo Doc...:")
DEVPOS(15,09);DEVOUT('Tipo Conta..: FO:    FV:    VV:    TR:    AD:    FI:    DV:')
DEVPOS(16,09);DEVOUT("Resumo/Geral:")
WHILE .T.
        mfo := SPACE(1)
        mfv := SPACE(1)
        mvv := SPACE(1)
        mad := SPACE(1)
        mtr := SPACE(1)
        mfi := SPACE(1)
        mdv := SPACE(1)
        mtipo_rel := 'G'
        mtipo_doc := SPACE(2)
        bank="   "
        mcod_for=0
        t_ini=CTOD("  /  /  ")
        t_fim=CTOD("  /  /  ")
        mod := 0
        mtipo_conta := SPACE(2)
        setcor(3)
        DEVPOS(10,24);DEVOUT('DOCUMENTOS APAGAR')
        setcor(1)
        mtit := "DUPLICATAS APAGAR (Contas a Pagar)"
        @ 11,22 GET mcod_for PICT '9999' VALID v_fornece(mcod_for,11,27)
        @ 12,22 GET t_ini VALID lim_get() WHEN men_get(13,35,'Informe a data inicial do periodo que deseja imprimir')
        @ 13,22 GET t_fim VALID lim_get() WHEN men_get(14,35,'Informe a data final do periodo que deseja imprimir')
        @ 14,22 GET mtipo_doc PICT '@!' VALID lim_get() WHEN men_get(15,35,'Informe o tipo de DOCUMENTO que deseja <PA>pagamento <DU>duplicata <CH>cheques <FR>fretes ou NAO informe nenhum tipo de DOCUMENTO')
        @ 15,27 GET mfo PICT '@!' VALID mfo $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,35,'Marque com um "X" p/consulta Conta de Fornecedor ou deixe em branco')
        @ 15,34 GET mfv PICT '@!' VALID mfv $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,42,'Marque com um "X" p/consulta Conta de Fixa de Venda ou deixe em branco')
        @ 15,41 GET mvv PICT '@!' VALID mvv $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,49,'Marque com um "X" p/consulta Conta de Variavel de Venda ou deixe em branco')
        @ 15,48 GET mtr PICT '@!' VALID mtr $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,56,'Marque com um "X" p/consulta Conta de Tributaria ou deixe em branco')
        @ 15,55 GET mad PICT '@!' VALID mad $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,63,'Marque com um "X" p/consulta Conta de Administrativa ou deixe em branco')
        @ 15,62 GET mfi PICT '@!' VALID mfi $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,70,'Marque com um "X" p/consulta Conta de Financeira ou deixe em branco')
        @ 15,69 GET mdv PICT '@!' VALID mfi $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(19,70,'Marque com um "X" p/consulta Conta de Financeira ou deixe em branco')
        @ 16,22 GET mtipo_rel PICT '@!' VALID lim_get() .AND. mtipo_rel $ 'R,G'  WHEN men_get(17,35,'Informe o [R] para resumo dos dias [G] para todas as duplicatas')
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
        RESTSCREEN(01,00,24,79,tela)
        IF t_ini=CTOD("  /  /  ")
                t_ini=CTOD("11/11/11")
        ENDIF
        IF t_fim=CTOD("  /  /  ")
                t_fim=CTOD("31/12/99")
        ENDIF
        td_mes=0    && tot.dup no mes
        tp_mes=0    && tot. pagar no mes
        mensagem('Confirma a Impressao [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        IF mtipo_con = 'A'
                mtit := mtit+' ANALITICO'
                *****************
                SELE('dupp');ORDSETFOCUS(4)
                GO TOP
                *****************
                dupp->(DBSEEK(DTOS(t_ini),.T.))
                IF EOF()
                        atencao('Nao existe movimento neste periodo !!!')
                        LOOP
                ENDIF
                RESTSCREEN(01,00,24,79,tela)
                SET DEVI TO PRINT
                pag=0
                vencer=0
                vencidas=0
                t_vencer=0
                t_vencidas=0
                tot=0
                tot_g=0
                tot_din=0
                tot_gin=0
                l_mes=SUBSTR(DTOC(venc),4,5)
                mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
                mt_dia := 0
                mdia := venc
                WHILE dupp->venc <= t_fim .AND. ! EOF()
                        IF ! EMPTY(dupp->pago)
                                SKIP
                                mdia := dupp->venc
                                LOOP
                        ENDIF
                        IF pag=0 .OR. PROW()>=59
                                pag=pag+1
                                IF pag>1
                                        EJECT
                                ENDIF
                                cabecalho(pag,mtit,mtipo)
                                imprt(mtipo_imp,'C',1)
                                DEVPOS(PROW(),00);DEVOUT("Cod.  Fornecedor                               Tipo Duplicata     Emissao   Vencim            Valor  Dias")
                                DEVPOS(PROW()+1,00);DEVOUT(REPLICATE("=",137))
                                DEVPOS(PROW()+1,00);DEVOUT(l_mes)
                                DEVPOS(PROW()+1,00);DEVOUT(' ')
                        ENDIF
                        IF mdia <> dupp->venc
                                IF mtipo_rel = 'G'
                                        DEVPOS(PROW()+1,89);DEVOUT('              --------------')
                                        DEVPOS(PROW()+1,74);DEVOUT('Total do Vencimento '+DTOC(mdia)+':')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mt_dia,'99,999,999.99'))
                                        DEVPOS(PROW()+1,00);DEVOUT(REPLICATE("-",137))
                                ELSE
                                        DEVPOS(PROW()+1,00);DEVOUT('Total do Vencimento '+DTOC(mdia)+':')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mt_dia,'99,999,999.99'))
                                ENDIF
                                mt_dia := 0
                                mdia := dupp->venc
                        ENDIF
                        IF ! EMPTY(mtipo_doc)
                                IF mtipo_doc <> dupp->tipo
                                        SKIP
                                        mdia := dupp->venc
                                        LOOP
                                ENDIF
                        ENDIF
                        *&*
                        IF ! EMPTY(mcod_for)
                                IF dupp->fornec<>STRZERO(mcod_for,4)
                                        SKIP
                                        mdia := dupp->venc
                                        LOOP
                                ENDIF
                        ELSE
                                WHILE dupp->venc <= t_fim .AND. ! EOF() .AND. EMPTY(mcod_for)
                                        IF ! EMPTY(dupp->pago)
                                                SKIP
                                                mdia := dupp->venc
                                                LOOP
                                        ENDIF
                                        IF mfo = 'X' .AND. 'FO' = dupp->tip_for
                                                EXIT
                                        ELSEIF mfv = 'X' .AND. 'FV' = dupp->tip_for
                                                EXIT
                                        ELSEIF mvv = 'X' .AND. 'VV' = dupp->tip_for
                                                EXIT
                                        ELSEIF mtr = 'X' .AND. 'TR' = dupp->tip_for
                                                EXIT
                                        ELSEIF mad = 'X' .AND. 'AD' = dupp->tip_for
                                                EXIT
                                        ELSEIF mfi = 'X' .AND. 'FI' = dupp->tip_for
                                                EXIT
                                        ELSEIF mdv = 'X' .AND. 'DV' = dupp->tip_for
                                                EXIT
                                        ELSE
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDDO
                        ENDIF
                        IF EOF()
                                EXIT
                        ENDIF
                        IF l_mes<>SUBSTR(DTOC(dupp->venc),4,5) .AND. ! EOF()
                                DEVPOS(PROW()+1,00);DEVOUT("Total de Documentos no mes de "+l_mes+": "+transform(tot,"9999")+" - "+ltrim(transform(tot_din,"@E 999,999,999,999.99")))
                                DEVPOS(PROW(),00);DEVOUT("Total de Documentos no mes de "+l_mes+": "+transform(tot,"9999")+" - "+ltrim(transform(tot_din,"@E 999,999,999,999.99")))
                                tot=0
                                tot_din=0
                                l_mes=SUBSTR(DTOC(dupp->venc),4,5)
                                DEVPOS(PROW()+1,00);DEVOUT(' ')
                                DEVPOS(PROW()+1,00);DEVOUT(l_mes)
                                LOOP
                        ENDIF
                        IF mtipo_rel = 'G'
                                DEVPOS(PROW()+1,00);DEVOUT(dupp->fornec)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(dupp->cliente+"  "+dupp->tip_for)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(dupp->tipo+duplicata)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(dupp->tipo+dupp->duplicata)
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(DTOC(dupp->emissao))
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(DTOC(dupp->venc))
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(dupp->valor,"@E 99,999,999.99"))
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(dupp->venc-mdata_sis,"9999"))
                        ENDIF
                        tot ++
                        tot_g ++
                        tot_din=tot_din+dupp->valor
                        tot_gin=tot_gin+dupp->valor
                        mt_dia := mt_dia + dupp->valor
                        SKIP
                        IF mdia <> venc
                                IF mtipo_rel = 'G'
                                        DEVPOS(PROW()+1,89);DEVOUT('              --------------')
                                        DEVPOS(PROW()+1,74);DEVOUT('Total do Vencimento '+DTOC(mdia)+':')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mt_dia,'99,999,999.99'))
                                        DEVPOS(PROW()+1,00);DEVOUT(REPLICATE("-",137))
                                ELSE
                                        DEVPOS(PROW()+1,00);DEVOUT('Total do Vencimento '+DTOC(mdia)+':')
                                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mt_dia,'99,999,999.99'))
                                ENDIF
                                mt_dia := 0
                                mdia := dupp->venc
                        ENDIF
                        IF l_mes<>SUBSTR(DTOC(dupp->venc),4,5) .AND. ! EOF()
                                DEVPOS(PROW()+1,00);DEVOUT("Total de Documentos no mes de "+l_mes+": "+transform(tot,"9999")+" - "+ltrim(transform(tot_din,"@E 999,999,999,999.99")))
                                DEVPOS(PROW(),00);DEVOUT("Total de Documentos no mes de "+l_mes+": "+transform(tot,"9999")+" - "+ltrim(transform(tot_din,"@E 999,999,999,999.99")))
                                tot=0
                                tot_din=0
                                l_mes=SUBSTR(DTOC(dupp->venc),4,5)
                                DEVPOS(PROW()+1,00);DEVOUT(' ')
                                DEVPOS(PROW()+1,00);DEVOUT(l_mes)
                                LOOP
                        ENDIF
                ENDDO
                ? "Total de Documentos: "+TRANSFORM(tot,"9999")+" - "+ltrim(TRANSFORM(tot_din,"@E 999,999,999,999.99"))
                IF PROW() > 53
                        EJECT
                        pag=pag+1
                        cabecalho(pag,mtit,mtipo)
                        imprt(mtipo_imp,'N',1)
                        DEVPOS(PROW()+1,00);DEVOUT("*** RESUMO FINAL ***")
                        DEVPOS(PROW()+1,00);DEVOUT(' ')
                        IF mod=2
                                DEVPOS(PROW()+1,00);DEVOUT("Total de Documentos Vencidas no Periodo: "+TRANSFORM(tot_g,"9999")+" - "+ltrim(TRANSFORM(tot_gin,"@E 999,999,999,999.99")))
                        ELSE
                                DEVPOS(PROW()+1,00);DEVOUT("Total de Documentos a Vencer no Periodo: "+TRANSFORM(tot_g,"9999")+" - "+ltrim(TRANSFORM(tot_gin,"@E 999,999,999,999.99")))
                        ENDIF
                        DEVPOS(PROW()+2,00);DEVOUT(time())
                ELSE
                        imprt(mtipo_imp,'N',1)
                        DEVPOS(PROW()+1,00);DEVOUT("*** RESUMO FINAL ***")
                        IF mod=2
                                DEVPOS(PROW()+2,00);DEVOUT("Total de Documentos Vencidas no Periodo: "+TRANSFORM(tot_g,"9999")+" - "+ltrim(TRANSFORM(tot_gin,"@E 999,999,999,999.99")))
                        ELSE
                                DEVPOS(PROW()+2,00);DEVOUT("Total de Documentos a Vencer no Periodo: "+TRANSFORM(tot_g,"9999")+" - "+ltrim(TRANSFORM(tot_gin,"@E 999,999,999,999.99")))
                        ENDIF
                        DEVPOS(PROW()+2,00);DEVOUT(time())
                ENDIF
        ELSEIF mtipo_con = 'S'  //SINTETICO
                mtit := mtit+' SINTETICO'
                *****************
                SELE('dupp');ORDSETFOCUS(6)
                GO TOP
                *****************
                RESTSCREEN(01,00,24,79,tela)
                botao(15,05,17,75)
                DEVPOS(16,06);DEVOUT('Registro Processado:')
                mtecla = LASTKEY()
                SET DEVI TO PRINT
                pag=0
                vencer=0
                vencidas=0
                t_vencer=0
                t_vencidas=0
                tot=0
                tot_g=0
                tot_din=0
                tot_gin=0
                l_mes=SUBSTR(DTOC(venc),4,5)
                mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
                mt_dia := 0
                mdia := venc
                WHILE ! EOF()
                        mcod_aux := dupp->fornec
                        mcliente := dupp->cliente
                        mtip_for := dupp->tip_for
                        WHILE mcod_aux = dupp->fornec .AND. ! EOF()
                                IF pag=0 .OR. PROW()>=59
                                        pag=pag+1
                                        IF pag>1
                                                EJECT
                                        ENDIF
                                        cabecalho(pag,mtit,mtipo)
                                        imprt(mtipo_imp,'C',1)
                                        DEVPOS(PROW(),00);DEVOUT("Cod.  Fornecedor                               Tipo Duplicata     Emissao   Vencim            Valor  Dias")
                                        DEVPOS(PROW()+1,00);DEVOUT(REPLICATE("=",137))
                                ENDIF
                                IF dupp->venc > t_fim .OR. dupp->venc < t_ini
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(dupp->pago)
                                        SKIP
                                        mdia := venc
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mtipo_doc)
                                        IF mtipo_doc <> dupp->tipo
                                                SKIP
                                                mdia := dupp->venc
                                                LOOP
                                        ENDIF
                                ENDIF
                                *&*
                                IF ! EMPTY(mcod_for)
                                        IF dupp->fornec<>STRZERO(mcod_for,4)
                                                SKIP
                                                mdia := dupp->venc
                                                LOOP
                                        ENDIF
                                ELSE
                                        WHILE dupp->venc <= t_fim .AND. ! EOF() .AND. EMPTY(mcod_for)
*                                       WHILE dupp->venc <= t_fim .AND. ! EOF()
                                                IF ! EMPTY(dupp->pago)
                                                        SKIP
                                                        mdia := dupp->venc
                                                        LOOP
                                                ENDIF
                                                IF mfo = 'X' .AND. 'FO' = dupp->tip_for
                                                        EXIT
                                                ELSEIF mfv = 'X' .AND. 'FV' = dupp->tip_for
                                                        EXIT
                                                ELSEIF mvv = 'X' .AND. 'VV' = dupp->tip_for
                                                        EXIT
                                                ELSEIF mtr = 'X' .AND. 'TR' = dupp->tip_for
                                                        EXIT
                                                ELSEIF mad = 'X' .AND. 'AD' = dupp->tip_for
                                                        EXIT
                                                ELSEIF mfi = 'X' .AND. 'FI' = dupp->tip_for
                                                        EXIT
                                                ELSEIF mdv = 'X' .AND. 'DV' = dupp->tip_for
                                                        EXIT
                                                ELSE
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDDO
                                ENDIF
                                IF mtipo_rel = 'G'
                                        DEVPOS(PROW()+1,00);DEVOUT(dupp->fornec)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(dupp->cliente+"  "+dupp->tip_for)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(dupp->tipo+duplicata)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(DTOC(dupp->emissao))
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(DTOC(dupp->venc))
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(dupp->valor,"@E 99,999,999.99"))
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(dupp->venc-mdata_sis,"9999"))
                                ENDIF
                                tot ++
                                tot_g ++
                                tot_din=tot_din+dupp->valor
                                tot_gin=tot_gin+dupp->valor
                                mt_dia := mt_dia + dupp->valor
                                SKIP
                                IF EOF()
                                        EXIT
                                ENDIF
                        ENDDO
                        SETPOS(16,26);DISPOUT(dupp->fornec+'-'+dupp->cliente)
                        IF ! EMPTY(tot_din)
                                IF PROW() > 53
                                        EJECT
                                        pag=pag+1
                                        cabecalho(pag,mtit,mtipo)
                                        imprt(mtipo_imp,'C',1)
                                        DEVPOS(PROW(),00);DEVOUT("Cod.  Fornecedor                               Tipo Duplicata     Emissao   Vencim            Valor  Dias")
                                        DEVPOS(PROW()+1,00);DEVOUT(REPLICATE("=",137))
                                ENDIF
                                IF mtipo_rel = 'G'
                                        DEVPOS(PROW()+1,00);DEVOUT(REPLICATE("-",137))
                                        DEVPOS(PROW()+1,00);DEVOUT('Total de Documentos: '+STRZERO(tot,6)+' - Valor Total:'+TRANSFORM(tot_din,'9,999,999.99'))
                                        DEVPOS(PROW()+1,00);DEVOUT(REPLICATE("-",137))
                                        tot := 0
                                        tot_din := 0
                                ELSE
                                        DEVPOS(PROW()+1,00);DEVOUT(mcod_aux)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT(mcliente+"  "+mtip_for)
                                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Total de Documentos: '+STRZERO(tot,6)+' - Valor Total:'+TRANSFORM(tot_din,'9,999,999.99'))
                                        tot := 0
                                        tot_din := 0
                                ENDIF
                        ENDIF
                ENDDO
                IF PROW() > 53
                        EJECT
                        pag=pag+1
                        cabecalho(pag,mtit,mtipo)
                ENDIF
                imprt(mtipo_imp,'N',1)
                DEVPOS(PROW()+1,00);DEVOUT("*** RESUMO FINAL ***")
                DEVPOS(PROW()+2,00);DEVOUT("Total de Documentos APAGAR no Periodo: "+TRANSFORM(tot_g,"9999")+" - "+ltrim(TRANSFORM(tot_gin,"@E 999,999,999,999.99")))
                DEVPOS(PROW()+2,00);DEVOUT(time())
        ENDIF
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(01,00,24,79,tela)
        CLOSE ALL
        RETURN
ENDDO
RETURN NIL
*************************** F I M ***************************
