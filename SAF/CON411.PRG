*********************************************************
**** CON411.PRG: Relatorio -> Duplicatas EMITIDAS (CONTAS APAGAR)
*********************************************************

FUNCTION con411
***************
LOCAL MPRG:='CON411'
LOCAL tela,t_ini,t_fim,mod,td_mes,tp_mes,pag,mcod_vend,;
      vencer,vencidas,t_vencer,t_vencidas,tot,tot_g,tot_din,tot_gin,l_mes,tt,;
      xxx,mtit,mtipo,mtip,mdia,mt_dia,opcao,mquantd,mtipo_doc
LOCAL mfo,mfv,mvv,mad,mtr,mfi,mdv

PRIVATE mfornece,muf,mcod_for

IF ! ver_nivel(mprg,'RELATORIO DE DOCUMENTOS RECEBIDOS (C.APAGAR)','1456',nivel_acess)
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
*---------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('insopera','sen');RETURN NIL;ENDIF
IF ! abriarq('safccpg','ccpg');RETURN NIL;ENDIF
IF ! abriarq('sacdupp','dupp');RETURN NIL;ENDIF
*---------------------------------------------------------------------
****************
SELE('ccpg');ORDSETFOCUS(1)
****************
botao(12,10,18,78)
janela(12,51," Vencer/Vencidas ")
@ 13, 12 SAY "Fornecedor.:"
@ 14, 12 SAY "Inicio.....:"
@ 15, 12 SAY "Final......:"
@ 16, 12 SAY "Tipo Doc...:"
DEVPOS(17,12);DEVOUT('Tipo Conta..: FO:    FV:    VV:    TR:    AD:    FI:    DV:')
WHILE .T.
        mensagem('Preencha os Campos - <ESC> Abandonar')
        mfo := SPACE(1)
        mfv := SPACE(1)
        mvv := SPACE(1)
        mad := SPACE(1)
        mtr := SPACE(1)
        mfi := SPACE(1)
        mdv := SPACE(1)
        mtipo_doc := SPACE(2)
        mcod_for = 0
        mcod_vend := 0
        mcliente := SPACE(30)
        t_ini=CTOD("  /  /  ")
        t_fim=CTOD("  /  /  ")
        mod := 0
        @ 13,25 GET mcod_for pict "9999" VALID v_fornece(mcod_for,13,30) .AND. lim_get() WHEN men_get(17,25,'Informe o Fornecedor que deseja ou nao informe p/imprimir todos')
        @ 14,25 GET t_ini VALID lim_get() WHEN men_get(15,25,'Informe a data inicial do periodo que deseja imprimir')
        @ 15,25 GET t_fim VALID lim_get() WHEN men_get(16,25,'Informe a data final do periodo que deseja imprimir')
        @ 16,25 GET mtipo_doc PICT '@!' VALID lim_get() WHEN men_get(17,25,'Informe o tipo de DOCUMENTO que deseja <PA>pagamento <DU>duplicata <CH>cheques <FR>fretes ou NAO informe nenhum tipo de DOCUMENTO')
        @ 17,30 GET mfo PICT '@!' VALID mfo $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(18,35,'Marque com um "X" p/consulta Conta de Fornecedor ou deixe em branco')
        @ 17,37 GET mfv PICT '@!' VALID mfv $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(18,42,'Marque com um "X" p/consulta Conta de Fixa de Venda ou deixe em branco')
        @ 17,44 GET mvv PICT '@!' VALID mvv $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(18,49,'Marque com um "X" p/consulta Conta de Variavel de Venda ou deixe em branco')
        @ 17,51 GET mtr PICT '@!' VALID mtr $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(18,56,'Marque com um "X" p/consulta Conta de Tributaria ou deixe em branco')
        @ 17,58 GET mad PICT '@!' VALID mad $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(18,63,'Marque com um "X" p/consulta Conta de Administrativa ou deixe em branco')
        @ 17,65 GET mfi PICT '@!' VALID mfi $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(18,70,'Marque com um "X" p/consulta Conta de Financeira ou deixe em branco')
        @ 17,72 GET mdv PICT '@!' VALID mfi $ 'X, ' .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(18,70,'Marque com um "X" p/consulta Conta de Financeira ou deixe em branco')
        READ
        IF LASTKEY()=27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN
        ENDIF
        RESTSCREEN(01,00,24,79,tela)
        mtit := "DUPLICATAS RECEBIDAS (Contas a Pagar)"
        RESTSCREEN(01,00,24,79,tela)
        mensagem('Quantidade de copias:')
        mquantd := 1
        @ 23,COL() GET mquantd PICT '99'
        READ
        IF LASTKEY() = 27 .OR. EMPTY(mquantd)
                LOOP
        ENDIF
        mensagem('Confirma a Impressao [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        RESTSCREEN(01,00,24,79,tela)
        mensagem('Aguarde o final da Impressao - [ESC] Abandonar')
        i := 0
        FOR i = 1 TO mquantd
                td_mes=0    && tot.dup no mes
                tp_mes=0    && tot. pagar no mes
                IF t_ini = CTOD("  /  /  ") .OR. t_fim = CTOD('  /  /  ')
                        t_ini=CTOD("11/11/11")
                        t_fim=CTOD("31/12/99")
                ENDIF
                IF ! EMPTY(mcod_for)
                        **************
                        SELE('dupp');ORDSETFOCUS(4)
                        DBSETFILTER({|| (emissao <= t_fim .AND. emissao >= t_ini);
                                        .AND. fornec = STRZERO(mcod_for,4)})
                        GO TOP
                        **************
                ELSE
                        **************
                        SELE('dupp');ORDSETFOCUS(4)
                        DBSETFILTER({|| (emissao <= t_fim .AND. emissao >= t_ini)})
                        GO TOP
                        **************
                ENDIF
                SET DEVI TO PRINT
                SET PRINT ON
                pag=0
                vencer=0
                vencidas=0
                t_vencer=0
                t_vencidas=0
                SET DATE brit
                tot=0
                tot_g=0
                tot_din=0
                tot_gin=0
                l_mes=SUBSTR(DTOC(venc),4,5)
                mt_dia := 0
                mdia := venc
                mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
                WHILE ! EOF()   // .AND. venc<=t_fim .AND. tecla()=.F.
                        mfornece := SPACE(30)
                        INKEY(.3)
                        IF pag=0 .OR. PROW()>=59
                                pag=pag+1
                                IF pag>1
                                        EJECT
                                ENDIF
                                cabecalho(pag,mtit,mtipo)
                                imprt('M','C')
                                IF mcod_vend > 0
                                        DEVPOS(PROW()+1,00);DEVOUT('Vendedor: '+STRZERO(mcod_vend,3)+' - '+mnome_vend)
                                        DEVPOS(PROW(),00);DEVOUT('          '+STRZERO(mcod_vend,3)+' - '+mnome_vend)
                                ENDIF
                                ? "Cod. Fornecedor                               Documento    Banco/Agencia        Emissao    Vencim          Valor  Situacao"
                                ? REPLICATE("-",137)
                                ? l_mes
                                ?
                        ENDIF
                        IF ! EMPTY(mtipo_doc)
                                IF mtipo_doc <> tipo
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        *&*
                        WHILE ! EOF()
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
                        *&*
                        IF l_mes<>SUBSTR(DTOC(venc),4,5) .AND. ! EOF()
                                ? "Total de Documentos: "+transform(tot,"9999")+" - "+ltrim(transform(tot_din,"@E 999,999,999,999.99"))
                                tot=0
                                tot_din=0
                                l_mes=SUBSTR(DTOC(venc),4,5)
                                ? l_mes
                                ? ' '
                                LOOP
                        ENDIF
                        ? dupp->fornec+" "
                        ?? dupp->cliente+" "
                        ?? dupp->tipo+dupp->duplicata+" "
                        ?? '                    '+" "
                        ?? DTOC(dupp->emissao)+" "
                        ?? DTOC(dupp->venc)+" "
                        ?? transform(dupp->valor,"@E 999,999,999.99")+" "
                        IF pago = 'B'
                                ?? '  Paga em '+DTOC(dupp->datpag)
                        ELSE
                                ?? '  em Aberto - a vencer:'+transform(dupp->venc-mdata_sis,"9999")+' dias'
                        ENDIF
                        tot=tot+1
                        tot_g=tot_g+1
                        tot_din=tot_din+dupp->valor
                        tot_gin=tot_gin+dupp->valor
                        mt_dia := mt_dia + dupp->valor
                        SKIP
                        IF mdia <> venc
                                DEVPOS(PROW()+1,85);DEVOUT('              --------------')
                                DEVPOS(PROW()+1,70);DEVOUT('Total do Vencimento '+DTOC(mdia)+':')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mt_dia,'99,999,999.99'))
                                ? REPLICATE("-",137)
                                mt_dia := 0
                                mdia := venc
                        ENDIF

                        IF l_mes<>SUBSTR(DTOC(venc),4,5) .AND. ! EOF()
                                ? "Total de Documentos: "+transform(tot,"9999")+" - "+ltrim(transform(tot_din,"@E 999,999,999,999.99"))
                                tot=0
                                tot_din=0
                                l_mes=SUBSTR(DTOC(venc),4,5)
                                ? l_mes
                                ?
                        ENDIF
                ENDDO
                ? "Total de Documentos: "+transform(tot,"9999")+" - "+ltrim(transform(tot_din,"@E 999,999,999,999.99"))
                IF PROW() >= 55
                        EJECT
                        pag=pag+1
                        cabecalho(pag,mtit,mtipo)
                ENDIF
                ?
                imprt('M','N',1)
                ?? "*** RESUMO FINAL ***"
                ?
                IF mod=2
                        ? "Total de Documentos Vencidos no Periodo: "+transform(tot_g,"9999")+" - "+ltrim(transform(tot_gin,"@E 999,999,999,999.99"))
                ELSE
                        ? "Total de Documentos a Vencer no Periodo: "+transform(tot_g,"9999")+" - "+ltrim(transform(tot_gin,"@E 999,999,999,999.99"))
                ENDIF
                ?
                ? time()
                EJECT
                SETPRC(00,00)
        NEXT
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(01,00,24,79,tela)
        CLOSE ALL
        RETURN
ENDDO
RETURN NIL
******************************* F I M *******************************

