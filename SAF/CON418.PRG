**** CON413.PRG: Relatorio -> RECIBO DE PAGAMENTO (APAGAR)
***
*********************************************************

FUNCTION con418
***************
LOCAL MPRG:='CON418'
LOCAL tela,t_ini,t_fim,pag,tot,tot_g,tot_din,tot_gin,mquantd,;
      mtit,mtipo,opcao

PRIVATE mbanco

IF ! ver_nivel(mprg,'RECIBO DE PAGAMENTO (C.APAGAR)','13456',nivel_acess)
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
*------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('safccpg','ccpg');RETURN NIL;ENDIF
IF ! abriarq('sacdupp','dupp');RETURN NIL;ENDIF
*------------------------------------------------------------------
WHILE .T.
        botao(15,07,18,45)
        @ 16,09 SAY 'Data Inicial..:'
        @ 17,09 SAY 'Data Final....:'
        mbanco := SPACE(3)
        mensagem('Digite a data que deseja emitir os recibos - [ESC] p/retornar')
        t_ini=mdata_sis
        t_fim=mdata_sis
        @ 16,25 GET t_ini VALID lim_get() WHEN men_get(16,33,'Informe a data de inicio da Emissao dos Recibos')
        @ 17,25 GET t_fim VALID lim_get() WHEN men_get(17,33,'Informe a data de final da Emissao dos Recibos')
        READ
        IF LASTKEY()=27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN
        ENDIF
        mensagem('Confirma a impressao [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        mensagem('Aguarde o final da Impressao - [ESC] Abandonar')
        *****************
        SELE('dupp');ORDSETFOCUS(2)
        DBSETFILTER({|| (venc <= t_fim  .AND. venc >= t_ini) .AND. pago <> 'B'})
        GO TOP
        *****************
        SET DEVI TO PRINT
        SET PRINT on
        pag=0
        tot=0
        tot_g=0
        tot_din=0
        tot_gin=0
        l_mes=SUBSTR(DTOC(venc),4,5)
        mtit := "RECIBOS DAS DUPLICATA (Contas a Pagar)"
        mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
        mfornec := fornec
        WHILE ! EOF()
                INKEY(.3)
                IF LASTKEY() = 27
                        DEVPOS(PROW()+2,00);DEVOUT('Impressao cancelada pelo operador')
                        EXIT
                ENDIF
                IF pag=0 .OR. PROW()>=59
                        pag=pag+1
                        IF pag>1
                                EJECT
                        ENDIF
                        mtit := "RECIBOS DAS DUPLICATA (Contas a Pagar)"
                        cabecalho(pag,mtit,mtipo)
                        imprt('M','C',1)
                        ?? "Cod. Fornecedor                                 Duplicata                           Emissao   Vencim               Valor"
                        ? REPLICATE("=",137)
                ENDIF
                IF fornec<>mfornec
                        IF PROW() > 28
                                DEVPOS(PROW()+1,74);DEVOUT("                            ------------------")
                                DEVPOS(PROW()+1,74);DEVOUT("Valor Total A PAGAR: "+TRANSFORM(tot,"9999")+" - "+TRANSFORM(tot_din,"@E 999,999,999,999.99"))
                                DEVPOS(PROW(),74);DEVOUT("                     "+TRANSFORM(tot,"9999")+" - "+TRANSFORM(tot_din,"@E 999,999,999,999.99"))
                                DEVPOS(59,00);DEVOUT(REPLI("-",137))
                                EJECT
                                mfornec := fornec
                                tot=0
                                tot_din=0
                                IF ! EOF()
                                        cabecalho(pag,mtit,mtipo)
                                        imprt('M','C',1)
                                        ?? "Cod. Fornecedor                                 Duplicata                           Emissao   Vencim               Valor"
                                        ? REPLICATE("=",137)
                                ELSE
                                        LOOP
                                ENDIF
                        ELSE
                                DEVPOS(PROW()+1,74);DEVOUT("                            ------------------")
                                DEVPOS(PROW()+1,74);DEVOUT("Valor Total A PAGAR: "+TRANSFORM(tot,"9999")+" - "+TRANSFORM(tot_din,"@E 999,999,999,999.99"))
                                DEVPOS(PROW(),74);DEVOUT("                     "+TRANSFORM(tot,"9999")+" - "+TRANSFORM(tot_din,"@E 999,999,999,999.99"))
                                DEVPOS(28,00);DEVOUT(REPLI("-",137))
                                mfornec := fornec
                                tot=0
                                tot_din=0
                                IF ! EOF()
                                        DEVPOS(PROW()+2,00);DEVOUT(' ')
                                        cabecalho(pag,mtit,mtipo)
                                        imprt('M','C',1)
                                        ?? "Cod. Fornecedor                                 Duplicata                           Emissao   Vencim               Valor"
                                        ? REPLICATE("=",137)
                                ELSE
                                        LOOP
                                ENDIF
                        ENDIF
                ENDIF
                ? fornec+" "
                mpoint := RECNO()
                ver_fab(VAL(dupp->fornec),PROW(),PCOL(),'*')
                GO mpoint
                DEVPOS(PROW(),PCOL()+3);DEVOUT(duplicata+" ")
                mpoint := RECNO()
                mbanco := VAL(banco)
*               bank('*')
                GO mpoint
                ?? SPACE(25)
                ?? DTOC(emissao)+" "
                ?? DTOC(venc)+" "
                ?? TRANSFORM(valor,"@E 999,999,999,999.99")+" "
*               ?? TRANSFORM(venc-mdata_sis,"9999")
                tot=tot+1
                tot_g=tot_g+1
                tot_din=tot_din+valor
                tot_gin=tot_gin+valor
                SKIP
        ENDDO
        IF tot > 0
                IF PROW() > 28
                        DEVPOS(PROW()+1,74);DEVOUT("                            ------------------")
                        DEVPOS(PROW()+1,74);DEVOUT("Valor Total A PAGAR: "+TRANSFORM(tot,"9999")+" - "+TRANSFORM(tot_din,"@E 999,999,999,999.99"))
                        DEVPOS(PROW(),74);DEVOUT("                     "+TRANSFORM(tot,"9999")+" - "+TRANSFORM(tot_din,"@E 999,999,999,999.99"))
                        DEVPOS(59,00);DEVOUT(REPLI("-",137))
                        tot=0
                        tot_din=0
                ELSE
                        DEVPOS(PROW()+1,74);DEVOUT("                            ------------------")
                        DEVPOS(PROW()+1,74);DEVOUT("Valor Total A PAGAR: "+TRANSFORM(tot,"9999")+" - "+TRANSFORM(tot_din,"@E 999,999,999,999.99"))
                        DEVPOS(PROW(),74);DEVOUT("                     "+TRANSFORM(tot,"9999")+" - "+TRANSFORM(tot_din,"@E 999,999,999,999.99"))
                        DEVPOS(28,00);DEVOUT(REPLI("-",137))
                        tot=0
                        tot_din=0
                ENDIF
        ENDIF
        EJECT
        pag=pag+1
        cabecalho(pag,mtit,mtipo)
        imprt('M','N',1)
        ? "*** RESUMO FINAL ***"
        ?
        ? "Total de Duplicatas: "+TRANSFORM(tot_g,"9999")+" - "+ltrim(TRANSFORM(tot_gin,"@E 999,999,999,999.99"))
        ?
        ? time()
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDDO
RETURN NIL
*************************** F I M ***************************

