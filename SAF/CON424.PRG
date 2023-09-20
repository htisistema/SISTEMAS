**** CON424.PRG: Relatorio -> Duplicatas Recebidas PAGAS
***
******************************************************

FUNCTION con424
****************
LOCAL MPRG:='CON424'
LOCAL tela,t_ini,t_fim,f_ini,f_fim,l_mes,td_mes,tp_mes,u_dat,pag,;
      vencer,vencidas,t_vencer,t_vencidas,tot_dup,tot_jur,tot_des,vlr_dup,;
      vlr_jur,vlr_des,mtot_dup,mtot_jur,mtot_des,mvlr_dup,mvlr_jur,mvlr_des,;
      mquantd,mtit,mtipo,mtip,mdia,mt_dia,bank,mtipo_dup,mtip_pg:='  ',mcod_vend,;
      mloc_pg:=' ',marea:=SPACE(15)
PRIVATE mcliente,mcgc,mcpf,t_for,mnome_vend,mcom_ven
PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'CONTA A RECEBER (RELATORIO DOCUMENTO PAGOS)','15',nivel_acess)
        RETURN NIL
ENDIF


tela := SAVESCREEN(01,00,24,79)

*---------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('saccfg','cfg');RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
*---------------------------------------------------------------------
mtipo_imp := cfg->imp_rel
mensagem('Preencha os dados que desejar')
botao(09,02,19,78,,' Duplicatas Pagas ')
@ 10,03 TO 10,77
@ 11,03 SAY "Codigo do Cliente.:"
@ 12,03 SAY "[C]arteira [B]anco:"
@ 13,03 SAY "Data do Inicio....:"
@ 14,03 SAY "Data do Final.....:"
@ 15,03 SAY "Tipo da Operacao..:"
@ 16,03 SAY "Cod.Vendedor......:"
@ 17,03 SAY "Local Pagamento...:"
@ 18,03 SAY "Area do Cliente...:"
t_for=0
t_ini=mdata_sis
t_fim=mdata_sis+30
bank=SPACE(1)
mtip_pg := '  '
mloc_pg := ' '
mcod_vend := 0
@ 11,23 GET t_for pict "99999" VALID ver_cli(t_for,11,28)
@ 12,23 GET bank PICT '@!' VALID bank $ 'C,B, '
@ 13,23 GET t_ini VALID IF(EMPTY(t_ini),.F.,.T.)
@ 14,23 GET t_fim VALID IF(EMPTY(t_fim),.F.,.T.)
@ 15,23 GET mtip_pg PICT '@!'
@ 16,23 GET mcod_vend pict "999" VALID ven(mcod_vend,16,27) .AND. lim_get() WHEN t_for = 0 .AND. men_get(17,39,'Informe o Vendedor que deseja ou nao informe p/imprimir todos')
@ 17,23 GET mloc_pg pict "@!" VALID mloc_pg $ 'M,C, ' .AND. lim_get() WHEN men_get(18,39,'Digite [M] pelo Movimento [C]aixa ou nao informe p/imprimir todos')
@ 18,23 GET marea PICT "@!" VALID lim_get() WHEN men_get(19,39,'Digite a AREA do cliente ou nao informe p/imprimir todos')
READ
IF LASTKEY()=27
        RESTSCREEN(01,00,24,79,tela)
        CLOSE ALL
        RETURN
ENDIF
l_mes=" "
td_mes=0    && tot.dup no mes
tp_mes=0    && tot. pagar no mes
RESTSCREEN(01,00,24,79,tela)
quadro("Opcoes: [1]Dup. [2]Cheq. [3]Cartao [4]Financ. [5]Geral - [ESC] p/Abandonar")
WHILE .T.
        INKEY(0)
        IF LASTKEY()=27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN
        ENDIF
        IF LASTKEY()=ASC("1")
                mtit := 'DUPLICATAS PAGAS (Contas a Receber)'
                mtip := 'DU'
                EXIT
        ELSEIF LASTKEY()=ASC("2")
                mtit := 'CHEQUES PAGOS (Contas a Receber)'
                mtip := 'CH'
                EXIT
        ELSEIF LASTKEY()=ASC("3")
                mtit := 'CARTOES PAGOS (Contas a Receber)'
                mtip := 'CT'
                EXIT
        ELSEIF LASTKEY()=ASC("4")
                mtit := 'FINANCEIRA PAGAS (Contas a Receber)'
                mtip := 'FI'
                EXIT
        ELSEIF LASTKEY()=ASC("5")
                mtit := 'DOCUMENTOS PAGOS (Contas a Receber)'
                mtip := 'GE'
                EXIT
        ENDIF
ENDDO
mtipo_dup := mensagem1('So as duplicata com JUROS:','N','S,N')
RESTSCREEN(01,00,24,79,tela)
**************
SELE('dupr');ORDSETFOCUS(3)
GO TOP
**************
dupr->(DBSEEK(DTOS(t_ini),.T.))
IF EOF()
        atencao('Nao existe movimento neste periodo !!!')
        RESTSCREEN(01,00,24,79,tela)
        CLOSE ALL
        RETURN NIL
ENDIF
IF ! imp_arq('DOC_PAGAS.REL','R')
        RESTSCREEN(01,00,24,79,tela)
        CLOSE ALL
        RETURN NIL
ENDIF
SET PRINT on
pag=0
vencer=0
vencidas=0
t_vencer=0
t_vencidas=0
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
mdia := datpag
mt_dia := 0
IF ! EMPTY(mcod_vend)
        mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)+' - '+STRZERO(mcod_vend,3)+'-'+mnome_vend
ELSE
        mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
ENDIF
mcliente := SPACE(40)
WHILE dupr->datpag <= t_fim .AND. ! EOF()
        IF pag=0 .OR. PROW()>=57
                pag=pag+1
                IF pag>1
                        EJECT
                ENDIF
                cabecalho(pag,mtit,mtipo,mprg)
                imprt(cfg->imp_rel,'C',1)
                ?? "Cod. Cliente                                  Documento    Local Pag.     D.Pagam    Valor Documento         Valor Pago    Dias"
                ? REPLICATE("-",137)
        ENDIF
        IF ! EMPTY(mcod_vend) .AND. STRZERO(mcod_vend,3) <> dupr->vendedor
                SKIP
                LOOP
        ENDIF
        IF mtipo_dup = 'S' .AND. dupr->juros = 0
                SKIP
                LOOP
        ENDIF
        IF dupr->pago <> 'B'
                SKIP
                LOOP
        ENDIF
        IF ! EMPTY(mtip_pg) .AND. mtip_pg <> dupr->tip_pg
                SKIP
                LOOP
        ENDIF
        IF ! EMPTY(mloc_pg) .AND. mloc_pg <> dupr->mov_cx
                SKIP
                LOOP
        ENDIF
        IF mdia <> datpag
                IF mt_dia > 0
                        DEVPOS(PROW()+1,89);DEVOUT('Total do dia:')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mt_dia,'99,999,999.99'))
                ENDIF
                mt_dia := 0
                mdia := datpag
        ENDIF
        IF t_for<>0 .AND. dupr->fornec<>STRZERO(t_for,5)
                SKIP
                LOOP
        ENDIF
        IF mtip <> 'GE' .AND. mtip <> dupr->tipo
                SKIP
                LOOP
        ENDIF
        IF bank<>SPACE(1) .AND. banco<>bank
                SKIP
                LOOP
        ENDIF
        IF l_mes<>SUBSTR(DTOC(datpag),4,5)
                l_mes=SUBSTR(DTOC(datpag),4,5)
                ?
                ? l_mes
                ?
        ENDIF
        ver_cli(VAL(fornec),,,'*')
        IF ! EMPTY(marea) .AND. marea <> cli->area
                SKIP
                LOOP
        ENDIF
        ? fornec+" "
        ?? mcliente+"  "
        ?? tipo+duplicata+conta+" "
        IF banco = 'B'
                ?? 'BANCO         '
        ELSEIF banco = 'C'
                ?? 'CARTEIRA      '
        ELSE
                ?? '              '
        ENDIF
        ?? DTOC(datpag)+" "
        ?? TRANSFORM(valor,"@E 999,999,999,999.99")+" "
        ?? TRANSFORM(vlpago,"@E 999,999,999,999.99")+" "
        ?? TRANSFORM(venc-mdata_sis,"9999")
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
        mt_dia := mt_dia + valor
        SKIP
        IF l_mes<>SUBSTR(DTOC(datpag),4,5)
                IF PROW()<57
                        ? "** RESUMO DO MES **"
                        ?
                        ? "Total de Juros.....: "+TRANSFORM(tot_jur,"9999")+" - "+TRANSFORM(vlr_jur,"@E 999,999,999,999.99")
                        ? "Total de Descontos.: "+TRANSFORM(tot_des,"9999")+" - "+TRANSFORM(vlr_des,"@E 999,999,999,999.99")
                        ? "Total de Documentos: "+TRANSFORM(tot_dup,"9999")+" - "+TRANSFORM(vlr_dup,"@E 999,999,999,999.99")
                        ?
                ELSE
                        EJECT
                        pag=pag+1
                        cabecalho(pag,mtit,mtipo,mprg)
                        imprt(cfg->imp_rel,'C',1)
                        ?? "Cod. Cliente                        Documento    Banco/Agencia         D.Pagam    Valor Documento         Valor Pago    Dias"
                        ? REPLICATE("-",137)
                        ?
                        ? l_mes
                        ? "** RESUMO DO MES **"
                        ?
                        ? "Total de Juros.....: "+TRANSFORM(tot_jur,"9999")+" - "+TRANSFORM(vlr_jur,"@E 999,999,999,999.99")
                        ? "Total de Descontos.: "+TRANSFORM(tot_des,"9999")+" - "+TRANSFORM(vlr_des,"@E 999,999,999,999.99")
                        ? "Total de Documentos: "+TRANSFORM(tot_dup,"9999")+" - "+TRANSFORM(vlr_dup,"@E 999,999,999,999.99")
                ENDIF
                tot_dup=0
                tot_jur=0
                tot_des=0
                vlr_dup=0
                vlr_jur=0
                vlr_des=0
        ENDIF
ENDDO
IF PROW() >= 52;pag=pag+1;cabecalho(pag,mtit,mtipo,mprg);ENDIF
?
?
imprt(cfg->imp_rel,'N',1)
?? "** RESUMO FINAL **"
?
? "Total de Juros.....: "+TRANSFORM(mtot_jur,"9999")+" - "+TRANSFORM(mvlr_jur,"@E 999,999,999,999.99")
? "Total de Descontos.: "+TRANSFORM(mtot_des,"9999")+" - "+TRANSFORM(mvlr_des,"@E 999,999,999,999.99")
? "Total de Documentos: "+TRANSFORM(mtot_dup,"9999")+" - "+TRANSFORM(mvlr_dup,"@E 999,999,999,999.99")
?
? time()
EJECT
SETPRC(00,00)
SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
RESTSCREEN(01,00,24,79,tela)
IF mimp_tipo = 2
        lertexto('DOC_PAGAS.REL')
ENDIF
CLOSE ALL
RETURN NIL




