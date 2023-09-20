*********************************************************
**** CON420.PRG: Relatorio -> Duplicatas EMITIDAS (conta a receber)
*********************************************************

FUNCTION con420
***************
LOCAL MPRG:='CON420'
LOCAL tela,t_ini,t_fim,mod,td_mes,tp_mes,pag,mcod_vend,;
      mfornec,tot,tot_g,tot_din,tot_gin,l_mes,tt,;
      xxx,mtit,mtipo,mtip,mdia,mt_dia,opcao,mquantd,mtipo_con,tipo_sit := ' '

PRIVATE mcliente,mcgc,mcpf,t_for,bank,mnome_vend,mcom_ven
PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)
PRIVATE marq_prin,mimp_rel,mporta_imp

IF ! ver_nivel(mprg,'CONTA A RECEBER (RELATORIO DE DOCUMENTOS EMITIDOS)','156',nivel_acess)
        RETURN NIL
ENDIF
tela := SAVESCREEN(01,00,24,79)
*---------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('sacped_s','ped_s');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
*---------------------------------------------------------------------
****************
SELE('cli');ORDSETFOCUS(1)
****************
botao(14,05,22,77,," Documentos Emitidos ")
@ 16,06 SAY "Codigo do Cliente........:"
@ 17,06 SAY "Codigo do Vendedor.......:"
@ 18,06 SAY "Pag.em [C]arteira [B]anco:"
@ 19,06 SAY "Data do Inicio...........:"
@ 20,06 SAY "Data do Final............:"
@ 21,06 SAY "So [P]agos [N]ao Pagos [G]eral:"
WHILE .T.
********* VARIAVEIS DE IMPRESSAO *******************
        mtipo_imp := cfg->imp_rel
****************************************************
        bank=SPACE(1)
        t_for = 0
        mcod_vend := 0
        mcliente := SPACE(30)
        t_ini=CTOD("  /  /  ")
        t_fim=CTOD("  /  /  ")
        mod := 0
        mfornec := SPACE(4)
        tipo_sit := ' '
        @ 16,33 GET t_for pict "99999" VALID lim_get() .AND. ver_cli(t_for,16,39) WHEN mcod_vend = 0 .AND. men_get(17,39,'Informe o cliente que deseja ou nao informe p/imprimir todos')
        @ 17,33 GET mcod_vend pict "999" VALID ven(mcod_vend,17,37) .AND. lim_get() WHEN t_for = 0 .AND. men_get(18,39,'Informe o Vendedor que deseja ou nao informe p/imprimir todos')
        @ 18,33 GET bank pict "@!" VALID bank $ 'C,B, '
        @ 19,33 GET t_ini VALID lim_get() WHEN men_get(20,33,'Informe a data inicial do periodo que deseja imprimir')
        @ 20,33 GET t_fim VALID lim_get() WHEN men_get(21,33,'Informe a data final do periodo que deseja imprimir')
        @ 21,40 GET tipo_sit PICT '@!' VALID tipo_sit $ 'P,N,G'
        READ
        IF LASTKEY()=27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN
        ENDIF
        RESTSCREEN(01,00,24,79,tela)
        quadro("Opcoes: [1]Dup. - [2]Cheq. - [3]Cartoes - [4]Financ. - [5]Geral - [ESC]Abandonar")
        WHILE .T.
                INKEY(0)
                IF LASTKEY()=27
                        RESTSCREEN(01,00,24,79,tela)
                        CLOSE ALL
                        RETURN
                ENDIF
                IF LASTKEY()=ASC("1")
                        mtit := "DUPLICATAS EMITIDAS (Contas a Receber)"
                        mtip := 'DU'
                        EXIT
                ELSEIF LASTKEY()=ASC("2")
                        mtit := "CHEQUES EMITIDOS (Contas a Receber)"
                        mtip := 'CH'
                        EXIT
                ELSEIF LASTKEY()=ASC("3")
                        mtit := "CARTOES EMITIDOS (Contas a Receber)"
                        mtip := 'CT'
                        EXIT
                ELSEIF LASTKEY()=ASC("4")
                        mtit := "FINANCEIRA EMITIDOS (Contas a Receber)"
                        mtip := 'FI'
                        EXIT
                ELSEIF LASTKEY()=ASC("5")
                        mtit := "DOCUMENTOS EMITIDOS (Contas a Receber)"
                        mtip := 'GE'
                        EXIT
                ENDIF
        ENDDO
        IF EMPTY(t_for)
                mtipo_con := mensagem1('Escolha o tipo de consulta [A]nalitico [S]intetico CLIENTE [T]Sintetico TIPO DOC.:','A','A,S,T')
        ELSE
                mtipo_con := 'A'
        ENDIF
        RESTSCREEN(01,00,24,79,tela)
        i := 0
        td_mes=0    && tot.dup no mes
        tp_mes=0    && tot. pagar no mes
        IF t_ini = CTOD("  /  /  ") .OR. t_fim = CTOD('  /  /  ')
                t_ini=CTOD("11/11/11")
                t_fim=CTOD("31/12/99")
        ENDIF
        IF mtipo_con = 'A'
                **************
                SELE('dupr');ORDSETFOCUS(5)
                GO TOP
                **************
                dupr->(DBSEEK(DTOS(t_ini),.T.))
                IF EOF()
                        atencao('Nao existe movimento neste periodo !!!')
                        LOOP
                ENDIF
                IF ! imp_arq('DOC_EMIT.REL','R')
                        RETURN NIL
                ENDIF
                SET PRINT ON
                pag=0
                tot=0
                tot_g=0
                tot_din=0
                tot_gin=0
                mfornec := dupr->fornec
                l_mes=SUBSTR(DTOC(venc),4,5)
                mt_dia := 0
                mdia := emissao
                mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
                WHILE dupr->emissao <= t_fim .AND. ! EOF()
                        mcliente := SPACE(30)
                        IF pag=0 .OR. PROW()>=59
                                pag=pag+1
                                IF pag>1
                                        EJECT
                                ENDIF
                                cabecalho(pag,mtit,mtipo)
                                imprt(cfg->imp_rel,'C')
                                IF mcod_vend > 0
                                        DEVPOS(PROW()+1,00);DEVOUT('Vendedor: '+STRZERO(mcod_vend,3)+' - '+mnome_vend)
                                        DEVPOS(PROW(),00);DEVOUT('          '+STRZERO(mcod_vend,3)+' - '+mnome_vend)
                                ENDIF
                                ? "Cod.  Cliente                                  No. Documento      Tp.Pag.  Emissao    Venc.      Valor  Dias  Sit."
                                ? REPLICATE("-",137)
                                ?
                        ENDIF
                        IF mdia <> emissao
*                               ? REPLICATE("*",137)
                                DEVPOS(PROW()+1,77);DEVOUT('           --------------')
                                DEVPOS(PROW()+1,62);DEVOUT('Total da Emissao '+DTOC(mdia)+':')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mt_dia,'99,999,999.99'))
                                ? REPLICATE("*",137)
                                mt_dia := 0
                                mdia := emissao
                        ENDIF
                        IF ! EMPTY(mcod_vend) .AND. STRZERO(mcod_vend,3) <> vendedor
                                SKIP
                                mdia := dupr->emissao
                                LOOP
                        ENDIF
                        IF tipo_sit <> 'G'
                                IF tipo_sit = 'P'
                                        IF EMPTY(dupr->datpag)
                                                SKIP
                                                mdia := dupr->emissao
                                                LOOP
                                        ENDIF
                                ELSE
                                        IF ! EMPTY(dupr->datpag)
                                                SKIP
                                                mdia := dupr->emissao
                                                LOOP
                                        ENDIF
                                ENDIF
                        ENDIF
                        IF ! EMPTY(t_for) .AND. dupr->fornec<>STRZERO(t_for,5)
                                SKIP
                                mdia := dupr->emissao
                                LOOP
                        ENDIF
                        IF ! EMPTY(bank) .AND. bank<>dupr->banco
                                SKIP
                                mdia := dupr->emissao
                                LOOP
                        ENDIF
                        IF mtip <> 'GE' .AND. mtip<>dupr->tipo
                                SKIP
                                mdia := dupr->emissao
                                LOOP
                        ENDIF
                        *&*
/*
                        IF mfornec<>dupr->fornec .AND. ! EOF()
                                IF tot_din > 0
                                        ? "Total de Documentos: "+transform(tot,"9999")+" - "+ltrim(transform(tot_din,"@E 999,999,999,999.99"))
                                        ? REPLICATE("-",137)
                                        ? ' '
                                ENDIF
                                tot=0
                                tot_din=0
                                mfornec := dupr->fornec
                                LOOP
                        ENDIF
*/
                        ? fornec+" "
                        ?? cliente+" "
                        ?? numero+" "
                        ?? tipo+duplicata+conta+" "
                        IF banco = 'C'
                                ?? 'CARTEIRA'+" "
                        ELSEIF banco = 'B'
                                ?? 'BANCO   '+" "
                        ELSE
                                ?? '        '+" "
                        ENDIF
                        ?? DTOC(emissao)+" "
                        ?? DTOC(venc)+" "
                        ?? transform(valor,"@E 999,999.99")+" "
                        ?? transform(venc-mdata_sis,"9999")
                        IF EMPTY(dupr->datpag)
                                ?? ' Aberto'
                        ELSE
                                ?? ' Pago'
                        ENDIF
                        IF ver_serie() = '141259' .OR. ver_serie() = '141266' .OR. ver_serie() = '141267' .OR. ver_serie() = '141268'
                                ver_ped(dupr->num_ped)
                        ENDIF
                        tot=tot+1
                        tot_g=tot_g+1
                        tot_din=tot_din+valor
                        tot_gin=tot_gin+valor
                        mt_dia := mt_dia + valor
                        SKIP
/*
                        IF mfornec<>dupr->fornec .AND. ! EOF()
                                IF tot_din > 0
                                        ? "Total de Documentos: "+transform(tot,"9999")+" - "+ltrim(transform(tot_din,"@E 999,999,999,999.99"))
                                        ? REPLICATE("-",137)
                                        ? ' '
                                ENDIF
                                tot=0
                                tot_din=0
                                mfornec := dupr->fornec
                        ENDIF
                        IF mdia <> emissao
                                ? REPLICATE("*",137)
                                DEVPOS(PROW()+1,74);DEVOUT('              --------------')
                                DEVPOS(PROW()+1,59);DEVOUT('Total da Emissao '+DTOC(mdia)+':')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mt_dia,'99,999,999.99'))
                                ? REPLICATE("*",137)
                                mt_dia := 0
                                mdia := venc
                        ENDIF
*/
                ENDDO
                IF mdia <> emissao
*                       ? REPLICATE("*",137)
                        DEVPOS(PROW()+1,77);DEVOUT('              --------------')
                        DEVPOS(PROW()+1,62);DEVOUT('Total da Emissao '+DTOC(mdia)+':')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mt_dia,'99,999,999.99'))
                        ? REPLICATE("*",137)
                        mt_dia := 0
                        mdia := venc
                ENDIF
                IF tot_din > 0
                        ? "Total de Documentos: "+transform(tot,"9999")+" - "+ltrim(transform(tot_din,"@E 999,999,999,999.99"))
                        ? REPLICATE("-",137)
                ENDIF
                IF PROW() >= 57
                        EJECT
                        pag=pag+1
                        cabecalho(pag,mtit,mtipo)
                ENDIF
                ?
                imprt(cfg->imp_rel,'N',1)
                ?? "*** RESUMO FINAL ***"
                ?
                ? "Total de Documentos EMITIDOS no Periodo: "+transform(tot_g,"9999")+" - "+ltrim(transform(tot_gin,"@E 999,999,999,999.99"))
                ?
                ? time()
                EJECT
                SETPRC(00,00)
        ELSEIF mtipo_con = 'S' .OR. mtipo_con = 'T'
                IF mtipo_con = 'S'
                        mtit := mtit+' SINTETICO p/CLIENTE'
                        **************
                        SELE('dupr');ORDSETFOCUS(5)
                        GO TOP
                        **************
                ELSE
                        mtit := mtit+' SINTETICO p/TIPO DOC.'
                        **************
                        SELE('dupr');ORDSETFOCUS(1)
                        GO TOP
                        **************
                ENDIF
                IF ! imp_arq('DOC_EMIT.REL','R')
                        LOOP
                ENDIF
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
                l_mes=SUBSTR(DTOC(dupr->emissao),4,5)
                mt_dia := 0
                mdia := venc
                mtot_juros := 0
                mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
                WHILE ! EOF()
                        mtot_ind := 0
                        mvlr_ind := 0
                        msub_ind := 0
                        mjuros_ind := 0
                        IF mtipo_con = 'S'
                                mcod_aux := dupr->fornec
                                mfornec  := dupr->fornec
                        ELSE
                                mcod_aux := dupr->tipo
                                mfornec  := dupr->tipo
                        ENDIF
*                       WHILE mcod_aux = dupr->fornec .AND. ! EOF()
                        WHILE mcod_aux = mfornec .AND. ! EOF()
                                IF pag=0 .OR. PROW()>=59
                                        pag=pag+1
                                        IF pag>1
                                                EJECT
                                        ENDIF
                                        cabecalho(pag,mtit,mtipo)
                                        imprt(mtipo_imp,'C')
                                        IF mcod_vend > 0
                                                DEVPOS(PROW()+1,00);DEVOUT('Vendedor: '+STRZERO(mcod_vend,3)+' - '+mnome_vend)
                                                DEVPOS(PROW(),00);DEVOUT('          '+STRZERO(mcod_vend,3)+' - '+mnome_vend)
                                        ENDIF
                                        ? "Cod.  Cliente                                     No. Doc.       Tp.Pag.   Emissao   Vencim         Valor  Dias     Juros      Total"
                                        ? REPLICATE("-",137)
                                ENDIF
                                ***************** CONDICOES ************************
                                IF dupr->emissao > t_fim
                                        SKIP
                                        IF mtipo_con = 'S'
                                                mfornec := dupr->fornec
                                        ELSE
                                                mfornec := dupr->tipo
                                        ENDIF
                                        LOOP
                                ENDIF
                                IF dupr->emissao < t_ini
                                        SKIP
                                        IF mtipo_con = 'S'
                                                mfornec := dupr->fornec
                                        ELSE
                                                mfornec := dupr->tipo
                                        ENDIF
                                        LOOP
                                ENDIF
                                IF mtip <> 'GE'
                                        IF mtip <> dupr->tipo
                                                SKIP
                                                IF mtipo_con = 'S'
                                                        mfornec := dupr->fornec
                                                ELSE
                                                        mfornec := dupr->tipo
                                                ENDIF
                                                mdia := dupr->emissao
                                                LOOP
                                        ENDIF
                                ENDIF
/*
                                IF ! EMPTY(mcartao)
                                        IF STRZERO(mcartao,3) <> dupr->numero
                                                SKIP
                                                IF mtipo_con = 'S'
                                                        mfornec := dupr->fornec
                                                ELSE
                                                        mfornec := dupr->tipo
                                                ENDIF
                                                mdia := dupr->emissao
                                                LOOP
                                        ENDIF
                                ENDIF
*/
                                IF ! EMPTY(mcod_vend)
                                        IF STRZERO(mcod_vend,3) <> dupr->vendedor
                                                SKIP
                                                IF mtipo_con = 'S'
                                                        mfornec := dupr->fornec
                                                ELSE
                                                        mfornec := dupr->tipo
                                                ENDIF
                                                mdia := dupr->emissao
                                                LOOP
                                        ENDIF
                                ENDIF
                                IF tipo_sit <> 'G'
                                        IF tipo_sit = 'P'
                                                IF EMPTY(dupr->datpag)
                                                        SKIP
                                                        IF mtipo_con = 'S'
                                                                mfornec := dupr->fornec
                                                        ELSE
                                                                mfornec := dupr->tipo
                                                        ENDIF
                                                        mdia := dupr->emissao
                                                        LOOP
                                                ENDIF
                                        ELSE
                                                IF ! EMPTY(dupr->datpag)
                                                        SKIP
                                                        IF mtipo_con = 'S'
                                                                mfornec := dupr->fornec
                                                        ELSE
                                                                mfornec := dupr->tipo
                                                        ENDIF
                                                        mdia := dupr->emissao
                                                        LOOP
                                                ENDIF
                                        ENDIF
                                ENDIF
                                IF ! EMPTY(t_for) .AND. dupr->fornec<>STRZERO(t_for,5)
                                        SKIP
                                        IF mtipo_con = 'S'
                                                mfornec := dupr->fornec
                                        ELSE
                                                mfornec := dupr->tipo
                                        ENDIF
                                        mdia := dupr->emissao
                                        LOOP
                                ENDIF
                                IF ! EMPTY(bank) .AND. dupr->banco<>bank
                                        SKIP
                                        IF mtipo_con = 'S'
                                                mfornec := dupr->fornec
                                        ELSE
                                                mfornec := dupr->tipo
                                        ENDIF
                                        mdia := dupr->emissao
                                        LOOP
                                ENDIF
                                ***************** FIM CONDICOES ************************
                                ? dupr->fornec+" "
                                ?? dupr->cliente+" "
                                ?? dupr->numero+" "
                                ?? dupr->tipo+dupr->duplicata+dupr->conta+" "
                                IF banco = 'C'
                                        ?? 'CARTEIRA'+" "
                                ELSEIF banco = 'B'
                                        ?? 'BANCO   '+" "
                                ELSE
                                        ?? '        '+" "
                                ENDIF
                                ?? DTOC(dupr->emissao)+" "
                                ?? DTOC(dupr->venc)+" "
                                ?? transform(dupr->valor,"@E 999,999.99")+" "
                                ?? transform(dupr->venc-mdata_sis,"9999")+' '
                                msub_ind := msub_ind + dupr->valor
                                mtot_ind ++
                                tot_g ++
                                mmulta := 0
                                mjuros := 0
                                mdias_atras := mdata_sis-dupr->venc
                                IF mdias_atras > 0
                                        mmulta := dupr->valor * ((set->multa_dup)/100)
                                        mjuros := mjuros + (dupr->valor * ((mdias_atras*set->juros_dup)/100))
                                ENDIF
                                mtot_juros := mtot_juros + mmulta+mjuros
                                mjuros_ind := mjuros_ind + mmulta+mjuros
                                mvlr_ind := mvlr_ind + dupr->valor+mmulta+mjuros
                                tot_gin=tot_gin+dupr->valor
                                ?? transform(mmulta+mjuros,"999,999.99")+' '
                                ?? transform(dupr->valor+mmulta+mjuros,"999,999.99")
                                SKIP
                                IF mtipo_con = 'S'
                                        mfornec := dupr->fornec
                                ELSE
                                        mfornec := dupr->tipo
                                ENDIF
                        ENDDO
                        IF ! EMPTY(mtot_ind)
                                ? REPLICATE("-",137)
                                ? 'QUANTD: '+STRZERO(mtot_ind,6)+' - SUB-TOTAL:'+TRANSFORM(msub_ind,'999,999.99')+' - JURO+MULTA:'+TRANSFORM(mjuros_ind,'999,999.99')+' - TOTAL:'+TRANSFORM(mvlr_ind,'9,999,999.99')
                                ? REPLICATE("-",137)
                                mtot_ind := 0
                                mvlr_ind := 0
                                msub_ind := 0
                                mjuros_ind := 0
                        ENDIF
                ENDDO
                IF PROW() >= 55
                        EJECT
                        pag=pag+1
                        cabecalho(pag,mtit,mtipo)
                ENDIF
                IF ! EMPTY(mtot_ind)
                        ? REPLICATE("-",137)
                        ? 'QUANTD: '+STRZERO(mtot_ind,6)+' - SUB-TOTAL:'+TRANSFORM(msub_ind,'999,999.99')+' - JURO+MULTA:'+TRANSFORM(mjuros_ind,'999,999.99')+' - TOTAL:'+TRANSFORM(mvlr_ind,'9,999,999.99')
                        ? REPLICATE("-",137)
                        mtot_ind := 0
                        mvlr_ind := 0
                        msub_ind := 0
                        mjuros_ind := 0
                ENDIF
                ?
                imprt(mtipo_imp,'N',1)
                ?? "*** RESUMO FINAL ***"
                ?
                ? "Total de Documentos a RECEBER no periodo  Quantd.: "+transform(tot_g,"9999")+" - Sub-Total: "+transform(tot_gin,"@E 99,999,999.99")+' Juros+Multa: '+transform(mtot_juros,"@E 9,999,999.99")+' - Total: '+transform(tot_gin+mtot_juros,"@E 9,999,999.99")
                ?
                ? time()
                EJECT
                SETPRC(00,00)
        ENDIF
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(01,00,24,79,tela)
        IF mimp_tipo = 2
                lertexto('DOC_EMIT.REL')
        ENDIF
        CLOSE ALL
        RETURN
ENDDO
RETURN NIL
******************************* F I M *******************************



