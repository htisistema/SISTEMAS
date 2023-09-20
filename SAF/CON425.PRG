*********************************************************
**** CON425.PRG: Relatorio -> Carta de Cobranca p/Clientes
*********************************************************

FUNCTION con425
***************
LOCAL MPRG:='CON425'
LOCAL tela,tela1,t_ini,t_fim,mod,td_mes,tp_mes,pag,mcod_vend,;
      vencer,vencidas,t_vencer,t_vencidas,tot,tot_g,tot_din,tot_gin,l_mes,tt,;
      xxx,mtit,mtipo,mtip,mdia,mt_dia,opcao,mquantd,mcartao,mtipo_rel,;
      mcod_aux,mtot_ind,mvlr_ind,msub_ind,mjuros_ind,mtipo_con,mmulta,mjuros,;
      mdias_atras,mtot_juros,mfornec,mt_juros,mop_dia,mtip_pg:='  ',mend,mcab,;
      mcidade,m_dup:={}

PRIVATE mcobra_fin,mtaxa_fin
PRIVATE mcliente,mcgc,mcpf,t_for,bank,mnome_vend,mcom_ven,mcliente,mcgc,mcpf

PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'CONTA A RECEBER (CARTA COBRANCA P/CLIENTES)','156',nivel_acess)
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
*---------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('sacfin','fin');RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccarta','car');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
*---------------------------------------------------------------------
****************
SELE('cli');ORDSETFOCUS(1)
****************
botao(10,10,21,79,,' Doc. a RECEBER ')
@ 12,12 SAY "Codigo do Cliente........:"
@ 13,12 SAY "Codigo do Vendedor.......:"
@ 14,12 SAY "Pag.em [C]arteira [B]anco:"
@ 15,12 SAY "Data do Inicio...........:"
@ 16,12 SAY "Data do Final............:"
@ 17,12 SAY "Resumo do dia ou Geral...:"
@ 18,12 SAY "Totalizador do dia [S/n].:"
@ 19,12 SAY "Tipo de Operacao.........:"
@ 20,12 SAY "Digite o nome da Cidade..:"
mtipo_imp := cfg->imp_rel
WHILE .T.
        mtipo_rel := 'G'
        bank=SPACE(1)
        t_for = 0
        mcod_vend := 0
        mcliente := SPACE(30)
        t_ini=CTOD("  /  /  ")
        t_fim=mdata_sis
        mod := 0
        mcartao := 0
        mop_dia := 'S'
        mtip_pg := '  '
        mend := ' '
        mcab := ' '
        mcidade := SPACE(20)
        setcor(3)
        DEVPOS(11,39);DEVOUT('DOCUMENTOS A RECEBER')
        setcor(1)
        @ 12,39 GET t_for pict "99999" VALID lim_get() .AND. ver_cli(t_for,12,45) WHEN mcod_vend = 0 .AND. men_get(13,39,'Informe o cliente que deseja ou nao informe p/imprimir todos')
        @ 13,39 GET mcod_vend pict "999" VALID ven(mcod_vend,17,43) .AND. lim_get() WHEN t_for = 0 .AND. men_get(14,39,'Informe o Vendedor que deseja ou nao informe p/imprimir todos')
        @ 14,39 GET bank pict "@!" VALID bank $ 'C,B, '
        @ 15,39 GET t_ini VALID lim_get() WHEN men_get(16,39,'Informe a data inicial do periodo que deseja imprimir')
        @ 16,39 GET t_fim VALID lim_get() WHEN men_get(17,39,'Informe a data final do periodo que deseja imprimir')
        @ 17,39 GET mtipo_rel PICT '@!' VALID lim_get() .AND. mtipo_rel $ 'R,G'  WHEN men_get(18,39,'Informe o [R] para resumo dos dias [G] para todas as duplicatas')
        @ 18,39 GET mop_dia PICT '@!' VALID lim_get() .AND. mop_dia $ 'S,N'  WHEN men_get(19,39,'Deseja que os dias seja TOTALIZADO=[S] ou [N]p/nao TOTALIZAR')
        @ 19,39 GET mtip_pg PICT '@!' VALID lim_get() WHEN men_get(20,39,'Informe o tipo de Operacao')
        @ 20,39 GET mcidade PICT '@!' VALID lim_get() WHEN men_get(21,39,'Informe a CIDADE que deseja ou deixe em BRANCO p/todos')
        READ
        IF LASTKEY()=27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN
        ENDIF
        RESTSCREEN(01,00,24,79,tela)
        quadro("Opcoes: [1]Duplicatas [2]Cheques [3]Cartoes [4]Financiamento [5]Geral")
        WHILE .T.
                INKEY(0)
                IF LASTKEY()=27
                        RESTSCREEN(01,00,24,79,tela)
                        CLOSE ALL
                        RETURN
                ENDIF
                IF LASTKEY()=ASC("1")
                        mtip := 'DU'
                        mtit := "DUPLICATAS A RECEBER (Contas a Receber)"
                        EXIT
                ENDIF
                IF LASTKEY()=ASC("2")
                        mtit := "CHEQUES A RECEBER (Contas a Receber)"
                        mtip := 'CH'
                        EXIT
                ENDIF
                IF LASTKEY()=ASC("3")
                        mtip := 'CT'
                        mtit := "CARTOES A RECEBER (Contas a Receber)"
                        mcartao := 0
                        tela1 := SAVESCREEN(01,00,24,79)
                        botao(19,10,21,75)
                        janela(19,51," RECEBER ")
                        @ 20,12 SAY "Codigo do Cartao.........:"
                        @ 20,39 GET mcartao PICT '999' VALID ver_cartao(mcartao,20,42) .AND. lim_get() WHEN men_get(21,39,'Informe o codigo do cartao ou deixe em branco para todos')
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mensagem('Confirma o Cartao [S/n]:')
                        opcao := 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        EXIT
                ENDIF
                IF LASTKEY()=ASC("4")
                        mtit := "FINANCIAMENTO A RECEBER (Contas a Receber)"
                        mtip := 'FI'
                        mcartao := 0
                        tela1 := SAVESCREEN(01,00,24,79)
                        botao(19,10,21,75)
                        janela(19,51," RECEBER ")
                        @ 20,12 SAY "Codigo da Financeira.....:"
                        @ 20,39 GET mcartao PICT '999' VALID IF(EMPTY(mcartao),.T.,ver_finan(mcartao,20,42)) .AND. lim_get() WHEN men_get(21,39,'Informe o codigo da Financeira ou deixe em branco para todos')
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mensagem('Confirma a Financeira [S/n]:')
                        opcao := 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        EXIT
                ENDIF
                IF LASTKEY()=ASC("5")
                        mtit := "DOCUMENTOS A RECEBER (Contas a Receber)"
                        mtip := 'GE'
                        EXIT
                ENDIF
        ENDDO
        RESTSCREEN(01,00,24,79,tela)
        td_mes=0    && tot.dup no mes
        tp_mes=0    && tot. pagar no mes
        IF t_ini = CTOD("  /  /  ")
                t_ini=CTOD("01/01/60")
        ENDIF
        IF t_fim = CTOD('  /  /  ')
                t_fim=CTOD("31/12/99")
        ENDIF
        IF ! EMPTY(mcidade)
                mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)+' da Cidade:'+mcidade
        ELSE
                mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
        ENDIF
        **************
        SELE('dupr');ORDSETFOCUS(6)
        GO TOP
        **************
        IF ! imp_arq('CARTA_COB.REL','R')
                LOOP
        ENDIF
*       SET PRINT ON
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
        l_mes=SUBSTR(DTOC(venc),4)
        mt_dia := 0
        mdia := venc
        mtot_juros := 0
        WHILE ! EOF()
                ASIZE(m_dup,0)
                mtot_ind := 0
                mvlr_ind := 0
                msub_ind := 0
                mjuros_ind := 0
                mcod_aux := dupr->fornec
                mfornec  := dupr->fornec
*               mcab := ' '
*               WHILE mcod_aux = mfornec .AND. ! EOF()

                WHILE mcod_aux = dupr->fornec .AND. ! EOF()
                        ***************** CONDICOES ************************
                        IF ! EMPTY(mcidade)
                                IF ver_cli(VAL(dupr->fornec),,,'*')
                                        IF cli->cidade <> mcidade
                                                SKIP
                                                LOOP
                                        ENDIF
                                ELSE
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF dupr->venc > t_fim
                                SKIP
                                IF mtipo_con = 'S'
                                        mfornec := dupr->fornec
                                ELSE
                                        mfornec := dupr->tipo
                                ENDIF
                                LOOP
                        ENDIF
                        IF dupr->venc < t_ini
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
                                        mdia := dupr->venc
                                        LOOP
                                ENDIF
                        ENDIF
                        IF ! EMPTY(mcartao)
                                IF STRZERO(mcartao,3) <> dupr->numero
                                        SKIP
                                        IF mtipo_con = 'S'
                                                mfornec := dupr->fornec
                                        ELSE
                                                mfornec := dupr->tipo
                                        ENDIF
                                        mdia := dupr->venc
                                        LOOP
                                ENDIF
                        ENDIF
                        IF ! EMPTY(dupr->pago) .OR. ! EMPTY(dupr->datpag)
                                SKIP
                                IF mtipo_con = 'S'
                                        mfornec := dupr->fornec
                                ELSE
                                        mfornec := dupr->tipo
                                ENDIF
                                mdia := dupr->venc
                                LOOP
                        ENDIF
                        IF ! EMPTY(mcod_vend)
                                IF STRZERO(mcod_vend,3) <> dupr->vendedor
                                        SKIP
                                        IF mtipo_con = 'S'
                                                mfornec := dupr->fornec
                                        ELSE
                                                mfornec := dupr->tipo
                                        ENDIF
                                        mdia := dupr->venc
                                        LOOP
                                ENDIF
                        ENDIF
                        IF ! EMPTY(t_for)
                                IF dupr->fornec<>STRZERO(t_for,5)
                                        SKIP
                                        IF mtipo_con = 'S'
                                                mfornec := dupr->fornec
                                        ELSE
                                                mfornec := dupr->tipo
                                        ENDIF
                                        mdia := dupr->venc
                                        LOOP
                                ENDIF
                        ENDIF
                        IF ! EMPTY(mtip_pg) .AND. mtip_pg <> dupr->tip_pg
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(bank)
                                IF dupr->banco<>bank
                                        SKIP
                                        IF mtipo_con = 'S'
                                                mfornec := dupr->fornec
                                        ELSE
                                                mfornec := dupr->tipo
                                        ENDIF
                                        mdia := dupr->venc
                                        LOOP
                                ENDIF
                        ENDIF
                        ***************** FIM CONDICOES ************************
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

                        AADD(m_dup,{dupr->fornec+'-'+dupr->cliente,dupr->tipo+dupr->duplicata+dupr->conta,DTOC(dupr->emissao),DTOC(dupr->venc),dupr->valor+mmulta+mjuros})

                        SKIP
                ENDDO
                IF LEN(m_dup) > 0
                        imprt(mtipo_imp,'N')
                        imprt(mtipo_imp,'E')
                        DEVPOS(PROW(),00);DEVOUT(memp_resa)
                        imprt(mtipo_imp,'N')
                        DEVPOS(PROW()+1,00);DEVOUT(mcgc_firm+' - '+minsc_firm)
                        DEVPOS(PROW()+1,00);DEVOUT(mend_firm)
                        DEVPOS(PROW()+1,00);DEVOUT(mcid_firm+' - '+mfone_firm)
                        DEVPOS(PROW()+1,00);DEVOUT(REPLICATE("-",80))
                        DEVPOS(PROW()+2,00);DEVOUT(PADL(mcid_firm+', '+DTOC(DATE()),80))
                        DEVPOS(PROW()+2,00);DEVOUT('COMUNICADO')
                        DEVPOS(PROW()+2,00);DEVOUT('Ao Sr.(a)')
                        DEVPOS(PROW()+1,00);DEVOUT(m_dup[1,1])
                        DEVPOS(PROW()+2,00);DEVOUT('             Em nossos arquivos verificamos que se encontra em aberto a(s)')
                        DEVPOS(PROW()+1,00);DEVOUT('duplicata(s) abaixo relacionada(s), favor entrar em contato  com  o  nosso')
                        DEVPOS(PROW()+1,00);DEVOUT('departamento financeiro para quitacao  da(s) mesma(s), pois a nao quitacao')
                        DEVPOS(PROW()+1,00);DEVOUT('implicara no bloqueio do seu credito.                                     ')
                        DEVPOS(PROW()+2,00);DEVOUT('Documento       Emissao    Vencimento      Valor R$')
                        DEVPOS(PROW()+1,00);DEVOUT('---------       -------    ----------  ------------')
                        FOR i = 1 TO LEN(m_dup)
                                DEVPOS(PROW()+1,00);DEVOUT(m_dup[i,2])
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(m_dup[i,3])
                                DEVPOS(PROW(),PCOL()+6);DEVOUT(m_dup[i,4])
                                DEVPOS(PROW(),PCOL()+4);DEVOUT(TRANSFORM(m_dup[i,5],'999,999.99'))
                        NEXT
                        DEVPOS(PROW()+1,00);DEVOUT('                                       ------------')
                        DEVPOS(PROW()+1,00);DEVOUT('Quantidade: '+STRZERO(mtot_ind,6)+'  -   Total a Pagar: '+TRANSFORM(mvlr_ind,'9,999,999.99'))
                        DEVPOS(PROW()+3,00);DEVOUT('             Porem se a(s)  mesma(s)  estiver(em)  paga(s),  desconsiderar')
                        DEVPOS(PROW()+1,00);DEVOUT('comunicado.')
                        DEVPOS(PROW()+3,00);DEVOUT('Sem mais momento agradece,')
                        DEVPOS(PROW()+5,00);DEVOUT(REPLI('-',LEN(ALLTRIM(memp_resa))))
                        DEVPOS(PROW()+1,00);DEVOUT(ALLTRIM(memp_resa))
                        mtot_ind := 0
                        mvlr_ind := 0
                        msub_ind := 0
                        mjuros_ind := 0
                        EJECT
                ENDIF
        ENDDO
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(01,00,24,79,tela)
        IF mimp_tipo = 2
                lertexto('CARTA_COB.REL')
        ENDIF
        CLOSE ALL
        RETURN NIL
ENDDO
RETURN NIL
******************************* F I M *******************************

