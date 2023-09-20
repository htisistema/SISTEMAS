   **** CON419.PRG: Relatorio -> Duplicatas Vencer/Vencidas
***  CONTAS A PAGAR X CONTAS A RECEBER
*********************************************************

FUNCTION con419
***************
LOCAL MPRG:='CON419'
LOCAL tela,t_ini,t_fim,mtit,mtipo,opcao,mbank,;
      mcod_cli,mcod_vend,m_doc:={},mtot_rec,mtot_apa,msaldo,mdata_aux,mtipo_rel,;
      msaldo_ini

PRIVATE mcod_for,mtipo_conta
PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DOCUMENTOS A PAGAR X A RECEBER','15',nivel_acess)
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)

*------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('sacforn','forn');RETURN NIL;ENDIF
IF ! AbriArq('sacdupp','dupp');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
*------------------------------------------------------------------
******************
SELE('forn');ORDSETFOCUS(1)
******************
botao(8, 07, 22, 78)
DEVPOS(09,09);DEVOUT("Data Inicio...........:")
DEVPOS(10,09);DEVOUT("Data Final............:")
@ 11,08 TO 11,77
janela(11,78,' CONTAS APAGAR ','*')
DEVPOS(12,09);DEVOUT("Codigo Fornecedor.....:")
DEVPOS(13,09);DEVOUT("Tipo Doc..............:")
DEVPOS(14,09);DEVOUT('Tipo Conta............:')
@ 15,08 TO 15,77
janela(15,78,' CONTAS A RECEBER ','*')
DEVPOS(16,09);DEVOUT("Codigo do Cliente.....:")
DEVPOS(17,09);DEVOUT("Codigo do Vendedor....:")
DEVPOS(18,09);DEVOUT("Pag.em [C]art. [B]anco:")
@ 19,08 TO 19,77
DEVPOS(20,09);DEVOUT('[V]encer/Vencida [P]agas ou [E]missao:')
DEVPOS(21,09);DEVOUT('Saldo Inicial R$........:')
WHILE .T.
********* VARIAVEIS DE IMPRESSAO *******************
        mtipo_imp := cfg->imp_rel
****************************************************
        t_ini=DATE()
        t_fim=DATE()+30
        mcod_for=0
        mtipo_doc := SPACE(2)
        mtipo_conta := SPACE(4)
        mtipo_rel := SPACE(1)
        mcod_cli := 0
        mcod_vend := 0
        bank=SPACE(1)
        msaldo_ini := 0
        mtot_rec := 0
        mtot_apa := 0

        @ 09,33 GET t_ini VALID lim_get() WHEN men_get(10,35,'Informe a data inicial do periodo que deseja imprimir')
        @ 10,33 GET t_fim VALID lim_get() WHEN men_get(11,35,'Informe a data final do periodo que deseja imprimir')
        @ 12,33 GET mcod_for PICT '9999' VALID v_fornece(mcod_for,12,37)
        @ 13,33 GET mtipo_doc PICT '@!' VALID lim_get() WHEN men_get(14,35,'Informe o tipo de DOCUMENTO que deseja <PA>pagamento <DU>duplicata <CH>cheques <FR>fretes ou NAO informe nenhum tipo de DOCUMENTO')
        @ 14,33 GET mtipo_conta PICT '@! X.XX' VALID verdesp(mtipo_conta,14,38,1) .AND. lim_get() WHEN EMPTY(mcod_for) .AND. men_get(15,38,'Identifique o Grupo da Conta preenchendo so o primeiro digito da conta, ou preenchendo todo os espaco p/o tipo de conta <F9>p/Consultar ou deixando em BRANCO p/TODOS')

        @ 16,33 GET mcod_cli PICT "99999" VALID lim_get() .AND. ver_cli(mcod_cli,16,39) WHEN mcod_vend = 0 .AND. men_get(17,39,'Informe o cliente que deseja ou nao informe p/imprimir todos')
        @ 17,33 GET mcod_vend PICT "999" VALID ven(mcod_vend,17,37) .AND. lim_get() WHEN mcod_cli = 0 .AND. men_get(18,39,'Informe o Vendedor que deseja ou nao informe p/imprimir todos')
        @ 18,33 GET bank PICT "@!" VALID bank $ 'C,B, '
        @ 20,48 GET mtipo_rel PICT "@!" VALID mtipo_rel $ 'V,P,E'
        @ 21,33 GET msaldo_ini PICT "99,999,999.99"
        READ
        IF LASTKEY()=27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN
        ENDIF
        RESTSCREEN(01,00,24,79,tela)
        IF t_ini=CTOD("  /  /  ")
                t_ini=CTOD("11/11/11")
        ENDIF
        IF t_fim=CTOD("  /  /  ")
                t_fim=CTOD("31/12/99")
        ENDIF
        ASIZE(m_doc,0)
*        mdata_aux := t_ini - 1
        AADD(m_doc,{t_ini,0,0})
        i := 0
        FOR i = 1 TO t_fim - t_ini
                AADD(m_doc,{t_ini+i,0,0})
        NEXT
        i := 0
        FOR i = 1 TO LEN(m_doc)
                IF mtipo_rel = 'V'
                        mtit := 'RELATORIO DE CONTAS A RECEBER X APAGAR (Vencimento)'
                        *****************
                        SELE('dupp');ORDSETFOCUS(4)
                        GO TOP
                        *****************
                        IF dupp->(DBSEEK(DTOS(m_doc[i,1])))
                                WHILE m_doc[i,1] = dupp->venc .AND. ! EOF()
                                        IF ! EMPTY(dupp->pago)
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> dupp->tipo
                                                SKIP
                                                LOOP
                                        ENDIF
                                        *&*
                                        IF ! EMPTY(SUBSTR(mtipo_conta,1,1))
                                                IF SUBSTR(mtipo_conta,1,1) <> SUBSTR(tip_for,1,1)
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF ! EMPTY(SUBSTR(mtipo_conta,3)) .AND. SUBSTR(mtipo_conta,3) <> SUBSTR(tip_for,3)
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDIF
                                        IF ! EMPTY(mcod_for) .AND. dupp->fornec<>STRZERO(mcod_for,4)
                                                SKIP
                                                mdia := dupp->venc
                                                LOOP
                                        ENDIF
                                        m_doc[i,2] := m_doc[i,2] + dupp->valor
                                        SKIP
                                ENDDO
                        ENDIF
                        *****************
                        SELE('dupr');ORDSETFOCUS(4)
                        GO TOP
                        *****************
                        IF dupr->(DBSEEK(DTOS(m_doc[i,1])))
                                WHILE m_doc[i,1] = dupr->venc .AND. ! EOF()
                                        IF ! EMPTY(dupr->pago)
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mcod_vend) .AND. STRZERO(mcod_vend,3) <> dupr->vendedor
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mcod_cli) .AND. dupr->fornec<>STRZERO(mcod_cli,5)
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(bank) .AND. dupr->banco<>bank
                                                SKIP
                                                LOOP
                                        ENDIF
                                        m_doc[i,3] := m_doc[i,3] + dupr->valor
                                        SKIP
                                ENDDO
                        ENDIF
                ELSEIF mtipo_rel = 'P'
                        mtit := 'RELATORIO DE CONTAS RECEBIDAS X PAGAS (Pagamentos)'
                        *****************
                        SELE('dupp');ORDSETFOCUS(3)
                        GO TOP
                        *****************
                        IF dupp->(DBSEEK(DTOS(m_doc[i,1])))
                                WHILE m_doc[i,1] = dupp->datpag .AND. ! EOF()
                                        IF EMPTY(dupp->pago)
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> dupp->tipo
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(SUBSTR(mtipo_conta,1,1))
                                                IF SUBSTR(mtipo_conta,1,1) <> SUBSTR(tip_for,1,1)
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF ! EMPTY(SUBSTR(mtipo_conta,3)) .AND. SUBSTR(mtipo_conta,3) <> SUBSTR(tip_for,3)
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDIF
                                        *&*
                                        IF ! EMPTY(mcod_for) .AND. dupp->fornec<>STRZERO(mcod_for,4)
                                                SKIP
                                                mdia := dupp->datpag
                                                LOOP
                                        ENDIF
                                        m_doc[i,2] := m_doc[i,2] + dupp->valor
                                        SKIP
                                ENDDO
                        ENDIF
                        *****************
                        SELE('dupr');ORDSETFOCUS(3)
                        GO TOP
                        *****************
                        IF dupr->(DBSEEK(DTOS(m_doc[i,1])))
                                WHILE m_doc[i,1] = dupr->datpag .AND. ! EOF()
                                        IF EMPTY(dupr->pago)
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mcod_vend) .AND. STRZERO(mcod_vend,3) <> dupr->vendedor
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mcod_cli) .AND. dupr->fornec<>STRZERO(mcod_cli,5)
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(bank) .AND. dupr->banco<>bank
                                                SKIP
                                                LOOP
                                        ENDIF
                                        m_doc[i,3] := m_doc[i,3] + dupr->valor
                                        SKIP
                                ENDDO
                        ENDIF
                ELSE
                        mtit := 'RELATORIO DE CONTAS RECEBIDAS X PAGAS (Emissao)'
                        *****************
                        SELE('dupp');ORDSETFOCUS(5)
                        GO TOP
                        *****************
                        IF dupp->(DBSEEK(DTOS(m_doc[i,1])))
                                WHILE m_doc[i,1] = dupp->emissao .AND. ! EOF()
                                        IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> dupp->tipo
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(SUBSTR(mtipo_conta,1,1))
                                                IF SUBSTR(mtipo_conta,1,1) <> SUBSTR(tip_for,1,1)
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                                IF ! EMPTY(SUBSTR(mtipo_conta,3)) .AND. SUBSTR(mtipo_conta,3) <> SUBSTR(tip_for,3)
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDIF
                                        *&*
                                        IF ! EMPTY(mcod_for) .AND. dupp->fornec<>STRZERO(mcod_for,4)
                                                SKIP
                                                mdia := dupp->datpag
                                                LOOP
                                        ENDIF
                                        m_doc[i,2] := m_doc[i,2] + dupp->valor
                                        SKIP
                                ENDDO
                        ENDIF
                        *****************
                        SELE('dupr');ORDSETFOCUS(5)
                        GO TOP
                        *****************
                        IF dupr->(DBSEEK(DTOS(m_doc[i,1])))
                                WHILE m_doc[i,1] = dupr->emissao .AND. ! EOF()
                                        IF ! EMPTY(mcod_vend) .AND. STRZERO(mcod_vend,3) <> dupr->vendedor
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mcod_cli) .AND. dupr->fornec<>STRZERO(mcod_cli,5)
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(bank) .AND. dupr->banco<>bank
                                                SKIP
                                                LOOP
                                        ENDIF
                                        m_doc[i,3] := m_doc[i,3] + dupr->valor
                                        SKIP
                                ENDDO
                        ENDIF
                ENDIF
        NEXT
        RESTSCREEN(01,00,24,79,tela)
        IF ! imp_arq('APAGXREC.REL','R')
                LOOP
        ENDIF
        mtipo := DTOC(t_ini)+' a '+DTOC(t_fim)
        pag=1
        i := 0
        msaldo := msaldo_ini
        cabecalho(pag,mtit,mtipo)
        imprt(mtipo_imp,'N')
        DEVPOS(PROW()+1,00);DEVOUT("  Data     Contas A Receber   Contas Apagar    Diferenca R$       Saldo R$")
        DEVPOS(PROW()+1,00);DEVOUT(REPLICATE("=",80))
        DEVPOS(PROW()+1,00);DEVOUT('SALDO INICIAL INFORMADO PELO OPERADOR')
        DEVPOS(PROW(),61);DEVOUT(TRANSFORM(msaldo,'99,999,999.99'))
        FOR i = 1 TO LEN(m_doc)
                IF pag=0 .OR. PROW()>=57
                        pag=pag+1
                        IF pag>1
                                EJECT
                        ENDIF
                        cabecalho(pag,mtit,mtipo)
                        imprt(mtipo_imp,'N')
                        DEVPOS(PROW()+1,00);DEVOUT("  Data     Contas A Receber   Contas Apagar    Diferenca R$       Saldo R$")
                        DEVPOS(PROW()+1,00);DEVOUT(REPLICATE("=",80))
                ENDIF
                IF EMPTY(m_doc[i,3]) .AND. EMPTY(m_doc[i,2])
                        LOOP
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_doc[i,1])
                DEVPOS(PROW(),14);DEVOUT(TRANSFORM(m_doc[i,3],'99,999,999.99'))
                DEVPOS(PROW(),30);DEVOUT(TRANSFORM(m_doc[i,2],'99,999,999.99'))
                DEVPOS(PROW(),46);DEVOUT(TRANSFORM(m_doc[i,3] - m_doc[i,2],'99,999,999.99'))
                DEVPOS(PROW(),61);DEVOUT(TRANSFORM(msaldo + (m_doc[i,3] - m_doc[i,2]),'99,999,999.99'))
                msaldo := msaldo + (m_doc[i,3] - m_doc[i,2])
                mtot_rec := mtot_rec + m_doc[i,3]
                mtot_apa := mtot_apa + m_doc[i,2]
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT("*** RESUMO FINAL ***")
        IF mtipo_rel = 'V'
                DEVPOS(PROW()+2,00);DEVOUT("Total de Documentos A RECEBER Periodo: "+TRANSFORM(mtot_rec,"@E 99,999,999,999.99"))
                DEVPOS(PROW()+1,00);DEVOUT("Total de Documentos APAGAR no Periodo: "+TRANSFORM(mtot_apa,"@E 99,999,999,999.99"))
        ELSE
                DEVPOS(PROW()+2,00);DEVOUT("Total de Documentos RECEBIDAS Periodo: "+TRANSFORM(mtot_rec,"@E 99,999,999,999.99"))
                DEVPOS(PROW()+1,00);DEVOUT("Total de Documentos PAGAS no Periodo : "+TRANSFORM(mtot_apa,"@E 99,999,999,999.99"))
        ENDIF
        DEVPOS(PROW()+1,00);DEVOUT("                                     : -----------------")
        DEVPOS(PROW()+1,00);DEVOUT("Total de Diferenca no Periodo........: "+TRANSFORM(mtot_rec - mtot_apa,"@E 99,999,999,999.99"))
        DEVPOS(PROW()+2,00);DEVOUT(time())
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(01,00,24,79,tela)
        IF mimp_tipo = 2
                lertexto('APAGXREC.REL')
        ENDIF
        RETURN
ENDDO
RETURN NIL
*************************** F I M ***************************
