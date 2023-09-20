*** CON429.PRG: EMISSAO DE FATURAMENTO (Receber)
***
**************************************************

FUNCTION con429()
***************
LOCAL MPRG:='CON429'
LOCAL mtela,tela1,mtipo,lci,cci,lba,cba,mtraco,mdata1,mdata2,mtipo_doc,mcod_cli,;
      mtot_dup,msub_tot,mtipo_dup,mcartao,mflag,mcod_aux,mvenc,m_dup:={},i

PRIVATE mcobra_fin,mtaxa_fin
PRIVATE mcliente,mcgc,mcpf,mtipo_imp

IF ! ver_nivel(mprg,'CONTA A RECEBER (EMISSAO DE FATURAS P/CLIENTES)','1345',nivel_acess)
        RETURN NIL
ENDIF
mtraco := REPLI('-',80)

mtela := SAVESCREEN(00,00,24,79)
lci := 02
cci := 00
lba := MAXROW()-2
cba := 79
*---------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('sacfin','fin');RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccarta','car');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
*---------------------------------------------------------------------
mtipo_imp := cfg->imp_rel

CLEAR GETS
msele := SELE()
morder := INDEXORD()
**************
SELE('dupr');ORDSETFOCUS(4)
***************
WHILE .T.
        mcod_cli := 0
        mtipo_dup := ' '
        msub_tot := 0
        mtot_dup := 0
        mcliente := SPACE(30)
        mcpf := SPACE(11)
        mcgc := SPACE(14)
        mtipo_doc := SPACE(2)
        mcartao := 0
        mdata1 := CTOD('  /  /  ')
        mdata2 := CTOD('  /  /  ')
        mvenc  := CTOD('  /  /  ')
        mensagem('Digite o periodo que deseja consultar - <ESC> p/retornar')
        botao(14,15,20,77)
        DEVPOS(15,17);DEVOUT('Cliente.....:')
        DEVPOS(16,17);DEVOUT('Data Inicial:')
        DEVPOS(17,17);DEVOUT('Data Final..:')
        DEVPOS(18,17);DEVOUT('Vencimento..:')
        DEVPOS(19,17);DEVOUT('[CH]Cheques [DU]Duplic. [CT]Cartao [FI]Financeira:')
        @ 15,31 GET mcod_cli PICT '99999' VALID ver_cli(mcod_cli,15,36)
        @ 16,31 GET mdata1 VALID IF(EMPTY(mdata1),.F.,.T.)
        @ 17,31 GET mdata2 VALID IF(mdata2 < mdata1,.F.,.T.)
        @ 18,31 GET mvenc VALID IF(mvenc < mdata1,.F.,.T.)
        @ 19,68 GET mtipo_doc PICT '@!' VALID mtipo_doc $ 'CH,DU,CT,FI,  '
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,mtela)
                RETURN NIL
        ENDIF
        IF mtipo_doc = 'CT'
                mcartao := 0
                tela1 := SAVESCREEN(01,00,24,79)
                botao(19,10,21,75)
                janela(19,51," Vencer/Vencidas ")
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
                RESTSCREEN(01,00,24,79,tela1)
        ENDIF
        IF mtipo_doc = 'FI'
                mcartao := 0
                tela1 := SAVESCREEN(01,00,24,79)
                botao(19,10,21,75)
                janela(19,51," Vencer/Vencidas ")
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
                RESTSCREEN(01,00,24,79,tela1)
        ENDIF
        **************
        SELE('dupr');ORDSETFOCUS(6)
        GO TOP
        ***************
        IF ! EMPTY(mcod_cli)
                IF ! dupr->(DBSEEK(STRZERO(mcod_cli,5)))
                        atencao('Nao Existe Movimento deste Cliente')
                        LOOP
                ENDIF
        ENDIF
        IF ! imp_arq('FATURA.REL','R')
                LOOP
        ENDIF

        WHILE ! EOF()
                ASIZE(m_dup,0)
                mcod_aux := dupr->fornec
                WHILE mcod_aux = dupr->fornec .AND. ! EOF()
                        IF dupr->pago <> ' '
                                SKIP
                                LOOP
                        ENDIF
                        IF dupr->venc > mdata2
                                SKIP
                                LOOP
                        ENDIF
                        IF dupr->venc < mdata1
                                SKIP
                                LOOP
                        ENDIF

                        IF ! EMPTY(mcod_cli) .AND. STRZERO(mcod_cli,5) <> dupr->fornec
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(mtipo_doc) .AND. mtipo_doc <> dupr->tipo
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(mcartao) .AND. STRZERO(mcartao,3) <> dupr->numero
                                SKIP
                                LOOP
                        ENDIF
//                                      1               2          3           4               5           6
                        AADD(m_dup,{dupr->fornec,dupr->cliente,dupr->tipo,dupr->duplicata,dupr->valor,dupr->emissao})
                        SKIP
                ENDDO
                IF EMPTY(m_dup)
                        LOOP
                ENDIF
                mtot_ind := 0
                mvlr_ind := 0
                msub_ind := 0
                mjuros_ind := 0
                mflag := 0
                imprt(mtipo_imp,'E')
                DEVPOS(PROW(),00);DEVOUT(memp_resa)
                imprt(mtipo_imp,'C',1)
                DEVPOS(PROW(),00);DEVOUT(mend_firm+'-'+mcid_firm+  +'Fone: '+mfone_firm)
                DEVPOS(PROW()+1,00);DEVOUT('C.G.C.: '+mcgc_firm+'  -  Inscricao Est.: '+minsc_firm)
                imprt(mtipo_imp,'N',1)
                DEVPOS(PROW(),00);DEVOUT(mtraco)
                DEVPOS(PROW()+1,00);DEVOUT('Cliente: '+m_dup[1,1]+' - '+m_dup[1,2])
                DEVPOS(PROW()+1,00);DEVOUT('Emissao: '+DTOC(mdata_sis)+'  -  Vencimento: '+DTOC(mvenc))
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                imprt(mtipo_imp,'C',1)
                DEVPOS(PROW(),00);DEVOUT('Documento         Valor  Emissao | Documento         Valor  Emissao | Documento         Valor  Emissao | Documento         Valor  Emissao ')
                imprt(mtipo_imp,'N',1)
                DEVPOS(PROW(),00);DEVOUT(mtraco)
                i := 0
                FOR i = 1 TO LEN(m_dup)
                        imprt(mtipo_imp,'C')
                        IF mflag = 0
                                DEVPOS(PROW()+1,00);DEVOUT(m_dup[i,3]+m_dup[i,4]+' '+TRANSFORM(m_dup[i,5],'99,999.99')+' '+DTOC(m_dup[i,6]))
                                mflag := 1
                        ELSEIF mflag = 1
                                DEVPOS(PROW(),34);DEVOUT('|'+m_dup[i,3]+m_dup[i,4]+' '+TRANSFORM(m_dup[i,5],'99,999.99')+' '+DTOC(m_dup[i,6]))
*                                DEVPOS(PROW(),34);DEVOUT('|'+dupr->tipo+dupr->duplicata+' '+TRANSFORM(dupr->valor,'99,999.99')+' '+DTOC(dupr->emissao))
                                mflag := 2
                        ELSEIF mflag = 2
                                DEVPOS(PROW(),69);DEVOUT('|'+m_dup[i,3]+m_dup[i,4]+' '+TRANSFORM(m_dup[i,5],'99,999.99')+' '+DTOC(m_dup[i,6]))
*                               DEVPOS(PROW(),69);DEVOUT('|'+dupr->tipo+dupr->duplicata+' '+TRANSFORM(dupr->valor,'99,999.99')+' '+DTOC(dupr->emissao))
                                mflag := 3
                        ELSEIF mflag = 3
                                DEVPOS(PROW(),104);DEVOUT('|'+m_dup[i,3]+m_dup[i,4]+' '+TRANSFORM(m_dup[i,5],'99,999.99')+' '+DTOC(m_dup[i,6]))
*                               DEVPOS(PROW(),104);DEVOUT('|'+dupr->tipo+dupr->duplicata+' '+TRANSFORM(dupr->valor,'99,999.99')+' '+DTOC(dupr->emissao))
                                mflag := 0
                        ENDIF
                        msub_tot := msub_tot + m_dup[i,5]
                        mtot_dup ++
                        IF PROW() >= 28 .AND. mcod_aux = m_dup[i,1]
                                imprt(mtipo_imp,'N',1)
                                DEVPOS(PROW(),00);DEVOUT(mtraco)
                                DEVPOS(PROW()+1,00);DEVOUT(PADC('Continuacao',80))
                                EJECT
                                imprt(mtipo_imp,'E')
                                DEVPOS(PROW(),00);DEVOUT(memp_resa)
                                imprt(mtipo_imp,'C',1)
                                DEVPOS(PROW(),00);DEVOUT(mend_firm+'-'+mcid_firm+  +'Fone: '+mfone_firm)
                                DEVPOS(PROW()+1,00);DEVOUT('C.G.C.: '+mcgc_firm+'  -  Inscricao Est.: '+minsc_firm)
                                imprt(mtipo_imp,'N',1)
                                DEVPOS(PROW(),00);DEVOUT(mtraco)
                                DEVPOS(PROW()+1,00);DEVOUT('Cliente: '+m_dup[i,1]+' - '+m_dup[i,2])
                                DEVPOS(PROW()+1,00);DEVOUT('Emissao: '+DTOC(mdata_sis)+'  -  Vencimento: '+DTOC(mvenc))
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                                imprt(mtipo_imp,'C',1)
                                DEVPOS(PROW(),00);DEVOUT('Documento       Valor     Emissao | Documento       Valor     Emissao | Documento       Valor     Emissao')
                                imprt(mtipo_imp,'N',1)
                                DEVPOS(PROW(),00);DEVOUT(mtraco)
                        ENDIF
                NEXT
                imprt(mtipo_imp,'C',2)
                DEVPOS(PROW(),00);DEVOUT('Qtd.Documento: '+TRANSFORM(mtot_dup,'99999'))
                DEVPOS(PROW(),PCOL()+2);DEVOUT('Total Valor: '+TRANSFORM(msub_tot,'99,999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Reconhce(emos) a exatidao desta FATURA de prestacao de servicos na importancia acima que pagarei(emos) a '+memp_resa+'.')
                DEVPOS(PROW()+2,00);DEVOUT(PADC('Data: ......../......../........         Ass.: ......................................',130))
                imprt(mtipo_imp,'N')
                @ PROW()+2,00 SAY PADC(RTRIM(set->roda_dup1),80)
                @ PROW()+1,00 SAY PADC(RTRIM(set->roda_dup2),80)
                EJECT
                msub_tot := 0
                mtot_dup := 0
                IF ! EMPTY(mcod_cli)
                        EXIT
                ENDIF
        ENDDO
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDDO
******************************* FIM **************************************8

