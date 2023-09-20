**** CON421.PRG: Relatorio analitico do clientes

*****************
FUNCTION con421()
*****************
LOCAL MPRG:='CON421'
LOCAL tela,tela1,op42,lci,cci,lba,cba,li,ci,lb,cb,mcod_cli,mtp,opcao,i,quantd,;
      pag,mtipo,mtit,mTGvencer,mTGvenci,mTGpagas,mTVvencer,mTVvenci,mTVpagas,;
      mCTvence,mCTvenci,mCTpagas,mCVvence,mCVvenci,mCVpagas,mtraco,mquantd,;
      mtot_ind,mvlr_ind,msub_ind,mjuros_ind,mtipo_con,mmulta,mjuros,;
      mdias_atras,mtot_juros

PRIVATE mcliente,mcpf,mcgc,mtipo_imp

IF ! ver_nivel(mprg,'CONTA A RECEBER (RELATORIO ANALITICO DE CLIENTE)','15',nivel_acess)
        RETURN NIL
ENDIF

lci := 11
cci := 06
lba := 17
cba := 32

li := 19
ci := 06
lb := 21
cb := 60

tela := SAVESCREEN(01,00,24,79)
*tela1 := SAVESCREEN(li,ci-1,lb+1,cb)
mtraco := REPLI('=',80)
botao(lci,cci,lba,cba)
*----------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
*----------------------------------------------------------------------
mtipo_imp := cfg->imp_rel
WHILE .T.
        mtipo := '  '
        mcod_cli := 0
        limpa(23,00,23,79)
        @ lci+1,cci+1 PROMPT "          Pagas          "  MESSAGE '** Relatorio de Duplicatas PAGAS do Cliente - <ESC>abandonar **'
        @ lci+2,cci+1 PROMPT "         A Vencer        "  MESSAGE '** Relatorio de Duplicatas A VENCER do Cliente - <ESC> Abandonar'
        @ lci+3,cci+1 PROMPT "         Vencidas        "  MESSAGE '** Relatorio de Duplicata VENCIDAS do Cliente - <ESC> Abandonar'
        @ lci+4,cci+1 PROMPT "   A Vencer e Vencidas   "  MESSAGE '** Relatorio de Duplicata A VENCER/VENCIDAS do Cliente - <ESC> Abandonar'
        @ lci+5,cci+1 PROMPT " A Vencer/Vencidas/Pagas "  MESSAGE '** Relatorio de Duplicata A VENCER/VENCIDAS/PAGAS do Cliente - <ESC> Abandonar'
        MENU TO op42
        DO CASE
                CASE op42=0
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN
                CASE op42=1
                        mtp := 'PG'
                        mtipo := 'Duplicatas PAGAS (Contas a Receber)'
                CASE op42=2
                        mtp := 'AV'
                        mtipo := 'Duplicatas A VENCER (Contas a Receber)'
                CASE op42=3
                        mtp := 'VE'
                        mtipo := 'Duplicatas VENCIDAS (Contas a Receber)'
                CASE op42=4
                        mtp := 'VV'
                        mtipo := 'A VENCER E VENCIDAS (Contas a Receber)'
                CASE op42=5
                        mtp := 'VP'
                        mtipo := 'VENCER, VENCIDAS E PAGAS (Contas a Receber)'
        ENDCASE
        mensagem('Digite Cod.cliente ou <ENTER>p/todos - <ESC> Retornar')
        botao(li,ci,lb,cb)
        DEVPOS(li+1,ci+2);DEVOUT('Cod.Cliente:')
        @ li+1,ci+14 GET mcod_cli PICT '99999' VALID ver_cli(mcod_cli,li+1,ci+21)
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Aguarde o Coletando dados p/ impressao - [ESC] Abandona.')
        ************************************************************
        SELE('dupr');ORDSETFOCUS(8)
        IF ! EMPTY(mcod_cli) .AND. mtp = 'PG'
                DBSETFILTER({|| STRZERO(mcod_cli,5) = fornec .AND. pago = 'B'})
        ELSEIF ! EMPTY(mcod_cli) .AND. mtp = 'AV'
                DBSETFILTER({|| STRZERO(mcod_cli,5) = fornec .AND. venc >= mdata_sis .AND. pago = ' '})
        ELSEIF ! EMPTY(mcod_cli) .AND. mtp = 'VE'
                DBSETFILTER({|| STRZERO(mcod_cli,5) = fornec .AND. venc < mdata_sis .AND. pago = ' '})
        ELSEIF ! EMPTY(mcod_cli) .AND. mtp = 'VV'
                DBSETFILTER({|| STRZERO(mcod_cli,5) = fornec .AND. pago = ' '})
        ELSEIF ! EMPTY(mcod_cli) .AND. mtp = 'VP'
                DBSETFILTER({|| STRZERO(mcod_cli,5) = fornec })
        ELSEIF EMPTY(mcod_cli) .AND. mtp = 'PG'
                DBSETFILTER({|| pago <> ' '})
        ELSEIF EMPTY(mcod_cli) .AND. mtp = 'AV'
                DBSETFILTER({|| venc >= mdata_sis .AND. pago = ' '})
        ELSEIF EMPTY(mcod_cli) .AND. mtp = 'VE'
                DBSETFILTER({|| venc < mdata_sis .AND. pago = ' '})
        ELSEIF EMPTY(mcod_cli) .AND. mtp = 'VV'
                DBSETFILTER({|| pago = ' '})
        ENDIF
        GO TOP
        mcod_cli := VAL(fornec)
        ************************************************************
        IF ! imp_arq('DOC_VENCE.REL','R')
                LOOP
        ENDIF
        pag=0
        mTGvencer := 0
        mTGvenci := 0
        mTGpagas := 0
        mTVvencer := 0
        mTVvenci := 0
        mTVpagas := 0
        mCTvencer := 0
        mCTvenci := 0
        mCTpagas := 0
        mCVvencer := 0
        mCVvenci := 0
        mCVpagas := 0
        mtot_juros := 0
        mtot_ind := 0
        mvlr_ind := 0
        msub_ind := 0
        mjuros_ind := 0
        mtit := 'DUPLICATAS A RECEBER P/CLIENTE'
        pag=1
        cabecalho(pag,mtit,mtipo,mprg)
        imprt(cfg->imp_rel,'C',1)
        DEVPOS(PROW(),00);DEVOUT('Documento    ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Pedido')
        DEVPOS(PROW(),PCOL()+2);DEVOUT(' Emissao')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('  Vencim')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Valor')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Vlr.+Juros')
        DEVPOS(PROW(),PCOl()+2);DEVOUT('Dta.Pag.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT(' Valor Pg.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('   Juros')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Desconto')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Dias')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Situacao')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Ven')
        imprt(cfg->imp_rel,'N')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        pitalico(.T.)
        DEVPOS(PROW()+1,00);DEVOUT(fornec)
        DEVPOS(PROW(),07);DEVOUT(cliente)
        pitalico()
        WHILE ! EOF()
                IF pag=0 .OR. PROW()>=59
                        pag=pag+1
                        IF pag>1
                                EJECT
                        ENDIF
                        cabecalho(pag,mtit,mtipo,mprg)
                        imprt(cfg->imp_rel,'C',1)
                        DEVPOS(PROW(),00);DEVOUT('Documento    ')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Pedido')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' Emissao')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('  Vencim')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Valor')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Vlr.+Juros')
                        DEVPOS(PROW(),PCOl()+2);DEVOUT('Dta.Pag.')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(' Valor Pg.')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('   Juros')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Desconto')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Dias')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Situacao')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Ven')
                        imprt(cfg->imp_rel,'N')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                ENDIF
                IF STRZERO(mcod_cli,5) <> fornec
                        IF pag=0 .OR. PROW()>55
                                pag=pag+1
                                IF pag>1
                                        EJECT
                                ENDIF
                                cabecalho(pag,mtit,mtipo,mprg)
                                imprt(cfg->imp_rel,'C',1)
                                DEVPOS(PROW(),00);DEVOUT('Documento    ')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Pedido')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(' Emissao')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('  Vencim')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('     Valor')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Vlr.+Juros')
                                DEVPOS(PROW(),PCOl()+2);DEVOUT('Dta.Pag.')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(' Valor Pg.')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('   Juros')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Desconto')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Dias')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Situacao')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Ven')
                                imprt(cfg->imp_rel,'N')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        ENDIF
                        imprt(cfg->imp_rel,'C')
                        DEVPOS(PROW()+1,00);DEVOUT('******************************************************** RESUMO DO CLIENTE **********************************************************')
                        DEVPOS(PROW()+1,00);DEVOUT('Total de Documentos A VENCER :')
                        DEVPOS(PROW(),31);DEVOUT(TRANSFORM(mCTvencer,'999999'))
                        DEVPOS(PROW(),37);DEVOUT(' - '+TRANSFORM(mCVvencer,'999,999,999.99'))
                        DEVPOS(PROW(),57);DEVOUT('VENCIDAS:')
                        DEVPOS(PROW(),67);DEVOUT(TRANSFORM(mCTvenci,'999999'))
                        DEVPOS(PROW(),73);DEVOUT(' - '+TRANSFORM(mCVvenci,'999,999,999.99'))
                        DEVPOS(PROW(),92);DEVOUT('Total A RECEBER:')
                        DEVPOS(PROW(),109);DEVOUT(TRANSFORM(mCTvencer+mCTvenci,'999999'))
                        DEVPOS(PROW(),115);DEVOUT(' - '+TRANSFORM(mCVvencer+mCVvenci,'999,999,999.99'))

                        DEVPOS(PROW()+1,00);DEVOUT('Total de Documentos VENCIDOS + JUROS:')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mCVvenci,'999,999,999.99'))
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Juros+Multas:'+TRANSFORM(mjuros_ind,'9,999,999.99'))
                        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Total Geral:'+TRANSFORM(mCVvenci + mjuros_ind,'999,999,999.99'))

                        DEVPOS(PROW()+1,00);DEVOUT('Total de Documentos RECEBIDOS:')
                        DEVPOS(PROW(),30);DEVOUT(TRANSFORM(mCTpagas,'999999'))
                        DEVPOS(PROW(),37);DEVOUT(' - '+TRANSFORM(mCVpagas,'999,999,999.99'))
                        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
                        IF pag=0 .OR. PROW()>57
                                pag=pag+1
                                IF pag>1
                                        EJECT
                                ENDIF
                                cabecalho(pag,mtit,mtipo,mprg)
                                imprt(cfg->imp_rel,'C',1)
                                DEVPOS(PROW(),00);DEVOUT('Documento    ')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Pedido')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(' Emissao')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('  Vencim')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('     Valor')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Vlr.+Juros')
                                DEVPOS(PROW(),PCOl()+2);DEVOUT('Dta.Pag.')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(' Valor Pg.')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('   Juros')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Desconto')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Dias')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Situacao')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Ven')
                                imprt(cfg->imp_rel,'N')
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        ENDIF
                        imprt(cfg->imp_rel,'N',1)
                        pitalico(.T.)
                        DEVPOS(PROW(),00);DEVOUT(fornec)
                        DEVPOS(PROW(),05);DEVOUT(cliente)
                        pitalico()
                        mcliente := SPACE(40)
                        mcod_cli := VAL(fornec)
                        mCTpagas := 0
                        mCVpagas := 0
                        mCTvencer := 0
                        mCVvencer := 0
                        mCTvenci := 0
                        mCVvenci := 0
                        mjuros_ind := 0
                ENDIF
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
                imprt(cfg->imp_rel,'C')
                DEVPOS(PROW()+1,00);DEVOUT(tipo+duplicata+conta)
                DEVPOS(PROW(),PCOL()+2);DEVOUT(num_ped)
                DEVPOS(PROW(),PCOL()+2);DEVOUT(DTOC(emissao))
                DEVPOS(PROW(),PCOL()+2);DEVOUT(DTOC(venc))
                DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(valor,"@E 999,999.99"))
                DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(dupr->valor+mmulta+mjuros,"@E 999,999.99"))
                DEVPOS(PROW(),PCOL()+2);DEVOUT(datpag)
                DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(vlpago,"999,999.99"))
                DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(juros,"9,999.99"))
                DEVPOS(PROW(),PCOl()+2);DEVOUT(TRANSFORM(desc,"9,999.99"))
                IF venc>=mdata_sis .AND. pago = ' '
                        DEVPOS(PROW(),PCOl()+2);DEVOUT(TRANSFORM(venc-mdata_sis,"9999"))
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('A Vencer')
                        mTGvencer ++
                        mTVvencer := mTVvencer + valor
                        mCTvencer ++
                        mCVvencer := mCVvencer + valor
                ELSEIF venc < mdata_sis .AND. pago = ' '
                        DEVPOS(PROW(),PCOL()+2);DEVOUT(TRANSFORM(venc-mdata_sis,"9999"))
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Vencida ')
                        mTGvenci ++
                        mTVvenci := mTVvenci + valor
                        mCTvenci ++
                        mCVvenci := mCVvenci + valor
                ELSEIF pago = 'B'
                        DEVPOS(PROW(),PCOl()+2);DEVOUT(TRANSFORM(venc-datpag,"9999"))
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('Pagas   ')
                        mTGpagas ++
                        mTVpagas := mTVpagas + valor
                        mCTpagas ++
                        mCVpagas := mCVpagas + valor
                ENDIF
                DEVPOS(PROW(),PCOl()+2);DEVOUT(vendedor)
                SKIP
        ENDDO
        IF PROW()>55
                EJECT
                pag=pag+1
                cabecalho(pag,mtit,mtipo,mprg)
                ?
                ?
        ENDIF
        imprt(cfg->imp_rel,'C')
        DEVPOS(PROW()+1,00);DEVOUT('******************************************************** RESUMO DO CLIENTE **********************************************************')
        DEVPOS(PROW()+1,00);DEVOUT('Total de Documentos A VENCER :')
        DEVPOS(PROW(),31);DEVOUT(TRANSFORM(mCTvencer,'999999'))
        DEVPOS(PROW(),37);DEVOUT(' - '+TRANSFORM(mCVvencer,'999,999,999.99'))
        DEVPOS(PROW(),57);DEVOUT('VENCIDAS:')
        DEVPOS(PROW(),67);DEVOUT(TRANSFORM(mCTvenci,'999999'))
        DEVPOS(PROW(),73);DEVOUT(' - '+TRANSFORM(mCVvenci,'999,999,999.99'))
        DEVPOS(PROW(),92);DEVOUT('Total A RECEBER:')
        DEVPOS(PROW(),109);DEVOUT(TRANSFORM(mCTvencer+mCTvenci,'999999'))
        DEVPOS(PROW(),115);DEVOUT(' - '+TRANSFORM(mCVvencer+mCVvenci,'999,999,999.99'))

        DEVPOS(PROW()+1,00);DEVOUT('Total de Documentos VENCIDOS + JUROS:')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mCVvenci,'999,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Juros+Multas:'+TRANSFORM(mjuros_ind,'9,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(' - Total Geral:'+TRANSFORM(mCVvenci + mjuros_ind,'999,999,999.99'))

        DEVPOS(PROW()+1,00);DEVOUT('Total de Documentos RECEBIDOS:')
        DEVPOS(PROW(),30);DEVOUT(TRANSFORM(mCTpagas,'999999'))
        DEVPOS(PROW(),37);DEVOUT(' - '+TRANSFORM(mCVpagas,'999,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',132))
        IF PROW()>55
                EJECT
                pag=pag+1
                cabecalho(pag,mtit,mtipo,mprg)
                ?
                ?
        ELSE
                ?
                ?
        ENDIF
        imprt(cfg->imp_rel,'N')
        imprt(cfg->imp_rel,'E')
        DEVPOS(PROW()+2,00);DEVOUT('RESUMO GERAL')
        imprt(cfg->imp_rel,'C')
        DEVPOS(PROW()+1,00);DEVOUT("Total de Documentos VENCIDOS....: "+TRANSFORM(mTGvenci,"9999")+" - "+TRANSFORM(mTVvenci,"@E 999,999,999.99"))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(" - Total de Juros: "+TRANSFORM(mtot_juros,"@E 999,999,999.99"))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(" - Total Geral: "+TRANSFORM(mTVvenci+mtot_juros,"@E 999,999,999.99"))
        DEVPOS(PROW()+1,00);DEVOUT("Total de Documentos a VENCER....: "+TRANSFORM(mTGvencer,"9999")+" - "+TRANSFORM(mTVvencer,"@E 999,999,999.99"))
        DEVPOS(PROW()+1,00);DEVOUT("Total Geral Documentos A RECEBER: "+TRANSFORM(mTGvencer+mTGvenci,"9999")+" - "+TRANSFORM(mTVvencer+mTVvenci,"@E 999,999,999.99"))
        DEVPOS(PROW(),00);DEVOUT("Total Geral Documentos A RECEBER: "+TRANSFORM(mTGvencer+mTGvenci,"9999")+" - "+TRANSFORM(mTVvencer+mTVvenci,"@E 999,999,999.99"))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(" - Total de JUROS: "+TRANSFORM(mtot_juros,"@E 999,999,999.99"))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(" - Total de GERAL: "+TRANSFORM(mTVvencer+mTVvenci+mtot_juros,"@E 999,999,999.99"))
        DEVPOS(PROW()+1,00);DEVOUT("Total de Documentos RECEBIDOS...: "+TRANSFORM(mTGpagas,"9999")+" - "+TRANSFORM(mTVpagas,"@E 999,999,999.99"))
        DEVPOS(PROW(),00);DEVOUT("Total de Documentos RECEBIDOS...: "+TRANSFORM(mTGpagas,"9999")+" - "+TRANSFORM(mTVpagas,"@E 999,999,999.99"))
        ?
        ? time()
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(01,00,24,79,tela)
ENDDO
*********************** f i m ********************************

