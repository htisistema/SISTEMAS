************************************
* RELATORIO FINANCEIRO P/ALUNOS C/DESCONTOS
************************************

FUNCTION saf5213
*****************
LOCAL mprg := 'SAF5213'
LOCAL tela,lci,cci,lba,cba,opcao,mtotal,mmulta,mbanco,mcart,mcont,mtraco,;
      mdata_ini,mdata_fim,mextra,mvlr_apaga,mvlr_pago,mqtd_parc,mmat_aux,;
      mtot_apaga,mtot_pago,mtot_parc,m_apaga[12],m_pago[12],i,mmes
LOCAL mtit,mtipo
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DE FINANCEIRO DE ALUNOS COM DESCONTOS','125')
        RETURN NIL
ENDIF

lci := 17
cci := 10
lba := 21
cba := 55
tela := SAVESCREEN(01,00,24,79)
opcao := 'S'
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
mtraco := REPLI('-',132)
WHILE .T.
        i := 0
        FOR i = 1 TO 12
                m_apaga[i] := 0
                m_pago[i] := 0
        NEXT
        mtotal := 0
        mmulta := 0
        mcont := 0
        mcart := 0
        mbanco := 0
        mextra := 0
        mdata_ini := CTOD('  /  /  ')
        mdata_fim := CTOD('  /  /  ')
        mmes := '  '
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Data Inicial:')
        DEVPOS(lci+2,cci+1);DEVOUT('Data Final..:')
        DEVPOS(lci+3,cci+1);DEVOUT('Parcela.....:')
        @ lci+1,cci+15 GET mdata_ini
        @ lci+2,cci+15 GET mdata_fim VALID IF(mdata_fim < mdata_ini,.F.,.T.)
        @ lci+3,cci+15 GET mmes PICT '99'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        *********************
        SELE('cdmen');ORDSETFOCUS(1)
        GO TOP
        *********************
        IF ! imp_arq('REL_FINA.REL')
                LOOP
        ENDIF
        mpag := 1
        mtit := 'Relatorio Financeiro de alunos c/DESCONTOS'
        mtipo := 'Periodo: '+DTOC(mdata_ini)+' a '+DTOC(mdata_fim)
        cabecalho(mpag,mtit,mtipo)
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('Matric')
        DEVPOS(PROW(),08);DEVOUT('Aluno')
        DEVPOS(PROW(),50);DEVOUT('GSTT')
        DEVPOS(PROW(),56);DEVOUT('Valor Apagar')
        DEVPOS(PROW(),70);DEVOUT('Valor Pago')
        DEVPOS(PROW(),82);DEVOUT('   Dif. R$')
        DEVPOS(PROW(),94);DEVOUT('Dif. %')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        mtot_apaga := 0
        mtot_pago  := 0
        mtot_parc := 0
        WHILE ! EOF()
                ver_aluno(cdmen->matricula,'*')
                mmat_aux := cdmen->matricula
                mvlr_apaga := 0
                mvlr_pago  := 0
                mqtd_parc := 0
                WHILE cdmen->matricula = mmat_aux
                        IF ! EMPTY(mmes)
                                IF cdmen->parcela <> mmes
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF cdmen->data_pag > mdata_fim .OR. cdmen->data_pag < mdata_ini
                                SKIP
                                LOOP
                        ENDIF
                        IF cdmen->valor > 0
                                IF ROUND((cdmen->valor - cdmen->multa),2) >= ROUND((cdmen->vlr_parc - cdmen->desc_bolsa),2)
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF ! EMPTY(cdmen->conta)
                                SKIP
                                LOOP
                        ENDIF
                        m_apaga[VAL(cdmen->parcela)] := m_apaga[VAL(cdmen->parcela)] + (cdmen->vlr_parc - cdmen->desc_bolsa)
                        m_pago[VAL(cdmen->parcela)] := m_pago[VAL(cdmen->parcela)] + (cdmen->valor - cdmen->multa)

                        mvlr_apaga := mvlr_apaga + (cdmen->vlr_parc - cdmen->desc_bolsa)
                        mvlr_pago  := mvlr_pago + (cdmen->valor - cdmen->multa)
                        mqtd_parc ++
                        mtot_apaga := mtot_apaga + (cdmen->vlr_parc - cdmen->desc_bolsa)
                        mtot_pago  := mtot_pago + (cdmen->valor - cdmen->multa)
                        mtot_parc ++
                        SKIP
                ENDDO
                IF ! EMPTY(mvlr_apaga)
                        DEVPOS(PROW()+1,00);DEVOUT(mmat_aux)
                        DEVPOS(PROW(),08);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),50);DEVOUT(cdmen->gstt)
                        DEVPOS(PROW(),56);DEVOUT(TRANSFORM(mvlr_apaga,'999,999.99'))
                        DEVPOS(PROW(),70);DEVOUT(TRANSFORM(mvlr_pago,'999,999.99'))
                        DEVPOS(PROW(),82);DEVOUT(TRANSFORM(mvlr_apaga - mvlr_pago,'999,999.99'))
                        DEVPOS(PROW(),94);DEVOUT(TRANSFORM(((mvlr_apaga - mvlr_pago)/mvlr_apaga)*100,'999.99'))
                ENDIF
                IF PROW() > 57
                        mpag ++
                        EJECT
                        cabecalho(mpag,mtit,mtipo)
                        imprt('M','C')
                        DEVPOS(PROW()+1,00);DEVOUT('Matric')
                        DEVPOS(PROW(),08);DEVOUT('Aluno')
                        DEVPOS(PROW(),50);DEVOUT('GSTT')
                        DEVPOS(PROW(),56);DEVOUT('Valor Apagar')
                        DEVPOS(PROW(),70);DEVOUT('Valor Pago')
                        DEVPOS(PROW(),82);DEVOUT('   Dif. R$')
                        DEVPOS(PROW(),94);DEVOUT('Dif. %')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                ENDIF
        ENDDO
        DEVPOS(PROW()+2,00);DEVOUT('Total de Parcela Pagas......: '+TRANSFORM(mtot_parc,'9,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Vlr.das Prestacoes..R$: '+TRANSFORM(mtot_apaga,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Valor PAGOS........ R$: '+TRANSFORM(mtot_pago,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Diferencas..........R$: '+TRANSFORM(mtot_apaga - mtot_pago,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Diferencas.........(%): '+TRANSFORM(((mtot_apaga - mtot_pago)/mtot_apaga)*100,'999.99'))

        mpag ++
        EJECT
        cabecalho(mpag,mtit,mtipo)
        imprt('M','C')
        DEVPOS(PROW()+2,00);DEVOUT('************** RESUMO *****************')
        DEVPOS(PROW()+1,20);DEVOUT('Valor Apagar    Valor Pago     Dif. R$  Dif. %')
        DEVPOS(PROW()+1,00);DEVOUT('Mes de JANEIRO..: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[1],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[1],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[1] - m_pago[1],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[1] - m_pago[1])/m_apaga[1])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de FEVEREIRO: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[2],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[2],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[2] - m_pago[2],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[2] - m_pago[2])/m_apaga[2])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de MARCO....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[3],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[3],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[3] - m_pago[3],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[3] - m_pago[3])/m_apaga[3])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de ABRIL....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[4],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[4],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[4] - m_pago[4],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[4] - m_pago[4])/m_apaga[4])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de MAIO.....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[5],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[5],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[5] - m_pago[5],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[5] - m_pago[5])/m_apaga[5])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de JUNHO....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[6],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[6],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[6] - m_pago[6],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[6] - m_pago[6])/m_apaga[6])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de JULHO....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[7],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[7],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[7] - m_pago[7],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[7] - m_pago[7])/m_apaga[7])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de AGOSTO...: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[8],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[8],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[8] - m_pago[8],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[8] - m_pago[8])/m_apaga[8])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de SETEMBRO.: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[9],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[9],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[9] - m_pago[9],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[9] - m_pago[9])/m_apaga[9])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de OUTUBRO..: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[10],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[10],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[10] - m_pago[10],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[10] - m_pago[10])/m_apaga[10])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de NOVEMBRO.: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[11],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[11],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[11] - m_pago[11],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[11] - m_pago[11])/m_apaga[11])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de DEZEMBRO.: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[12],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_pago[12],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_apaga[12] - m_pago[12],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((m_apaga[12] - m_pago[12])/m_apaga[12])*100,'999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('TOTAL GERAL.....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mtot_apaga,'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mtot_pago,'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mtot_apaga - mtot_pago,'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(((mtot_apaga - mtot_pago)/mtot_apaga)*100,'999.99'))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(01,00,24,79,tela)
        IF mimp_tipo = 2
                lertexto('REL_FINA.REL')
        ENDIF
        RETURN NIL
ENDDO
RETURN NIL

