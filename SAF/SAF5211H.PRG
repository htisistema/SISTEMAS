************************************************
* MAPA FINANCEIRO
************************************************

FUNCTION saf5211
***************
LOCAL mprg:='SAF5211'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,y,mvalor,mfaixa,;
      tela,mpoint,maux,mmat_aux
LOCAL mtot_alu,mtot_pag,mtot_bru,mtot_bol,mtot_liq,mtot_rec,mtot_fal,mtot_mul,;
      mmes_alu,mmes_pag,mmes_bru,mmes_bol,mmes_liq,mmes_rec,mmes_fal,mmes_mul,mqtd_bol

PRIVATE m_valores:={},m_mes:={}
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DO MAPA FINANCEIRO','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
lci := 18
cci := 10
lba := 20
cba := 42
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mmes := SPACE(2)
        mquantd = 1
        ***************
        SELE('aluno');ORDSETFOCUS(1)
        GO TOP
        ***************
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Digite ate que MES deseja: ')
        IF LASTKEY() = 27
                CLOSE ALL
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        @ lci+1,cci+29 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Confirma a Impressao [S/n]:')
        opcao = 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF opcao = 'N'
                LOOP
        ENDIF
        IF LASTKEY() = 27
                LOOP
        ENDIF
        ***************
        SELE('aluno');ORDSETFOCUS(2)
        GO TOP
        ***************
        ASIZE(m_valores,0)
        ASIZE(m_mes,0)
        mensagem('Espere o Coletando informacoes p/o MAPA FINANCEIRO - [ESC]Abandonar')
        IF mmes = '01'
                i := 0
                mvalor := 0
                FOR i = 1 TO VAL(mmes)
                        ***************
                        SELE('cdmen');ORDSETFOCUS(5)
                        GO TOP
                        ***************
                        SEEK STRZERO(i,2)
                        BLOQARQ()
                        WHILE STRZERO(i,2) = cdmen->parcela .AND. ! EOF()
                                IF mmat_aux = cdmen->matricula
                                        atencao('Esta matricula existe duplicidade:'+cdmen->matricula+' - Parcela: '+cdmen->parcela)
                                        mmat_aux := cdmen->matricula
                                ELSE
                                        mmat_aux := cdmen->matricula
                                ENDIF
                                IF ! ver_aluno(cdmen->matricula)
                                        atencao('Esta matricula existe duplicidade:'+cdmen->matricula+' - Parcela: '+cdmen->parcela)
                                ENDIF
                                cdmen->bolsa := aluno->faixa_1
*                               IF aluno->faixa_1 = 'I' .OR. aluno->faixa_1 = 'W' .OR. aluno->faixa_1 = 'Y' .OR. aluno->faixa_1 = 'Z'
*                                       SKIP
*                                       LOOP
*                               ENDIF
                                IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),'01')
                                        atencao('Este aluno nao foi incluso GSTT: '+aluno->GSTT+' - Matricula: '+aluno->matricula+'-'+aluno->aluno)
                                        SKIP
                                        LOOP
                                ENDIF
                                mvalor := 0
                                IF ver_fx(aluno->faixa_1)
                                        IF fx->desconto > 0
                                                mvalor := (tabmen->valor - (tabmen->valor*(fx->desconto/100)))
                                        ELSE
                                                mvalor := tabmen->valor
                                        ENDIF
                                ELSE
                                        mvalor := tabmen->valor
                                ENDIF
                                cdmen->valor := mvalor
                                SKIP
                        ENDDO
                        DBCOMMIT()
                        DBUNLOCK()
                NEXT
        ENDIF
        i := 0
        FOR i = 1 TO VAL(mmes)
                AADD(m_mes,{i,0})
        NEXT
        y := 0
        i := 0
        mmat_aux := SPACE(6)
        FOR i = 1 TO VAL(mmes)
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                ***************
                AADD(m_valores,{'Pre-Escola  ',i,0,0,0,0,0,0,0,0})
                WHILE VAL(SUBSTR(aluno->gstt,1,2)) >= 01 .AND. VAL(SUBSTR(aluno->gstt,1,2)) <= 05 .AND. ! EOF()
                        IF ! EMPTY(aluno->dt_tranf)
                                IF i >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF i < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                SKIP
                                LOOP
                        ENDIF
                        IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),STRZERO(i,2))
                                atencao('Este aluno nao foi incluso GSTT: '+aluno->GSTT+' - Matricula: '+aluno->matricula+'-'+aluno->aluno)
                                SKIP
                                LOOP
                        ENDIF
                        mvalor := 0
                        IF i = 1
                                mfaixa := aluno->faixa_1
                        ELSEIF i = 2
                                mfaixa := aluno->faixa_2
                        ELSEIF i = 3
                                mfaixa := aluno->faixa_3
                        ELSEIF i = 4
                                mfaixa := aluno->faixa_4
                        ELSEIF i = 5
                                mfaixa := aluno->faixa_5
                        ELSEIF i = 6
                                mfaixa := aluno->faixa_6
                        ELSEIF i = 7
                                mfaixa := aluno->faixa_7
                        ELSEIF i = 8
                                mfaixa := aluno->faixa_8
                        ELSEIF i = 9
                                mfaixa := aluno->faixa_9
                        ELSEIF i = 10
                                mfaixa := aluno->faixa_10
                        ELSEIF i = 11
                                mfaixa := aluno->faixa_11
                        ELSEIF i = 12
                                mfaixa := aluno->faixa_12
                        ENDIF
                        IF ver_fx(mfaixa)
                                IF fx->desconto = 100
                                        m_valores[i+y,9] := m_valores[i+y,9] + 1                       // BOLSA DE 100%
                                        mvalor := 0
                                ELSE
                                        IF fx->desconto > 0
                                                mvalor := (tabmen->valor - (tabmen->valor*(fx->desconto/100)))
                                        ELSE
                                                mvalor := tabmen->valor
                                        ENDIF
                                ENDIF
                        ELSE
                                mvalor := tabmen->valor
                        ENDIF
                        m_valores[i+y,2] := i                                        // parcela
                        m_valores[i+y,3] := m_valores[i+y,3] + 1                       // total de alunos
                        m_valores[i+y,4] := m_valores[i+y,4] + tabmen->valor              // Receita Bruta
                        m_valores[i+y,5] := m_valores[i+y,5] + (tabmen->valor - mvalor)   // Bolsas
                        m_valores[i+y,6] := m_valores[i+y,6] +  mvalor                 // Receita Liquida
                        m_mes[i,2] := m_mes[i,2] + tabmen->valor
                        SKIP
                ENDDO
                mpoint := RECNO()
                ***************
                SELE('cdmen');ORDSETFOCUS(5)
                GO TOP
                ***************
                SEEK STRZERO(i,2)
                WHILE STRZERO(i,2) = cdmen->parcela .AND. ! EOF()
                        IF VAL(SUBSTR(cdmen->gstt,1,2)) < 01 .OR. VAL(SUBSTR(cdmen->gstt,1,2)) > 05
                                SKIP
                                LOOP
                        ENDIF
                        IF cdmen->valor > 0
                                m_valores[i+y,7] := m_valores[i+y,7] + 1
                                m_valores[i+y,8] := m_valores[i+y,8] + cdmen->valor-cdmen->multa
                                m_valores[i+y,10] := m_valores[i+y,10] + cdmen->multa
                        ENDIF
                        SKIP
                ENDDO
                y ++
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO mpoint
                ***************
                AADD(m_valores,{'1 Grau Menor',i,0,0,0,0,0,0,0,0})
                WHILE VAL(SUBSTR(aluno->gstt,1,2)) >= 11 .AND. VAL(SUBSTR(aluno->gstt,1,2)) <= 14 .AND. ! EOF()
                        IF ! EMPTY(aluno->dt_tranf)
                                IF i > VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF i < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                SKIP
                                LOOP
                        ENDIF
                        IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),STRZERO(i,2))
                                atencao('Este aluno nao foi incluso GSTT: '+aluno->GSTT+' - Matricula: '+aluno->matricula+'-'+aluno->aluno)
                                SKIP
                                LOOP
                        ENDIF
                        mvalor := 0
                        IF i = 1
                                mfaixa := aluno->faixa_1
                        ELSEIF i = 2
                                mfaixa := aluno->faixa_2
                        ELSEIF i = 3
                                mfaixa := aluno->faixa_3
                        ELSEIF i = 4
                                mfaixa := aluno->faixa_4
                        ELSEIF i = 5
                                mfaixa := aluno->faixa_5
                        ELSEIF i = 6
                                mfaixa := aluno->faixa_6
                        ELSEIF i = 7
                                mfaixa := aluno->faixa_7
                        ELSEIF i = 8
                                mfaixa := aluno->faixa_8
                        ELSEIF i = 9
                                mfaixa := aluno->faixa_9
                        ELSEIF i = 10
                                mfaixa := aluno->faixa_10
                        ELSEIF i = 11
                                mfaixa := aluno->faixa_11
                        ELSEIF i = 12
                                mfaixa := aluno->faixa_12
                        ENDIF
                        IF ver_fx(mfaixa)
                                IF fx->desconto = 100
                                        mvalor := 0
                                        m_valores[i+y,9] := m_valores[i+y,9] + 1                       // BOLSA DE 100%
                                ELSE
                                        IF fx->desconto > 0
                                                mvalor := (tabmen->valor - (tabmen->valor*(fx->desconto/100)))
                                        ELSE
                                                mvalor := tabmen->valor
                                        ENDIF
                                ENDIF
                        ELSE
                                mvalor := tabmen->valor
                        ENDIF
                        m_valores[i+y,2] := i                                        // parcela
                        m_valores[i+y,3] := m_valores[i+y,3] + 1                       // total de alunos
                        m_valores[i+y,4] := m_valores[i+y,4] + tabmen->valor              // Receita Bruta
                        m_valores[i+y,5] := m_valores[i+y,5] + (tabmen->valor - mvalor)   // Saldo da receita
                        m_valores[i+y,6] := m_valores[i+y,6] +  mvalor                 // Receita Liquida
                        m_mes[i,2] := m_mes[i,2] + tabmen->valor
                        SKIP
                ENDDO
                mpoint := RECNO()
                ***************
                SELE('cdmen');ORDSETFOCUS(2)
                GO TOP
                ***************
                SEEK STRZERO(i,2)
                WHILE STRZERO(i,2) = cdmen->parcela .AND. ! EOF()
                        IF VAL(SUBSTR(cdmen->gstt,1,2)) < 11 .OR. VAL(SUBSTR(cdmen->gstt,1,2)) > 14
                                SKIP
                                LOOP
                        ENDIF
                        IF cdmen->valor > 0
                                m_valores[i+y,7] := m_valores[i+y,7] + 1
                                m_valores[i+y,8] := m_valores[i+y,8] + cdmen->valor-cdmen->multa
                                m_valores[i+y,10] := m_valores[i+y,10] + cdmen->multa
                        ENDIF
                        SKIP
                ENDDO
                y ++
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO mpoint
                ***************
                AADD(m_valores,{'1 Grau Maior',i,0,0,0,0,0,0,0,0})
                WHILE VAL(SUBSTR(aluno->gstt,1,2)) >= 15 .AND. VAL(SUBSTR(aluno->gstt,1,2)) <= 18 .AND. ! EOF()
                        IF ! EMPTY(aluno->dt_tranf)
                                IF i > VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF i < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                SKIP
                                LOOP
                        ENDIF
                        IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),STRZERO(i,2))
                                atencao('Este aluno nao foi incluso GSTT: '+aluno->GSTT+' - Matricula: '+aluno->matricula+'-'+aluno->aluno)
                                SKIP
                                LOOP
                        ENDIF
                        mvalor := 0
                        IF i = 1
                                mfaixa := aluno->faixa_1
                        ELSEIF i = 2
                                mfaixa := aluno->faixa_2
                        ELSEIF i = 3
                                mfaixa := aluno->faixa_3
                        ELSEIF i = 4
                                mfaixa := aluno->faixa_4
                        ELSEIF i = 5
                                mfaixa := aluno->faixa_5
                        ELSEIF i = 6
                                mfaixa := aluno->faixa_6
                        ELSEIF i = 7
                                mfaixa := aluno->faixa_7
                        ELSEIF i = 8
                                mfaixa := aluno->faixa_8
                        ELSEIF i = 9
                                mfaixa := aluno->faixa_9
                        ELSEIF i = 10
                                mfaixa := aluno->faixa_10
                        ELSEIF i = 11
                                mfaixa := aluno->faixa_11
                        ELSEIF i = 12
                                mfaixa := aluno->faixa_12
                        ENDIF
                        IF ver_fx(mfaixa)
                                IF fx->desconto = 100
                                        m_valores[i+y,9] := m_valores[i+y,9] + 1                       // BOLSA DE 100%
                                        mvalor := 0
                                ELSE
                                        IF fx->desconto > 0
                                                mvalor := (tabmen->valor - (tabmen->valor*(fx->desconto/100)))
                                        ELSE
                                                mvalor := tabmen->valor
                                        ENDIF
                                ENDIF
                        ELSE
                                mvalor := tabmen->valor
                        ENDIF
                        m_valores[i+y,2] := i                                        // parcela
                        m_valores[i+y,3] := m_valores[i+y,3] + 1                       // total de alunos
                        m_valores[i+y,4] := m_valores[i+y,4] + tabmen->valor              // Receita Bruta
                        m_valores[i+y,5] := m_valores[i+y,5] + (tabmen->valor - mvalor)   // Saldo da receita
                        m_valores[i+y,6] := m_valores[i+y,6] +  mvalor                 // Receita Liquida
                        m_mes[i,2] := m_mes[i,2] + tabmen->valor
                        SKIP
                ENDDO
                mpoint := RECNO()
                ***************
                SELE('cdmen');ORDSETFOCUS(2)
                GO TOP
                ***************
                SEEK STRZERO(i,2)
                WHILE STRZERO(i,2) = cdmen->parcela .AND. ! EOF()
                        IF VAL(SUBSTR(cdmen->gstt,1,2)) < 15 .OR. VAL(SUBSTR(cdmen->gstt,1,2)) > 18
                                SKIP
                                LOOP
                        ENDIF
                        IF cdmen->valor > 0
                                m_valores[i+y,7] := m_valores[i+y,7] + 1
                                m_valores[i+y,8] := m_valores[i+y,8] + cdmen->valor-cdmen->multa
                                m_valores[i+y,10] := m_valores[i+y,10] + cdmen->multa
                        ENDIF
                        SKIP
                ENDDO
                y ++
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO mpoint
                ***************
                AADD(m_valores,{'2 Grau      ',i,0,0,0,0,0,0,0,0})
                WHILE VAL(SUBSTR(aluno->gstt,1,2)) >= 21 .AND. VAL(SUBSTR(aluno->gstt,1,2)) <= 23 .AND. ! EOF()
                        IF ! EMPTY(aluno->dt_tranf)
                                IF i > VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
                        IF i < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                SKIP
                                LOOP
                        ENDIF
                        IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),STRZERO(i,2))
                                atencao('Este aluno nao foi incluso GSTT: '+aluno->GSTT+' - Matricula: '+aluno->matricula+'-'+aluno->aluno)
                                SKIP
                                LOOP
                        ENDIF
                        mvalor := 0
                        IF i = 1
                                mfaixa := aluno->faixa_1
                        ELSEIF i = 2
                                mfaixa := aluno->faixa_2
                        ELSEIF i = 3
                                mfaixa := aluno->faixa_3
                        ELSEIF i = 4
                                mfaixa := aluno->faixa_4
                        ELSEIF i = 5
                                mfaixa := aluno->faixa_5
                        ELSEIF i = 6
                                mfaixa := aluno->faixa_6
                        ELSEIF i = 7
                                mfaixa := aluno->faixa_7
                        ELSEIF i = 8
                                mfaixa := aluno->faixa_8
                        ELSEIF i = 9
                                mfaixa := aluno->faixa_9
                        ELSEIF i = 10
                                mfaixa := aluno->faixa_10
                        ELSEIF i = 11
                                mfaixa := aluno->faixa_11
                        ELSEIF i = 12
                                mfaixa := aluno->faixa_12
                        ENDIF
                        IF ver_fx(mfaixa)
                                IF fx->desconto = 100
                                        m_valores[i+y,9] := m_valores[i+y,9] + 1                       // BOLSA DE 100%
                                        mvalor := 0
                                ELSE
                                        IF fx->desconto > 0
                                                mvalor := (tabmen->valor - (tabmen->valor*(fx->desconto/100)))
                                        ELSE
                                                mvalor := tabmen->valor
                                        ENDIF
                                ENDIF
                        ELSE
                                mvalor := tabmen->valor
                        ENDIF
                        m_valores[i+y,2] := i                                        // parcela
                        m_valores[i+y,3] := m_valores[i+y,3] + 1                       // total de alunos
                        m_valores[i+y,4] := m_valores[i+y,4] + tabmen->valor              // Receita Bruta
                        m_valores[i+y,5] := m_valores[i+y,5] + (tabmen->valor - mvalor)   // Saldo da receita
                        m_valores[i+y,6] := m_valores[i+y,6] +  mvalor                 // Receita Liquida
                        m_mes[i,2] := m_mes[i,2] + tabmen->valor
                        SKIP
                ENDDO
                mpoint := RECNO()
                ***************
                SELE('cdmen');ORDSETFOCUS(2)
                GO TOP
                ***************
                SEEK STRZERO(i,2)

                WHILE STRZERO(i,2) = cdmen->parcela .AND. ! EOF()
                        IF VAL(SUBSTR(cdmen->gstt,1,2)) < 21 .OR. VAL(SUBSTR(cdmen->gstt,1,2)) > 23
                                SKIP
                                LOOP
                        ENDIF
                        IF cdmen->valor > 0
                                m_valores[i+y,7] := m_valores[i+y,7] + 1
                                m_valores[i+y,8] := m_valores[i+y,8] + cdmen->valor-cdmen->multa
                                m_valores[i+y,10] := m_valores[i+y,10] + cdmen->multa
                        ENDIF
                        SKIP
                ENDDO
        NEXT
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        IF ! imp_arq('MAP_FINA.REL')
                LOOP
        ENDIF
        mpag := 1
        mtit := 'MAPA FINANCEIRO'
        mtipo := 'ate o mes de '+mmes+'/'+mdiretorio
        cabecalho(mpag,mtit,mtipo)
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('            ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Qtd.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Bols')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Qtd.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('   Receita')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('  Total de')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Total de')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
        DEVPOS(PROW(),PCOL()+2);DEVOUT(' Qtd.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Quantd.')
        DEVPOS(PROW()+1,00);DEVOUT('Mes/Ano     ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Alu.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('100%')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Pagm')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('  Bruta R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT(' Bolsas R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Bolsa (%)')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Liquido R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Pag.')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('Recebido R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('    Multas')
        DEVPOS(PROW(),PCOL()+2);DEVOUT(' A rec. R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Inadip.')
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT(mtraco)
        imprt('M','C')
        maux := 1
        i := 0
        mtot_alu := 0
        mtot_bru := 0
        mtot_bol := 0
        mtot_liq := 0
        mtot_pag := 0
        mtot_rec := 0
        mtot_fal := 0
        mtot_mul := 0

        mqtd_bol := 0
        mmes_alu := 0
        mmes_bru := 0
        mmes_bol := 0
        mmes_liq := 0
        mmes_pag := 0
        mmes_rec := 0
        mmes_fal := 0
        mmes_mul := 0
        DEVPOS(PROW()+1,00);DEVOUT('Jan/'+mdiretorio)
        FOR i = 1 TO LEN(m_valores)
                INKEY(.1)
                IF LASTKEY() = 27
                        DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                        EXIT
                ENDIF
                IF maux < m_valores[i,2]
                        DEVPOS(PROW()+1,00);DEVOUT('------------')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('----')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('----')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('----')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('---------')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('----')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
                        DEVPOS(PROW(),PCOL()+2);DEVOUT('-------')
                        DEVPOS(PROW()+1,00);DEVOUT('Totais......')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_alu,'9999')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mqtd_bol,'9999')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_alu-mqtd_bol,'9999')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_bru,'999,999.99')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_bol,'999,999.99')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT((mmes_bol / mmes_bru) * 100,'9999.9999')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_liq,'999,999.99')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_pag,'9999')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_rec,'999,999.99')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_mul,'999,999.99')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_liq - mmes_rec,'999,999.99')
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(((mmes_liq - mmes_rec) / mmes_liq) * 100,'999.99')
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('%')
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT(REPLI('-',80))
                        imprt('M','C')
                        IF m_valores[i,2] = 2
                                DEVPOS(PROW()+1,00);DEVOUT('Fev/'+mdiretorio)
                        ELSEIF m_valores[i,2] = 3
                                DEVPOS(PROW()+1,00);DEVOUT('Mar/'+mdiretorio)
                        ELSEIF m_valores[i,2] = 4
                                DEVPOS(PROW()+1,00);DEVOUT('Abr/'+mdiretorio)
                        ELSEIF m_valores[i,2] = 5
                                DEVPOS(PROW()+1,00);DEVOUT('Mai/'+mdiretorio)
                        ELSEIF m_valores[i,2] = 6
                                DEVPOS(PROW()+1,00);DEVOUT('Jun/'+mdiretorio)
                        ELSEIF m_valores[i,2] = 7
                                mpag ++
                                EJECT
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('            ')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Qtd.')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Bols')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Qtd.')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('   Receita')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('  Total de')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Total de')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(' Qtd.')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Quantd.')
                                DEVPOS(PROW()+1,00);DEVOUT('Mes/Ano     ')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Alu.')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('100%')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Pagm')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('  Bruta R$')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(' Bolsas R$')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Bolsa (%)')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Liquido R$')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Pag.')
                                DEVPOS(PROW(),PCOL()+1);DEVOUT('Recebido R$')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('    Multas')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT(' A rec. R$')
                                DEVPOS(PROW(),PCOL()+2);DEVOUT('Inadip.')
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT(mtraco)
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('Jul/'+mdiretorio)
                        ELSEIF m_valores[i,2] = 8
                                DEVPOS(PROW()+1,00);DEVOUT('Ago/'+mdiretorio)
                        ELSEIF m_valores[i,2] = 9
                                DEVPOS(PROW()+1,00);DEVOUT('Set/'+mdiretorio)
                        ELSEIF m_valores[i,2] = 10
                                DEVPOS(PROW()+1,00);DEVOUT('Out/'+mdiretorio)
                        ELSEIF m_valores[i,2] = 11
                                DEVPOS(PROW()+1,00);DEVOUT('Nov/'+mdiretorio)
                        ELSEIF m_valores[i,2] = 12
                                DEVPOS(PROW()+1,00);DEVOUT('Dez/'+mdiretorio)
                        ENDIF
                        mmes_alu := 0
                        mqtd_bol := 0
                        mmes_bru := 0
                        mmes_bol := 0
                        mmes_liq := 0
                        mmes_pag := 0
                        mmes_rec := 0
                        mmes_fal := 0
                        mmes_mul := 0
                        maux := m_valores[i,2]
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_valores[i,1])
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,3],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,9],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,3] - m_valores[i,9],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,4],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,5],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT((m_valores[i,5] / m_mes[m_valores[i,2],2]) * 100,'9999.9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,6],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,7],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,8],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,10],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,6] - m_valores[i,8],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(((m_valores[i,6] - m_valores[i,8]) / m_valores[i,6]) * 100,'999.99')
                DEVPOS(PROW(),PCOL()+1);DEVOUT('%')
                mtot_alu := mtot_alu + m_valores[i,3]
                mtot_bru := mtot_bru + m_valores[i,4]
                mtot_bol := mtot_bol + m_valores[i,5]
                mtot_liq := mtot_liq + m_valores[i,6]
                mtot_pag := mtot_pag + m_valores[i,7]
                mtot_rec := mtot_rec + m_valores[i,8]
                mtot_fal := mtot_fal + (m_valores[i,6] - m_valores[i,8])
                mtot_mul := mtot_mul + m_valores[i,10]

                mmes_alu := mmes_alu + m_valores[i,3]
                mqtd_bol := mqtd_bol + m_valores[i,9]
                mmes_bru := mmes_bru + m_valores[i,4]
                mmes_bol := mmes_bol + m_valores[i,5]
                mmes_liq := mmes_liq + m_valores[i,6]
                mmes_pag := mmes_pag + m_valores[i,7]
                mmes_rec := mmes_rec + m_valores[i,8]
                mmes_fal := mmes_fal + (m_valores[i,6] - m_valores[i,8])
                mmes_mul := mmes_mul + m_valores[i,10]
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT('------------')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('----')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('----')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('----')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('---------')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('----')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('----------')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('-------')
        DEVPOS(PROW()+1,00);DEVOUT('Totais......')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_alu,'9999')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mqtd_bol,'9999')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_alu-mqtd_bol,'9999')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_bru,'999,999.99')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_bol,'999,999.99')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT((mmes_bol / mmes_bru) * 100,'9999.9999')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_liq,'999,999.99')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_pag,'9999')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_rec,'999,999.99')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_mul,'999,999.99')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(mmes_liq - mmes_rec,'999,999.99')
        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(((mmes_liq - mmes_rec) / mmes_liq) * 100,'999.99')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('%')

        IF PROW() > 50
                mpag ++
                EJECT
                cabecalho(mpag,mtit,mtipo)
        ENDIF
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT(REPLI('=',80))
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('*************** TOTAIS GERAL *****************')
        DEVPOS(PROW()+1,00);DEVOUT('Receita Bruta......R$: '+TRANSFORM(mtot_bru,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total de Bolsas....R$: '+TRANSFORM(mtot_bol,'9,999,999.99')+' - Total de Bolsas...(%): '+TRANSFORM((mtot_bol / mtot_bru) * 100,'9999.9999')+' %')
        DEVPOS(PROW()+1,00);DEVOUT('Sub-Total..........R$: ------------')
        DEVPOS(PROW()+1,00);DEVOUT('Total Liquido......R$: '+TRANSFORM(mtot_liq,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Recebido.....R$: '+TRANSFORM(mtot_rec,'9,999,999.99')+' + Multas R$: '+TRANSFORM(mtot_mul,'9,999,999.99')+' = Total R$: '+TRANSFORM(mtot_rec+mtot_mul,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Sub-Total..........R$: ------------')
        DEVPOS(PROW()+1,00);DEVOUT('Total A Receber....R$: '+TRANSFORM(mtot_liq - mtot_rec,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Inadiplencia......(%): '+TRANSFORM(((mtot_liq - mtot_rec) / mtot_liq) * 100,'999.99')+' %')
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('MAP_FINA.REL')
        ENDIF
        LOOP
ENDDO
RETURN NIL
**************************** F I M **************************

