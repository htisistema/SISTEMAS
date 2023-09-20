************************************************
* MAPA FINANCEIRO
************************************************

FUNCTION saf5211
***************
LOCAL mprg:='SAF5211'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,y,mvalor,mfaixa,;
      tela,mpoint,maux,mmat_aux,mdesconto:=0,mgstt
LOCAL mtot_alu,mtot_pag,mtot_bru,mtot_bol,mtot_liq,mtot_rec,mtot_fal,mtot_mul,;
      mmes_alu,mmes_pag,mmes_bru,mmes_bol,mmes_liq,mmes_rec,mmes_fal,mmes_mul,mqtd_bol,;
      mtot_desc := 0
PRIVATE m_valores:={},m_mes:={}
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DO MAPA FINANCEIRO','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',137)
lci := 17
cci := 10
lba := 19
cba := 50
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
        ASIZE(m_valores,0)
        ASIZE(m_mes,0)
        mensagem('Espere o Coletando informacoes p/o MAPA FINANCEIRO - [ESC]Abandonar')
        i := 0
        FOR i = 1 TO VAL(mmes)
                AADD(m_valores,{'Totais.....:',i,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0})
                AADD(m_mes,{i,0})
        NEXT
        y := 0
        i := 0
        mmat_aux := SPACE(6)
        ***************
        SELE('cdmen');ORDSETFOCUS(5)
        GO TOP
        ***************
        FOR i = 1 TO VAL(mmes)
                ***************
                SELE('aluno');ORDSETFOCUS(3)
                GO TOP
                ***************
                WHILE ! EOF()
                        IF (aluno->gstt = 'DEBI');
                           .OR. (EMPTY(aluno->contrato));
                           .OR. (i < VAL(SUBSTR(DTOC(aluno->data_ing),4,2)));
                           .OR. (! EMPTY(aluno->dt_tranf) .AND. i >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2)))
                                SKIP
                                LOOP
                        ENDIF
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := vlr_men(mgstt,STRZERO(i,2))
                        mdesconto := faixa_vlr(ver_faixa(STRZERO(i,2)))
                        m_valores[i,3] := m_valores[i,3] + 1                       // total de alunos

*                       m_valores[i,4] := m_valores[i,4] + mvlr_men              // Receita Bruta
*                       m_valores[i,5] := m_valores[i,5] + ROUND((mvlr_men * (mdesconto/100)),2) // desconto das bolsas
*                       m_valores[i,6] := m_valores[i,6] + (mvlr_men - ROUND((mvlr_men * (mdesconto/100)),2)) // Receita Liquida

                        IF mdesconto = 100
                                m_valores[i,9] := m_valores[i,9] + 1   // BOLSA DE 100%
                        ELSEIF mdesconto > 1
                                 m_valores[i,12] := m_valores[i,12] + 1  // BOLSA PARCIAL
                        ENDIF
                        m_mes[i,2] := m_mes[i,2] + mvlr_men
                        SKIP
                ENDDO
                ***************
                SELE('cdmen');ORDSETFOCUS(5)
                GO TOP
                cdmen->(DBSEEK(STRZERO(i,2)))
                ***************
*               AADD(m_valores,{'Totais.....:',i,0,0,0,0,0,0,0,0,0,0})
                WHILE STRZERO(i,2) = cdmen->parcela .AND. ! EOF()
                        IF mdiretorio <> cdmen->ano;
                           .OR. cdmen->gstt = 'DEBI'
                                SKIP
                                LOOP
                        ENDIF
                        IF cdmen->tipo <> 'ME'
                                m_valores[i,16] := m_valores[i,16] + cdmen->vlr_parc
                                m_valores[i,17] := m_valores[i,17] + 1
                                IF ! EMPTY(cdmen->data_pag)
                                        m_valores[i,20] := m_valores[i,20] + (cdmen->vlr_parc - cdmen->valor)
                                        m_valores[i,18] := m_valores[i,18] + cdmen->valor
                                        m_valores[i,19] := m_valores[i,19] + 1
                                ENDIF
                                SKIP
                                LOOP
                        ENDIF
                        IF ! ver_aluno(cdmen->matricula,'*')
                                SKIP
                                LOOP
                        ENDIF
                        IF (aluno->gstt = 'DEBI');
                           .OR. (EMPTY(aluno->contrato));
                           .OR. (i < VAL(SUBSTR(DTOC(aluno->data_ing),4,2)));
                           .OR. (! EMPTY(aluno->dt_tranf) .AND. i >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2)))
                                SKIP
                                LOOP
                        ENDIF
                        mgstt := SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1)
                        mvlr_men := vlr_men(mgstt,STRZERO(i,2))
                        mdesconto := ROUND(faixa_vlr(ver_faixa(STRZERO(i,2))),2)
                        IF mdesconto < 100
                        ENDIF
                        m_valores[i,2] := i  // parcela
                        m_valores[i,15] := m_valores[i,15] + ROUND((cdmen->vlr_parc - cdmen->desc_bolsa),2)
                        // TESTE
*                       m_valores[i,4] := m_valores[i,4] + mvlr_men    // Receita Bruta
                        m_valores[i,4] := m_valores[i,4] + cdmen->vlr_parc    // Receita Bruta
                        m_valores[i,5] := m_valores[i,5] + cdmen->desc_bolsa // desconto das bolsas
                        m_valores[i,6] := m_valores[i,6] + ROUND((cdmen->vlr_parc - cdmen->desc_bolsa),2)  // Receita Liquida
                        // TESTE
                        IF cdmen->valor > 0
                                m_valores[i,7] := m_valores[i,7] + 1
                                m_valores[i,8] := m_valores[i,8] + ROUND((cdmen->valor-cdmen->multa),2) // vlr.recebido
                                m_valores[i,10] := m_valores[i,10] + cdmen->multa
                                m_valores[i,11] := m_valores[i,11] + (ROUND((cdmen->vlr_parc - cdmen->desc_bolsa),2) - ROUND((cdmen->valor - cdmen->multa),2))         // Descontos
                        ELSE
                                IF mdesconto < 100
                                        m_valores[i,13] := m_valores[i,13] + ROUND((cdmen->vlr_parc - cdmen->desc_bolsa),2)
                                        m_valores[i,14] := m_valores[i,14] + 1
                                ENDIF
                        ENDIF
                        SKIP
                ENDDO
        NEXT
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        IF ! imp_arq('MAP_FINA.REL')
                LOOP
        ENDIF
        mpag := 1
        mtit := 'MAPA FINANCEIRO DAS MENSALIDADES ESCOLAR'
        mtipo := 'ate o mes de '+mmes+'/'+mdiretorio
        cabecalho(mpag,mtit,mtipo)
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('            ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Qtd.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Bols')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Bols')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Sem ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('   Receita')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('  Total de')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Tot. ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Qtd.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Qtd.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('(%)    ')
        DEVPOS(PROW()+1,00);DEVOUT('Mes/Ano     ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Alu.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('100%')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Parc')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Bols')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('  Bruta R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT(' Bolsas R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Bolsa')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Liquido R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Pag.')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('Recebido R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('    Multas')
        DEVPOS(PROW(),PCOL()+2);DEVOUT(' A rec. R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Inad.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Inadip.')
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        maux := 1
        mtot_alu := 0
        mtot_bru := 0
        mtot_bol := 0
        mtot_liq := 0
        mtot_pag := 0
        mtot_rec := 0
        mtot_fal := 0
        mtot_mul := 0
        mtot_desc := 0

        mqtd_bol := 0
        mmes_alu := 0
        mmes_bru := 0
        mmes_bol := 0
        mmes_liq := 0
        mmes_pag := 0
        mmes_rec := 0
        mmes_fal := 0
        mmes_mul := 0
        i := 0
        FOR i = 1 TO VAL(mmes)
                IF i = 1
                        DEVPOS(PROW()+1,00);DEVOUT('Jan/'+mdiretorio)
                ELSEIF i = 2
                        DEVPOS(PROW()+1,00);DEVOUT('Fev/'+mdiretorio)
                ELSEIF i = 3
                        DEVPOS(PROW()+1,00);DEVOUT('Mar/'+mdiretorio)
                ELSEIF i = 4
                        DEVPOS(PROW()+1,00);DEVOUT('Abr/'+mdiretorio)
                ELSEIF i = 5
                        DEVPOS(PROW()+1,00);DEVOUT('Mai/'+mdiretorio)
                ELSEIF i = 6
                        DEVPOS(PROW()+1,00);DEVOUT('Jun/'+mdiretorio)
                ELSEIF i = 7
                        DEVPOS(PROW()+1,00);DEVOUT('Jul/'+mdiretorio)
                ELSEIF i = 8
                        DEVPOS(PROW()+1,00);DEVOUT('Ago/'+mdiretorio)
                ELSEIF i = 9
                        DEVPOS(PROW()+1,00);DEVOUT('Set/'+mdiretorio)
                ELSEIF i = 10
                        DEVPOS(PROW()+1,00);DEVOUT('Out/'+mdiretorio)
                ELSEIF i = 11
                        DEVPOS(PROW()+1,00);DEVOUT('Nov/'+mdiretorio)
                ELSEIF i = 12
                        DEVPOS(PROW()+1,00);DEVOUT('Dez/'+mdiretorio)
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_valores[i,1])
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,3],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,9],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,12],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,3] - (m_valores[i,12] + m_valores[i,9]),'9999')
*               DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,3] - m_valores[i,9],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,4],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,5],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT((m_valores[i,5] / m_valores[i,4]) * 100,'9999')
                DEVPOS(PROW(),PCOL());DEVOUT('%')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,6],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,7],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,8],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,10],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,13],'999,999.99')
*               DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,6] - m_valores[i,8],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,14],'9999')
                IF EMPTY(m_valores[i,13])
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(0,'999.99')
                ELSE
                        DEVPOS(PROW(),PCOL()+2);DEVOUTPICT((m_valores[i,13] / m_valores[i,4]) * 100,'999.99')
*                       DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(((m_valores[i,6] - m_valores[i,8]) / m_valores[i,4]) * 100,'999.99')
                ENDIF
                DEVPOS(PROW(),PCOL()+1);DEVOUT('%')
*               DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,15],'999,999.99')
*               DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,11],'999,999.99')
                mtot_alu := mtot_alu + m_valores[i,3]
                mtot_bru := mtot_bru + m_valores[i,4]
                mtot_bol := mtot_bol + m_valores[i,5]
                mtot_liq := mtot_liq + m_valores[i,6]
                mtot_pag := mtot_pag + m_valores[i,7]
                mtot_rec := mtot_rec + m_valores[i,8]
                mtot_fal := mtot_fal + (m_valores[i,6] - m_valores[i,8])
                mtot_mul := mtot_mul + m_valores[i,10]
                mtot_desc := mtot_desc + m_valores[i,11]
        NEXT

        IF PROW() > 50
                mpag ++
                EJECT
                cabecalho(mpag,mtit,mtipo)
        ENDIF
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT(REPLI('=',80))
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('*************** TOTAIS GERAIS ****************')
        DEVPOS(PROW()+1,00);DEVOUT('Receita Bruta......R$: '+TRANSFORM(mtot_bru,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total de Bolsas....R$: '+TRANSFORM(mtot_bol,'9,999,999.99')+' - Total de Bolsas...(%): '+TRANSFORM((mtot_bol / mtot_bru) * 100,'9999.9999')+' %')
        DEVPOS(PROW()+1,00);DEVOUT('Sub-Total..........R$: ------------')
        DEVPOS(PROW()+1,00);DEVOUT('Total Liquido......R$: '+TRANSFORM(mtot_liq,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Recebido.....R$: '+TRANSFORM(mtot_rec,'9,999,999.99')+' + Multas R$: '+TRANSFORM(mtot_mul,'9,999,999.99')+' = Total R$: '+TRANSFORM(mtot_rec+mtot_mul,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Descontos....R$: '+TRANSFORM(mtot_desc,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Sub-Total..........R$: ------------')
        DEVPOS(PROW()+1,00);DEVOUT('Total A Receber....R$: '+TRANSFORM(mtot_liq - mtot_rec - mtot_desc,'9,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Inadimplencia.....(%): '+TRANSFORM(((mtot_liq - mtot_rec - mtot_desc) / mtot_bru) * 100,'999.99')+' %')
        EJECT
        SETPRC(00,00)


        mpag := 1
        mtit := 'MAPA FINANCEIRO DAS MENSALIDADES ESPORTIVAS'
        mtipo := 'ate o mes de '+mmes+'/'+mdiretorio
        cabecalho(mpag,mtit,mtipo)
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('            ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Qtd.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('   Receita')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Men.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Total')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Qtd.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('(%)    ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('     Valor')
        DEVPOS(PROW()+1,00);DEVOUT('Mes/Ano     ')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Men.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('  Bruta R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Paga')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('Recebido R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT(' A rec. R$')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Inad.')
        DEVPOS(PROW(),PCOL()+2);DEVOUT('Inadip.')
        DEVPOS(PROW(),PCOL()+1);DEVOUT('Desconto R$')
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        maux := 1
        mtot_alu := 0
        mtot_bru := 0
        mtot_pag := 0
        mtot_fal := 0
        mtot_desc := 0
        i := 0
        FOR i = 1 TO VAL(mmes)
                IF i = 1
                        DEVPOS(PROW()+1,00);DEVOUT('Jan/'+mdiretorio)
                ELSEIF i = 2
                        DEVPOS(PROW()+1,00);DEVOUT('Fev/'+mdiretorio)
                ELSEIF i = 3
                        DEVPOS(PROW()+1,00);DEVOUT('Mar/'+mdiretorio)
                ELSEIF i = 4
                        DEVPOS(PROW()+1,00);DEVOUT('Abr/'+mdiretorio)
                ELSEIF i = 5
                        DEVPOS(PROW()+1,00);DEVOUT('Mai/'+mdiretorio)
                ELSEIF i = 6
                        DEVPOS(PROW()+1,00);DEVOUT('Jun/'+mdiretorio)
                ELSEIF i = 7
                        DEVPOS(PROW()+1,00);DEVOUT('Jul/'+mdiretorio)
                ELSEIF i = 8
                        DEVPOS(PROW()+1,00);DEVOUT('Ago/'+mdiretorio)
                ELSEIF i = 9
                        DEVPOS(PROW()+1,00);DEVOUT('Set/'+mdiretorio)
                ELSEIF i = 10
                        DEVPOS(PROW()+1,00);DEVOUT('Out/'+mdiretorio)
                ELSEIF i = 11
                        DEVPOS(PROW()+1,00);DEVOUT('Nov/'+mdiretorio)
                ELSEIF i = 12
                        DEVPOS(PROW()+1,00);DEVOUT('Dez/'+mdiretorio)
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_valores[i,1])
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,17],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,16],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,19],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,18],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,16] - m_valores[i,18],'999,999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,17] - m_valores[i,19],'9999')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(((m_valores[i,16] - m_valores[i,18]) / m_valores[i,16]) * 100,'999.99')
                DEVPOS(PROW(),PCOL()+2);DEVOUTPICT(m_valores[i,20],'999,999.99')
                mtot_alu := mtot_alu + m_valores[i,17]
                mtot_bru := mtot_bru + m_valores[i,16]
                mtot_pag := mtot_pag + m_valores[i,18]
                mtot_fal := mtot_fal + m_valores[i,19]
                mtot_desc := mtot_desc + m_valores[i,20]
        NEXT

        IF PROW() > 50
                mpag ++
                EJECT
                cabecalho(mpag,mtit,mtipo)
        ENDIF
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT(REPLI('=',80))
        imprt('M','C')
        DEVPOS(PROW()+1,00);DEVOUT('*************** TOTAIS GERAIS *****************')
        DEVPOS(PROW()+1,00);DEVOUT('Receita Bruta......R$: '+TRANSFORM(mtot_bru,'9,999,999.99')+'  -  Quantd.Mensalidades..: '+TRANSFORM(mtot_alu,'999,999'))
        DEVPOS(PROW()+1,00);DEVOUT('Total Recebido.....R$: '+TRANSFORM(mtot_pag,'9,999,999.99')+'  -  Quantd.Mensalidades..: '+TRANSFORM(mtot_fal,'999,999'))
        DEVPOS(PROW()+1,00);DEVOUT('Sub-Total..........R$: ------------')
        DEVPOS(PROW()+1,00);DEVOUT('Total A Receber....R$: '+TRANSFORM(mtot_bru - mtot_pag,'9,999,999.99')+'  -  Quantd.Mensalidades..: '+TRANSFORM(mtot_alu - mtot_fal,'999,999'))
        DEVPOS(PROW()+1,00);DEVOUT('Inadimplencia.....(%): '+TRANSFORM(((mtot_bru - mtot_pag) / mtot_bru) * 100,'999.99')+' %')
        DEVPOS(PROW()+1,00);DEVOUT('Total Descontos....R$: '+TRANSFORM(mtot_desc,'9,999,999.99'))
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

