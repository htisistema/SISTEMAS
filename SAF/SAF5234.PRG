************************************************
* RELATORIO DE ALUNOS BOLSISTA (RESUMO)
************************************************

FUNCTION saf5234
***************
LOCAL mprg:='SAF5234'
LOCAL opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,f,;
      mdesconto,mvlr_men,mtot_men,mtot_des,mger_men,mger_des,mger_aluno,;
      tela,mfaixa,mtipo_fx,mordem,mgstt:=SPACE(4),m_bolsa:={},mgs1,mgs2,;
      mtot_bol:=0

PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIOS RESUMO DE BOLSAS','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        ASIZE(m_bolsa,0)
        mcont_aluno := 0
        mtot_men := 0
        mtot_des := 0
        mger_men := 0
        mger_des := 0
        mger_aluno := 0
        mmes := SPACE(2)
        mquantd = 1
        mfaixa := SPACE(2)
        mtipo_fx := SPACE(1)
        mordem := 1
        mgstt := SPACE(4)
        mgs1 := SPACE(2)
        mgs2 := SPACE(2)
        ***************
        SELE('aluno');ORDSETFOCUS(5)
        GO TOP
        ***************
        mensagem('Digite o mes que Deseja [01...12]:')
        opcao = 'S'
        @ 23,COL()+1 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12,  '
        READ
        IF LASTKEY() = 27
                RETURN NIL
        ENDIF
        mensagem('Tipo de ordem [1]-Respons. [2]-GSTT [3]-Aluno [4]-Matricula:')
        opcao = 'S'
        @ 23,COL()+1 GET mordem PICT '9' RANGE 1,4
        READ
        IF LASTKEY() = 27
                RETURN NIL
        ENDIF
        mensagem('Digite o GSTT ou deixe em branco p/todos:')
        mgstt = '    '
        @ 23,COL()+1 GET mgstt PICT '@!' VALID ver_gstt(mgstt,,'*')
        READ
        IF LASTKEY() = 27
                RETURN NIL
        ENDIF
        mensagem('Digite o Intervalo do Grau e Serie:    a ')
        mgs1 = '  '
        mgs2 = '  '
        @ 23,46 GET mgs1 PICT '@!' WHEN EMPTY(mgstt) VALID mgs1 $ '01,02,03,04,05,11,12,13,14,15,16,17,18,21,22,23,  '
        @ 23,51 GET mgs2 PICT '@!' WHEN EMPTY(mgstt) .AND. ! EMPTY(mgs1) VALID mgs2 $ '01,02,03,04,05,11,12,13,14,15,16,17,18,21,22,23,  '
        READ
        IF LASTKEY() = 27
                RETURN NIL
        ENDIF
        ***************
        SELE('fx');ORDSETFOCUS(1)
        GO TOP
        ***************
        WHILE ! EOF()
                AADD(m_bolsa,{fx->faixa,fx->desconto,fx->descricao,0,0,0})
                SKIP
        ENDDO
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        IF ! imp_arq('BOLSA_IN.REL')
                LOOP
        ENDIF
        IF ! EMPTY(mmes)
                mtit := 'Relacao de Bolsas do mes: '+mmes
                i := 0
                FOR i = 1 TO LEN(m_bolsa)
                        ***************
                        SELE('aluno');ORDSETFOCUS(mordem)
                        GO TOP
                        ***************
                        mpag := 0
                        WHILE ! EOF()
                                IF ver_faixa(mmes) <> m_bolsa[i,1] .OR. EMPTY(aluno->contrato)
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mgstt) .AND. mgstt <> aluno->gstt
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mgs1)
                                        IF VAL(SUBSTR(aluno->gstt,1,2)) < VAL(mgs1) .OR. VAL(SUBSTR(aluno->gstt,1,2)) > VAL(mgs2)
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                IF VAL(mmes) < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                        SKIP
                                        LOOP
                                ENDIF
                                IF ! EMPTY(aluno->dt_tranf)
                                        IF VAL(mmes) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                SKIP
                                                LOOP
                                        ENDIF
                                ENDIF
                                IF aluno->gstt = 'DEBI'
                                        SKIP
                                        LOOP
                                ENDIF
                                mvlr_men := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1),mmes)
                                mdesconto := m_bolsa[i,2]
                                m_bolsa[i,4] := m_bolsa[i,4] + mvlr_men
                                m_bolsa[i,5] := m_bolsa[i,5] + 1
                                m_bolsa[i,6] := m_bolsa[i,6] + ROUND((mvlr_men * (mdesconto/100)),2)
                                mger_aluno ++
                                mger_men := mger_men + mvlr_men
                                mger_des := mger_des + ROUND((mvlr_men * (mdesconto/100)),2)
                                SKIP
                        ENDDO
                NEXT
        ELSE
                mtit := 'Relacao de Bolsas do 12 meses'
                f := 0
                FOR f = 1 TO 12
                        i := 0
                        FOR i = 1 TO LEN(m_bolsa)
                                ***************
                                SELE('aluno');ORDSETFOCUS(mordem)
                                GO TOP
                                ***************
                                mpag := 0
                                WHILE ! EOF()
                                        IF ver_faixa(STRZERO(f,2)) <> m_bolsa[i,1] .OR. EMPTY(aluno->contrato)
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mgstt) .AND. mgstt <> aluno->gstt
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(mgs1)
                                                IF VAL(SUBSTR(aluno->gstt,1,2)) < VAL(mgs1) .OR. VAL(SUBSTR(aluno->gstt,1,2)) > VAL(mgs2)
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDIF
                                        IF f < VAL(SUBSTR(DTOC(aluno->data_ing),4,2))
                                                SKIP
                                                LOOP
                                        ENDIF
                                        IF ! EMPTY(aluno->dt_tranf)
                                                IF VAL(STRZERO(f,2)) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                                        SKIP
                                                        LOOP
                                                ENDIF
                                        ENDIF
                                        IF aluno->gstt = 'DEBI'
                                                SKIP
                                                LOOP
                                        ENDIF
                                        mvlr_men := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1),STRZERO(f,2))
                                        mdesconto := m_bolsa[i,2]
                                        m_bolsa[i,4] := m_bolsa[i,4] + mvlr_men
                                        m_bolsa[i,5] := m_bolsa[i,5] + 1
                                        m_bolsa[i,6] := m_bolsa[i,6] + ROUND((mvlr_men * (mdesconto/100)),2)
                                        mger_aluno ++
                                        mger_men := mger_men + mvlr_men
                                        mger_des := mger_des + ROUND((mvlr_men * (mdesconto/100)),2)
                                        SKIP
                                ENDDO
                        NEXT
                NEXT
        ENDIF
        mtot_bol:=0
        mtipo := 'Resumo das Bolsa'
        IF ! EMPTY(mgstt)
                mtipo := mtipo+' -  GSTT: '+mgstt
        ELSEIF ! EMPTY(mgs1)
                mtipo := mtipo+' -  Intervalo GRAU e SERIE: '+mgs1+' a '+mgs2
        ENDIF
        IF PROW() > 57 .OR. mpag = 0
                IF mpag > 0
                        EJECT
                ENDIF
                mpag := mpag + 1
                cabecalho(mpag,mtit,mtipo,mprg)
                DEVPOS(PROW()+1,00);DEVOUT('Tipo')
                DEVPOS(PROW(),6);DEVOUT('% Desc.')
                DEVPOS(PROW(),17);DEVOUT('Quantd.')
                DEVPOS(PROW(),27);DEVOUT('Vlr.Mensal.')
                DEVPOS(PROW(),42);DEVOUT('Vlr.Bolsas')
                DEVPOS(PROW(),55);DEVOUT('Vlr.Receber')
                DEVPOS(PROW(),68);DEVOUT('Bolsas 100%')
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        ENDIF
        imprt('M','N')
        i := 0
        FOR i = 1 TO LEN(m_bolsa)
                IF EMPTY(m_bolsa[i,5])
                        LOOP
                ENDIF
                DEVPOS(PROW()+1,01);DEVOUT(m_bolsa[i,1])
                DEVPOS(PROW(),07);DEVOUT(TRANSFORM(m_bolsa[i,2],'999.99'))
                DEVPOS(PROW(),19);DEVOUT(TRANSFORM(m_bolsa[i,5],'9,999'))
                DEVPOS(PROW(),28);DEVOUT(TRANSFORM(m_bolsa[i,4],'999,999.99'))
                DEVPOS(PROW(),42);DEVOUT(TRANSFORM(m_bolsa[i,6],'999,999.99'))
                DEVPOS(PROW(),56);DEVOUT(TRANSFORM(m_bolsa[i,4] - m_bolsa[i,6],'999,999.99'))
                IF m_bolsa[i,2] = 0
                        DEVPOS(PROW(),70);DEVOUT(TRANSFORM(0,'9,999.99'))
                ELSE
                        DEVPOS(PROW(),70);DEVOUT(TRANSFORM(ROUND((m_bolsa[i,2]/100)*m_bolsa[i,5],2),'9,999.99'))
                        mtot_bol := mtot_bol + ROUND(((m_bolsa[i,2]/100)*m_bolsa[i,5]),2)
                ENDIF
        NEXT
        DEVPOS(PROW()+1,15);DEVOUT('---------')
        DEVPOS(PROW(),26);DEVOUT('------------')
        DEVPOS(PROW(),40);DEVOUT('------------')
        DEVPOS(PROW(),54);DEVOUT('------------')
        DEVPOS(PROW(),68);DEVOUT('----------')
        DEVPOS(PROW()+1,00);DEVOUT('TOTAL GERAL:')
        DEVPOS(PROW(),18);DEVOUT(TRANSFORM(mger_aluno,'99,999'))
        DEVPOS(PROW(),26);DEVOUT(TRANSFORM(mger_men,'9,999,999.99'))
        DEVPOS(PROW(),40);DEVOUT(TRANSFORM(mger_des,'9,999,999.99'))
        DEVPOS(PROW(),54);DEVOUT(TRANSFORM(mger_men-mger_des,'9,999,999.99'))
        DEVPOS(PROW(),68);DEVOUT(TRANSFORM(mtot_bol,'999,999.99'))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('BOLSA_IN.REL')
        ENDIF
        RETURN NIL
ENDDO
RETURN NIL
**************************** F I M **************************









































