************************************************
* RELATORIO DE ALUNOS BOLSISTA GERAL
************************************************

FUNCTION saf5232
***************
LOCAL mprg:='SAF5232'
LOCAL opcao,mquantd,mtraco,mtit,mtipo,mpag,mmes,i,mcont_aluno,;
      mdesconto,mvlr_men,mtot_men,mtot_des,mger_men,mger_des,mger_aluno,;
      tela,mfaixa,mordem,m_mensa[12],m_desc[12],mmatricula,msele,morde,mgstt:=SPACE(4)
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIOS DE ALUNOS BOLSISTAS','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',132)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mcont_aluno := 0
        mtot_men := 0
        mtot_des := 0
        mger_men := 0
        mger_des := 0
        mger_aluno := 0
        mmes := SPACE(2)
        mquantd = 1
        mfaixa := SPACE(2)
        mordem := 1
        ***************
        SELE('aluno');ORDSETFOCUS(5)
        GO TOP
        ***************
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
        ***************
        SELE('aluno');ORDSETFOCUS(mordem)
        GO TOP
        ***************
        FOR i = 1 TO 12
                m_mensa[i] := 0
                m_desc[i] := 0
        NEXT
        IF ! imp_arq('BOLSA_GE.REL')
                LOOP
        ENDIF
        mfaixa := ver_faixa(mmes)
        mtit := 'Relatorio Financeiro de Alunos com Bolsa'
        mtipo:= '****************************************'
        mpag := 0
        WHILE ! EOF()
                IF PROW() > 57 .OR. mpag = 0
                        IF mpag > 0
                                EJECT
                        ENDIF
                        mpag := mpag + 1
                        cabecalho(mpag,mtit,mtipo)
                        imprt('M','C')
                        DEVPOS(PROW()+1,00);DEVOUT(' Matric.')
                        DEVPOS(PROW(),10);DEVOUT('Aluno')
                        DEVPOS(PROW(),52);DEVOUT('Vlr Mens')
                        DEVPOS(PROW(),62);DEVOUT('Desconto')
                        DEVPOS(PROW(),72);DEVOUT(' Receber')
                        DEVPOS(PROW(),92);DEVOUT('GSTT')
                        DEVPOS(PROW(),98);DEVOUT('Responsavel')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                ENDIF
                IF ! EMPTY(mgstt) .AND. mgstt <> aluno->gstt
                        SKIP
                        LOOP
                ENDIF
                IF aluno->gstt = 'DEBI'
                        SKIP
                        LOOP
                ENDIF
                mmatricula := aluno->matricula
                msele := SELE()
                morde := INDEXORD()
                ***************
                SELE('cdmen');ORDSETFOCUS(1)
                GO TOP
                ***************
                IF ! cdmen->(DBSEEK(mmatricula+mdiretorio))
                        ***************
                        SELE(msele);ORDSETFOCUS(morde)
                        ***************
                        SKIP
                        LOOP
                ENDIF
                WHILE mmatricula = cdmen->matricula .AND. cdmen->ano = mdiretorio .AND. ! EOF()
                        IF cdmen->tipo <> 'ME'  //.OR. cdmen->ano <> mdiretorio
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                IF VAL(cdmen->parcela) >= VAL(SUBSTR(DTOC(aluno->dt_tranf),4,2))
                                        SKIP
                                        LOOP
                                ENDIF
                        ENDIF
*                       mvlr_men := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1),STRZERO(i,2))
                        mvlr_men := cdmen->vlr_parc
                        mdesconto := faixa_vlr(cdmen->bolsa)
*                       IF ! EMPTY(mdesconto)
                                mtot_men := mtot_men + mvlr_men
                                mtot_des := mtot_des + (mvlr_men * (mdesconto/100))
                                m_mensa[VAL(cdmen->parcela)]:=m_mensa[VAL(cdmen->parcela)] + mvlr_men
                                m_desc[VAL(cdmen->parcela)]:=m_desc[VAL(cdmen->parcela)] + (mvlr_men * (mdesconto/100))
*                       ENDIF
                        SKIP
                ENDDO
                IF EMPTY(mtot_men)
                        ***************
                        SELE(msele);ORDSETFOCUS(morde)
                        ***************
                        SKIP
                        LOOP
                ENDIF

                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                DEVPOS(PROW(),52);DEVOUT(TRANSFORM(mtot_men,'9,999.99'))
                DEVPOS(PROW(),62);DEVOUT(TRANSFORM(mtot_des,'9,999.99'))
                DEVPOS(PROW(),72);DEVOUT(TRANSFORM(mtot_men - mtot_des,'9,999.99'))
                DEVPOS(PROW(),92);DEVOUT(aluno->gstt)
                DEVPOS(PROW(),98);DEVOUT(aluno->responsa)

                mger_aluno ++
                mger_men := mger_men + mtot_men
                mger_des := mger_des + mtot_des
                mtot_men := 0
                mtot_des := 0
                ***************
                SELE(msele);ORDSETFOCUS(morde)
                ***************
                SKIP
        ENDDO
        imprt('M','N')
        DEVPOS(PROW()+2,00);DEVOUT('T O T A L    G E R A L')
        DEVPOS(PROW()+1,00);DEVOUT('Quantidade de Alunos: '+TRANSFORM(mger_aluno,'999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total em Mensalidade: '+TRANSFORM(mger_men,'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total de Desconto...: '+TRANSFORM(mger_des,'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total a Receber.....: '+TRANSFORM(mger_men-mger_des,'99,999,999.99'))
        EJECT

        mpag ++
        cabecalho(mpag,mtit,mtipo)
        imprt('M','C')
        DEVPOS(PROW()+2,00);DEVOUT('************** RESUMO *****************')
        DEVPOS(PROW()+1,20);DEVOUT('Valor Mensal      Desconto     Receber')
        DEVPOS(PROW()+1,00);DEVOUT('Mes de JANEIRO..: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[1],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[1],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[1] - m_desc[1],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de FEVEREIRO: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[2],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[2],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[2] - m_desc[2],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de MARCO....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[3],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[3],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[3] - m_desc[3],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de ABRIL....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[4],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[4],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[4] - m_desc[4],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de MAIO.....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[5],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[5],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[5] - m_desc[5],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de JUNHO....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[6],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[6],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[6] - m_desc[6],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de JULHO....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[7],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[7],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[7] - m_desc[7],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de AGOSTO...: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[8],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[8],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[8] - m_desc[8],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de SETEMBRO.: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[9],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[9],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[9] - m_desc[9],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de OUTUBRO..: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[10],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[10],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[10] - m_desc[10],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de NOVEMBRO.: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[11],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[11],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[11] - m_desc[11],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Mes de DEZEMBRO.: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[12],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_desc[12],'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(m_mensa[12] - m_desc[12],'99,999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('                   ------------- ------------- -------------')
        DEVPOS(PROW()+1,00);DEVOUT('TOTAL GERAL.....: ')
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mger_men,'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mger_des,'99,999,999.99'))
        DEVPOS(PROW(),PCOL()+1);DEVOUT(TRANSFORM(mger_men - mger_des,'99,999,999.99'))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('BOLSA_GE.REL')
        ENDIF
        RETURN NIL
ENDDO
RETURN NIL
**************************** F I M **************************

