************************************************
* RELATORIO DE ALUNOS BOLSISTA
************************************************

FUNCTION saf59
***************
LOCAL mprg:='SAF59'
LOCAL opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mdesconto,mvlr_men,mtot_men,mtot_des,mger_men,mger_des,mger_aluno,;
      tela,mfaixa,mtipo_fx

IF ! ver_nivel(mprg,'RELATORIO DE ALUNOS BOLSISTAS','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
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
        mtipo_fx := SPACE(1)
        ***************
        SELE('aluno');ORDSETFOCUS(5)
        GO TOP
        ***************
        mensagem('Informa a Bolsa que deseja ou ENTER p/todos [A...Z]:')
        @ 23,COL()+1 GET mtipo_fx
        READ
        IF LASTKEY() = 27
                RETURN NIL
        ENDIF
        mensagem('Digite o mes que Deseja [01...12]:')
        opcao = 'S'
        @ 23,COL()+1 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        READ
        IF LASTKEY() = 27
                RETURN NIL
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
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        SET DEVI TO PRINT
        * * SET PRINT TO LPT1
        mfaixa := aluno->faixa_1
        mpag := 0
        WHILE ! EOF()
                IF LASTKEY() = 27
                        DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                        EXIT
                ENDIF
                IF ! EMPTY(mtipo_fx)
                        IF aluno->faixa_1 <> mtipo_fx
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                mfaixa := faixa_1
                WHILE mfaixa = aluno->faixa_1
                        mdesconto := faixa_vlr(aluno->faixa_1)
                        IF PROW() > 57 .OR. mpag = 0
                                EJECT
                                mpag := mpag + 1
                                mtit := 'Relacao dos Alunos Bolsista do mes: '+mmes
                                mtipo := 'Valor da Bolsa: '+TRANSFORM(mdesconto,'999.99')+' %'
                                cabecalho(mpag,mtit,mtipo)
                                imprt('M','C',1)
                                DEVPOS(PROW(),00);DEVOUT(' Matric.')
                                DEVPOS(PROW(),10);DEVOUT('Aluno')
                                DEVPOS(PROW(),52);DEVOUT('Vlr Mens')
                                DEVPOS(PROW(),62);DEVOUT('Desconto')
                                DEVPOS(PROW(),72);DEVOUT(' Receber')
                                DEVPOS(PROW(),82);DEVOUT('Tp.Bolsa')
                                DEVPOS(PROW(),92);DEVOUT('GSTT')
                                imprt('M','N',1)
                                DEVPOS(PROW(),00);DEVOUT(mtraco)
                                imprt('M','C')
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mvlr_men := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4,1),mmes)
                        mdesconto := faixa_vlr(aluno->faixa_1)
                        imprt('M','C',1)
                        DEVPOS(PROW(),00);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),52);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                        DEVPOS(PROW(),62);DEVOUT(TRANSFORM(mvlr_men * (mdesconto/100),'9,999.99'))
                        DEVPOS(PROW(),72);DEVOUT(TRANSFORM(mvlr_men - (mvlr_men * (mdesconto/100)),'9,999.99'))
                        DEVPOS(PROW(),86);DEVOUT(aluno->faixa_1)
                        DEVPOS(PROW(),92);DEVOUT(aluno->gstt)
                        mcont_aluno ++
                        mtot_men := mtot_men + mvlr_men
                        mtot_des := mtot_des + (mvlr_men * (mdesconto/100))
                        mger_aluno ++
                        mger_men := mger_men + mvlr_men
                        mger_des := mger_des + (mvlr_men * (mdesconto/100))
                        SKIP
                ENDDO
                mpag := 0
                mfaixa := aluno->faixa_1
                DEVPOS(PROW()+1,00);DEVOUT('T O T A I S')
                DEVPOS(PROW()+1,00);DEVOUT('Quantidade de Alunos: '+TRANSFORM(mcont_aluno,'9,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total em Mensalidade: '+TRANSFORM(mtot_men,'999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total de Desconto...: '+TRANSFORM(mtot_des,'999,999.99'))
                DEVPOS(PROW()+1,00);DEVOUT('Total a Receberto...: '+TRANSFORM(mtot_men-mtot_des,'999,999.99'))
                mcont_aluno := 0
                mtot_men := 0
                mtot_des := 0
        ENDDO
        imprt('M','N',1)
        DEVPOS(PROW()+1,00);DEVOUT('T O T A L    G E R A L')
        DEVPOS(PROW()+1,00);DEVOUT('Quantidade de Alunos: '+TRANSFORM(mger_aluno,'9,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total em Mensalidade: '+TRANSFORM(mger_men,'999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total de Desconto...: '+TRANSFORM(mger_des,'999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT('Total a Receber.....: '+TRANSFORM(mger_men-mger_des,'999,999.99'))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RETURN NIL
ENDDO
RETURN NIL
**************************** F I M **************************

