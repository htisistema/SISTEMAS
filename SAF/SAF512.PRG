************************************************
* TABELA DE BOLSAS
************************************************

FUNCTION saf512
***************
LOCAL mprg:='SAF512'
LOCAL opcao,mtraco,mtit,mtipo

IF ! ver_nivel(mprg,'RELATORIO TABELAS DE BOLSAS','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mensagem('Confirma a Impressao [S/n]:')
        opcao = 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF opcao = 'N'
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF LASTKEY() = 27
                LOOP
        ENDIF
        ********************************
        SELE('fx');ORDSETFOCUS(1)
        GO TOP
        ********************************
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        mrel := 'LPT1'
        SET DEVI TO PRINT
*       SET PRINT TO 'LPT1'
        mcont_aluno := 0
        mpag := 1
        mtit := 'T A B E L A   D E   B O L S A S'
        cabecalho(mpag,mtit)
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT('Faixa')
        DEVPOS(PROW(),08);DEVOUT('Desconto')
        DEVPOS(PROW(),22);DEVOUT('Descricao')
        DEVPOS(PROW(),35);DEVOUT('Operador')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        WHILE ! EOF()
                INKEY(.1)
                IF LASTKEY() = 27
                        DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                        EXIT
                ENDIF
                DEVPOS(PROW()+1,02);DEVOUT(fx->faixa)
                DEVPOS(PROW(),11);DEVOUT(TRANSFORM(fx->desconto,'999.99'))
                DEVPOS(PROW(),22);DEVOUT(fx->descricao)
                DEVPOS(PROW(),38);DEVOUT(fx->operador)
                SKIP
                IF PROW() >= 58
                        EJECT
                        mpag ++
                        cabecalho(mpag,mtit)
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT('Faixa')
                        DEVPOS(PROW(),08);DEVOUT('Desconto')
                        DEVPOS(PROW(),22);DEVOUT('Descricao')
                        DEVPOS(PROW(),35);DEVOUT('Operador')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        LOOP
                ENDIF
        ENDDO
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        CLOSE ALL
        RETURN NIL
ENDDO

