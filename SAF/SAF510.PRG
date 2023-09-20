************************************************
* TABELA DE DISCIPLINA
************************************************

FUNCTION saf510
***************
LOCAL mprg := 'SAF510'
LOCAL opcao,mtraco,mtit,mtipo

IF ! ver_nivel(mprg,'RELATORIO TABELAS DE DISCIPLINAS','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
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
        SELE('disc');ORDSETFOCUS(1)
        GO TOP
        ********************************
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        mrel := 'LPT1'
        SET DEVI TO PRINT
        SET PRINT TO 'LPT1'
        mcont_aluno := 0
        mpag := 1
        mtit := 'T A B E L A   D E   D I S C I P L I N A'
        cabecalho(mpag,mtit)
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT('Codigo')
        DEVPOS(PROW(),09);DEVOUT('Disciplina')
        DEVPOS(PROW(),32);DEVOUT('Tipo')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        i := 0
        WHILE ! EOF()
                INKEY(.1)
                IF LASTKEY() = 27
                        DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                        EXIT
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(disc->cod_disc)
                DEVPOS(PROW(),09);DEVOUT(disc->descr)
                DEVPOS(PROW(),32);DEVOUT(disc->tipo)
                SKIP
                IF PROW() >= 58
                        EJECT
                        mpag ++
                        cabecalho(mpag,mtit)
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT('Codigo')
                        DEVPOS(PROW(),09);DEVOUT('Disciplina')
                        DEVPOS(PROW(),32);DEVOUT('Tipo')
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

