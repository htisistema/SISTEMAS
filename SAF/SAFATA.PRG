************************************************
* ATA DE ALUNOS P/PROFESSORES
************************************************

FUNCTION safata
***************
LOCAL mprg:='SAFATA'
LOCAL opcao,mquantd,mtraco,mtit,mtipo,mcont_aluno,mgstt

IF ! ver_nivel(mprg,'RELATORIO DE ATA DE ALUNOS P/PROFESSORES','15')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mgstt := SPACE(4)
        mensagem('Digite o GSTT que deseja:')
        @ 23,COL()+1 GET mgstt PICT '@!'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        ***************
        SELE('aluno');ORDSETFOCUS(6)
        DBSETFILTER({|| aluno->gstt = mgstt})
        GO TOP
        ***************
        IF EOF()
                atencao('Nao existe aluno neste GSTT')
                LOOP
        ENDIF
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
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        SET DEVI TO PRINT
        * SET PRINT TO LPT1
        mcont_aluno := 0
        mpag := 1
        mtit := 'Relacao de Alunos para Professor'
        mtipo := 'G S T T: '+mgstt
        cabecalho(mpag,mtit,mtipo)
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT('No.')
        DEVPOS(PROW(),04);DEVOUT('Matr.')
        DEVPOS(PROW(),12);DEVOUT('Nome')
        DEVPOS(PROW(),54);DEVOUT('OBSERVACAO')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        i := 0
        WHILE ! EOF()
                INKEY(.1)
                IF LASTKEY() = 27
                        DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                        EXIT
                ENDIF
                IF ! EMPTY(dt_tranf)
                        SKIP
                        LOOP
                ENDIF
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT(aluno->numero)
                DEVPOS(PROW(),04);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),12);DEVOUT(aluno->aluno)
*               DEVPOS(PROW(),54);DEVOUT(REPLI('_',26))
                DEVPOS(PROW(),54);DEVOUT('|')
                DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
                mcont_aluno ++
                SKIP
                IF PROW() >= 58
                        EJECT
                        mpag ++
                        cabecalho(mpag,mtit,mtipo)
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT('No.')
                        DEVPOS(PROW(),04);DEVOUT('Matr.')
                        DEVPOS(PROW(),12);DEVOUT('Nome')
                        DEVPOS(PROW(),54);DEVOUT('OBSERVACAO')
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

