************************************************
* RELATORIO DE ALUNOS TRANSFERIDOS
************************************************

FUNCTION saftrans
***************
LOCAL mprg:='SAFTRANS'
LOCAL opcao,mquantd,mtraco,mtit,mtipo,mcont_aluno,mgstt,mmes

IF ! ver_nivel(mprg,'RELATORIO DE ALUNOS TRANSFERIDOS','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mgstt := SPACE(4)
        mmes := 0
        opcao := mensagem1('Deseja Emitir a Listagem [G]->Geral ou [S]->GSTT:',' ','G,S')
        CLEAR GETS
        IF LASTKEY() = 27
                RETURN NIL
        ENDIF
        IF opcao = 'G'
                mtit := 'Relacao de Alunos TRANSFERIDO GERAL'
                mcont_aluno := 0
                mquantd = 1
                mensagem('Escolha a ordem que deseja emitir [3]Alfabetica [4]Matricula:')
                @ 23,COL()+1 GET mquantd PICT '9' VALID IF(mquantd<3 .OR. mquantd>4,.F.,.T.)
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RETURN NIL
                ENDIF
                ***************
                SELE('aluno');ORDSETFOCUS(mquantd)
                GO TOP
                ***************
        ELSE
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
                mtit := 'Relacao de Alunos TRANSFERIDOS do GSTT: '+mgstt
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
*       SET PRINT TO 'LPT1'
        mcont_aluno := 0
        mpag := 1
        cabecalho(mpag,mtit)
        imprt('M','N',1)
        DEVPOS(PROW(),00);DEVOUT('Nome')
        DEVPOS(PROW(),42);DEVOUT('Matricula')
        DEVPOS(PROW(),53);DEVOUT('No.')
        DEVPOS(PROW(),58);DEVOUT('GSTT')
        DEVPOS(PROW(),64);DEVOUT('Dta.Transf.')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        i := 0
        WHILE ! EOF()
*               INKEY(.1)
                IF LASTKEY() = 27
                        DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                        EXIT
                ENDIF
                IF EMPTY(dt_tranf)
                         SKIP
                         LOOP
                ENDIF
                imprt('M','N',1)
                DEVPOS(PROW(),00);DEVOUT(aluno->aluno)
                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),53);DEVOUT(aluno->numero)
                DEVPOS(PROW(),58);DEVOUT(aluno->gstt)
                DEVPOS(PROW(),64);DEVOUT(aluno->dt_tranf)
                mcont_aluno ++
                SKIP
                IF PROW() >= 59
                        EJECT
                        mpag ++
                        cabecalho(mpag,mtit)
                        imprt('M','N',1)
                        DEVPOS(PROW(),00);DEVOUT('Nome')
                        DEVPOS(PROW(),42);DEVOUT('Matricula')
                        DEVPOS(PROW(),53);DEVOUT('No.')
                        DEVPOS(PROW(),58);DEVOUT('GSTT')
                        DEVPOS(PROW(),64);DEVOUT('Dta.Transf.')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        LOOP
                ENDIF
        ENDDO
        imprt('M','N',2)
        DEVPOS(PROW(),00);DEVOUT('Quantidade de Alunos: '+STRZERO(mcont_aluno,5))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        CLOSE ALL
        RETURN NIL
ENDDO

