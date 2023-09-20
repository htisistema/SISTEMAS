************************************************
* RELATORIO DE ALUNOS EM GERAL
************************************************

FUNCTION saf501
***************
LOCAL mprg:='SAF501',;
      opcao,mquantd,mtraco,mtit,mtipo,mcont_aluno,mgstt,mmes
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO DE ALUNOS EM GERAL','125')
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
        opcao := mensagem1('Deseja Emitir a Listage [G]->Geral ou [S]->GSTT:',' ','G,S')
        CLEAR GETS
        IF LASTKEY() = 27
                RETURN NIL
        ENDIF
        IF opcao = 'G'
                mtit := 'Relacao de Alunos em GERAL'
                mcont_aluno := 0
                mquantd = 1
                mensagem('Escolha a ordem que deseja: [3]Alfabetica [4]Matricula [1]Bairro:')
                @ 23,COL()+1 GET mquantd PICT '9' VALID IF(mquantd<1 .OR. mquantd>4,.F.,.T.)
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RETURN NIL
                ENDIF
                IF mquantd = 2
                        LOOP
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
                mtit := 'Relacao de Alunos pelo GSTT: '+mgstt
        ENDIF
        mensagem('Digite o mes da FAIXA que deseja [01..12]:')
        @ 23,COL()+1 GET mmes PICT '99' RANGE 1,12
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        IF ! imp_arq('REL_ALU.REL')
                LOOP
        ENDIF
        mcont_aluno := 0
        mpag := 1
        cabecalho(mpag,mtit)
        imprt('M','N')
        DEVPOS(PROW()+1,00);DEVOUT('Nome')
        DEVPOS(PROW(),42);DEVOUT('Matricula')
        DEVPOS(PROW(),53);DEVOUT('No.')
        DEVPOS(PROW(),58);DEVOUT('GSTT')
        DEVPOS(PROW(),64);DEVOUT('Faixa')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        i := 0
        WHILE ! EOF()
                IF opcao = 'G' .AND. aluno->gstt = 'DEBI'
                        SKIP
                        LOOP
                ENDIF
                imprt('M','N')
                DEVPOS(PROW()+1,00);DEVOUT(aluno->aluno)
                DEVPOS(PROW(),42);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),53);DEVOUT(aluno->numero)
                DEVPOS(PROW(),58);DEVOUT(aluno->gstt)
                IF mmes = 1
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_1)
                ELSEIF mmes = 2
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_2)
                ELSEIF mmes = 3
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_3)
                ELSEIF mmes = 4
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_4)
                ELSEIF mmes = 5
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_5)
                ELSEIF mmes = 6
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_6)
                ELSEIF mmes = 7
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_7)
                ELSEIF mmes = 8
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_8)
                ELSEIF mmes = 9
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_9)
                ELSEIF mmes = 10
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_10)
                ELSEIF mmes = 11
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_11)
                ELSEIF mmes = 12
                        DEVPOS(PROW(),64);DEVOUT(aluno->faixa_12)
                ENDIF
                IF ! EMPTY(dt_tranf)
                        DEVPOS(PROW(),PCOL()+1);DEVOUT('** transf **')
*                        SKIP
*                        LOOP
                ENDIF
                mcont_aluno ++
                SKIP
                IF PROW() >= 59
                        EJECT
                        mpag ++
                        cabecalho(mpag,mtit)
                        imprt('M','N')
                        DEVPOS(PROW()+1,00);DEVOUT('Nome')
                        DEVPOS(PROW(),42);DEVOUT('Matricula')
                        DEVPOS(PROW(),53);DEVOUT('No.')
                        DEVPOS(PROW(),58);DEVOUT('GSTT')
                        DEVPOS(PROW(),64);DEVOUT('Faixa')
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        LOOP
                ENDIF
        ENDDO
        imprt('M','N')
        DEVPOS(PROW()+1,00);DEVOUT('Quantidade de Alunos: '+STRZERO(mcont_aluno,5))
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        CLOSE ALL
        IF mimp_tipo = 2
                lertexto('REL_ALU.REL')
        ENDIF
        RETURN NIL
ENDDO

