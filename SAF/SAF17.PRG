*************************
* CADASTRO DE DESPESAS
*************************
FUNCTION saf17
***************
LOCAL MPRG:='SAF17'
LOCAL tela,lci,cci,lba,cba,point,pode,msele,morde,men,;
      opcao,mmerc,f

PRIVATE mgrupo

IF ! ver_nivel(mprg,'CADASTRO DAS DESPESAS','15',nivel_acess)
        RETURN NIL
ENDIF

lci := 04
cci := 01
lba := 17
cba := 79

tela := SAVESCREEN(01,00,24,79)
botao(lci,cci,lba,cba,,' DESPESAS ')
@ lci+2,cci+1 TO lci+2,cba-1
@ lci+1,cci+07 SAY CHR(194)
@ lba,cci+07 SAY CHR(193)
@ lci+2,cci+07 TO lba-1,cci+07

DEVPOS(lci+1,cci+2);DEVOUT('Cod.')
DEVPOS(lci+1,cci+09);DEVOUT('Descricao')
pode=.t.

************
CLOSE ALL
IF ! AbriArq('safcadde','desp');RETURN NIL;ENDIF
SELE('desp');ORDSETFOCUS(1)
GO TOP
************
WHILE .T.
        mensagem('< > Consulta  <I>nclusao  <A>lteracao  <E>xclusao  <ESC> Retorna')
        opcao := 'S'
        point=RECNO()
        FOR f=1 TO 10
                IF EOF()
                        setcor(3)
                        DEVPOS(lci+2+f,cci+3);DEVOUT("   ")
                        DEVPOS(lci+2+f,cci+09);DEVOUT(SPACE(40))
                        setcor(1)
                        LOOP
                ENDIF
                setcor(3)
                DEVPOS(lci+2+f,cci+3);DEVOUT(codigo)
                DEVPOS(lci+2+f,cci+09);DEVOUT(desc1+LEFT(desc2,28))
                setcor(1)
                SKIP
        NEXT f
        IF EOF()
                pode=.f.
        ENDIF
        GO point
        INKEY(0)
        DO CASE
                CASE LASTKEY()=5
                        IF ! BOF()
                                SKIP -1
                                pode=.T.
                        ENDIF
                CASE LASTKEY()=24
                        IF ! EOF() .AND. pode=.T.
                                SKIP
                        ENDIF
                CASE LASTKEY()=18
                        IF ! BOF()
                                SKIP -9
                                pode=.T.
                        ENDIF
                CASE LASTKEY()=3
                        IF ! EOF() .AND. pode=.T.
                                SKIP 9
                        ENDIF

                CASE LASTKEY()=27
                        RESTSCREEN(01,00,24,79,tela)
                        CLOSE ALL
                        RELEASE mgrupo
                        RETURN NIL

                CASE LASTKEY() = ASC('I') .OR. LASTKEY() = ASC('i')
                        tela1 := SAVESCREEN(01,00,24,79)
                        WHILE .T.
                                *************
                                GO BOTT
                                *************
                                mensagem('Digite o Codigo da despesa e faca a descricao - <ESC> p/Retornar')
                                mcoddesp := VAL(desp->codigo)+1
                                mdesc1 := SPACE(40)
                                mdesc2 := SPACE(40)
                                botao(lba+2,cci,lba+5,cba,,' Inclusao Despesas ')
                                DEVPOS(lba+3,cci+2);DEVOUT('Cod.:')
                                DEVPOS(lba+3,cci+15);DEVOUT('Descricao:')
                                @ lba+3,cci+8 GET mcoddesp PICT '999' VALID IF(mgrupo=0,.F.,.T.)
                                READ

                                IF LASTKEY() = 27
                                        RESTSCREEN(01,00,24,79,tela1)
                                        EXIT
                                ENDIF
                                IF EMPTY(mcoddesp)
                                        LOOP
                                ENDIF
                                GO TOP
                                *************
                                SEEK  STRZERO(mcoddesp,3)
                                *************
                                IF FOUND()
                                        atencao('Despesa ja Cadastrada')
                                        LOOP
                                ENDIF
                                setcor(3)
                                DEVPOS(lba+3,cci+8);DEVOUT(STRZERO(mcoddesp,3))
                                setcor(1)

                                @ lba+3,cci+26 GET mdesc1 PICT '@!'
                                @ lba+4,cci+26 GET mdesc2 PICT '@!'
                                READ
                                IF LASTKEY() = 27 .OR. EMPTY(mdesc1)
                                        LOOP
                                ENDIF
                                mensagem('Confirma Inclusao [S/n]:')
                                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                READ
                                IF LASTKEY() = 27
                                        LOOP
                                ENDIF
                                IF opcao = 'S'
                                        IF ADIREG()
                                                desp-> codigo := STRZERO(mcoddesp,3)
                                                desp-> desc1 := mdesc1
                                                desp-> desc2 := mdesc2
                                                COMMIT
                                                UNLOCK
                                        ELSE
                                                atencao('Nao foi possivel acessar o Arquivo !!!')
                                                ORDSETFOCUS(3)
                                                LOOP
                                        ENDIF
                                        ORDSETFOCUS(1)
                                        GO TOP
                                        FOR f=1 TO 10
                                                IF EOF()
                                                        setcor(3)
                                                        DEVPOS(lci+2+f,cci+3);DEVOUT("   ")
                                                        DEVPOS(lci+2+f,cci+09);DEVOUT(SPACE(40))
                                                        setcor(1)
                                                        LOOP
                                                ENDIF
                                                setcor(3)
                                                DEVPOS(lci+2+f,cci+3);DEVOUT(codigo)
                                                DEVPOS(lci+2+f,cci+09);DEVOUT(desc1+LEFT(desc2,28))
                                                setcor(1)
                                                SKIP
                                        NEXT f
                                ENDIF
                        ENDDO
                CASE LASTKEY() = ASC('A') .OR. LASTKEY() = ASC('a')
                        tela1 := SAVESCREEN(01,00,24,79)
                        WHILE .T.
                                mensagem('Digite o Codigo da Despesa a ser Alterado - <ESC> p/Retornar')
                                mcoddesp := 0
                                mdesc1 := SPACE(40)
                                mdesc2 := SPACE(40)
                                botao(lba+2,cci,lba+5,cba,,' Alteracao ')
                                DEVPOS(lba+3,cci+2);DEVOUT('Cod.:')
                                DEVPOS(lba+3,cci+15);DEVOUT('Descricao:')
                                @ lba+3,cci+8 GET mcoddesp PICT '999' VALID IF(mcoddesp=0,.F.,.T.)
                                READ

                                IF LASTKEY() = 27
                                        RESTSCREEN(01,00,24,79,tela1)
                                        EXIT
                                ENDIF
                                *************
                                GO TOP
                                *************
                                *************
                                SEEK  STRZERO(mcoddesp,3)
                                *************
                                IF ! FOUND()
                                        atencao('Grupo nao Cadastrado !!!')
                                        LOOP
                                ENDIF
                                IF ! BLOQREG()
                                        atencao('Nao foi possivel acessar o Arquivo !!!')
                                        LOOP
                                ENDIF

                                mdesc1 := desc1
                                mdesc2 := desc2
                                setcor(3)
                                DEVPOS(lba+3,cci+8);DEVOUT(STRZERO(mcoddesp,3))
                                setcor(1)

                                @ lba+3,cci+26 GET mdesc1 PICT '@!'
                                @ lba+4,cci+26 GET mdesc2 PICT '@!'
                                READ
                                IF LASTKEY() = 27
                                        UNLOCK
                                        LOOP
                                ENDIF
                                mensagem('Confirma Alteracao [S/n]:')
                                opcao := 'S'
                                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                READ
                                IF LASTKEY() = 27
                                        UNLOCK
                                        LOOP
                                ENDIF
                                IF opcao = 'S'
                                        desp-> codigo := STRZERO(mcoddesp,3)
                                        desp-> desc1 := mdesc1
                                        desp-> desc2 := mdesc2
                                        COMMIT
                                        UNLOCK
                                        ORDSETFOCUS(1)
                                        GO TOP
                                        FOR f=1 TO 10
                                                IF EOF()
                                                        setcor(3)
                                                        DEVPOS(lci+2+f,cci+3);DEVOUT("  ")
                                                        DEVPOS(lci+2+f,cci+09);DEVOUT(SPACE(40))
                                                        setcor(1)
                                                        LOOP
                                                ENDIF
                                                setcor(3)
                                                DEVPOS(lci+2+f,cci+3);DEVOUT(codigo)
                                                DEVPOS(lci+2+f,cci+09);DEVOUT(desc1+LEFT(desc2,28))
                                                setcor(1)
                                                SKIP
                                        NEXT f
                                ENDIF
                        ENDDO

                CASE LASTKEY() = ASC('E') .OR. LASTKEY() = ASC('e')
                        tela1 := SAVESCREEN(01,00,24,79)
                        WHILE .T.
                                mensagem('Digite o Codigo da Despesa a ser Excluida - <ESC> p/Retornar')
                                mcoddesp := 0
                                mdesc1 := SPACE(40)
                                mdesc2 := SPACE(40)
                                botao(lba+2,cci,lba+5,cba,,' Exclusao ')
                                DEVPOS(lba+3,cci+2);DEVOUT('Cod.:')
                                DEVPOS(lba+3,cci+15);DEVOUT('Descricao:')
                                @ lba+3,cci+8 GET mcoddesp PICT '999' VALID IF(mcoddesp=0,.F.,.T.)
                                READ

                                IF LASTKEY() = 27
                                        RESTSCREEN(01,00,24,79,tela1)
                                        EXIT
                                ENDIF
                                *************
                                GO TOP
                                *************
                                *************
                                SEEK  STRZERO(mcoddesp,3)
                                *************
                                IF ! FOUND()
                                        atencao('Despesa nao Cadastrado !!!')
                                        LOOP
                                ENDIF
                                IF ! BLOQARQ()
                                        atencao('Nao foi possivel acessar o Arquivo !!!')
                                        LOOP
                                ENDIF
                                setcor(3)
                                DEVPOS(lba+3,cci+8);DEVOUT(STRZERO(mcoddesp,3))
                                DEVPOS(lba+3,cci+26);DEVOUT(desc1)
                                DEVPOS(lba+4,cci+26);DEVOUT(desc2)
                                setcor(1)
                                mensagem('Confirma Exclusao [S/n]:')
                                opcao := 'S'
                                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                READ
                                IF LASTKEY() = 27
                                        UNLOCK
                                        LOOP
                                ENDIF
                                IF opcao = 'S'
                                        DELE
                                        COMMIT
                                        UNLOCK
                                        ORDSETFOCUS(1)
                                        GO TOP
                                        FOR f=1 TO 10
                                                IF EOF()
                                                        setcor(3)
                                                        DEVPOS(lci+2+f,cci+3);DEVOUT("   ")
                                                        DEVPOS(lci+2+f,cci+09);DEVOUT(SPACE(40))
                                                        setcor(1)
                                                        LOOP
                                                ENDIF
                                                setcor(3)
                                                DEVPOS(lci+2+f,cci+3);DEVOUT(codigo)
                                                DEVPOS(lci+2+f,cci+09);DEVOUT(desc1+LEFT(desc2,28))
                                                setcor(1)
                                                SKIP
                                        NEXT f
                                ENDIF
                        ENDDO
        ENDCASE
ENDDO
RETURN NIL
*************************** f i m *******************************************

