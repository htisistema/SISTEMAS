*************************
* CADASTRO DE BOLSAS
*************************
FUNCTION saf13
***************
LOCAL mprg:='SAF13'
LOCAL tela1,tela,li,ci,lb,cb,lci,cci,lba,cba,point,pode,men,f,;
      mfaixa,mdesconto,mdescricao

IF ! ver_nivel(mprg,'CADASTRO DAS BOLSAS DA MENSALIDADES','125')
        RETURN NIL
ENDIF

lci := 03
cci := 40
lba := 21
cba := 78

li := 03
ci := 30
lb := 21
cb := 65

tela := SAVESCREEN(01,00,24,79)
setcor(3)
botao(lci,cci,lba,cba)
*@ lci+2,cci+1 TO lci+2,cba-1
setcor(1)

janela(lci,cci,' Mensalidades ','*','*')
DEVPOS(lci+1,cci+1);DEVOUT('Faixa')
DEVPOS(lci+1,cci+08);DEVOUT('Desconto')
DEVPOS(lci+1,cci+18);DEVOUT('Descricao')
DEVPOS(lci+1,cci+30);DEVOUT('Operador')
pode=.t.
CLOSE ALL
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
**************************
SELE('fx');ORDSETFOCUS(1)
GO TOP
**************************
WHILE .T.
        mensagem('< > Consulta <1/I>nclusao <2/A>lteracao <3/E>xclusao <ESC>Retorna')
        point=RECNO()
        f := 0
        FOR f=1 TO 16
                IF ! EOF()
                        IF EOF()
                                setcor(3)
                                DEVPOS(lci+1+f,cci+1);DEVOUT('                                ')
                                setcor(1)
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(lci+1+f,cci+1);DEVOUT('  '+fx->faixa+'      '+TRANSFORM(fx->desconto,'999.99')+'     '+fx->descricao+'  '+fx->operador)
                        setcor(1)
                        SKIP
                ELSE
                        setcor(3)
                        DEVPOS(lci+1+f,cci+1);DEVOUT('                                ')
                        setcor(1)
                ENDIF
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
                        RETURN NIL

                CASE LASTKEY() = ASC('I') .OR. LASTKEY() = ASC('i') .OR. LASTKEY() = 49
                        tela1 := SAVESCREEN(01,00,24,79)
                        mfaixa := ' '
                        mdesconto:= 0
                        mdescricao:= SPACE(10)
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 38

                        setcor(3)
                        botao(li,ci,lb,cb)
                        setcor(1)
                        janela(li,ci,' Inclusao da Mensalidades ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('Faixa')
                        DEVPOS(li+1,ci+08);DEVOUT('Desconto')
                        DEVPOS(li+1,ci+18);DEVOUT('Descricao')
                        @ li+2,ci+3 GET mfaixa
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                GO TOP
                                LOOP
                        ENDIF
                        IF fx->(DBSEEK(mfaixa))
                                atencao('Esta FAIXA ja estar cadastrada')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO TOP
                                LOOP
                        ENDIF
                        @ li+2,ci+11 GET mdesconto PICT '999.99'
                        @ li+2,ci+18 GET mdescricao PICT '@!'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mensagem('Confirma a inclusao [S/n]:')
                        opcao := 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF ! ADIREG()
                                atencao('Nao foi possivel adicionar o registro no arquivo')
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        fx->faixa := mfaixa
                        fx->desconto := mdesconto
                        fx->descricao:= mdescricao
                        fx->operador := cod_operado
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
                CASE LASTKEY() = ASC('A') .OR. LASTKEY() = ASC('a') .OR. LASTKEY() = 50
                        tela1 := SAVESCREEN(01,00,24,79)
                        mfaixa := ' '
                        mdesconto:= 0
                        mdescricao:= SPACE(10)
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 38

                        setcor(3)
                        botao(li,ci,lb,cb)
                        setcor(1)
                        janela(li,ci,' Alteracao da Mensalidades ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('Faixa')
                        DEVPOS(li+1,ci+08);DEVOUT('Desconto')
                        DEVPOS(li+1,ci+18);DEVOUT('Descricao')
                        @ li+2,ci+3 GET mfaixa
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                GO TOP
                                LOOP
                        ENDIF
                        IF ! fx->(DBSEEK(mfaixa))
                                atencao('Esta FAIXA nao estar cadastrada')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO TOP
                                LOOP
                        ENDIF
                        mdesconto:= fx->desconto
                        mdescricao:= fx->descricao
                        @ li+2,ci+11 GET mdesconto PICT '999.99'
                        @ li+2,ci+18 GET mdescricao PICT '@!'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mensagem('Confirma a Alteracao [S/n]:')
                        opcao := 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                RESTSCREEN(01,00,24,79,tela1)
                                GO TOP
                                LOOP
                        ENDIF
                        IF ! BLOQREG()
                                atencao('Nao foi possivel BLOQUEAR o registro')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO TOP
                                LOOP
                        ENDIF
                        fx->faixa := mfaixa
                        fx->desconto := mdesconto
                        fx->descricao:= mdescricao
                        fx->operador := cod_operado
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
                CASE LASTKEY() = ASC('E') .OR. LASTKEY() = ASC('e') .OR. LASTKEY() = 51
                        tela1 := SAVESCREEN(01,00,24,79)
                        mfaixa := ' '
                        mdesconto:= 0
                        mdescricao:= SPACE(10)
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 38

                        setcor(3)
                        botao(li,ci,lb,cb)
                        setcor(1)
                        janela(li,ci,' Exclusao da Mensalidades ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('Faixa')
                        DEVPOS(li+1,ci+08);DEVOUT('Desconto')
                        DEVPOS(li+1,ci+18);DEVOUT('Descricao')
                        @ li+2,ci+3 GET mfaixa
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                GO TOP
                                LOOP
                        ENDIF
                        IF ! fx->(DBSEEK(mfaixa))
                                atencao('Esta FAIXA nao estar cadastrada')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO TOP
                                LOOP
                        ENDIF
                        SETCOR(3)
                        @ li+2,ci+11 SAY fx->desconto PICT '999.99'
                        @ li+2,ci+18 SAY fx->descricao PICT '@!'
                        SETCOR(1)
                        mensagem('Confirma a Exclusao [S/n]:')
                        opcao := 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                RESTSCREEN(01,00,24,79,tela1)
                                GO TOP
                                LOOP
                        ENDIF
                        IF ! BLOQREG()
                                atencao('Nao foi possivel BLOQUEAR o registro')
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        DELE
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
        ENDCASE
ENDDO
RETURN NIL
*************************** f i m *******************************************
