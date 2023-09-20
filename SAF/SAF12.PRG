*************************
* CADASTRO DE GSTT
*************************
FUNCTION saf12
***************
LOCAL mprg:='SAF12'
LOCAL tela1,tela,li,ci,lb,cb,lci,cci,lba,cba,point,pode,men,f,mcod_disc,mdisci,mgstT,mch

IF ! ver_nivel(mprg,'CADASTRO DE GSTT','125')
        RETURN NIL
ENDIF

lci := 03
cci := 30
lba := 21
cba := 70

tela := SAVESCREEN(01,00,24,79)
setcor(3)
botao(lci,cci,lba,cba)
*@ lci+2,cci+1 TO lci+2,cba-1
setcor(1)

janela(lci,cci,' Mensalidades ','*','*')
DEVPOS(lci+1,cci+1);DEVOUT('GSTT')
DEVPOS(lci+1,cci+7);DEVOUT('Disciplina')
DEVPOS(lci+1,cci+31);DEVOUT('Carga Hr.')
pode=.t.
*----------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
*--------------------------------------------------
**************
SELE('tbgstt');ORDSETFOCUS(1)
GO TOP
************
WHILE .T.
        mensagem('< > Consulta <1/I>nclusao <2/A>lteracao <3/E>xclusao <ESC>Retorna')
        point=RECNO()
        f := 0
        FOR f=1 TO 16
                IF ! EOF()
                        IF EOF()
                                setcor(3)
                                DEVPOS(lci+1+f,cci+1);DEVOUT(SPACE(cba-cci-2))
                                setcor(1)
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(lci+1+f,cci+1);DEVOUT(tbgstt->gstt+'  '+tbgstt->cod_disc+'-'+tbgstt->disci1+'  '+STR(tbgstt->ch))
                        setcor(1)
                        SKIP
                ELSE
                        setcor(3)
                        DEVPOS(lci+1+f,cci+1);DEVOUT(SPACE(cba-cci-2))
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
                        mgstt    := SPACE(4)
                        mcod_disc:= SPACE(2)
                        mdisci   := SPACE(20)
                        mch      := 0
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 45
                        setcor(3)
                        botao(li,ci,lb,cb)
                        setcor(1)
                        janela(li,ci,' Inclusao de GSTT ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('GSTT')
                        DEVPOS(li+1,ci+7);DEVOUT('Disciplina')
                        DEVPOS(li+1,ci+31);DEVOUT('Carga Hr.')
                        @ li+2,ci+1 GET mgstt PICT '@!'
                        @ li+2,ci+7 GET mcod_disc PICT '99'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mdisci:= ver_disc(mcod_disc)
                        IF EMPTY(mdisci)
                                atencao('Nao existe esta DISCIPLINA')
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF tbgstt->(DBSEEK(mgstt+mcod_disc))
                                atencao('Ja existe este GSTT com esta DISCPLINA')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO point
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(li+2,ci+10);DEVOUT(mdisci)
                        setcor(1)
                        @ li+2,ci+34 GET mch PICT '9999'
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
                        tbgstt->gstt := mgstt
                        tbgstt->cod_disc := mcod_disc
                        tbgstt->disci1  := mdisci
                        tbgstt->ch      := mch
                        tbgstt->operador := cod_operado
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
                CASE LASTKEY() = ASC('A') .OR. LASTKEY() = ASC('a') .OR. LASTKEY() = 50
                        tela1 := SAVESCREEN(01,00,24,79)
                        mgstt    := SPACE(4)
                        mcod_disc:= SPACE(2)
                        mdisci   := SPACE(20)
                        mch      := 0
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 45
                        setcor(3)
                        botao(li,ci,lb,cb)
                        setcor(1)
                        janela(li,ci,' Inclusao de GSTT ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('GSTT')
                        DEVPOS(li+1,ci+7);DEVOUT('Disciplina')
                        DEVPOS(li+1,ci+31);DEVOUT('Carga Hr.')
                        @ li+2,ci+1 GET mgstt PICT '@!'
                        @ li+2,ci+7 GET mcod_disc PICT '99'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mdisci:= ver_disc(mcod_disc)
                        IF EMPTY(mdisci)
                                atencao('Nao existe esta DISCIPLINA')
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF ! tbgstt->(DBSEEK(mgstt+mcod_disc))
                                atencao('Nao existe este GSTT com esta DISCPLINA')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO point
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(li+2,ci+10);DEVOUT(mdisci)
                        setcor(1)
                        mch := tbgstt->ch
                        @ li+2,ci+34 GET mch PICT '9999'
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
                                LOOP
                        ENDIF
                        IF ! BLOQREG()
                                atencao('Nao foi possivel bloquear o registro no arquivo')
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        tbgstt->gstt := mgstt
                        tbgstt->cod_disc := mcod_disc
                        tbgstt->disci1  := mdisci
                        tbgstt->ch      := mch
                        tbgstt->operador := cod_operado
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
                CASE LASTKEY() = ASC('E') .OR. LASTKEY() = ASC('e') .OR. LASTKEY() = 51
                        tela1 := SAVESCREEN(01,00,24,79)
                        mgstt    := SPACE(4)
                        mcod_disc:= SPACE(2)
                        mdisci   := SPACE(20)
                        mch      := 0
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 45
                        setcor(3)
                        botao(li,ci,lb,cb)
                        setcor(1)
                        janela(li,ci,' Inclusao de GSTT ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('GSTT')
                        DEVPOS(li+1,ci+7);DEVOUT('Disciplina')
                        DEVPOS(li+1,ci+31);DEVOUT('Carga Hr.')
                        @ li+2,ci+1 GET mgstt PICT '@!'
                        @ li+2,ci+7 GET mcod_disc PICT '99'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mdisci:= ver_disc(mcod_disc)
                        IF EMPTY(mdisci)
                                atencao('Nao existe esta DISCIPLINA')
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF ! tbgstt->(DBSEEK(mgstt+mcod_disc))
                                atencao('Nao existe este GSTT com esta DISCPLINA')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO point
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(li+2,ci+10);DEVOUT(mdisci)
                        DEVPOS(li+2,ci+10);DEVOUTPICT(tbgstt->ch,'9999')
                        setcor(1)
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mensagem('Confirma a Exclusao [S/n]:')
                        opcao := 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF ! BLOQREG()
                                atencao('Nao foi possivel bloquear o registro no arquivo')
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        tbgstt->operador := cod_operado
                        DELE
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        **************
                        SELE('nota');ORDSETFOCUS(2)
                        GO TOP
                        ************
                        IF nota->(DBSEEK(mgstt+mcod_disc))
                                IF ! BLOQARQ()
                                        atencao('Nao foi possivel bloquear o ARQUIVO')
                                        RESTSCREEN(01,00,24,79,tela1)
                                        LOOP
                                ENDIF
*                               DELE FOR mgstt = nota->gstt .AND. mcod_disc = nota->cod_disc
                                WHILE mgstt = nota->gstt .AND. mcod_disc = nota->cod_disc
                                        DELE
                                        SKIP
                                ENDDO
                        ENDIF
                        **************
                        SELE('tbgstt');ORDSETFOCUS(1)
                        ************
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
        ENDCASE
ENDDO
RETURN NIL
*************************** f i m *******************************************
