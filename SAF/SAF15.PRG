*************************
* CADASTRO DE PROFESSOR
*************************
FUNCTION saf15
***************
LOCAL mprg:='SAF15'
LOCAL tela1,tela,li,ci,lb,cb,lci,cci,lba,cba,point,pode,men,f,;
      mcodigo,mnome,mgstt,mcod_disc

IF ! ver_nivel(mprg,'CADASTRO DE PROFESSORES','125')
        RETURN NIL
ENDIF

lci := 03
cci := 02
lba := 21
cba := 77

tela := SAVESCREEN(01,00,24,79)
setcor(3)
CAIXA(mold,lci,cci,lba,cba,,,.T.)
*@ lci+2,cci+1 TO lci+2,cba-1
setcor(1)

janela(lci,cci,' Mensalidades ','*','*')
DEVPOS(lci+1,cci+3);DEVOUT('Cod.')
DEVPOS(lci+1,cci+9);DEVOUT('Professor')
DEVPOS(lci+1,cci+41);DEVOUT('Disciplinas')
DEVPOS(lci+1,cci+65);DEVOUT('GSTT')
pode=.t.
*----------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadprof','prof');RETURN NIL;ENDIF
*--------------------------------------------------
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
                        DEVPOS(lci+1+f,cci+1);DEVOUT('  '+prof->codigo+'   '+prof->nome+'  '+prof->cod_disc+' '+prof->disciplina+' '+prof->gstt)
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
                        **************
                        SELE('prof');ORDSETFOCUS(1)
                        GO TOP
                        ************
                        tela1 := SAVESCREEN(01,00,24,79)
                        mcodigo:= 0
                        mnome    := SPACE(30)
                        mgstt := SPACE(4)
                        mcod_disc := SPACE(2)
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 77
                        setcor(3)
                        CAIXA(mold,li,ci,lb,cb,,,.T.)
                        setcor(1)
                        janela(li,ci,' Inclusao de Professor ','*','*')
                        DEVPOS(li+1,ci+3);DEVOUT('Cod.')
                        DEVPOS(li+1,ci+9);DEVOUT('Professor')
                        DEVPOS(li+1,ci+41);DEVOUT('Disciplina')
                        DEVPOS(li+1,ci+65);DEVOUT('GSTT')
                        @ li+2,ci+3 GET mcodigo PICT '999'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF prof->(DBSEEK(STRZERO(mcodigo,3)))
                                mnome := prof->nome
                                setcor(3)
                                DEVPOS(li+2,ci+9);DEVOUT(mnome)
                                setcor(1)
                        ELSE
                                @ li+2,ci+9 GET mnome PICT '@!'
                        ENDIF
                        @ li+2,ci+41 GET mcod_disc PICT '99'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF

                        ver_disc(mcod_disc)
                        setcor(3)
                        DEVPOS(li+2,ci+44);DEVOUT(disc->descr)
                        setcor(1)
                        @ li+2,ci+65 GET mgstt PICT '@!'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        ORDSETFOCUS(2)
                        IF prof->(DBSEEK(mcod_disc+mgstt))
                                atencao('Esta disciplina com este GSTT ja existe')
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        ORDSETFOCUS(1)
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
                        prof->codigo := STRZERO(mcodigo,3)
                        prof->nome := mnome
                        prof->gstt   := mgstt
                        prof->cod_disc := mcod_disc
                        prof->disciplina := disc->descr
                        prof->operador := cod_operado
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
                CASE LASTKEY() = ASC('A') .OR. LASTKEY() = ASC('a') .OR. LASTKEY() = 50
                        tela1 := SAVESCREEN(01,00,24,79)
                        mcod_disc:= SPACE(2)
                        mdisci   := SPACE(20)
                        mdescr_min := SPACE(3)
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 45
                        setcor(3)
                        CAIXA(mold,li,ci,lb,cb,,,.T.)
                        setcor(1)
                        janela(li,ci,' Alteracao de Disciplina ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('Cod.')
                        DEVPOS(li+1,ci+7);DEVOUT('Disciplina')
                        DEVPOS(li+1,ci+31);DEVOUT('Min.Disc.')
                        @ li+2,ci+2 GET mcod_disc PICT '99'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF ! disc->(DBSEEK(mcod_disc))
                                atencao('Nao existe esta DISCPLINA')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO point
                                LOOP
                        ENDIF
                        mdisci := disc->descr
                        mdescr_min := disc->descr_min
                        @ li+2,ci+07 GET mdisci PICT '@!'
                        @ li+2,ci+34 GET mdescr_min PICT '@!'
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
                                LOOP
                        ENDIF
                        IF ! BLOQREG()
                                atencao('Nao foi possivel bloquear o registro no arquivo')
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        disc->descr := mdisci
                        disc->descr_min   := mdescr_min
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
                CASE LASTKEY() = ASC('E') .OR. LASTKEY() = ASC('e') .OR. LASTKEY() = 51
                        tela1 := SAVESCREEN(01,00,24,79)
                        mcod_disc:= SPACE(2)
                        mdisci   := SPACE(20)
                        mdescr_min := SPACE(3)
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 45
                        setcor(3)
                        CAIXA(mold,li,ci,lb,cb,,,.T.)
                        setcor(1)
                        janela(li,ci,' Exclusao de Disciplina ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('Cod.')
                        DEVPOS(li+1,ci+7);DEVOUT('Disciplina')
                        DEVPOS(li+1,ci+31);DEVOUT('Min.Disc.')
                        @ li+2,ci+2 GET mcod_disc PICT '99'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF ! disc->(DBSEEK(mcod_disc))
                                atencao('Nao existe esta DISCPLINA')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO point
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(li+2,ci+07);DEVOUT(disc->descr)
                        DEVPOS(li+2,ci+34);DEVOUT(disc->descr_min)
                        setcor(1)
                        mensagem('Confirma a Exclusao [S/n]:')
                        opcao := 'N'
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
                        DELE
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
        ENDCASE
ENDDO
RETURN NIL
*************************** f i m *******************************************
