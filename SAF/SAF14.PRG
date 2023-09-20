*************************
* CADASTRO DE DISCIPLINA
*************************
FUNCTION saf14
***************
LOCAL mprg:='SAF14'
LOCAL tela1,tela,li,ci,lb,cb,lci,cci,lba,cba,point,pode,men,f,mcod_disc,mdisci,;
      mdisci1,mdescr_min

IF ! ver_nivel(mprg,'CADASTRO DE DISCIPLINA','125')
        RETURN NIL
ENDIF

lci := 03
cci := 20
lba := 21
cba := 70

tela := SAVESCREEN(01,00,24,79)
setcor(3)
botao(lci,cci,lba,cba)
setcor(1)

janela(lci,cci,' Mensalidades ','*','*')
DEVPOS(lci+1,cci+1);DEVOUT('Cod.')
DEVPOS(lci+1,cci+7);DEVOUT('Disciplinas')
DEVPOS(lci+1,cci+29);DEVOUT('Min.Disc.')
DEVPOS(lci+1,cci+39);DEVOUT('Tipo')
pode=.t.
*----------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
*--------------------------------------------------
**************
SELE('disc');ORDSETFOCUS(1)
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
                        DEVPOS(lci+1+f,cci+1);DEVOUT(' '+disc->cod_disc+'  '+disc->descr+'   '+disc->descr_min+'        '+disc->tipo)
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
                        mcod_disc:= SPACE(2)
                        mdisci   := SPACE(10)
                        mdisci1  := SPACE(09)
                        mdescr_min := SPACE(3)
                        mtipo      := SPACE(1)
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 45
                        setcor(3)
                        botao(li,ci,lb,cb)
                        setcor(1)
                        janela(li,ci,' Inclusao de Disciplina ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('Cod.')
                        DEVPOS(li+1,ci+7);DEVOUT('Disciplina')
                        DEVPOS(li+1,ci+29);DEVOUT('Min.Disc.')
                        DEVPOS(li+1,ci+39);DEVOUT('Tipo')
                        @ li+2,ci+2 GET mcod_disc PICT '99'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF disc->(DBSEEK(mcod_disc))
                                atencao('Ja existe esta DISCPLINA')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO point
                                LOOP
                        ENDIF
                        @ li+2,ci+07 GET mdisci PICT '@!'
                        @ li+2,COL() SAY '-'
                        @ li+2,COL() GET mdisci1 PICT '@!'
                        @ li+2,ci+32 GET mdescr_min PICT '@!'
                        @ li+2,ci+39 GET mtipo PICT '@!' VALID mtipo $ 'C,N'
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
                        disc->cod_disc := mcod_disc
                        disc->descr := mdisci+' '+mdisci1
                        disc->descr_min   := mdescr_min
                        disc->tipo := mtipo
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
                CASE LASTKEY() = ASC('A') .OR. LASTKEY() = ASC('a') .OR. LASTKEY() = 50
                        tela1 := SAVESCREEN(01,00,24,79)
                        mcod_disc:= SPACE(2)
                        mdisci   := SPACE(10)
                        mdisci1  := SPACE(09)
                        mdescr_min := SPACE(3)
                        mtipo := SPACE(1)
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 45
                        setcor(3)
                        botao(li,ci,lb,cb)
                        setcor(1)
                        janela(li,ci,' Alteracao de Disciplina ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('Cod.')
                        DEVPOS(li+1,ci+7);DEVOUT('Disciplina')
                        DEVPOS(li+1,ci+29);DEVOUT('Min.Disc.')
                        DEVPOS(li+1,ci+39);DEVOUT('Tipo')
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
                        mdisci := SUBSTR(disc->descr,1,10)
                        mdisci1 := SUBSTR(disc->descr,11)
                        mdescr_min := disc->descr_min
                        mtipo := disc->tipo
                        @ li+2,ci+07 GET mdisci PICT '@! XXXXXXXXXX.XXXXXXXXX'
                        @ li+2,COL() SAY '-'
                        @ li+2,COL() GET mdisci1 PICT '@!'
                        @ li+2,ci+32 GET mdescr_min PICT '@!'
                        @ li+2,ci+39 GET mtipo PICT '@!' VALID mtipo $ 'C,N'
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
                        disc->descr := mdisci+' '+mdisci1
                        disc->descr_min   := mdescr_min
                        disc->tipo := mtipo
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
                        botao(li,ci,lb,cb)
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
