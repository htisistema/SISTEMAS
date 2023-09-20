*************************
* CADASTRO DE IGPM
*************************
FUNCTION saf18
***************
LOCAL mprg:='SAF18'
LOCAL tela1,tela,li,ci,lb,cb,lci,cci,lba,cba,point,pode,men,f,mmes,mvlr_igpm

IF ! ver_nivel(mprg,'CADASTRO DE IGPM','125')
        RETURN NIL
ENDIF

lci := 06
cci := 30
lba := 21
cba := 54

tela := SAVESCREEN(01,00,24,79)
setcor(3)
botao(lci,cci,lba,cba,,' Mensalidades ')
DEVPOS(lci+1,cci+1);DEVOUT('Mes')
DEVPOS(lci+1,cci+7);DEVOUT('Valor IGPM')
DEVPOS(lci+1,cci+19);DEVOUT('Oper')
pode=.t.
*----------------------------------------------
CLOSE ALL
IF ! abriarq('safigpm','igpm');RETURN NIL;ENDIF
*--------------------------------------------------
**************
SELE('igpm');ORDSETFOCUS(1)
GO TOP
************
WHILE .T.
        mensagem('< > Consulta <1/I>nclusao <2/A>lteracao <3/E>xclusao <ESC>Retorna')
        point=RECNO()
        f := 0
        FOR f=1 TO 13
                IF ! EOF()
                        IF EOF()
                                setcor(3)
                                DEVPOS(lci+1+f,cci+1);DEVOUT(SPACE(cba-cci-2))
                                setcor(1)
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(lci+1+f,cci+1);DEVOUT(' '+igpm->mes+'  '+TRANSFORM(igpm->vlr_igpm,'9,999.9999')+'   '+igpm->oper)
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
                        mmes:= 0
                        mvlr_igpm := 0
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 45
                        setcor(3)
                        botao(li,ci,lb,cb,,' Inclusao de IGPM ')
                        DEVPOS(li+1,ci+1);DEVOUT('Mes')
                        DEVPOS(li+1,ci+7);DEVOUT('Valor IGPM')
                        @ li+2,ci+2 GET mmes PICT '99' VALID IF(mmes>12,.F.,.T.)
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF igpm->(DBSEEK(STRZERO(mmes,2)))
                                atencao('Ja existe este MES DO IGPM')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO point
                                LOOP
                        ENDIF
                        @ li+2,ci+07 GET mvlr_igpm PICT '9,999.9999'
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
                        igpm->mes := STRZERO(mmes,2)
                        igpm->vlr_igpm := mvlr_igpm
                        igpm->oper := cod_operado
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
                CASE LASTKEY() = ASC('A') .OR. LASTKEY() = ASC('a') .OR. LASTKEY() = 50
                        tela1 := SAVESCREEN(01,00,24,79)
                        mmes:= 0
                        mvlr_igpm := 0
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 45
                        setcor(3)
                        botao(li,ci,lb,cb,,' Alteracao de IGPM ')
                        DEVPOS(li+1,ci+1);DEVOUT('Mes')
                        DEVPOS(li+1,ci+7);DEVOUT('Valor IGPM')
                        @ li+2,ci+2 GET mmes PICT '99'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF ! igpm->(DBSEEK(STRZERO(mmes,2)))
                                atencao('Nao existe este Mes do IGPM')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO point
                                LOOP
                        ENDIF
                        mvlr_igpm:= igpm->vlr_igpm
                        @ li+2,ci+07 GET mvlr_igpm PICT '9,999.9999'
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
                        igpm->mes := STRZERO(mmes,2)
                        igpm->vlr_igpm := mvlr_igpm
                        igpm->oper := cod_operado
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
                CASE LASTKEY() = ASC('E') .OR. LASTKEY() = ASC('e') .OR. LASTKEY() = 51
                        tela1 := SAVESCREEN(01,00,24,79)
                        mmes:= 0
                        mvlr_igpm := 0
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 45
                        setcor(3)
                        botao(li,ci,lb,cb,,' Exclusao de IGPM ')
                        DEVPOS(li+1,ci+1);DEVOUT('Mes')
                        DEVPOS(li+1,ci+7);DEVOUT('Valor IGPM')
                        @ li+2,ci+2 GET mmes PICT '99'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF ! igpm->(DBSEEK(STRZERO(mmes,2)))
                                atencao('Nao existe esta IGPM')
                                RESTSCREEN(01,00,24,79,tela1)
                                GO point
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(li+2,ci+07);DEVOUTPICT(igpm->vlr_igpm,'9,999.9999')
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
