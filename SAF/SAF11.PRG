*************************
* CADASTRO DE MENSALIDADES
*************************
FUNCTION saf11
***************
LOCAL mprg:='SAF11'
LOCAL tela1,tela,li,ci,lb,cb,lci,cci,lba,cba,point,pode,men,f,mparcela,mgst,mvalor,;
      mdata_venc

IF ! ver_nivel(mprg,'CADASTRO DE MENSALIDADES','125')
        RETURN NIL
ENDIF

lci := 03
cci := 30
lba := 21
cba := 75

li := 03
ci := 20
lb := 21
cb := 65

tela := SAVESCREEN(01,00,24,79)
setcor(3)
botao(lci,cci,lba,cba)
*@ lci+2,cci+1 TO lci+2,cba-1
setcor(1)

janela(lci,cci,' Mensalidades ','*','*')
DEVPOS(lci+1,cci+1);DEVOUT('Parcela')
DEVPOS(lci+1,cci+10);DEVOUT('GSTT')
DEVPOS(lci+1,cci+16);DEVOUT('Valor R$')
DEVPOS(lci+1,cci+26);DEVOUT('Dia Venc.')
DEVPOS(lci+1,cci+37);DEVOUT('Operador')
pode=.t.
*----------------------------------------------
CLOSE ALL
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
*--------------------------------------------------
**************
SELE('tabmen');ORDSETFOCUS(3)
ORDSETFOCUS(2)
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
                                DEVPOS(lci+1+f,cci+1);DEVOUT('                                ')
                                setcor(1)
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(lci+1+f,cci+1);DEVOUT('  '+tabmen->parcela+'      '+tabmen->gst+'  '+TRANSFORM(tabmen->valor,'9,999.99')+'  '+tabmen->data_venc+'       '+tabmen->operador)
                        setcor(1)
                        SKIP
                ELSE
                        setcor(3)
                        DEVPOS(lci+1+f,cci+1);DEVOUT('                                          ')
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
                        mparcela := '  '
                        mgst     := '   '
                        mvalor   := 0
                        mdata_venc := '     '
                        li := 13
                        ci := 02
                        lb := 16
                        cb := 48

                        setcor(3)
                        botao(li,ci,lb,cb)
                        setcor(1)
                        janela(li,ci,' Inclusao da Mensalidades ','*','*')
                        DEVPOS(li+1,ci+1);DEVOUT('Parcela')
                        DEVPOS(li+1,ci+10);DEVOUT(' GST')
                        DEVPOS(li+1,ci+16);DEVOUT('Valor R$')
                        DEVPOS(li+1,ci+26);DEVOUT('Dia Venc.')
                        @ li+2,ci+3 GET mparcela PICT '@!' VALID mparcela $ '01,02,03,04,05,06,07,08,09,10,11,12'
                        @ li+2,ci+11 GET mgst PICT '@!'
                        READ
                        IF LASTKEY() = 27
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        IF tabmen->(DBSEEK(mparcela+mgst))
                                atencao('Esta parcela deste GST ja estar cadastrada')
                                RESTSCREEN(01,00,24,79,tela1)
                                LOOP
                        ENDIF
                        mdata_venc := '05/'+mparcela
                        @ li+2,ci+17 GET mvalor PICT '9,999.99'
                        @ li+2,ci+26 GET mdata_venc PICT '99/99' VALID DDMM(mdata_venc)
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
                        tabmen->gst := mgst
                        tabmen->parcela := mparcela
                        tabmen->valor   := mvalor
                        tabmen->operador := cod_operado
                        tabmen->data_venc := mdata_venc
                        DBCOMMITALL()
                        DBUNLOCKALL()
                        RESTSCREEN(01,00,24,79,tela1)
                        LOOP
                CASE LASTKEY() = ASC('A') .OR. LASTKEY() = ASC('a') .OR. LASTKEY() = 50
                        saf111()
                CASE LASTKEY() = ASC('E') .OR. LASTKEY() = ASC('e') .OR. LASTKEY() = 51
                        saf112()
        ENDCASE
ENDDO
RETURN NIL
*************************** f i m *******************************************
