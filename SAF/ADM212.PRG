*  ADM212.PRG: Alteracao de Visitas
*************************************

FUNCTION adm212

LOCAL mcodigo,mdatavis,mobs,mtela1:=SAVESCREEN(00,00,24,79)

IF ! AbriArq('visita','vis');RETURN NIL;ENDIF
IF ! AbriArq('regiao','reg');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
WHILE .T.
        RESTSCREEN(00,00,24,79,mtela1)
        **************************
        SELE('cli');ORDSETFOCUS(1)
        **************************
        mensagem(" Mensagem: Digite o C¢digo do Cliente. [ESC] Abandona.")
        mcodigo=0
        mdatavis=CTOD("  /  /  ")
        mobs=SPACE(60)
        @ lci+2,cci+14 GET mcodigo PICTURE "99999"
        READ
        IF LASTKEY()=27 .OR. EMPTY(mcodigo)
                CLOSE ALL
                RETURN
        ENDIF
        SEEK STRZERO(mcodigo,5)
        IF ! FOUND()
                atencao("N„o existe nenhuma visita marcada neste cliente")
                LOOP
        ENDIF
        setcor(3)
        @ lci+2,cci+20 SAY cli->razao
        @ lci+3,cci+14 SAY cli->regiao
        ver_regi(cli->regiao,lci+3,cci+20)
        ver_ven(reg->codvend)
        @ lci+4,cci+14 SAY sen->scod_op+' - '+sen->snome
        setcor(1)
        @ lci+5,cci+14 GET mdatavis VALID IF(EMPTY(mdatavis),.F.,.T.)
        READ
        IF LASTKEY()=27
                LOOP
        ENDIF
        **************************
        SELE('vis');ORDSETFOCUS(1)
        **************************
        SEEK STRZERO(mcodigo,5)+DTOS(mdatavis)
        IF ! FOUND()
                atencao("N„o existe nenhuma visita marcada p/esta DATA")
                LOOP
        ENDIF
        mobs := vis->obs
        @ lci+5,cci+14 GET mdatavis VALID IF(EMPTY(mdatavis) .OR. mdatavis < mdata_sis,.F.,.T.)
        @ lci+6,cci+14 GET mobs PICT '@!'
        READ
        IF LASTKEY()=27
                LOOP
        ENDIF
        mensagem("Confirma os dados digitados ? [S/n]:")
        s=INKEY(0)
        IF s=asc("n") .OR. s=asc("N") .OR. LASTKEY() = 27
                LOOP
        ENDIF
        mensagem("ÍÍ Salvando dados !!!")
        SELE('vis')
        BLOQREG()
        vis->codigo := STRZERO(mcodigo,5)
        vis->data   := mdatavis
        vis->obs    := mobs
        vis->ok     := .F.
        DBCOMMIT()
        DBUNLOCK()
        SELE('cli');ORDSETFOCUS(1)
ENDDO
RETURN NIL


