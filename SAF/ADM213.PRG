*  ADM213.PRG: Exclusao de Visitas
*************************************

FUNCTION adm213

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
        @ lci+2,cci+20 SAY razao
        @ lci+3,cci+14 SAY regiao
        ver_regi(cli->regiao,lci+3,cci+20)
        ver_ven(cli->codvend)
        @ lci+4,cci+14 SAY sen->scod_op+' - '+sen->snome
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
        setcor(3)
        @ lci+6,cci+14 SAY mobs
        setcor(1)
        mensagem("Confirma a EXCLUSAO da Visita ? [S/n]:")
        s=INKEY(0)
        IF s=asc("n") .OR. s=asc("N") .OR. LASTKEY() = 27
                LOOP
        ENDIF
        mensagem("ÍÍ Salvando dados !!!")
        SELE('vis')
        BLOQREG()
        DELE
        DBCOMMIT()
        DBUNLOCK()
        SELE('cli');ORDSETFOCUS(1)
ENDDO
RETURN NIL


