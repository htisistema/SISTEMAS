FUNCTION TESTE

LOCAL letra:=SPACE(1)

CLEAR
DEVPOS(00,00);DEVOUT('Tecle alguma tecla para ver o codigo ASCII')
WHILE .T.
        INKEY(0)
        IF LASTKEY() = 27
                EXIT
        ENDIF
        DEVPOS(02,00);DEVOUT(LASTKEY())
ENDDO
CLEAR
RETURN NIL
