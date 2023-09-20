********************
* MENU DE FINANCEIRO
********************

FUNCTION saf5212
***************

LOCAL opcao,lci,cci,lba,cba,tela

lci = 14
cci = 37
lba = 17
cba = 55
tela = SAVESCREEN(01,00,24,79)
opcao = 0
mtraco := REPLI('=',80)
WHILE .T.
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' Individual      ' MESSAGE '** Relatorio Fianaceiro Individual - <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' GERAL           ' MESSAGE '** Relatorio Financeiro GERAL - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                *          RELATORIO FINANCEIRO INDIVIDUAL
                         saf52121()
                CASE opcao = 2
                *        RELATORIOS DE FINANCEIRO GERAL
                         saf52122()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

