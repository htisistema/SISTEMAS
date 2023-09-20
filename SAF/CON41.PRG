**** CON41.PRG: Menu de Relatorios  (Pagar)
***
********************************************

FUNCTION con41
***************
LOCAL MPRG:='CON41'

LOCAL tela,op41,lci,cci,lba,cba

lci := 08
cci := 11
lba := 13
cba := 42

tela := SAVESCREEN(01,00,24,79)
botao(lci,cci,lba,cba)

op41 := 0
WHILE .T.
        @ lci+1,cci+1 PROMPT "    Duplicatas Recebidas      "
        @ lci+2,cci+1 PROMPT " Duplicatas a Vencer/Vencidas "
        @ lci+3,cci+1 PROMPT "      Duplicatas Pagas        "
        @ lci+4,cci+1 PROMPT "  Emissao de Recibo Pagamento "
*       @ lci+5,cci+1 PROMPT "    Emissao de Etiquetas      "
        MENU TO op41
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        DO CASE
                CASE op41=1
                        con411()
                CASE op41=2
                        con413()
                CASE op41=3
                        con414()
                CASE op41=4
                        con418()
*               CASE op41=5
*                       con416()
        ENDCASE
ENDDO











