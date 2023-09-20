************************************
* MENU BAIXA E ESTORNO DE PAGAMENTOS
************************************

FUNCTION saf20
**************

LOCAL tela,opcao,lci,cci,lba,cba

tela := SAVESCREEN(00,00,24,79)

lci := 05
cci := 08
lba := 15
cba := 72
WHILE .T.
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula.........:')
        DEVPOS(lci+2,cci+1);DEVOUT('Nome..............:')
        DEVPOS(lci+3,cci+1);DEVOUT('GSTT..............:')
        DEVPOS(lci+3,cci+30);DEVOUT('Bolsa:')
        DEVPOS(lci+4,cci+1);DEVOUT('Parcela...[01..12]:')
        DEVPOS(lci+5,cci+1);DEVOUT('Valor Parcela...R$:')
        DEVPOS(lci+5,cci+30);DEVOUT('Desconto Bolsa.R$:')
        DEVPOS(lci+6,cci+1);DEVOUT('Data Vencimento...:')
        DEVPOS(lci+7,cci+1);DEVOUT('Valor a Pagar...R$:')
        DEVPOS(lci+8,cci+1);DEVOUT('Data Pagamento....:')
        DEVPOS(lci+9,cci+1);DEVOUT('[C]arteira [B]anco:')
        limpa(23,00,23,79)
        opcao := 0
        DEVPOS(23,00);DEVOUT('Escolha a OPCAO:')
        setcor(1)
        @ 23,COL()+2 PROMPT ' BAIXA '
        @ 23,COL()+2 PROMPT ' ESTORNO '
        @ 23,COL()+2 PROMPT ' RETORNA '
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        DO CASE
                CASE opcao = 1
                        saf201()
                CASE opcao = 2
                        saf202()
                CASE opcao = 3
                        RESTSCREEN(00,00,24,79,tela)
                        RETURN NIL
        ENDCASE
ENDDO
RETURN NIL
