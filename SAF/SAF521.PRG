********************
* MENU DE FATURAMENTO
********************

FUNCTION saf521
***************

LOCAL opcao,lci,cci,lba,cba,tela

lci = 13
cci = 17
lba = 17
cba = 37
tela = SAVESCREEN(01,00,24,79)
opcao = 0
mtraco := REPLI('=',80)
WHILE .T.
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' Mapa Financeiro   ' MESSAGE '** Emissao do Mapa Financeiro - <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' Faturamento       ' MESSAGE '** Relatorios de Faturamrnto - <ESC> Retornar **'
        @ lci+3,cci+1 PROMPT ' Alunos c/Desconto ' MESSAGE '** Relatorios de Faturamrnto - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                *          EMISSAO DO MAPA FINANCEIRO
                         saf5211()
                CASE opcao = 2
                *        RELATORIOS DE FATURAMENTO
                         saf5212()
                CASE opcao = 3
                *        RELATORIOS DE FATURAMENTO CLIENTES C/DESCONTO
                         saf5213()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

