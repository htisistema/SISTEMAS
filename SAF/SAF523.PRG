********************
* MENU DE BOLSISTAS
********************

FUNCTION saf523
***************

LOCAL opcao,lci,cci,lba,cba,tela

lci = 13
cci = 12
lba = 19
cba = 37
tela = SAVESCREEN(01,00,24,79)
opcao = 0
mtraco := REPLI('=',80)
WHILE .T.
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' Por Bolsa Indiv.      ' MESSAGE '** Relatorio por Bolsa e Mes - <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' Bolsa Indiv.End.      ' MESSAGE '** Relatorio por Bolsa, Mes e Endereco- <ESC> Retornar **'
        @ lci+3,cci+1 PROMPT ' Bolsa Geral           ' MESSAGE '** Relatorios de Alunos BOLSISTA GERAL - <ESC> Retornar **'
        @ lci+4,cci+1 PROMPT ' Resumo de Bolsas      ' MESSAGE '** Relatorios do Resumo das BOLSAS GERAL - <ESC> Retornar **'
        @ lci+5,cci+1 PROMPT ' Bolsa Indiv.End.Anual ' MESSAGE '** Relatorios por Bolsa e Endereco ANUAL - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                *        RELATORIO POR BOLSAS INDIVIDUAL
                         saf5231()
                CASE opcao = 2
                *        RELATORIO POR BOLSAS INDIVIDUAL ENDERECO
                         saf5233()
                CASE opcao = 3
                *        RELATORIOS DE BOLSISTA GERAL
                         saf5232()
                CASE opcao = 4
                *        RELATORIOS DE BOLSISTA GERAL
                         saf5234()
                CASE opcao = 5
                *        RELATORIOS DE BOLSISTA COM ENDERECO ANUAL
                         saf5235()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

