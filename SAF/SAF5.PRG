********************
* MENU DE RELATORIOS
********************

FUNCTION saf5
***************

LOCAL opcao,lci,cci,lba,cba,tela

lci = 04
cci = 48
lba = 08
cba = 75
tela = SAVESCREEN(01,00,24,79)
opcao = 0
WHILE .T.
        exibi_prg('SAF5')
        @ 23,00 CLEAR TO 23,79
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' 0 - Relatorio ACADEMICOS ' MESSAGE '** Relacao dos Alunos Geral (p/ordem ALFA E MATRICULA) - <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' 1 - Relatorio FINANCEIRO ' MESSAGE '** Emissao de Boletim (Geral, Individual e por GSTT) - <ESC> Retornar **'
        @ lci+3,cci+1 PROMPT ' 2 - Relatorio CADASTRO   ' MESSAGE '** Relatorio de CADSTRO dos CLIENTES - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                *       RELATORIO ACADEMICOS
                         saf50()
                CASE opcao = 2
                *       RELATORIO FINANCEIROS
                         saf51()
                CASE opcao = 3
                *       RELATORIO DE CADASTRO DOS ALUNOS
                         saf1cad()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

