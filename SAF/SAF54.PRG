********************
* MENU DE ETIQUETAS
********************

FUNCTION saf54
***************

LOCAL opcao,lci,cci,lba,cba,tela

lci = 10
cci = 17
lba = 14
cba = 40
tela = SAVESCREEN(01,00,24,79)
opcao = 0
mtraco := REPLI('=',80)
WHILE .T.
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        CAIXA(mold,lci,cci,lba,cba,,,.T.)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' Etiquetas Cobrancas  ' MESSAGE '** Emissao de Etiquetas para Cobrancas - <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' Etiquetas Alunos     ' MESSAGE '** Emissao de Etiquetas de Alunos - <ESC> Retornar **'
        @ lci+3,cci+1 PROMPT ' Etiquetas Colegio    ' MESSAGE '** Emissao de Etiquetas do COLEGIO - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                *          EMISSAO DO PRE-PEDIDO
                         saf541()
                CASE opcao = 2
                *          EMISSAO DE ALUNOS
                         saf542()
                CASE opcao = 3
                *          EMISSAO DO COLEGIO
                         saf543()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

