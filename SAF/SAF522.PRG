********************
* MENU DE ETIQUETAS
********************

FUNCTION saf522
***************

LOCAL opcao,lci,cci,lba,cba,tela

lci = 10
cci = 17
lba = 18
cba = 45
tela = SAVESCREEN(01,00,24,79)
opcao = 0
mtraco := REPLI('=',80)
WHILE .T.
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba,,' MENU DE ETIQUETAS ')
        setcor(1)
        @ lci+1,cci+1 PROMPT ' Etiquetas Cobrancas     ' MESSAGE '** Emissao de Etiquetas para Cobrancas - <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' Etiquetas Alunos        ' MESSAGE '** Emissao de Etiquetas de Alunos - <ESC> Retornar **'
        @ lci+3,cci+1 PROMPT ' Alunos p/Cardeneta Prof.' MESSAGE '** Emissao de Etiquetas de Alunos p/cardeneta Professores - <ESC> Retornar **'
        @ lci+4,cci+1 PROMPT ' Etiquetas Responsavel   ' MESSAGE '** Emissao de Etiquetas dos Responsavel - <ESC> Retornar **'
        @ lci+5,cci+1 PROMPT ' Etiquetas Colegio       ' MESSAGE '** Emissao de Etiquetas do COLEGIO - <ESC> Retornar **'
        @ lci+6,cci+1 PROMPT ' Etiquetas p/Ficha       ' MESSAGE '** Emissao de Etiquetas p/Ficha de frequencia - <ESC> Retornar **'
        @ lci+7,cci+1 PROMPT ' Etiquetas Personalizada ' MESSAGE '** Emissao de Etiquetas p/Personalizada - <ESC> Retornar **'
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
                         saf5221()
                CASE opcao = 2
                *          EMISSAO DE ALUNOS
                         saf5222()
                CASE opcao = 3
                *          EMISSAO DE etiq. ALUNOS P/CARDENETA PROF.
                         saf5226()
                CASE opcao = 4
                *          EMISSAO DE etiq. RESPONSAVEL
                         saf5224()
                CASE opcao = 5
                *          EMISSAO DO COLEGIO
                         saf5223()
                CASE opcao = 6
                *        EMISSAO DE ETIQUETA DA FICHA DE FREQUENCIA
                         saf5225()
                CASE opcao = 7
                *        EMISSAO DE ETIQUETA DA PERSONALIZADA
                         saf5etq4()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

