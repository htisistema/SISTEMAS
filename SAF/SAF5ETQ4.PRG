***********************************
* MENU DE ETIQUETAS PERSONALIZADAS
***********************************

FUNCTION saf5etq4()
****************
LOCAL MPRG:='SAC5ETQ4'

LOCAL opcao,lci,cci,lba,cba,tela

lci := 15
cci := 06
lba := 19
cba := 25

tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        setcor(1)
        limpa(23,00,23,79)
        botao(lci,cci,lba,cba)
        @ lci+1,cci+1 TO lci+1,cba-1
        janela(lci,cba,' Produtos ')
        @ lci+2,cci+1 PROMPT ' Etiqueta Normal  ' MESSAGE '*** Emissao de Etiquetas Padrao - <ESC> p/abandonar ***'
        @ lci+3,cci+1 PROMPT ' Etiqueta Pequena ' MESSAGE '*** Emissao de Etiquetas Pequenas - <ESC> p/abandonar ***'
        MENU TO opcao
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                EXIT
        ENDIF
        DO CASE
                CASE opcao = 1
                        sac5et41()
                CASE opcao = 2
                        sac5et42()
        ENDCASE
ENDDO
*********************************** F I M ************************************

