********************************************
**** CON416.PRG: Etiquetas de Fornecedores
********************************************

FUNCTION con416
***************
LOCAL MPRG:='CON416'

LOCAL tela,esc,f_ini,f_fim

tela := SAVESCREEN(01,00,24,79)

botao(9, 27, 13, 51)
@ 9, 29 SAY "¶ Etiquetas Gomadas Ç"
@ 11, 29 SAY "Forn. Inicial.:"
@ 12, 29 SAY "Forn. Final...:"
f_ini="    "
f_fim="    "
@ 11,46 GET f_ini PICT "9999"
@ 12,46 GET f_fim PICT "9999"
READ
IF LASTKEY()=27
        RESTSCREEN(01,00,24,79,tela)
        CLOSE ALL
        RETURN
ENDIF
IF f_ini=SPACE(4)
        f_ini="0000"
ENDIF
IF f_fim=SPACE(4)
        f_fim="9999"
ENDIF
RESTSCREEN(01,00,24,79,tela)
botao(9, 28, 18, 50)
@ 9, 33 SAY "¶ Etiquetas Ç"
@ 10, 28 SAY 'Ã'
@ 10, 50 SAY '´'
@ 14, 28 SAY 'Ã'
@ 14, 50 SAY '´'
@ 18, 38 SAY "Á"
@ 10, 29 SAY replicate('Ä',9)+"Â"+replicate('Ä',11)
@ 14, 29 SAY replicate('Ä',9)+"Å"+replicate('Ä',11)
@ 11, 30 PROMPT "Normal  ³ 1 Coluna "
@ 12, 30 PROMPT "Normal  ³ 2 Colunas"
@ 13, 30 PROMPT "Normal  ³ 3 Colunas"
@ 15, 30 PROMPT "Contato ³ 1 Coluna "
@ 16, 30 PROMPT "Contato ³ 2 Colunas"
@ 17, 30 PROMPT "Contato ³ 3 Colunas"
MENU TO esc
IF LASTKEY()= 27
        RESTSCREEN(01,00,24,79,tela)
        CLOSE ALL
        RETURN
ENDIF
RESTSCREEN(01,00,24,79,tela)
quadro("Tecle [ÄÙ] Para Imprimir, ou [ESC] Para Sair.")
IF LASTKEY()=27
        RESTSCREEN(01,00,24,79,tela)
        CLOSE ALL
        RETURN
ENDIF
*------------------------------------------------------------------
CLOSE ALL
USE safccpg ALIAS ccpg SHARED NEW
*------------------------------------------------------------------
**************
SELE('ccpg');ORDSETFOCUS(1)
**************
RESTSCREEN(01,00,24,79,tela)
WHILE ! isprinter()
        quadro("Impressora nao esta Ligada! [ÄÙ] Imprimir, ou [ESC] Abandona.")
        ?? chr(7)
        INKEY(0)
        IF LASTKEY()=27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN
        ENDIF
ENDDO
RESTSCREEN(01,00,24,79,tela)
quadro("Imprimindo Etiquetas. [ESC] Interrompe")
SET CONSO off
SET PRINT on
DO CASE
        CASE esc=1
                LABEL FORM etiq11 TO PRINT for cod_forn>=f_ini .AND. cod_forn<=f_fim WHILE ! tecla()
        CASE esc=2
                LABEL FORM etiq12 TO PRINT for cod_forn>=f_ini .AND. cod_forn<=f_fim  WHILE ! tecla()
        CASE esc=3
                LABEL FORM etiq13 TO PRINT for cod_forn>=f_ini .AND. cod_forn<=f_fim  WHILE ! tecla()
        CASE esc=4
                LABEL FORM etiq21 TO PRINT for cod_forn>=f_ini .AND. cod_forn<=f_fim  WHILE ! tecla()
        CASE esc=5
                LABEL FORM etiq22 TO PRINT for cod_forn>=f_ini .AND. cod_forn<=f_fim  WHILE ! tecla()
        CASE esc=6
                LABEL FORM etiq23 TO PRINT for cod_forn>=f_ini .AND. cod_forn<=f_fim  WHILE ! tecla()
ENDCASE
SET PRINT off
SET CONSO on
RESTSCREEN(01,00,24,79,tela)
CLOSE ALL
RETURN

