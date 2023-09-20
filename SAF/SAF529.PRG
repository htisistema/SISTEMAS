****************************
* MENU DE RELATORIO P/SEGURADORA
****************************

FUNCTION saf529
***************

LOCAL mopcao,opcao,mopcao1,lci,cci,lba,cba,tela,tela1,mtit,mpag,mtot_icm,mtraco,;
      mquantd

lci = 10
cci = 10
lba = 13
cba = 30
tela = SAVESCREEN(01,00,24,79)
opcao = 0
mtraco := REPLI('=',80)
WHILE .T.
        @ 23,00 CLEAR TO 23,79
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' Relatorio 1       ' MESSAGE '** Relacao p/Seguradora (ALUNO e RESPONSAVEL) - <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' Relatorio 2       ' MESSAGE '** Relacao P/Seguradora (ALUNO,RESPONSAVEL,PAI e MAE) - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                         saf5291()
                CASE opcao = 2
                        saf5292()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

