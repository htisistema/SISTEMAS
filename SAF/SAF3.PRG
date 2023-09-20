*********************
* MENU DE MOVIMENTOS
*********************

FUNCTION saf3
***************

LOCAL opcao,tela,tecla,lci,cci,lba,cba

lci = 04
cci = 17
lba = 15
cba = 47
tela = SAVESCREEN(01,00,24,79)
opcao:=0
setcor(1)
****************************************************
WHILE .T.
        exibi_prg('SAF3')
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba,,' C A I X A ')
        setcor(1)
        @ lci+1,cci+01 PROMPT ' Recebimento MENSALIDADES    ' MESSAGE '** Recebimentos de MENSALIDADES - <ESC> Retornar **'
        @ lci+2,cci+01 PROMPT ' Recebimento DIVERSOS        ' MESSAGE '** Recebimentos DIVERSOS - <ESC> Retornar **'
        @ lci+3,cci+01 PROMPT ' Retirada para DEPOSITO      ' MESSAGE '** Retirada para DEPOSITO - <ESC> Retornar **'
        @ lci+4,cci+01 PROMPT ' Fechamento do CAIXA         ' MESSAGE '** Fechamento do CAIXA - <ESC> Retornar **'
        @ lci+5,cci+01 PROMPT ' Retirada de DESPESA         ' MESSAGE '** Retirada de DESPESA - <ESC> Retornar **'
        @ lci+6,cci+01 PROMPT ' Alt.ou Cancelar DESPESA     ' MESSAGE '** Alterar ou Cancelar DESPESA - <ESC> Retornar **'
        @ lci+7,cci+01 PROMPT ' Fundo de Caixa              ' MESSAGE '** Inclusao do Fundo de Caixa - <ESC> Retornar **'
        @ lci+8,cci+01 PROMPT ' 2a.via Recibo ou Autenticar ' MESSAGE '** Emitir 2a.via do Recibo ou Autenticar - <ESC> Retornar **'
        @ lci+9,cci+01 PROMPT ' Estorno de Recebimento      ' MESSAGE '** Estorno de Recebimentos - <ESC> Retornar **'
        @ lci+10,cci+01 PROMPT ' Preenchimento de Cheques    ' MESSAGE '** Preenchimento de Cheques formulario continuo - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        DO CASE
                CASE opcao = 1
        *               RECEBIMENTO MENSALIDADES
                         saf201('*')
                CASE opcao = 2
        *               RECEBIMENTOS DIVERSOS
                        saf301()
                CASE opcao = 3
        *               RETIRADA P/DEPOSITO
                        saf37()
                CASE opcao = 4
        *               FECHAMENTO DO CAIXA
                        saf33()
                CASE opcao = 5
        *               RETIRADA DE DESPESAS
                        saf31_1()
                CASE opcao = 6
        *               ALTERAR OU CANCELAR DE DESPESAS
                        saf31_3()
                CASE opcao = 7
        *               INCLUSAO DO FUNDO DE CAIXA
                        saf36()
                CASE opcao = 8
        *               2a.VIA DO RECIBO OU AUTENTICAR
                        saf32()
                CASE opcao = 9
        *               ESTORNO DE RECEBIMENTOS
                        saf202()
                CASE opcao = 10
        *               PREENCHIMENTO DE CHEQUES
                        safpcheq()
        ENDCASE
ENDDO
RETURN NIL

*************************** F I M **************************************
