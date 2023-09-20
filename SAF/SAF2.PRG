*********************
* MENU DE MOVIMENTOS
*********************

FUNCTION saf2
***************

LOCAL opcao,tela,tecla,lci,cci,lba,cba

lci = 04
cci = 07
lba = 07
cba = 33
tela = SAVESCREEN(01,00,24,79)
opcao:=0
setcor(1)
****************************************************
WHILE .T.
        exibi_prg('SAF2')
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba,,' Movimentacao ')
        setcor(1)
        @ lci+1,cci+01 PROMPT ' Movimentacao FINANCEIRA ' MESSAGE '** Movimentacao Financeira - <ESC> Retornar **'
        @ lci+2,cci+01 PROMPT ' Movimentacao ACADEMICA  ' MESSAGE '** Movimentacao Academica - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        DO CASE
                CASE opcao = 1
        *               ACADEMICO
                         saf2_f1()
                CASE opcao = 2
        *               FINANCEIRO
                         saf2_a1()
        ENDCASE
ENDDO
RETURN NIL

*************************** F I M **************************************

*********************
* MENU DE MOVIMENTOS
*********************

FUNCTION saf2_f1
***************

LOCAL opcao,tela,tecla,lci,cci,lba,cba

lci = 09
cci = 07
lba = 16
cba = 30
tela = SAVESCREEN(01,00,24,79)
opcao:=0
setcor(1)
****************************************************
WHILE .T.
        exibi_prg('SAF2_F1')
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba,,' FINANCEIRA ')
        setcor(1)
        @ lci+1,cci+01 PROMPT ' Baixa de Pagamentos  ' MESSAGE '** Baixa de mensalidades - <ESC> p/Retornar **'
        @ lci+2,cci+01 PROMPT ' Inclusao Documentos  ' MESSAGE '** Inclusao de Documentos (mensalidades) - <ESC> p/Retornar **'
        @ lci+3,cci+01 PROMPT ' Alteracao Documentos ' MESSAGE '** Alteracao de Documentos (mensalidades) - <ESC> p/Retornar **'
        @ lci+4,cci+01 PROMPT ' Exclusao Documentos  ' MESSAGE '** Exclusao de Documentos (mensalidades) - <ESC> p/Retornar **'
        @ lci+5,cci+01 PROMPT ' Contas Apagar        ' MESSAGE '** Inclusao, Alt., Exc. e Estornos Documentos Apagar - <ESC> p/Retornar **'
        @ lci+6,cci+01 PROMPT ' Baixas Contas Apagar ' MESSAGE '** Baixas de Documentos Apagar - <ESC> p/Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        DO CASE
                CASE opcao = 1
        *               BAIXA DE PAGAMENTOS
                        saf20()
                CASE opcao = 2
        *               INCLUSAO DE DOCUMENTOS OU MENSALIDADES
                        saf203()
                CASE opcao = 3
        *               ALTERACAO DE DOCUMENTOS OU MENSALIDADES
                        saf204()
                CASE opcao = 4
        *               EXCLUSAO DE DOCUMENTOS OU MENSALIDADES
                        saf205()
                CASE opcao = 5
        *               CONTAS APAGAR
                        con21()
                CASE opcao = 6
        *               BAIXA DE DOCUMENTOS APAGAR
                        con22()
        ENDCASE
ENDDO
RETURN NIL

*************************** F I M **************************************
*********************
* MENU DE MOVIMENTOS
*********************

FUNCTION saf2_a1
***************

LOCAL opcao,tela,tecla,lci,cci,lba,cba

lci = 09
cci = 07
lba = 12
cba = 30
tela = SAVESCREEN(01,00,24,79)
opcao:=0
setcor(1)
****************************************************
WHILE .T.
        exibi_prg('SAF2_A1')
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba,,' ACADEMICA ')
        setcor(1)
        @ lci+1,cci+01 PROMPT ' Manutencao de Notas  ' MESSAGE '** Inclusao, Alteracao e Exclusao de NOTAS - <ESC> Retornar **'
        @ lci+2,cci+01 PROMPT ' Transf.Turma p/ Turma' MESSAGE '** Transferencia de aluno de uma turma p/outra - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        DO CASE
                CASE opcao = 1
        *               MANUTENCAO DE NOTAS
                         saf21()
                CASE opcao = 2
        *               MANUTENCAO DE NOTAS
                         saf22()
        ENDCASE
ENDDO
RETURN NIL

*************************** F I M **************************************



