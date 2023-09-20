*********************
* MENU DE CADASTRO
*********************

FUNCTION saf1
***************

LOCAL opcao,tela,tecla,lci,cci,lba,cba

lci:=04
cci:=02
lba:=15
cba:=26
tela = SAVESCREEN(01,00,24,79)
opcao:=0
setcor(1)
WHILE .T.
        exibi_prg('SAF1')
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' 0 -> Alunos           ' MESSAGE '** Cadastro de Receptores - <ESC> p/Retornar **'
        @ lci+2,cci+1 PROMPT ' 1 -> Operador         ' MESSAGE '** Cadastro de Operadores - <ESC> p/Retornar **'
        @ lci+3,cci+1 PROMPT ' 2 -> Mensalidades     ' MESSAGE '** Cadastro de Mensalidades - <ESC> p/Retornar **'
        @ lci+4,cci+1 PROMPT ' 3 -> Tab. GSTT        ' MESSAGE '** Cadastro de Tabela de GSTT - <ESC> p/Retornar **'
        @ lci+5,cci+1 PROMPT ' 4 -> Tab. de Bolsa    ' MESSAGE '** Cadastro de Tabela das BOLSAS - <ESC> p/Retornar **'
        @ lci+6,cci+1 PROMPT ' 5 -> Tab. Disciplina  ' MESSAGE '** Cadastro de Tabela das DISCIPLINAS - <ESC> p/Retornar **'
        @ lci+7,cci+1 PROMPT ' 6 -> Professores      ' MESSAGE '** Cadastro de PROFESSORES - <ESC> p/Retornar **'
        @ lci+8,cci+1 PROMPT ' 7 -> Contas Apagar    ' MESSAGE '** Cadastro de CONTAS apagar - <ESC> p/Retornar **'
        @ lci+9,cci+1 PROMPT ' 8 -> Conta Despesas   ' MESSAGE '** Cadastro de CONTAS DESPESAS - <ESC> p/Retornar **'
        @ lci+10,cci+1 PROMPT ' 9 -> Cadastro IGPM    ' MESSAGE '** Cadastro de IGPM do Mes - <ESC> p/Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        DO CASE
                CASE opcao = 1
        *               ALUNOS
                        saf10()
                CASE opcao = 2
        *               OPERADOR
                        senha1()
                CASE opcao = 3
        *               MENSALIDADES
                        saf11()
                CASE opcao = 4
        *               GSTT
                        saf12()
                CASE opcao = 5
        *               BOLSAS
                        saf13()
                CASE opcao = 6
        *               DISCIPLINAS
                        saf14()
                CASE opcao = 7
        *               PROFESSORES
                        saf15()
                CASE opcao = 8
        *               CONTAS APAGAR
                        ******************************************
                        CLOSE ALL
                        USE safccpg ALIAS ccpg SHARED NEW
                        SELE('ccpg');ORDSETFOCUS(2)
                        GO TOP
                        ******************************************
                        F6_ccpg('*')
                CASE opcao = 9
        *               CONTA DESPESAS
                        saf17()
                CASE opcao = 10
        *               IGPM DO MES
                        saf18()
        ENDCASE
ENDDO
RETURN NIL

*************************** F I M **************************************



