**********************
* MENU DE UTILITARIOS
**********************

FUNCTION saf6
***************

LOCAL opcao,lci,cci,lba,cba,tela,mdata_sis,mopcao,drverr

lci = 04
cci = 47
lba = 17
cba = 74

tela = SAVESCREEN(01,00,24,79)

opcao = 0

DO WHILE .T.
        exibi_prg('SAF6')
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' Muda Data                ' MESSAGE '** Muda a Data do Sistema Operacional, <ESC> p/Retorno **'
        @ lci+2,cci+1 PROMPT ' Configuracao Sistema     ' MESSAGE '** Configuracoes do Sistema, <ESC> p/Retorno **'
        @ lci+3,cci+1 PROMPT ' Calculadora              ' MESSAGE '** Calculadora do sistema, <ESC> p/Retorno **'
        @ lci+4,cci+1 PROMPT ' Calendario               ' MESSAGE '** Calendario do sistema, <ESC> p/Retorno **'
        @ lci+5,cci+1 PROMPT ' Disco Seguradora         ' MESSAGE '** Gerar disco de mensalidade p/seguradora <ESC> p/Retorno **'
        @ lci+6,cci+1 PROMPT ' Gerar No.de chamada      ' MESSAGE '** Gerar o Numero de Chamada dos Alunos <ESC> p/Retorno **'
        @ lci+7,cci+1 PROMPT ' Disco de Carnes (SISMETA)' MESSAGE '** Gerar Disco de Carnes p/impressao (SISMETA), <ESC> p/Retorno **'
        @ lci+8,cci+1 PROMPT ' Disco de Carnes (ELOGICA)' MESSAGE '** Gerar Disco de Carnes p/impressao (ELOGICA), <ESC> p/Retorno **'
        @ lci+9,cci+1 PROMPT ' Gerar Arquivo Remessa    ' MESSAGE '** Gerar Arquivo de Remessa de Pagamento do Banco, <ESC> p/Retorno **'
        @ lci+10,cci+1 PROMPT ' Gerar Disco p/Contrato   ' MESSAGE '** Gerar Disco para Impresao Contrato, <ESC> p/Retorno **'
        @ lci+11,cci+1 PROMPT ' Gerar Disco Promissoaria ' MESSAGE '** Gerar Disco para Carne de matricula e Promissoria, <ESC> p/Retorno **'
        @ lci+12,cci+1 PROMPT ' Recibo da INSERCOM       ' MESSAGE '** Emissao de Recibo da INSERCOM - <ESC> p/Retorno **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                        alt_data()
                CASE opcao = 2
                        saf61()
                CASE opcao = 3
                        maqcalc()
                CASE opcao = 4
                        cale()
                CASE opcao = 5
                        safdisco()
                CASE opcao = 6
                        safnum()
                CASE opcao = 7
                        safdcarn()
                CASE opcao = 8
                        safdcar1()
                CASE opcao = 9
                        safreme()
                CASE opcao = 10
                        safcontr()
                CASE opcao = 11
                        safpromi()
                CASE opcao = 12
                        saf63()
        ENDCASE
ENDDO
RETURN NIL
**************************** F I M **********************************
