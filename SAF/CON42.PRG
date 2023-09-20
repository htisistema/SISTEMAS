*** con42.PRG: Menu de Relatorios  (Receber)
***
***********************************************

FUNCTION con42
***************
LOCAL MPRG:='CON42'

LOCAL tela,op42,lci,cci,lba,cba

lci := 05
cci := 16
lba := 15
cba := 52

tela := SAVESCREEN(01,00,24,79)

WHILE .T.
        exibi_prg(mprg)
        botao(lci,cci,lba,cba,,' Relatorio Doc. A RECEBER ')
        limpa(23,00,23,79)
        @ lci+1,cci+1 PROMPT " Documentos Emitidas              "  MESSAGE '** Relatorio de Documentos Emitidas - <ESC>abandonar **'
        @ lci+2,cci+1 PROMPT " Analitico dos Clientes           "  MESSAGE '** Relatorio de Documento/Cliente (vencidos/vencer/pagos) - <ESC>abandonar **'
        @ lci+3,cci+1 PROMPT " Documentos a Vencer/Vencidas     "  MESSAGE '** Relatorio de Documento (A VENCER e VENCIDAS) - <ESC> Abandonar'
        @ lci+4,cci+1 PROMPT " Documentos Pagas                 "  MESSAGE '** Relatorio de Documento PAGAS - <ESC> Abandonar'
        @ lci+5,cci+1 PROMPT " Emissao de Documento             "  MESSAGE '** Emissao de Duplicata - <ESC> Abandonar'
        @ lci+6,cci+1 PROMPT " Documento a Receber X a Pagar    "  MESSAGE '** Relatorio de Documento A Receber X Apagar - <ESC>Abandonar **'
        @ lci+7,cci+1 PROMPT " Emissao de Faturamento p/Cliente "  MESSAGE '** Relatorio de Faturamento p/Clientes - <ESC>Abandonar **'
        @ lci+8,cci+1 PROMPT " Carta de Cobranca p/Clientes     "  MESSAGE '** Emissao de Cartas de Cobranca p/Cliente  - <ESC>Abandonar **'
        @ lci+9,cci+1 PROMPT " Demonstrativo de DEBITO          "  MESSAGE '** Emissao de Demonstrativo de DEBITO p/Cliente  - <ESC>Abandonar **'
        MENU TO op42
        DO CASE
                CASE op42=0
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN
                CASE op42=1
                        * DOCUMENTOS EMITIDOS
                        con420()
                CASE op42=2
                        * ANALITICOS DE CLIENTES
                        con421()
                CASE op42=3
                        * DOCUMENTOS A VENCER/VENCIDAS
                        con423()
                CASE op42=4
                        * DOCUMENTOS PAGOS
                        con424()
                CASE op42=5
                        con428()
                CASE op42=6
                        con419()
                CASE op42=7
                        con429()
                CASE op42=8
                        con425()
                CASE op42=9
                        con252(,'*')
        ENDCASE
ENDDO
*********************** f i m ********************************










