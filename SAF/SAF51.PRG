********************
* MENU DE RELATORIOS FINANCEIROS
********************

FUNCTION saf51
***************

LOCAL opcao,lci,cci,lba,cba,tela

lci = 03
cci = 18
lba = 22
cba = 45
tela = SAVESCREEN(01,00,24,79)
opcao = 0
WHILE .T.
        exibi_prg('SAF51')
        @ 23,00 CLEAR TO 23,79
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' 0 - Relacao Debitos      ' MESSAGE '** Relacao dos Alunos em DEBITOS - <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' 1 - Faturamento          ' MESSAGE '** Relatorio de Faturamento (Bolsistas, Bols.Parcial, Nao Blasista e Geral **'
        @ lci+3,cci+1 PROMPT ' 2 - Emissao de Etiquetas ' MESSAGE '** Emissao de Etiquetas - <ESC> Retornar **'
        @ lci+4,cci+1 PROMPT ' 3 - Bolsistas            ' MESSAGE '** Relatorio de Bolsistas - <ESC> Retornar **'
        @ lci+5,cci+1 PROMPT ' 4 - Carta de Cobranca    ' MESSAGE '** Emissao de Boletim (Geral, Individual e por GSTT) - <ESC> Retornar **'
        @ lci+6,cci+1 PROMPT ' 5 - Emissao de Carnes    ' MESSAGE '** Emissao de Carnes (DEBITOS ou do MES) - <ESC> Retornar **'
        @ lci+7,cci+1 PROMPT ' 6 - Emissao de Contrato  ' MESSAGE '** Emissao de Contrato - <ESC> Retornar **'
        @ lci+8,cci+1 PROMPT ' 7 - Emissao Carne Matric.' MESSAGE '** Emissao de Carne de Matricula - <ESC> Retornar **'
        @ lci+9,cci+1 PROMPT ' 8 - Declaracao Pag.IR    ' MESSAGE '** Emissao da Declaracao de pagamento de mensalidade p/IR - <ESC> Retornar **'
        @ lci+10,cci+1 PROMPT ' 9 - Relacao p/Seguro     ' MESSAGE '** Relacao dos Responsaveis e Aluno para Seguradora - <ESC> Retornar **'
        @ lci+11,cci+1 PROMPT ' A - Listagem Arq.Remessa ' MESSAGE '** Listagem de Arquivo de Remessa - <ESC> Retornar **'
        @ lci+12,cci+1 PROMPT ' B - Atualizacao Endereco ' MESSAGE '** Relatorio de Atualizacao de endereco - <ESC> Retornar **'
        @ lci+13,cci+1 PROMPT ' C - Tabela Mensalidades  ' MESSAGE '** Tabela de Mensalidades - <ESC> Retornar **'
        @ lci+14,cci+1 PROMPT ' D - Tabela Bolsas        ' MESSAGE '** Tabela de Bolsas - <ESC> Retornar **'
        @ lci+15,cci+1 PROMPT ' E - Contas Apagar        ' MESSAGE '** Relatorio Contas Apagar - <ESC> Retornar **'
        @ lci+16,cci+1 PROMPT ' F - Carta Alunos em dia  ' MESSAGE '** Emissao de Carta de Alunos em dia - <ESC> Retornar **'
        @ lci+17,cci+1 PROMPT ' G - Relacao Alunos       ' MESSAGE '** Relacao de Alunos - <ESC> Retornar **'
        @ lci+18,cci+1 PROMPT ' H - Emissao de Boletos   ' MESSAGE '** Emissao de Boletos - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                *       RELATORIO ALUNOS EM DEBITOS
                         saf520()
                CASE opcao = 2
                *       RELATORIO DO FATURAMENTO
                        saf521()
                CASE opcao = 3
                *       EMISSAO DE ETIQUETAS
                         saf522()
                CASE opcao = 4
                *       RELATORIO DE BOLSISTAS
                         saf523()
                CASE opcao = 5
                *       EMISSAO CARTA COBRANCA
                         saf524()
                CASE opcao = 6
                *       EMISSAO CARNES
                         saf525()
                CASE opcao = 7
                *       EMISSAO CONTRATO
                         saf526()
                CASE opcao = 8
                *       EMISSAO CARNE DE MATRICULA
                         saf527()
                CASE opcao = 9
                *       DECLARACAO DO IMPOSTO DE RENDA
                         saf528()
                CASE opcao = 10
                *       RELACAO P/SEGURADORA
                         saf529()
                CASE opcao = 11
                *       RELACAO P/SEGURADORA
                         safreme('*')
                CASE opcao = 12
                *       RELATORIO ATUALIZACAO DE ENDERECO
                         safende()
                CASE opcao = 13
                *        TABELA DE MENSALIDADES
                        saf511()
                CASE opcao = 14
                *        TABELA DE BOLSAS
                        saf512()
                CASE opcao = 15
                *       RELATORIO DE CONTAS APAGAR
                        con41()
                CASE opcao = 16
                *       CARTA DE ALUNOS EM DIA
                        saf5201()
                CASE opcao = 17
                *       RELACAO DE ALUNOS
                        saf530()
                CASE opcao = 18
                *       EMISSAO DE BOLETOS
                        safbolet()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

