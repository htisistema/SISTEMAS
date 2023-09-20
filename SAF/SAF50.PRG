********************
* MENU DE RELATORIOS ACADEMICOS
********************

FUNCTION saf50
***************

LOCAL opcao,lci,cci,lba,cba,tela

lci = 04
cci = 18
lba = 21
cba = 45
tela = SAVESCREEN(01,00,24,79)
opcao = 0
WHILE .T.
        exibi_prg('SAF50')
        @ 23,00 CLEAR TO 23,79
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' 0 - Relacao de Alunos    ' MESSAGE '** Relacao dos Alunos Geral (p/ordem ALFA E MATRICULA) - <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' 1 - Emissao de Boletim   ' MESSAGE '** Emissao de Boletim (Geral, Individual e por GSTT) - <ESC> Retornar **'
        @ lci+3,cci+1 PROMPT ' 2 - Mapa de Notas        ' MESSAGE '** Emissao do Mapa das Medias por UNIDADE/TURMA - <ESC> Retornar **'
        @ lci+4,cci+1 PROMPT ' 3 - Mapa de Medias       ' MESSAGE '** Emissao do Mapa de Medias por UNIDADE/DISCIPLINA - <ESC> Retornar **'
        @ lci+5,cci+1 PROMPT ' 4 - Ata para Professor   ' MESSAGE '** Ata de Alunos p/Professor - <ESC> Retornar **'
        @ lci+6,cci+1 PROMPT ' 5 - Folha Aval.Ed.Fisica ' MESSAGE '** Folha para avaliacao de Educacao Fisica - <ESC> Retornar **'
        @ lci+7,cci+1 PROMPT ' 6 - Folha de Frequencia  ' MESSAGE '** Folha de Frequencia de Aluno - <ESC> Retornar **'
        @ lci+8,cci+1 PROMPT ' 7 - Folha de Avaliacao   ' MESSAGE '** Folha de Avaliacao - <ESC> Retornar **'
        @ lci+9,cci+1 PROMPT ' 8 - Ficha Individual     ' MESSAGE '** Ficha Individual - <ESC> Retornar **'
        @ lci+10,cci+1 PROMPT ' 9 - Tabela Disciplina    ' MESSAGE '** Tabela de Disciplina - <ESC> Retornar **'
        @ lci+11,cci+1 PROMPT ' A - Emissao de Etiquetas ' MESSAGE '** Emissao de Etiquetas - <ESC> Retornar **'
        @ lci+12,cci+1 PROMPT ' B - Relacao Aluno p/Nasc.' MESSAGE '** Relacao de Alunos p/Data de Nascimento - <ESC> Retornar **'
        @ lci+13,cci+1 PROMPT ' C - Relacao Aluno Transf.' MESSAGE '** Relacao de Alunos Transferidos - <ESC> Retornar **'
        @ lci+14,cci+1 PROMPT ' D - ATA Resultado Final  ' MESSAGE '** ATA Resultado Final - <ESC> Retornar **'
        @ lci+15,cci+1 PROMPT ' E - Resumo de Boletim    ' MESSAGE '** Relatorio Resumo de Boletins - <ESC> Retornar **'
        @ lci+16,cci+1 PROMPT ' F - T E X T O            ' MESSAGE '** Editor de Texto Simples - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                *       RELACAO DOS EM GERAL P/ORDEM ALFA E MATRICULA
                         saf501()
                CASE opcao = 2
                *       EMISSAO DE BOLETIM
                         saf502()
                CASE opcao = 3
                *       MAPA DE NOTAS
                         saf503()
                CASE opcao = 4
                *       EMISSAO MAPA DE MEDIAS POR DISCIPLINA
                         saf504()
                CASE opcao = 5
                *       ATA DE ALUNOS P/PROFESSORES
                         saf505()
                CASE opcao = 6
                *        MAPA DE AVALIACAO ED.FISICA
                        saf506()
                CASE opcao = 7
                *        MAPA DE FREQUENCIA
                        saf507()
                CASE opcao = 8
                *        FOLHA DE AVALIACAO
                        saf508()
                CASE opcao = 9
                *        FICHA INDIVIDUAL
                        saf509()
                CASE opcao = 10
                *        TABELA DE DISCIPLINA
                        saf510()
                CASE opcao = 11
                *       EMISSAO DE ETIQUETAS
                         saf522()
                CASE opcao = 12
                *       RELACAO DE ALUNOS P/DATA NASCIMENTO
                         safnasc()
                CASE opcao = 13
                *       RELACAO DE ALUNOS P/DATA NASCIMENTO
                         saftrans()
                CASE opcao = 14
                *       ATA RESULTADO FINAL
                         saf513()
                CASE opcao = 15
                *       RESUMO DE BOLETINS
                         saf514()
                CASE opcao = 16
                *       TEXTO
                        texto()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

