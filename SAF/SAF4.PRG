********************
* MENU DE CONSULTAS
********************

FUNCTION saf4
***************

LOCAL mopcao,opcao,mopcao1,lci,cci,lba,cba,tela,tela1,mtit,mpag,mtot_icm,mtraco,;
      mquantd

lci = 04
cci = 20
lba = 09
cba = 43
tela = SAVESCREEN(01,00,24,79)
opcao = 0
mtraco := REPLI('=',80)
WHILE .T.
        exibi_prg('SAF4')
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+1,cci+1 PROMPT ' Consulta de Alunos   ' MESSAGE '** Consulta de Alunos - <ESC> Retornar **'
        @ lci+2,cci+1 PROMPT ' Estatistica Notas    ' MESSAGE '** Consulta da estatistica das notas por GSTT - <ESC> Retornar **'
        @ lci+3,cci+1 PROMPT ' 10 melhor p/GSTT     ' MESSAGE '** Consulta dos 10 melhores por GSTT - <ESC> Retornar **'
        @ lci+4,cci+1 PROMPT ' Contas Apagar        ' MESSAGE '** Consulta das Contas Apagar - <ESC> Retornar **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                *        CONSULTA DE ALUNOS
                        saf41()
                CASE opcao = 2
                *        CONSULTA DE ESTATISTICA DE NOTAS
                        saf42()
                CASE opcao = 3
                *        CONSULTA DE 10 melhores p/GSTT
                        saf515()
                CASE opcao = 4
                        *------------------------------------------------------------------
                        CLOSE ALL
                        IF ! abriarq('safccpg','ccpg');RETURN NIL;ENDIF
                        IF ! abriarq('sacdupp','dupp');RETURN NIL;ENDIF
                        *------------------------------------------------------------------
                *        CONSULTA DE CONTAS APAGAR
                         con214()
        ENDCASE
ENDDO
RETURN NIL
*********************************  F I M ***********************************

