***
*** CON25.PRG: MENU Baixa de Duplicatas (Receber)
***
*********************************************

FUNCTION con25(tp)
***************
LOCAL MPRG:='CON25'

LOCAL lci,cci,lba,cba,tela,tela1,opcao

lci = 19
cci = 02
lba = 21
cba = 77

*ela1 = SAVESCREEN(li,ci-1,lb+1,cb)
tela = SAVESCREEN(01,00,24,79)
*------------------------------------------------------------------------------------
CLOSE ALL
USE sacsetup ALIAS set SHARED NEW
IF ! abriarq('insopera','sen');RETURN NIL;ENDIF
USE saccheq ALIAS cheq SHARED NEW
USE sacdupr ALIAS dupr SHARED NEW
USE sacped_s ALIAS ped_s SHARED NEW
USE (ALLTRIM(set->arq_cli)+'saccli') ALIAS cli SHARED NEW
*------------------------------------------------------------------------------------

WHILE .T.
        limpa(23,00,23,79)
        opcao := 0
        botao(lci,cci,lba,cba)
        janela(lci,cba,' Menu de Baixa DOCUMENTOS A RECEBER ')
        mensagem('Escolha sua opcao que deseja')
        @ lci+1,cci+1 PROMPT ' Individual ' MESSAGE '** Baixa de Documentos INDIVIDUAL - <ESC> p/Retornar **'
        @ lci+1,COL()+1 PROMPT ' Geral Documentos ' MESSAGE '** Baixa GERAL de Cheques ou Cartao - <ESC> p/Retornar **'
        @ lci+1,COL()+1 PROMPT ' Estorno Documentos ' MESSAGE '** Estorno de Baixa de Documentos - <ESC> p/Retornar **'
        @ lci+1,COL()+1 PROMPT ' Transf. Documento ' MESSAGE '** Transferencia de documentos entre CONTAS - <ESC> p/Retornar **'
        SET INTEN ON
        MENU TO opcao
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        DO CASE
                CASE opcao = 1
                        con251(tp)
                CASE opcao = 2
                        con252()
                CASE opcao = 3
                        con245()        // prg CON244
                CASE opcao = 4
                        con253()
        ENDCASE
ENDDO
RETURN NIL
