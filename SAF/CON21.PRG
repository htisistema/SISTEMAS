*** con21.PRG: Menu de Duplicatas (a pagar)
***
*******************************************

FUNCTION con21(tipo)
***************
LOCAL MPRG:='CON21'

LOCAL tela

PRIVATE lci,cci,lba,cba


lci := 09
cci := 00
lba := 20
cba := 78

tela := SAVESCREEN(01,00,24,79)
*------------------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('safccpg','ccpg');RETURN NIL;ENDIF
IF ! abriarq('sacdupp','dupp');RETURN NIL;ENDIF
IF ! abriarq('insopera','sen');RETURN NIL;ENDIF
*------------------------------------------------------------------------------------
WHILE .T.
        exibi_prg(mprg)
        botao(lci,cci,lba,cba)
        @ lci+1,cci+1 TO lci+1,cba-1
        DEVPOS(lci,cci+1);DEVOUT(" Contas a Pagar ")
        DEVPOS(lci+2,cci+2);DEVOUT('Tipo Documento.........:')
        DEVPOS(lci+3,cci+2);DEVOUT('Numero Documento.......:')
        DEVPOS(lci+4,cci+2);DEVOUT('Codigo do Fornecedor...:')
        DEVPOS(lci+5,cci+2);DEVOUT('Data de Emissao........:')
        DEVPOS(lci+6,cci+2);DEVOUT('Data de Vencimento.....:')
        DEVPOS(lci+7,cci+2);DEVOUT('Valor Duplicata R$.....:')
        DEVPOS(lci+8,cci+2);DEVOUT('Pag. [C]arteria [B]anco:')
        DEVPOS(lci+9,cci+2);DEVOUT('Numero da Nota Fiscal..:')
        DEVPOS(lci+10,cci+2);DEVOUT('Observacao:')
        IF tipo = NIL
                mensagem("[I]nclusao [A]lteracao [E]xclusao [C]onsulta  e[S]torno [ESC]Retorna")
                INKEY(0)
                IF LASTKEY()=27
                        RESTSCREEN(01,00,24,79,tela)
                        CLOSE ALL
                        RETURN
                ENDIF
                IF LASTKEY()=ASC("i") .OR. LASTKEY()=ASC("I")
                        con211()
                ENDIF
                IF LASTKEY()=ASC("A") .OR. LASTKEY()=ASC("a")
                        con212()
                ENDIF
                IF LASTKEY()=ASC("E") .OR. LASTKEY()=ASC("e")
                        con213()
                ENDIF
                IF LASTKEY()=ASC("C") .OR. LASTKEY()=ASC("c")
                        con214()
                ENDIF
                IF LASTKEY()=ASC("S") .OR. LASTKEY()=ASC("s")
                        con215()        // PRG CON214.PRG
                ENDIF
        ELSE
                con214()
        ENDIF
ENDDO
