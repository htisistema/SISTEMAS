*** con24.PRG: Menu de Duplicatas (Receber)
***
*******************************************

FUNCTION con24(tipo)
***************
LOCAL MPRG:='CON24'

LOCAL tela,opcao

PRIVATE lci,cci,lba,cba


lci := 03
cci := 00
lba := 18
cba := 79

tela := SAVESCREEN(01,00,24,79)
*------------------------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('sacfin','fin');RETURN NIL;ENDIF
IF ! AbriArq('saccaixa','caix');RETURN NIL;ENDIF
IF ! AbriArq('saccarta','car');RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('saccheq','cheq');RETURN NIL;ENDIF
IF ! AbriArq('sacdupr','dupr');RETURN NIL;ENDIF
IF ! AbriArq('sacped_s','ped_s');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
*------------------------------------------------------------------------------------
WHILE .T.
        exibi_prg(mprg)
        botao(lci,cci,lba,cba,,' Menu Documentos A RECEBER ')
        @ lci+1,cci+1 TO lci+1,cba-1
        DEVPOS(lci+2,cci+2);DEVOUT('Tipo Documentos........:')
        DEVPOS(lci+3,cci+2);DEVOUT('No.Banco/Cod.Cartao....:')
        DEVPOS(lci+3,cci+32);DEVOUT('Agencia:')
        DEVPOS(lci+3,cci+51);DEVOUT('C/C:')
        DEVPOS(lci+4,cci+2);DEVOUT('Numero Documentos......:')
        DEVPOS(lci+5,cci+2);DEVOUT('Codigo do Cliente......:')
        DEVPOS(lci+6,cci+2);DEVOUT('Data de Emissao........:')
        DEVPOS(lci+7,cci+2);DEVOUT('Data de Vencimento.....:')
        DEVPOS(lci+8,cci+2);DEVOUT('Valor Documento R$.....:')
        DEVPOS(lci+9,cci+2);DEVOUT('Pag. [C]arteria [B]anco:')
        DEVPOS(lci+10,cci+2);DEVOUT('Codigo do Vendedor.....:')
        DEVPOS(lci+11,cci+2);DEVOUT('Numero do Pedido/NF....:')
        DEVPOS(lci+12,cci+2);DEVOUT('% de Comissao .........:')
        DEVPOS(lci+13,cci+2);DEVOUT('Nome do Correntista....:')
        DEVPOS(lci+14,cci+2);DEVOUT('OBS.:')
        IF tipo = NIL
                opcao := mensagem1("[I]nclusao - [A]lteracao - [E]xclusao - [C]onsulta - e[S]torno - [B]aixar Documento - [R]elatorios - [V]Inclusao de varios - [ESC]Retorna",' ','I,A,E,C,S,B,R,V')
                IF LASTKEY()=27
                        RESTSCREEN(01,00,24,79,tela)
                        fimset()
                        RETURN
                ENDIF
                IF opcao = 'V'
                        con2411()
                ELSEIF opcao = "I"
                        con241()
                ELSEIF opcao = 'A'
                        con251(,'A')
                ELSEIF opcao = 'E'
                        con243()
                ELSEIF opcao = 'C'
                        con244()
                ELSEIF opcao = 'S'
                        con251(,'S')
                ELSEIF opcao = 'B'
                        con25()
                ELSEIF opcao = 'R'
                        con42()
                ENDIF
        ELSE
                con244()
        ENDIF
ENDDO
