***********************
* MENU DO CAIXA
***********************

FUNCTION sac0(mdir)
*****************
LOCAL MPRG:='CAIXA'

LOCAL opcao,tela,tecla,lci,cci,lba,cba

lci = 06
cci = 07
lba = 21
cba = 69
tela = SAVESCREEN(01,00,24,79)
opcao = 0
setcor(3)
limpa(01,00,24,79)
SETCOLOR('W+/B,x')
limpa(00,00,00,79)
DEVPOS(00,01);DEVOUT(memp_resa)
DEVPOS(00,70);DEVOUT(mdata_sis)
limpa(23,00,23,79)
DEVPOS(24,00);DEVOUT(PADL(mfirma_ins+' >> SIAC '+mversao+' << Fone:(81)3326.8332 >>',80))
*DEVPOS(24,00);DEVOUT(PADL(mfirma_ins,80))
*DEVPOS(24,00);DEVOUT(sistema+' '+mversao)
SETCOLOR('w/b,x')
DEVPOS(02,67);DEVOUT('Operador: '+cod_operado)
DEVPOS(02,00);DEVOUT('Diretorio: '+CURDIR())
SETCOLOR('B/B,x')

setcor(1)
caixa(mol,01,00,22,79,,.F.)
@ 01,01 SAY '   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 02,01 SAY '   ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³      ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³      ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³'
@ 03,01 SAY '   ³  ÄÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³      ³  ÄÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³      ³  ÄÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³'
@ 04,01 SAY '   ³   ÄÄÄÄÄÜÜÄÄÄÄÄ   ³      ³   ÄÄÄÄÄÜÜÄÄÄÄÄ   ³      ³   ÄÄÄÄÄÜÜÄÄÄÄÄ   ³'
@ 05,01 SAY '   ³   ÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³      ³   ÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³      ³   ÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³'
@ 06,01 SAY '   ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³      ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³      ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³'
@ 07,01 SAY '   ³                  ³      ³                  ³      ³                  ³'
@ 08,01 SAY '   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
@ 12,01 SAY '   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 13,01 SAY '   ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³      ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³      ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³'
@ 14,01 SAY '   ³  ÄÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³      ³  ÄÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³      ³  ÄÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³'
@ 15,01 SAY '   ³   ÄÄÄÄÄÜÜÄÄÄÄÄ   ³      ³   ÄÄÄÄÄÜÜÄÄÄÄÄ   ³      ³   ÄÄÄÄÄÜÜÄÄÄÄÄ   ³'
@ 16,01 SAY '   ³   ÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³      ³   ÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³      ³   ÄÄÄÄÄÜÜÄÄÄÄÄÄ  ³'
@ 17,01 SAY '   ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³      ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³      ³ ÄÄÄÜÜÜÜÜÜÜÜÜÜÄÄÄ ³'
@ 18,01 SAY '   ³                  ³      ³                  ³      ³                  ³'
@ 19,01 SAY '   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
DEVPOS(04,67);DEVOUT('Operador: '+cod_operado)
DEVPOS(21,00);DEVOUT(PADL('Caminho: '+ALLTRIM(mcaminho),80))
DEVPOS(02,00);DEVOUT('Diretorio: '+CURDIR())
SETCOLOR('gr+/b,x')
DEVPOS(22,00);DEVOUT('ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ')
BANNER(01,'CAIXA',setcor(3))
*BANNER(01,'CAIXA',setcor(6))
setcor(1)
opcao:=0
IF mdir <> NIL .AND. UPPER(mdir) = 'OLD'
        janela(02,79,' Arq.Morto ')
ENDIF

WHILE .T.
        CLOSE ALL
        exibi_prg(mprg)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        @ lci+1,cci+31 TO lba-1,cci+31
        setcor(1)
        @ lci+1,cci+1 PROMPT ' 0 -> Recebimentos Pedidos    ' MESSAGE '** Recebimentos do TP - <ESC> p/Retornar **'
        @ lci+2,cci+1 PROMPT ' 1 -> Recebimento Diversos    ' MESSAGE '** Recebimentos Diversos - <ESC> p/Retornar **'
        @ lci+3,cci+1 PROMPT ' 2 -> Vales                   ' MESSAGE '** Retirada de Vales - <ESC> p/Retornar **'
        @ lci+4,cci+1 PROMPT ' 3 -> Pagamento  Vales        ' MESSAGE '** Pagamentos de Vales - <ESC> p/Retornar **'
        @ lci+5,cci+1 PROMPT ' 4 -> Cancelar/Alterar Vales  ' MESSAGE '** Cancelar ou Alterar Vales - <ESC> p/Retornar **'
        @ lci+6,cci+1 PROMPT ' 5 -> Despesas                ' MESSAGE '** Retirada de Despesas - <ESC> p/Retornar **'
        @ lci+7,cci+1 PROMPT ' 6 -> Cancelar/Alt. Despesas  ' MESSAGE '** Cancelar ou Alterar Despesas Diarias ou ADM - <ESC> p/Retornar **'
        @ lci+8,cci+1 PROMPT ' 7 -> Fechamento/Consulta     ' MESSAGE '** Fechamento ou Consulta CAIXA do dia - <ESC> p/Retornar **'
        @ lci+9,cci+1 PROMPT ' 8 -> Cancela Pedidos Pedente ' MESSAGE "** T.P's Pedentes - <ESC> p/Retornar **"
        @ lci+10,cci+1 PROMPT ' 9 -> Fundo Caixa             ' MESSAGE '** Inclusao do Fundo de Caixa do dia - <ESC> p/Retornar **'
        @ lci+11,cci+1 PROMPT ' A -> Retirada Deposito       ' MESSAGE '** Retirada de Deposito - <ESC> p/Retornar **'
        @ lci+12,cci+1 PROMPT ' B -> Baixa Duplicata\Cheques ' MESSAGE '** Baixas de Duplicatas\Cheques a Receber - <ESC> p/Retornar **'
        @ lci+13,cci+1 PROMPT ' C -> Estorno Recebimento     ' MESSAGE '** Estorno de Recebimentos - <ESC> p/Retornar **'
        @ lci+14,cci+1 PROMPT ' D -> Despesas Administrativa ' MESSAGE '** Despesas Administrativas - <ESC> p/Retornar **'

        @ lci+1,cci+32 PROMPT ' E -> Emissao/Cancelar N.F    ' MESSAGE '** Emissao e Cancelamento de Nota Fical - <ESC> p/Retornar **'
        @ lci+2,cci+32 PROMPT ' F -> Recibos                 ' MESSAGE '** Emissao de Recibos - <ESC> p/Retornar **'
        @ lci+3,cci+32 PROMPT ' G -> Consulta de Cliente     ' MESSAGE '** Consulta de Cliente - <ESC> p/Retornar **'
        @ lci+4,cci+32 PROMPT ' H -> Inclusao de Cliente     ' MESSAGE '** Inclusao de Cliente - <ESC> p/Retornar **'
        @ lci+5,cci+32 PROMPT " I -> Consulta de CEP'S       " MESSAGE "** Consulta de CEP'S - <ESC> p/Retornar **"
        @ lci+6,cci+32 PROMPT ' J -> Muda Data               ' MESSAGE '** Muda a Data do Sistema, <ESC> p/Fim do sistema **'
        @ lci+7,cci+32 PROMPT ' K -> Calculadora             ' MESSAGE '** Calculadora **'
        @ lci+8,cci+32 PROMPT ' L -> Calendario              ' MESSAGE '** Calendario **'
        @ lci+9,cci+32 PROMPT ' M -> Emissao de Boleto       ' MESSAGE '** Emissao de Boleto Bancario **'
        @ lci+10,cci+32 PROMPT ' N -> Preencher CHEQUE        ' MESSAGE '** Preenchimento de CHEQUES **'
        @ lci+11,cci+32 PROMPT ' O -> Consulta Pedido         ' MESSAGE '** Consulta de Pedido **'
        @ lci+12,cci+32 PROMPT ' P -> Emissao de Duplicatas   ' MESSAGE '** Emissao de Duplicata a receber - <ESC> p/retornar **'
        @ lci+13,cci+32 PROMPT ' Q -> Emissao de CARNES       ' MESSAGE '** Emissao de CARNES - <ESC> p/retornar **'
        @ lci+14,cci+32 PROMPT ' X -> Fim do Sistema          ' MESSAGE '** Sai do sistema **'
        SET INTEN ON
        MENU TO opcao
        IF LASTKEY() = 27
                fim('"SIAC"-Sistema Integrado de Automacao Comercial ('+mversao+')')
                opcao = 27
                LOOP
        ENDIF
        DO CASE
                CASE opcao = 1
                *        RECEBIMENTO
                         sac30()
                CASE opcao = 2
                *        RECEBIMENTOS DIVERSOS
                         sac301()
                CASE opcao = 3
                *        VALES
                         sac31()
                CASE opcao = 4
                *        PAGAMENTO DE VALES
                         sac31_0()
                CASE opcao = 5
                *        CANCELAMENTO VALES
                         sac31_4()
                CASE opcao = 6
                *        DESPESAS
                         sac31_1()      // sac31.prg
                CASE opcao = 7
                *        CANCELAMENTO DE DESPESAS
                         sac31_3()     // sac31.prg
                CASE opcao = 8
                *        FECHAMENTO / CONSULTA
                         sac33()
                CASE opcao = 9
                *        CANCELAMENTO DE PEDIDOS
                         sac34()
                CASE opcao = 10
                *        FUNDO DE CAIXA
                         sac36()
                CASE opcao = 11
                *        RETIRADA DE DEPOSITO
                         sac37()
                CASE opcao = 12
                *        BAIXA DE DUPLICATA
                         con25('*')
**************************************************************************
                CASE opcao = 13
                *        ESTORNO DE RECEBIMENTOS
                        sac38()
                CASE opcao = 14
                *        DESPESAS ADMINISTRATIVA
                         con21()
                CASE opcao = 15
                *        SAIDA P/NOTA FISCAL
                         sac210()
                CASE opcao = 16
                *        EMISSAO DE RECIBOS
                         mnitemmenu = 0
                         WHILE .T.
                                 tela := SAVESCREEN(01,00,24,79)
                                 limpa(23,00,23,79)
                                 botao(08,18,13,40,,' Menu de Recibos ')
                                 @ 09,19 PROMPT ' Recibo Firma        ' MESSAGE '*** Emissao de Recibo da Firma - <ESC> p/abandonar ***'
                                 @ 10,19 PROMPT ' Recibo Comissao     ' MESSAGE '*** Emissao de recibo Pagemento de comissao - <ESC> p/abandonar ***'
                                 @ 11,19 PROMPT ' Recibo Pag.Func.    ' MESSAGE '*** Emissao de Recibo Pagamento a Funcionarios - <ESC> p/abandonar ***'
                                 @ 12,19 PROMPT ' Recibo Pag.Doc.     ' MESSAGE '*** Emissao de Recibo Pagamento de Documentos a Receber - <ESC> p/abandonar ***'
                                 MENU TO mnitemmenu
                                 IF LASTKEY() = 27
                                         RESTSCREEN(01,00,24,79,tela)
                                         EXIT
                                 ENDIF
                                 DO CASE
                                         CASE mnitemmenu = 1
                                                 *********************************
                                                 IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
                                                 *********************************
                                                 sacrec()
                                         CASE mnitemmenu = 2
                                                 sacrccom()
                                         CASE mnitemmenu = 3
                                                 *********************************
                                                 IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
                                                 *********************************
                                                 sacrec1()       // mesmo prg sacrec.prg
                                         CASE mnitemmenu = 4
                                                 *********************************
                                                 IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
                                                 *********************************
                                                 sacrec2()       // mesmo prg sacrec.prg
                                 ENDCASE
                         ENDDO
                CASE opcao = 17
                *       CONSULTA DE CLIENTES
                         sac42()
                CASE opcao = 18
                *       INCLUSAO DE CLIENTES
                        *************
                        CLOSE ALL
                        IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
                        IF ! AbriArq('saccep','cep');RETURN NIL;ENDIF
                        IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
                        IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
                        IF ! AbriArq('sactabme','tabme');RETURN NIL;ENDIF
                        IF ! AbriArq('regiao','reg');RETURN NIL;ENDIF
                        IF ! AbriArq('sacmerc','merc');RETURN NIL;ENDIF
                        set_key('cliente')
                        SELE('cli');ORDSETFOCUS(2)
                        GO TOP
                        *************
                        SET KEY -6 TO f7_cli
                        f7_cli('*')
*                        sac13()
                CASE opcao = 19
                *       CONSULTA DE CEP'S
                         saccep()
                CASE opcao = 20
                        alt_data()
                CASE opcao = 21
                        calcu()
                CASE opcao = 22
                        cale()
                CASE opcao = 23
                        sacbolet()
                CASE opcao = 24
                        sacpcheq()
                CASE opcao = 25
                        sac491()
                CASE opcao = 26
                        con428()
                CASE opcao = 27
                        emi_carne()
                CASE opcao = 28
                        fim('"SIAC"-Sistema Integrado de Automa‡ao Comercial ('+mversao+')')
        ENDCASE
ENDDO
RETURN NIL

