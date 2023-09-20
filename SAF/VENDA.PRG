*********************
* MENU DE PEDIDOS
*********************

FUNCTION sac0(mdir)
***************

LOCAL MPRG:='VENDA'
LOCAL opcao,tela,tecla,lci,cci,lba,cba

msit_tip := 'VENDA'
exibi_prg(mprg)

lci:=08
cci:=25
lba:=21
cba:=52
tela = SAVESCREEN(01,00,24,79)
opcao:=0

setcor(3)
limpa(01,00,24,79)
SETCOLOR('W+/B,x')
limpa(00,00,00,79)
DEVPOS(00,42);DEVOUT(datseman(mdata_sis)+', '+DTOC(mdata_sis)+' Hr:')
SHOWTIME(00,COL(),.F.,'W+/b',.F.,.F.)
*DEVPOS(00,00);DEVOUT(PADL(datseman(mdata_sis)+', '+imp_data(mdata_sis),80))
DEVPOS(00,01);DEVOUT(memp_resa)
limpa(23,00,23,79)
setcor(6)
DEVPOS(24,00);DEVOUT(PADL(mfirma_ins+' >> SIAC '+mversao+' << Fone:(81)3326.8332 >>',80))
*DEVPOS(24,00);DEVOUT(PADL(mfirma_ins,80))
*DEVPOS(24,00);DEVOUT(sistema+' '+mversao)
setcor(1)
caixa(mol,01,00,22,79,,,.F.)
SETCOLOR('gr+/b,x')
DEVPOS(22,00);DEVOUT('ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ')
setcor(3)
@ 01,01 SAY '  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 02,01 SAY '  ³ ÄÄÄÜÜÜÜÜÜÄÄÄ ³    ³ ÄÄÄÜÜÜÜÜÜÄÄÄ ³    ³ ÄÄÄÜÜÜÜÜÜÄÄÄ ³    ³ ÄÄÄÜÜÜÜÜÜÄÄÄ ³'
@ 03,01 SAY '  ³  ÄÄÄÄÜÜÄÄÄÄ  ³    ³  ÄÄÄÄÜÜÄÄÄÄ  ³    ³  ÄÄÄÄÜÜÄÄÄÄ  ³    ³  ÄÄÄÄÜÜÄÄÄÄ  ³'
@ 04,01 SAY '  ³   ÄÄÄÜÜÄÄÄ   ³    ³   ÄÄÄÜÜÄÄÄ   ³    ³   ÄÄÄÜÜÄÄÄ   ³    ³   ÄÄÄÜÜÄÄÄ   ³'
@ 05,01 SAY '  ³   ÄÄÄÜÜÄÄÄÄ  ³    ³   ÄÄÄÜÜÄÄÄÄ  ³    ³   ÄÄÄÜÜÄÄÄÄ  ³    ³   ÄÄÄÜÜÄÄÄÄ  ³'
@ 06,01 SAY '  ³ ÄÄÄÜÜÜÜÜÜÄÄÄ ³    ³ ÄÄÄÜÜÜÜÜÜÄÄÄ ³    ³ ÄÄÄÜÜÜÜÜÜÄÄÄ ³    ³ ÄÄÄÜÜÜÜÜÜÄÄÄ ³'
@ 07,01 SAY '  ³              ³    ³              ³    ³              ³    ³              ³'
@ 08,01 SAY '  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
DEVPOS(04,67);DEVOUT('Operador: '+cod_operado)
DEVPOS(21,00);DEVOUT(PADL('Caminho: '+ALLTRIM(mcaminho),80))
DEVPOS(21,00);DEVOUT('Diretorio: '+CURDIR())
BANNER(02,'VENDA',setcor(3))
opcao:=0
IF mdir <> NIL .AND. UPPER(mdir) = 'OLD'
        janela(02,79,' Arq.Morto ')
ENDIF
WHILE .T.
        CLOSE ALL
        @ 23,00 CLEAR TO 23,79
        setcor(1)
        botao(09,04,11,27)
        botao(09,28,11,51)
        botao(09,52,11,75)

        botao(12,04,14,27)
        botao(12,28,14,51)
        botao(12,52,14,75)

        botao(15,04,17,27)
        botao(15,28,17,51)
        botao(15,52,17,75)

        botao(18,04,20,27)
        botao(18,28,20,51)
        botao(18,52,20,75)

        @ 10,05 PROMPT '     0 - Pedidos      ' MESSAGE '** Emissao de Pedidos - <ESC> p/Fim do sistema **'
        @ 10,29 PROMPT '   1 - Orcamentos     ' MESSAGE '** Emissao de Orcamentos - <ESC> p/Fim do sistema **'
        @ 10,53 PROMPT '      2 - Trocas      ' MESSAGE '** Trocas de Mercadorias - <ESC> p/Fim do sistema **'
        @ 13,05 PROMPT '  3 - Incl.Clientes   '  MESSAGE '** Inclusao, Alteracao e Exclusao de Cliente - <ESC> p/Fim do sistema **'
        @ 13,29 PROMPT '  4 - Cons.Clientes   ' MESSAGE '** Consulta de Clientes - <ESC> p/Fim do sistema **'
        @ 13,53 PROMPT ' 5 - Emissao N.fiscal ' MESSAGE '** Emissao de nota fiscal <ESC> p/Fim do sistema **'
        @ 16,05 PROMPT ' 6 - Entrega  Pedidos ' MESSAGE '** Entrega de Pedidos - <ESC> p/Fim do sistema **'
        @ 16,29 PROMPT '     7 - Muda Data    ' MESSAGE '** Muda a Data do Sistema, <ESC> p/Fim do sistema **'
        @ 16,53 PROMPT '    8 - Calculadora   ' MESSAGE '** Calculadora **'
        @ 19,05 PROMPT '    9 - Calendario    ' MESSAGE '** Calendario **'
        @ 19,29 PROMPT 'A - Gerar Arq.Remessa ' MESSAGE '** Gerar o Arquivo Remessa **'
        @ 19,53 PROMPT '  X - Fim do Sistema  ' MESSAGE '** Sai do sistema **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                fim('"SIAC"-Sistema Integrado de Automacao Comercial ('+mversao+')')
                opcao = 11
                LOOP
        ENDIF
        DO CASE
                CASE opcao = 1
        *                 PEDIDOS
                        sac23(1)
                 CASE opcao = 2
        *                 ORCAMENTOS
*                       sac23p1(1)
                        sac23(,1)
                 CASE opcao = 3
        *                 TROCA
                        IF mcgc_firm = '70.167.853/0001-06'
                                atencao('Acesso nao Autorizador !!!')
                                LOOP
                        ENDIF
                        sactroca()
                CASE opcao = 4
        *                 INCLUSAO CLIENTE
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
*                       sac13()
                CASE opcao = 5
        *                 CONSULTA DE CLIENTES
                        sac42()
                CASE opcao = 6
        *                 EMISSAO DE NOTA FISCAL
                         sac210()
                CASE opcao = 7
        *                 ENTREGA DE PEDIDOS
                        sacentre()
                CASE opcao = 8
                        alt_data()
                CASE opcao = 9
                        calcu()
                CASE opcao = 10
                        cale()
                CASE opcao = 11
                        sac2ped(1)
                CASE opcao = 12
                        fim('"SIAC"-Sistema Integrado de Automa‡ao Comercial ('+mversao+')')
        ENDCASE
ENDDO
RETURN NIL

*************************** F I M **************************************



