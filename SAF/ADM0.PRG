*******************
* MENU PRINCIPAL
*******************
FUNCTION sac0()
***************

LOCAL opcao,tela
LOCAL aMPosIni   := {}, /* Posicao Inicial das Opcoes no Array aMTopWin */;
      aMColunas  := {}, /* Colunas Iniciais das Opcoes Horizontais */;
      aMCores    := {}, /* Cores do Menu */;
      aMTop      := {}, /* Menu Horizontal */;
      aMTopMsg   := {}, /* Mensagens de cada Opcao do Menu Horizontal */;
      aMTopWin   := {}, /* Itens de cada Janela de Sub-Menu */;
      aMTopWnMsg := {}, /* Mensagens das Opcoes do Sub-Menu */;
      aAltKeys   := {}, /* Teclas Alternativas de Ativacao  */;
      nOpcao     := 1,  /* Numero da Opcao Horizontal Selecionada */;
      nItemMenu  := 0,  /* Item da Opcao Selecionada */;
      cStatPdv   := ""   /* Status do PDV */

sistema:='Sistema de Administracao de Vendas'
SET KEY 306 TO SAC610(1)        // alt+m manutencao
setcor(3)
limpa(01,00,24,79)
SETCOLOR('W+/B,x')
limpa(00,00,00,79)
DEVPOS(00,42);DEVOUT(datseman(mdata_sis)+', '+DTOC(mdata_sis)+' Hr:')
SHOWTIME(00,COL(),.F.,'W+/b',.F.,.F.)
DEVPOS(00,01);DEVOUT(memp_resa)
limpa(23,00,23,79)
setcor(6)
DEVPOS(24,00);DEVOUT(PADL(mfirma_ins+' >> SIAC '+mversao+' <<',80))
*DEVPOS(24,00);DEVOUT('>> SIAC '+mversao+' <<')
setcor(1)
CAIXA(mol,01,00,22,79,,,.F.)
@ 02,00 TO 02,79
SETCOLOR('gr+/b,x')
DEVPOS(22,00);DEVOUT('ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ')
SETCOLOR('n+/w')
*SHOWTIME(22,01,.F.,'gr+/b',.F.,.F.)
*SHOWTIME(01,72,.F.,'W+/b',.F.,.F.)

IF ! EMPTY(set->apre2)
        BANNER(06,RTRIM(set->apre1),SETCOLOR('n+/w'))
        BANNER(13,RTRIM(set->apre2),SETCOLOR('n+/w'))
        DEVPOS(19,00);DEVOUT(PADC(sistema,80))
ELSE
        SETCOLOR('n+/w')
        DEVPOS(07,00);DEVOUT(PADC(sistema,80))
        DEVPOS(08,00);DEVOUT(PADC(REPL('ß',LEN(RTRIM(set->apre1))*8),80))
        BANNER(09,RTRIM(set->apre1),SETCOLOR('n+/w'))
        DEVPOS(15,00);DEVOUT(PADC(REPL('ß',LEN(RTRIM(set->apre1))*8),80))
        DEVPOS(16,00);DEVOUT(PADC(RTRIM(set->apre3),80))
ENDIF
DEVPOS(04,67);DEVOUT('Operador: '+cod_operado)
DEVPOS(21,00);DEVOUT(PADL('Caminho: '+ALLTRIM(mcaminho),80))
DEVPOS(21,00);DEVOUT('Diretorio: '+CURDIR())
admindex()
setcor(1)
AADD(aMColunas,1)
AADD(aMColunas,14)
AADD(aMColunas,27)
AADD(aMColunas,36)
*AADD(aMColunas,48)
*AADD(aMColunas,61)
*AADD(aMColunas,75)
* --- Cores do Menu Pull-Down

AADD(aMCores,"N/w")                   // Cor do Menu Horizontal e Mensagens
AADD(aMCores,"W+/N")                   // Cor da Opcao Barra Horizontal
AADD(aMCores,"N/w")                   // Cor Janela Pull-Down Ativa
AADD(aMCores,"N/w+")                  // Cor Janela Pull-Down Inativa
AADD(aMCores,"W+/N")                   // Cor Janela Pull-Down apos Selecao
AADD(aMCores,"W+/N")                   // Cor Barra Horizontal apos Selecao

/* Menu Horizontal Linha 1 */

AADD(aMTop," Cadastro ")
AADD(aMTop," Movimento ")
AADD(aMTop," Relatorio ")
AADD(aMTop," F I M ")

/* Mensagens do Menu Horizontal */

AADD(aMTopMsg,"*** Cadastro em Geral ***")
AADD(aMTopMsg,"*** Movimentos em Geral ***")
AADD(aMTopMsg,"*** Relatorio em Geral ***")
AADD(aMTopMsg,"*** Fim do Sistema ***")


/* Sub-Menu da 1a. Opcao do Menu Horizontal : CADASTRO */

AADD(aMPosIni,LEN(aMTopWin)+1)
AADD(aMTopWin,' 0 -> Clientes           ')
AADD(aMTopWin,' 1 -> Vendedores         ')
AADD(aMTopWin,' 2 -> Vend./Reg.         ')
AADD(aMTopWin,' 3 -> Atividades         ')


/* Mensagens do Sub-Menu 1a. Opcao do Menu Horizontal : CADASTRO */

AADD(aMTopWnMsg,'** Cadastro de Clientes - <ESC> p/Retornar **')
AADD(aMTopWnMsg,'** Cadastro de Vendedores - <ESC> p/Retornar **')
AADD(aMTopWnMsg,'** Cadastro de Vend/Reg. - <ESC> p/Retornar **')
AADD(aMTopWnMsg,'** Cadastro de Atividades - <ESC> p/Retornar **')


/* Sub-Menu da 2a. Opcao do Menu Horizontal : MOVIMENTOS */

AADD(aMPosIni,LEN(aMTopWin)+1)
AADD(aMTopWin,' 0 -> Programacao ')
AADD(aMTopWin,' 1 -> Visitas     ')
AADD(aMTopWin,' 2 -> Proposta    ')

/* Mensagens do Sub-Menu 2a. Opcao do Menu Horizontal : MOVIMENTOS */

AADD(aMTopWnMsg,'** Inclusao das Programacoes - <ESC> p/Retornar **')
AADD(aMTopWnMsg,'** Inclusao de Visitas - <ESC> p/Retornar **')
AADD(aMTopWnMsg,'** Inclusao de Proposta - <ESC> p/Retornar **')

/* Sub-Menu da 3a. Opcao do Menu Horizontal : RELATORIO */

AADD(aMPosIni,LEN(aMTopWin)+1)
AADD(aMTopWin,' 0 -> Ficha de Visitas   ')
AADD(aMTopWin,' 1 -> Hist. de Visitas   ')
AADD(aMTopWin,' 2 -> Visitas em Aberto  ')
AADD(aMTopWin,' 3 -> Propostas Emitidas ')
AADD(aMTopWin,' 4 -> Lista de Clientes  ')

/* Mensagens do Sub-Menu 3a. Opcao do Menu Horizontal : RELATORIOS*/

AADD(aMTopWnMsg,'** Emissao de Ficha de Visitas - <ESC> Retornar **')
AADD(aMTopWnMsg,'** Emissao do Historico de Visitas - <ESC> Retornar **')
AADD(aMTopWnMsg,'** Emissao de Visitas em Aberto - <ESC> Retornar **')
AADD(aMTopWnMsg,'** Emissao Propostas Emitidas - <ESC> Retornar **')
AADD(aMTopWnMsg,'** Listagem de Clientes - <ESC> Retornar **')

/* Sub-Menu da 4a. Opcao do Menu Horizontal : FIM */

AADD(aMPosIni,LEN(aMTopWin)+1)
AADD(aMTopWin," Fim ")

/* Mensagens do Sub-Menu 4a. Opcao do Menu Horizontal : FIM  */

AADD(aMTopWnMsg,"Finalizar o Sistema")
*
* --- Primeira Linha de Mensagens
prmtrow = 23
* --- Chama funcao de Inicializacao do Menu
PDOWNINIT(1,aMColunas,aMTop,aMTopWin,aMPosIni,,.T.,aMCores)

set key 23 to
opcao:=0
WHILE .T.
        exibi_prg('SACADM')
        CLOSE ALL
        * --- Faz o Menu Pull-Down
        MENU(@nOpcao,@nItemMenu, aMTop, aMTopWin, aMColunas, aMPosIni, aMTopMsg, aMTopWnMsg)

        IF nopcao == 0 .OR. LASTKEY() = 27
                fim('"SIAC"-Sistema Integrado de Automa‡ao Comercial ('+mversao+')')
                LOOP
        ENDIF
        DO CASE
                CASE nOpcao = 1                  // CADASTRO
                        DO CASE
                                CASE nItemMenu = 1
                        *                 CLIENTE
                                        *************
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
                                CASE nItemMenu = 2
                                        senha1()
                                CASE nItemMenu = 3
                                        adm13()
                                CASE nItemMenu = 4
                                        adm14()
                        ENDCASE
                CASE nOpcao = 2                  // Vendas
                        DO CASE
                                CASE nitemmenu = 1
                                        adm21()
                                CASE nitemmenu = 2
                                        adm22()
                                CASE nitemmenu = 3
                                        adm23()
                        ENDCASE
                CASE nOpcao = 3                 // CONSULTA
                        DO CASE
                                CASE nitemmenu = 1
                                        adm31()
                                CASE nitemmenu = 2
                                        adm32()
                                CASE nitemmenu = 3
                                        adm33()
                                CASE nitemmenu = 4
                                        adm34()
                                CASE nitemmenu = 5
                                        adm35()
                        ENDCASE
                CASE nOpcao = 4                  // FIM
                        DO CASE
                                CASE nItemMenu = 1
                                        fim('"SIAC"-Sistema Integrado de Automa‡ao Comercial ('+mversao+')')
                        ENDCASE
        ENDCASE
ENDDO
RETURN NIL


