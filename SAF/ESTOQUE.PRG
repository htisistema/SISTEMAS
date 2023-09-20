***********************
* MENU DO ESTOQUE
***********************

FUNCTION sac0(mdir)
*****************
LOCAL MPRG:='ESTOQUE'

LOCAL opcao,tela,tecla,lci,cci,lba,cba

lci = 04
cci = 07
lba = 22
cba = 69
tela = SAVESCREEN(01,00,24,79)
opcao = 0
setcor(3)
limpa(01,00,24,79)
SETCOLOR('W+/B,x')
limpa(00,00,00,79)
DEVPOS(00,00);DEVOUT(PADL(datseman(mdata_sis)+', '+imp_data(mdata_sis),80))
DEVPOS(00,01);DEVOUT(memp_resa)
limpa(23,00,23,79)
setcor(6)
DEVPOS(24,00);DEVOUT(PADL(mfirma_ins,80))
DEVPOS(24,00);DEVOUT(sistema+' '+mversao)
setcor(1)
CAIXA(mol,01,00,22,79,,,.F.)
SETCOLOR('N+/W,x')
DEVPOS(04,67);DEVOUT('Operador: '+cod_operado)
DEVPOS(21,00);DEVOUT('Diretorio: '+CURDIR())
setcor(3)
*BANNER(01,'ESTOQUE',setcor(3))
EST_BANNER(01,10,'E S T O Q U E ')
setcor(1)
opcao:=0
IF mdir <> NIL .AND. UPPER(mdir) = 'OLD'
        janela(02,79,' Arq.Morto ')
ENDIF
WHILE .T.
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        @ lci+1,cci+31 TO lba-1,cci+31
        setcor(1)
        @ lci+1,cci+1 SAY '    M O V I M E N T A C A O   '
        setcor(3)
        @ lci+2,cci+1 PROMPT ' 0 -> Entrada de Mercadoria   ' MESSAGE '** Entrada de Mercadoria - <ESC> p/Retornar **'
        @ lci+3,cci+1 PROMPT ' 1 -> Pedido de Compras       ' MESSAGE '** Pedido de Compras - <ESC> p/Retornar **'
        @ lci+4,cci+1 PROMPT ' 2 -> Promocao                ' MESSAGE '** Colocar Mercadoria em PROMOCAO - <ESC> p/Retornar **'
        @ lci+5,cci+1 PROMPT ' 3 -> Reajuste de Mercadoria  ' MESSAGE '** Reajuste de Mercadoria - <ESC> p/Retornar **'
        @ lci+6,cci+1 PROMPT ' 4 -> Estorno de Ent.Produto  ' MESSAGE '** Estorno de Entrada de Produto - <ESC> p/Retornar **'
        @ lci+7,cci+1 PROMPT ' 5 -> Estoque Haver           ' MESSAGE '** Estoque Haver - <ESC> p/Retornar **'
        @ lci+8,cci+1 PROMPT ' 6 -> Gerar No.Serie Produto  ' MESSAGE '** Gerar No.de Serie dos Produtos - <ESC> p/Retornar **'
        @ lci+9,cci+1 PROMPT ' 7 -> Manutencao Inventario   ' MESSAGE '** Manutencao do Inventario - <ESC> p/Retornar **'
        setcor(1)
        @ lci+10,cci+1 SAY '      C O N S U L T A S       '
        setcor(3)
        @ lci+11,cci+1 PROMPT ' 8 -> Produto                 ' MESSAGE '** Consulta Produtos - <ESC> p/Retornar **'
        @ lci+12,cci+1 PROMPT ' 9 -> Movimento de Produtos   ' MESSAGE '** Consulta de Movimento de Produtos - <ESC> p/Retornar **'
        @ lci+13,cci+1 PROMPT ' A -> Vendas e Comissoes      ' MESSAGE '** Consulta de Vendas e Comissoes - <ESC> p/Retornar **'
        @ lci+14,cci+1 PROMPT ' B -> Produto Abaixo Minimo   ' MESSAGE '** Consulta de Produto Abaixo do Minimo - <ESC> p/Retornar **'
        @ lci+15,cci+1 PROMPT ' C -> Calculo de Custo        ' MESSAGE '** Consulta Calculo de Custo - <ESC> p/Retornar **'
        setcor(1)
        @ lci+1,cci+32 SAY '     R E L A T O R I O        '
        setcor(3)

        @ lci+2,cci+32 PROMPT ' D -> Pre-Pedido              ' MESSAGE '** Solicitacao de Pre_pedido - <ESC> p/Retornar **'
        @ lci+3,cci+32 PROMPT ' E -> Tabela de Preco         ' MESSAGE '** Tabela de Preco - <ESC> p/Retornar **'
        @ lci+4,cci+32 PROMPT ' F -> Tabela de Preco Manual  ' MESSAGE '** Tabela de Preco Manual - <ESC> p/Retornar **'
        @ lci+5,cci+32 PROMPT ' G -> Tabela p/Terminal       ' MESSAGE '** Tabela p/Terminal - <ESC> p/Retornar **'
        @ lci+6,cci+32 PROMPT ' H -> Tabela de Custo         ' MESSAGE '** Tabela de Custo - <ESC> p/Retornar **'
        @ lci+7,cci+32 PROMPT ' I -> Movimento de Produto    ' MESSAGE '** Listagem Movimento de Produto - <ESC> p/Retornar **'
        @ lci+8,cci+32 PROMPT ' J -> Produto Abaixo Minimo   ' MESSAGE '** Listagem Produto Abaixo do Minimo - <ESC> p/Retornar **'
        @ lci+9,cci+32 PROMPT ' K -> Vendas e Comissoes      ' MESSAGE "** Listagem de Vendas e Comissao - <ESC> p/Retornar **"
        @ lci+10,cci+32 PROMPT ' L -> Listagem p/Balanco      ' MESSAGE '** Listagem para Balanco - <ESC> p/Retornar **'
        @ lci+11,cci+32 PROMPT ' M -> Listagem Est. Credito   ' MESSAGE '** Listagem do Estoque de Credito - <ESC> p/Retornar **'
        @ lci+12,cci+32 PROMPT ' N -> Produto s/Movimento     ' MESSAGE '** Listagem de Produto sem Movimento - <ESC> p/retornar *'
        @ lci+13,cci+32 PROMPT ' O -> Notas Fiscais           ' MESSAGE '** Emissao de Relatorio de Notas Fiscais  - <ESC> p/retornar *'
        @ lci+14,cci+32 PROMPT ' P -> No.de Serie Produto     ' MESSAGE "** Emissao de No.de Serie do Produto - <ESC> p/Retornar **"
        @ lci+15,cci+32 PROMPT ' Q -> Relatorio do Inventario ' MESSAGE "** Relatorio do Inventario - <ESC> p/Retornar **"
        @ lci+16,cci+32 PROMPT ' R -> Etiqueta Transportadora ' MESSAGE "** Etiqueta para Transportadora - <ESC> p/Retornar **"
        @ lci+17,cci+32 PROMPT ' X -> Fim do Sistema          ' MESSAGE '** Sai do sistema **'
        SET INTEN ON
        MENU TO opcao
        IF LASTKEY() = 27
                fim('"SIAC"-Sistema Integrado de Automacao Comercial ('+mversao+')')
                opcao = 27
                LOOP
        ENDIF
        DO CASE
                CASE opcao = 1
                *        ENTRADA
                        ****************************************************
                        * VERIFICAR NIVEL DE ACESSO DA SENHA
                        ****************************************************
                        * Niveis de sao: 1 e 2
                        ****************************************************
                         setcor(1)
                         sac22()
                CASE opcao = 2
                *        PEDIDO COMPRA
                        ****************************************************
                        * VERIFICAR NIVEL DE ACESSO DA SENHA
                        ****************************************************
                        * Niveis de sao: 1 e 2
                        ****************************************************
                         setcor(1)
                         sac230()
                CASE opcao = 3
                *        PROMOCAO
                        ****************************************************
                        * VERIFICAR NIVEL DE ACESSO DA SENHA
                        ****************************************************
                        * Niveis de sao: 1 e 2
                        ****************************************************
                         setcor(1)
                         sac25()
                CASE opcao = 4
                *        REAJUSTE
                        ****************************************************
                        * VERIFICAR NIVEL DE ACESSO DA SENHA
                        ****************************************************
                        * Niveis de sao: 1 e 2
                        ****************************************************
                         setcor(1)
                         sac27()
                CASE opcao = 5
                *        ESTORNO DE ENTRADA DE MERCADORIA
                        ****************************************************
                        * VERIFICAR NIVEL DE ACESSO DA SENHA
                        ****************************************************
                        * Niveis de sao: 1 e 2
                        ****************************************************
                         setcor(1)
                         sac29()
                CASE opcao = 6
                *        ESTOQUE AV
                        ****************************************************
                        * VERIFICAR NIVEL DE ACESSO DA SENHA
                        ****************************************************
                        * Niveis de sao: 1 e 2
                        ****************************************************
                         setcor(1)
                        sac2av()
                CASE opcao = 7
                *       GERAR NO.DE SERIE DOS PRODUTOS
                        ****************************************************
                        * VERIFICAR NIVEL DE ACESSO DA SENHA
                        ****************************************************
                         setcor(1)
                        sac2ser()
                CASE opcao = 8
                *       MANUTENCAO DE INVENTARIO
                        ****************************************************
                        * VERIFICAR NIVEL DE ACESSO DA SENHA
                        ****************************************************
                         setcor(1)
                        sacinve()

*************************************************************************


                CASE opcao = 9
                        ****************************************************
                *        CONSULTA DE PRODUTO
                         sac40()
                CASE opcao = 10
                *        CONSULTA DE MOVIMENTO
                         sac41()
                CASE opcao = 11
                *        CONSULTA DE VENDAS E COMISSOES
                         sac44()
                CASE opcao = 12
                *        ESTOQUE MINIMO
                         sac4min()
                CASE opcao = 13
*                       CALCULO DE CUSTO
                        sac4cal()

*************************************************************************

                CASE opcao = 14
                *          EMISSAO DO PRE-PEDIDO
                         sac501()
                CASE opcao = 15
                *          TABELA DE PRECOS
                         sac51()
                CASE opcao = 16
                *          TABELA DE PRECOS MANUAL
                        SAC512()
                CASE opcao = 17
                *          TABELA TERMINAL
                         sac510()
                CASE opcao = 18
                *          TABELA DE CUSTOS
                         sac511()
                CASE opcao = 19
                *          LISTAGEM MOVIMENTO
                         sac52()
                CASE opcao = 20
                *          RELATORIO PRODUTO ABAIXO DO MINIMO
                        sac5min()
                CASE opcao = 21
                *          EMISSAO DE COMISSAO DE VENDAS
                        sac5com()
                CASE opcao = 22
                *          LISTAGEM PARA BALANCO
                        sac58()
                CASE opcao = 23
                *          LISTAGEM PRODUTO SEM MOVIMENTO
                        sac59()
                CASE opcao = 24
                *        LISTAGEM DE CREDITO DE SALDO
                        sac580()
                CASE opcao = 25
                *          RELATORIO DE NOTAS FISCAIS EMITIDAS
                        sac5nota()
                CASE opcao = 26
                         ** EMISSAO DE NO.DE SERIE DO PRODUTO
                         setcor(1)
                        sacserie()
                CASE opcao = 27
                         ** EMISSAO DE RELATORIO DO INVENTARIO
                         setcor(1)
                        sac5inve()
                CASE opcao = 28
                         ** EMISSAO DE ETIQUETA PARA TRANSPORTADORA
                         setcor(1)
                        sacetqtr()
                CASE opcao = 28
                        fim('"SIAC"-Sistema Integrado de Automaáao Comercial ('+mversao+')')
        ENDCASE
ENDDO
RETURN NIL
*****************************************************************************
FUNCTION EST_BANNER(LIN,COL,STRING,ESPACEJAMENTO)

PRIVATE LEN_CARAC,NUM,I
DECLARE L[4],S[4]
IF PCOUNT() = 3
   ESPACEJAMENTO = 1
ENDIF

AFILL(S,"")

FOR I = 1 TO LEN(STRING)
    NUM = SUBS(STRING,I,1)

    IF NUM = " "
       L[1] =  "   "
       L[2] =  "   "
       L[3] =  "   "
       L[4] =  "   "
    ELSEIF NUM = "-"
       L[1] =  "   "
       L[2] =  "‹‹‹"
       L[3] =  "   "
       L[4] =  "   "
    ELSEIF NUM = "1"
       L[1] =  "ﬂ€ "
       L[2] =  " € "
       L[3] =  " € "
       L[4] =  "ﬂﬂﬂ"
    ELSEIF NUM = "2"
       L[1] =  "ﬂﬂ€"
       L[2] =  "‹‹€"
       L[3] =  "€  "
       L[4] =  "ﬂﬂﬂ"
    ELSEIF NUM = "3"
       L[1] =  "ﬂﬂ€"
       L[2] =  "‹‹€"
       L[3] =  "  €"
       L[4] =  "ﬂﬂﬂ"
    ELSEIF NUM = "4"
       L[1] =  "€ €"
       L[2] =  "€‹€"
       L[3] =  "  €"
       L[4] =  "  ﬂ"
    ELSEIF NUM = "5"
       L[1] =  "€ﬂﬂ"
       L[2] =  "€‹‹"
       L[3] =  "  €"
       L[4] =  "ﬂﬂﬂ"
    ELSEIF NUM = "6"
       L[1] =  "€  "
       L[2] =  "€‹‹"
       L[3] =  "€ €"
       L[4] =  "ﬂﬂﬂ"
    ELSEIF NUM = "7"
       L[1] =  "ﬂﬂ€"
       L[2] =  "  €"
       L[3] =  "  €"
       L[4] =  "  ﬂ"
    ELSEIF NUM = "8"
       L[1] =  "€ﬂ€"
       L[2] =  "€‹€"
       L[3] =  "€ €"
       L[4] =  "ﬂﬂﬂ"
    ELSEIF NUM = "9"
       L[1] =  "€ﬂ€"
       L[2] =  "€‹€"
       L[3] =  "  €"
       L[4] =  "ﬂﬂﬂ"
    ELSEIF NUM = "0"
       L[1] =  "€ﬂ€"
       L[2] =  "€ €"
       L[3] =  "€ €"
       L[4] =  "ﬂﬂﬂ"
    ELSEIF NUM = ":"
       L[1] =  " "
       L[2] =  "ﬂ"
       L[3] =  "ﬂ"
       L[4] =  " "
    ELSEIF NUM = ","
       L[1] =  "  "
       L[2] =  "  "
       L[3] =  "  "
       L[4] =  "ﬂ›"
    ELSEIF NUM = "."
       L[1] =  " "
       L[2] =  " "
       L[3] =  " "
       L[4] =  "ﬂ"
    ELSEIF NUM = "A"
       L[2] =  "‹‹‹‹"
       L[3] =  "€‹‹€"
       L[4] =  "€  €"
    ELSEIF NUM = "B"
       L[2] =  "‹‹‹ "
       L[3] =  "€‹€‹"
       L[4] =  "€‹‹€"
    ELSEIF NUM = "C"
       L[2] =  '‹‹‹‹'
       L[3] =  '€   '
       L[4] =  '€‹‹‹'
    ELSEIF NUM = "D"
       L[2] =  '‹‹‹‹‹'
       L[3] =  ' €  €'
       L[4] =  '‹€‹‹€'
    ELSEIF NUM = "E"
       L[2] =  '‹‹‹‹'
       L[3] =  '€‹  '
       L[4] =  '€‹‹‹'
    ELSEIF NUM = "F"
       L[2] =  '‹‹‹‹'
       L[3] =  '€‹  '
       L[4] =  '€   '
    ELSEIF NUM = "G"
       L[2] =  '‹‹‹‹'
       L[3] =  '€ ‹‹'
       L[4] =  '€‹‹€'
    ELSEIF NUM = "H"
       L[2] =  '‹  ‹'
       L[3] =  '€‹‹€'
       L[4] =  '€  €'
    ELSEIF NUM = "I"
       L[2] =  '‹‹‹'
       L[3] =  ' € '
       L[4] =  '‹€‹'
    ELSEIF NUM = "J"
       L[2] =  ' ‹‹‹'
       L[3] =  '  € '
       L[4] =  '‹‹€ '
    ELSEIF NUM = "K"
       L[2] =  '‹ ‹‹'
       L[3] =  '€‹€‹'
       L[4] =  '€  €'
    ELSEIF NUM = "L"
       L[2] =  '‹   '
       L[3] =  '€   '
       L[4] =  '€‹‹‹'
    ELSEIF NUM = "M"
       L[2] =  '‹‹‹‹‹'
       L[3] =  '€ € €'
       L[4] =  '€ € €'
    ELSEIF NUM = "N"
       L[2] =  '‹‹‹ ‹'
       L[3] =  '€ € €'
       L[4] =  '€ €‹€'
    ELSEIF NUM = "O"
       L[2] =  '‹‹‹‹'
       L[3] =  '€  €'
       L[4] =  '€‹‹€'
    ELSEIF NUM = "P"
       L[2] =  '‹‹‹‹'
       L[3] =  '€‹‹€'
       L[4] =  '€   '
    ELSEIF NUM = "Q"
       L[2] =  '‹‹‹‹'
       L[3] =  '€  €'
       L[4] =  '€‹€€'
    ELSEIF NUM = "R"
       L[2] =  '‹‹‹‹'
       L[3] =  '€‹‹€'
       L[4] =  '€ €‹'
    ELSEIF NUM = "S"
       L[2] =  '‹‹‹‹'
       L[3] =  '€‹‹‹'
       L[4] =  '‹‹‹€'
    ELSEIF NUM = "T"
       L[2] =  '‹‹‹‹‹'
       L[3] =  '  €  '
       L[4] =  '  €  '
    ELSEIF NUM = "U"
       L[2] =  '‹  ‹'
       L[3] =  '€  €'
       L[4] =  '€‹‹€'
    ELSEIF NUM = "V"
       L[2] =  '‹  ‹'
       L[3] =  '€ ‹€'
       L[4] =  '€‹€ '
    ELSEIF NUM = "W"
       L[2] =  '‹ ‹ ‹'
       L[3] =  '€ € €'
       L[4] =  '€‹€‹€'
    ELSEIF NUM = "X"
       L[2] =  '‹  ‹'
       L[3] =  'ﬂ‹‹ﬂ'
       L[4] =  '€  €'
    ELSEIF NUM = "Y"
       L[2] =  '‹  ‹'
       L[3] =  '€‹‹€'
       L[4] =  '‹‹‹€'
    ELSEIF NUM = "Z"
       L[2] =  '‹‹‹‹'
       L[3] =  ' ‹‹ﬂ'
       L[4] =  '€‹‹‹'
    ENDIF

*   S[1] = S[1] + L[1] + SPACE(ESPACEJAMENTO)
    S[2] = S[2] + L[2] + SPACE(ESPACEJAMENTO)
    S[3] = S[3] + L[3] + SPACE(ESPACEJAMENTO)
    S[4] = S[4] + L[4] + SPACE(ESPACEJAMENTO)
    IF NUM = ","
*      S[1] = SPACE(1) + S[1]
       S[2] = SPACE(1) + S[2]
       S[3] = SPACE(1) + S[3]
       S[4] = SPACE(1) + S[4]
    ELSEIF NUM = "."
*      S[1] = SPACE(2) + S[1]
       S[2] = SPACE(2) + S[2]
       S[3] = SPACE(2) + S[3]
       S[4] = SPACE(2) + S[4]
    ENDIF

NEXT I

*S[1] = SUBS(S[1] , 1 , LEN(S[1]) - ESPACEJAMENTO)
S[2] = SUBS(S[2] , 1 , LEN(S[2]) - ESPACEJAMENTO)
S[3] = SUBS(S[3] , 1 , LEN(S[3]) - ESPACEJAMENTO)
S[4] = SUBS(S[4] , 1 , LEN(S[4]) - ESPACEJAMENTO)

*@ LIN  ,COL SAY S[1]
@ LIN,COL SAY S[2]
@ LIN+1,COL SAY S[3]
@ LIN+2,COL SAY S[4]

RETURN

