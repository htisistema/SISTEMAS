FUNCTION sac0

LOCAL lci:=01,cci:=01,lba:=15,cba:=36

PUBLIC memp_resa
PUBLIC mfirma_ins := 'INSERCOM Informatica Ltda.'
PUBLIC mversao := 'v1.00'


PUBLIC mcaminho,msituacao:='O',mtip_term
PUBLIC mol := '        '      // Molduras das janelas

PUBLIC vc_buffer,inicio_protocolo,fim_protocolo_driver


#include "fileio.ch"

// MODULO PRA ACESSAR ARQUIVOS .CDX
REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
//

SET MESSAGE TO 23 CENTER
SET CONS OFF
SET WRAP ON
SET SCOREBOARD OFF
SET DATE BRITI
SET DELE ON
SET INTEN ON
SET EPOCH TO 1911
set score off
set status off
CLEAR
IF ! AbriArq('saccfg','cfg');RETURN NIL;ENDIF
mtip_term := cfg->tip_term
IF EMPTY(mcaminho)
        mcaminho := ALLTRIM(cfg->caminho)
ENDIF
IF ! AbriArq('sacsetup','set',,1);RETURN NIL;ENDIF
msituacao := set->situacao
memp_resa := RTRIM(set->razao)

setcor(3)
limpa(01,00,24,79)
SETCOLOR('W+/B,x')
limpa(00,00,00,79)
DEVPOS(00,42);DEVOUT(datseman(DATE())+', '+DTOC(DATE())+' Hr:')
SHOWTIME(00,COL(),.F.,'W+/b',.F.,.F.)
DEVPOS(00,01);DEVOUT(memp_resa)
setcor(6)
DEVPOS(24,00);DEVOUT(PADL(mfirma_ins+' >> SIAC '+mversao+' <<',80))
DEVPOS(24,00);DEVOUT('>> SIAC '+mversao+' <<')
setcor(1)
CAIXA(mol,01,00,22,79,,,.F.)
SETCOLOR('gr+/b,x')
DEVPOS(22,00);DEVOUT('ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ')
setcor(1)
limpa(23,00,23,79)
WHILE .t.
         DBCLOSEALL()
         botao(lci,cci,lba,cba,,' Menu Principal ')
         vn_menu := 0
         SETCOLOR('N/W,W+/N,X')
         @ lci+1,cci+1 PROMPT "Cupom Fiscal                      "
         @ lci+2,cci+1 PROMPT "Cadastro de Produtos              "
         @ lci+3,cci+1 PROMPT "Relatorios                        "
         @ lci+4,cci+1 PROMPT "Al¡quota Tribut ria               "
         @ lci+5,cci+1 PROMPT "Sai do Sistema                    "

         MENU TO vn_menu

         tamanho_de_retorno := 0
         vc_buffer := inicio_protocolo := chr(27) + chr(251)         /* abertura   protocolo */
         fim_protocolo_driver := "|"+ chr(27)    /* fechamento protocolo */

// sequencia_retorno  := ""
         IF LASTKEY() = 27
                 fim('"SIAC"-Sistema Integrado de Automa‡ao Comercial ('+mversao+')')
         ENDIF

         do CASE
            CASE vn_menu = 1
                 outro_04_menu()
            CASE vn_menu = 2
                 sac11()
            CASE vn_menu = 3
                 outro_02_menu()
            CASE vn_menu = 4
                 outro_06_menu()
            CASE vn_menu = 5
                 fim('"SIAC"-Sistema Integrado de Automa‡ao Comercial ('+mversao+')')
         endCASE
end
RETURN

******************************  F I M  *********************************
static FUNCTION outro_02_menu()

LOCAL tela:=SAVESCREEN(00,00,24,79),mopcao:=' ',ret

WHILE .t.
        botao(01,01,06,40)
        SETCOLOR('N/W,W+/N,X')
        vn_menu3 := 0
        @  2,2 PROMPT "Letura X                              "
        @  3,2 PROMPT "Redu‡Æo Z                             "
        @  4,2 PROMPT "Leitura da Mem¢ria Fiscal p/Arquivo   "
        @  5,2 PROMPT "Leitura da Mem¢ria Fiscal p/Impressora"
        MENU TO vn_menu3

        prepara_string := NIL

        DO CASE
                CASE vn_menu3 = 0
                        RESTSCREEN(00,00,24,79,tela)
                        exit
                CASE vn_menu3 = 1
                        mensagem('Confirma a impressao da LEITURA "X" [s/N]:')
                        mopcao := 'N'
                        @ 23,COL()+1 GET mopcao PICT '@!' VALID mopcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. mopcao = 'N'
                                RESTSCREEN(00,00,24,79,tela)
                                LOOP
                        ENDIF
                        ret := leituraX('LeituraX')
                        IF ret <> 0
                                atencao('Retorno da Impressora:'+ret)
                        ENDIF
                CASE vn_menu3 = 2
                        mensagem('Confirma a impressao da LEITURA "Z" [s/N]:')
                        mopcao := 'N'
                        @ 23,COL()+1 GET mopcao PICT '@!' VALID mopcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. mopcao = 'N'
                                RESTSCREEN(00,00,24,79,tela)
                                LOOP
                        ENDIF
                        ret := reducaoZ('LeituraZ')
                        IF ret <> 0
                                atencao('Retorno da Impressora:'+ret)
                        ENDIF
                CASE vn_menu3 = 3
                        SET CENTURY OFF
                        mem_fis1()
                        SET CENTURY ON
                CASE vn_menu3 = 4
                        SET CENTURY OFF
                        mem_fis2()
                        SET CENTURY ON

        ENDCASE

ENDDO

RETURN NIL

************************** F I M ****************************************

static FUNCTION outro_04_menu()

LOCAL tela:=SAVESCREEN(00,00,24,79),ret:=0

WHILE .t.
        botao(01,01,12,46,,' Menu Cupom Fiscal ')
        SETCOLOR('N/W,W+/N,X')
        vn_menu5 := 0
        @  2,2 PROMPT "Caixa                                       "
        @  3,2 PROMPT "Venda de Produtos                           "
        @  4,2 PROMPT "Cancela Cupom Fiscal - Em andamento         "
        @  5,2 PROMPT "Cancela Cupom Fiscal - ja Finalizado        "
        @  6,2 PROMPT "Cancela Ultimo Item Vendido                 "
        @  7,2 PROMPT "Cancela Item Gen‚rico                       "
        @  8,2 PROMPT "Fecha Cupom Fiscal sem Formas de Pagamento  "
        @  9,2 PROMPT "Fecha Cupom Fiscal com Formas de Pagamento  "
        @ 10,2 PROMPT "Retorna o Subtotal                          "
        @ 11,2 PROMPT "N£mero do Cupom                             "

        MENU TO vn_menu5

        DO CASE
                CASE vn_menu5 = 0
                        RESTSCREEN(00,00,24,79,tela)
                             exit
                CASE vn_menu5 = 1        // CAIXA
                        schaped()
                CASE vn_menu5 = 2       // VENDA DE PRODUTO
                        sac23ecf()
                CASE vn_menu5 = 3        // CANCELA CUPOM FISCAL
                        ret := cancvenda(cod_operado)
                        IF ret <> 0
                                atencao('Retorno da impressao: '+ret)
                                LOOP
                        ELSE
                                ret := linefeed(1,10)
                                IF ret <> 0
                                        atencao('Retorno da impressao: '+ret)
                                        LOOP
                                ENDIF
                        ENDIF
                CASE vn_menu5 = 4        // CANCELA CUPOM FISCAL
                        ret := cancdoc(cod_operado)
                        IF ret <> 0
                                atencao('Retorno da impressao: '+ret)
                                LOOP
                        ELSE
                                ret := linefeed(1,10)
                                IF ret <> 0
                                        atencao('Retorno da impressao: '+ret)
                                        LOOP
                                ENDIF
                        ENDIF
                CASE vn_menu5 = 5        // CANCELA ULTIMO ITEM
                        canultitem()
                CASE vn_menu5 = 6        // CANCELA ITEM GENERICO
                        can_item()
                CASE vn_menu5 = 7        // FECHA CUPOM FISCAL SEM FORMA DE PAGAMENTO
                CASE vn_menu5 = 8       // INICIA FECHAMENTO DE CUPOM COM FORMAS DE PAGAMENTO */
                CASE vn_menu5 = 9        // RETORNA SUBTOTAL DO CUPOM
                CASE vn_menu5 = 10       // NUMERO DO CUPOM

        ENDCASE

ENDDO
RETURN NIL
************************** F I M ****************************************

static FUNCTION mostra_informacao( mensagem, conteudo )

atencao(mensagem + " þ" + conteudo + "þ")
RETURN NIL
************************** F I M ****************************************

static FUNCTION outro_06_menu()

LOCAL tela:=SAVESCREEN(00,00,24,79),maliquota:=' '

WHILE .t.
        botao(01,01,04,46)
        SETCOLOR('N/W,W+/N,X')
        vn_menu7 := 0
        @  2,2 PROMPT "Retorno de Al¡quotas                        "
        @  3,2 PROMPT "Adi‡Æo de Al¡quota Tribut ria               "
        MENU TO vn_menu7

        prepara_string := NIL
        tamanho_de_retorno := 0

        do CASE
                CASE vn_menu7 = 0
                        RESTSCREEN(00,00,24,79,tela)
                        exit
                CASE vn_menu7 = 1
                        ver_aliquota()
                CASE vn_menu7 = 2
        ENDCASE

ENDDO
RETURN NIL
************************** F I M ****************************************
* CANCELA ULTIMO ITEM VENDIDO
**************************
FUNCTION canultitem

LOCAL opcao

CLEAR GETS

mensagem('Confirma o cancelamento do ultimo item [S/n]:')
opcao := 'S'
@ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
READ
IF LASTKEY() = 27 .OR. opcao = 'N'
        RETURN NIL
ENDIF
RETUR NIL
************************** F I M ****************************
* CANCELA ITEM VENDIDO
**************************
FUNCTION can_item
LOCAL tela,item_a_ser_cancelado
telai := SAVESCREEN(00,00,24,79)
CLEAR GETS
item_a_ser_cancelado := space(4)
botao(15,01,17,45,,' Cancelar item generico ')
@ 16,02 SAY "N£mero do item a ser cancelado" get item_a_ser_cancelado picture "9999"
READ
IF LASTKEY() = 27 .OR. EMPTY(item_a_ser_cancelado)
        RESTSCREEN(00,00,24,79,telai)
        RETURN NIL
ENDIF
RESTSCREEN(00,00,24,79,telai)
RETUR NIL
****************************  F I M **************************************
***************************************************************************
* Se no parƒmetro "Tipo" for selecionado per¡odo entre datas, os valores dos
* parƒmetros "Ri" e "Rf" sÆo ignorados. Se no parƒmetro "Tipo" for selecionado
* per¡odo entre redu‡äes, os parƒmetros "Di" e "Df" sÆo ignorados. No entanto, todos
* os parƒmetro de chamada da fun‡Æo devem ser passados independente de serem
* considerados ou nÆo.
* Para aplica‡äes que utilizem a biblioteca e o driver em modo protegido, deve-se
* realizar a leitura da mem¢ria fiscal em blocos, sendo que cada bloco deve ser
* selecionado em no m ximo 100 redu‡äes.
* Exemplos:
* O exemplo a seguir utiliza o comando "Leitura de Disponibilidade da Mem¢ria Fiscal"
* para definir o n£mero de vetores para os dados dos usu rios e das redu‡äes. Ap¢s a leitura da
* mem¢ria fiscal os dados sÆo colocados em um arquivo.
***************************************************************************

FUNCTION mem_fis1
******************

LOCAL lci:=10,cci:=10,lba:=17,cba:=40
LOCAL vRet:= {}, vUsu:= {}, vRed:= {}, vTot:= {}, i, vRetF:= {}
LOCAL DtIni, DtFim, Tipo, Red_i, ReD_f, Drive, Arquivo
DtIni := DtFim := ctod ( "" )
Tipo := 1
Red_i := Res_f := 0
Arquivo := SPACE(11)
Drive := "C"
AADD( vRet, 0 )
botao(lci,cci,lba,cba,,' Leitura Memoria Fiscal ')
@ lci+1,cci+1 SAY "Tipo: " get Tipo pict "@K 9" valid Tipo < 5 .and. Tipo > 0
@ lci+2,cci+1 SAY "Data Inicial: " get DtIni pict "@K" when Tipo == 1 .or. Tipo == 3
@ lci+3,cci+1 SAY "Data Final: " get DtFim pict "@K" when Tipo == 1 .or. Tipo == 3
@ lci+4,cci+1 SAY "Redu‡Æo Inicial:" get Red_i pict "@K 9999" when Tipo == 2 .or. Tipo == 4
@ lci+5,cci+1 SAY "Redu‡Æo Final: " get Red_f pict "@K 9999" when Tipo == 2 .or. Tipo == 4
@ lci+6,cci+1 SAY "Arquivo: " get Arquivo pict "@K XXXXXXXXXXX" when Tipo > 2
IF( lastkey() != 27 )
        DtIni := DTOC( DtIni )
        DtFim := DTOC( DtFim )
        DtIni := SUBSTR(DtIni,1,2) + SUBSTR(DtIni,4,2) + SUBSTR(DtIni,7,2)
        DtFim := SUBSTR( DtFim,1,2) + SUBSTR(DtFim,4,2) + SUBSTR( DtFim, 7, 2 )
        mensagem("Aguarde um momento fazendo a leitura na Impressora Fiscal...")
        AADD( vRet, 0 )
        AADD( vRet, 0 )
        AADD( vRet, 0 )
        ret := LeitDispMF( VRet, tipo, DtIni, DtFim, Red_i, Red_f )
        if( vRet[ 1 ] == 0 )
                vRet[ 2 ] = if( vRet[ 2 ] == 0, 1, vRet[ 2 ] )
                vRet[ 3 ] = if( vRet[ 3 ] == 0, 1, vRet[ 3 ] )
                FOR i = 1 to vRet[ 2 ]
                        AADD( vUsu, SPACE( 59 ) )
                NEXT
                FOR i = 1 to vRet[ 3 ]
                        AADD( vRed, SPACE( 41 ) )
                NEXT
                AADD( vCab, 0 )
                AADD( vCab, 0 )
                AADD( vCab, 0 )
                AADD( vCab, SPACE( 20 ) )
                LeitMFisc( vCab, vUsu, vRed, tipo, DtIni, DtFim, Red_i, Red_f )
                if( Tipo == 3 .or. Tipo == 4 )
                        Arquivo := alltrim( Arquivo )
                        SET printer TO ( Arquivo )
                        SET device TO printer
                        FOR i = 1 TO len( vUsu )
                                @ prow()+1,1 SAY SUBSTR(vUsu[i], 1,18)+;
                                SUBSTR( vUsu[ i ], 19, 15 ) + " " +;
                                SUBSTR( vUsu[ i ], 34, 8 ) + " " +;
                                SUBSTR( vUsu[ i ], 42, 5 ) + " " +;
                                SUBSTR( vUsu[ i ], 47, 8 ) + " " +;
                                SUBSTR( vUsu[ i ], 55, 5 )
                        NEXT
                        FOR i = 1 TO len( vRed )
                                @ prow() + 1, 1 SAY SUBSTR( vRed[ i ], 1, 4 )+" "+;
                                SUBSTR( vRed[ i ], 5, 4 ) + " " +;
                                SUBSTR( vRed[ i ], 9, 8 ) + " " +;
                                SUBSTR( vRed[ i ], 17, 5 ) + " " +;
                                SUBSTR( vRed[ i ], 22, 20 )
                        NEXT
                        @ prow() + 1, 1 SAY SUBSTR( vCab[ 4 ], 1, 20 )
                        SET device TO screen
                        SET printer to
                ENDIF
        ENDIF
ENDIF
RETURN NIL
****************************  F I M **************************************
****************************************************
* LEITURA MEMORIA FISCAL VIA SERIAL
****************************************************

FUNCTION mem_fis2

local DtIni, DtFim, Tipo, Red_i, Res_f, Drive, Arquivo
local vRet := {}
DtIni := DtFim := ctod( "" )
Tipo := 1
Red_i := Res_f := 0
Arquivo := space( 11 )
Drive := "A"
aadd( vRet, 0 )
@ 13,24 SAY "Tipo: " GET Tipo PICT "@K 9" valid Tipo < 5 .and. Tipo > 0
@ 14,24 SAY "Data Inicial: " GET DtIni PICT "@K" WHEN Tipo == 1 .OR. Tipo == 3
@ 15,24 SAY "Data Final: " GET DtFim PICT "@K" WHEN Tipo == 1 .OR. Tipo == 3
@ 16,24 SAY "Redu‡Æo Inicial:" GET Red_i PICT "@K 9999" WHEN Tipo == 2 .OR. Tipo == 4
@ 17,24 SAY "Redu‡Æo Final: " GET Res_f PICT "@K 9999" WHEN Tipo == 2 .OR. Tipo == 4
@ 18,24 SAY "Arquivo: " GET Arquivo PICT "@K XXXXXXXXXXX" WHEN Tipo > 2
READ
IF( lastkey() != 27 )
        DtIni := DTOC( DtIni )
        DtFim := DTOC( DtFim )
        DtIni := SUBSTR(DtIni,1,2) + SUBSTR(DtIni,4,2) +;
        SUBSTR(DtIni,7,2)
        DtFim := SUBSTR( DtFim,1,2) + SUBSTR(DtFim,4,2) +;
        SUBSTR( DtFim, 7, 2 )
        @ 19,24 SAY "Aguarde..."
        vRet:= LeitMemFisc ( Arquivo, Tipo, DtIni, DtFim, ;
        Red_i, Res_f )
        IF ( vRet[ 1 ] != 0 )
                @ 19,24 SAY "Erro..."
                INKEY( 0 )
        ELSE
                @ 19,24 SAY "Ok..."
                INKEY( 0 )
        ENDIF
ENDIF
RETURN NIL

****************************  F I M **************************************
* VER ALIQUOTAS
***************************
FUNCTION ver_aliquota

LOCAL VRet, Janela, cor
LOCAL IndAliq, numaliq
botao(14, 20, 19, 71,," Status de uma Aliquota ")
IndAliq := space( 2 )
IndAliq := "00"
numaliq := 0
@ 16, 23 SAY "Indice da Aliquota: " GET IndAliq PICT "@K 99"
READ
numaliq := val(Indaliq)
IF (numaliq > 20)
        atencao("Entrada de dados incorreta")
        RETURN NIL
ELSE
        @ 18, 23 SAY "Aguarde..."
        WHILE  LASTKEY() <> 27
                VRet := StatusAl(numaliq)
                IF ! EMPTY(VRet[1])
                        atencao( "Retorno Impressora: "+VRet[ 1 ] )
                ELSE
                        IF ( VRet[2] == "S" )
                                @ 16, 23 SAY " "
                                @ 16, 23 SAY "ISSQN : "
                                ?? VRet[ 3 ]
                        ENDIF
                        IF ( VRet[2] == "T" )
                                @ 16, 23 SAY " "
                                @ 16, 23 SAY "ICMS : "
                                ?? VRet[ 3 ]
                        ENDIF
                        @ 17, 23 SAY " Total parcial : "
                        ?? VRet[ 4 ]
                        @ 18, 23 SAY "Imposto incidente : "
                        ?? VRet[ 5 ]
                ENDIF
                INKEY(1)
        ENDDO
ENDIF
RETURN NIL

