FUNCTION sac0

LOCAL lci:=01,cci:=01,lba:=15,cba:=36

PUBLIC memp_resa
PUBLIC mfirma_ins := 'INSERCOM Informatica Ltda.'
PUBLIC mversao := 'v6.00'


PUBLIC mcaminho,msituacao:='O',mtip_term
PUBLIC mol := '        '      // Molduras das janelas

PUBLIC vc_buffer,inicio_protocolo,fim_protocolo_driver


#include "fileio.ch"

#define ST1_BIT_0 "n£mero de parÉmetros de CMD inv†lido"
#define ST1_BIT_1 "cupom aberto"
#define ST1_BIT_2 "comando inexistente"
#define ST1_BIT_3 "primeiro dado de CMD n∆o foi ESC (1Bh)"
#define ST1_BIT_4 "impressora em erro"
#define ST1_BIT_5 "erro no rel¢gio"
#define ST1_BIT_6 "pouco papel"
#define ST1_BIT_7 "fim de papel"

#define ST2_BIT_0 "comando n∆o executado"
#define ST2_BIT_1 "CGC/IE do propriet†rio n∆o programados"
#define ST2_BIT_2 "cancelamento n∆o permitido"
#define ST2_BIT_3 "capacidade de al°quotas program†veis lotada"
#define ST2_BIT_4 "al°quota n∆o programada"
#define ST2_BIT_5 "erro na Mem¢ria RAM CMOS n∆o vol†til"
#define ST2_BIT_6 "Mem¢ria fiscal lotada"
#define ST2_BIT_7 "tipo de parÉmetro de CMD inv†lido"

/* global executou_sn := .t.*/

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

qout("Um momento, abrindo porta serial ("+cfg->bematech+")...")

numero_abertura_porta := fopen((cfg->bematech), FO_READWRITE + FO_COMPAT)

IF ferror () != 0
           qout("Problemas de comunicacao. Pressione qualquer tecla.")
           RETURN
ENDIF

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
DEVPOS(22,00);DEVOUT('‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹')
setcor(1)
limpa(23,00,23,79)
WHILE .t.
         DBCLOSEALL()
         botao(lci,cci,lba,cba,,' Menu Principal ')
         vn_menu := 0
         SETCOLOR('N/W,W+/N,X')
         @ lci+1,cci+1 PROMPT "Cupom Fiscal                      "
         @ lci+2,cci+1 PROMPT "Cupom N∆o Fiscal                  "
         @ lci+3,cci+1 PROMPT "Cadastro de Produtos              "
         @ lci+4,cci+1 PROMPT "Flags                             "
         @ lci+5,cci+1 PROMPT "Relatorios                        "
         @ lci+6,cci+1 PROMPT "Entra em hor†rio de ver∆o         "
         @ lci+7,cci+1 PROMPT "Sai do hor†rio de ver∆o           "
         @ lci+8,cci+1 PROMPT "Truncamento/Arredondamento        "
*        @ lci+8,cci+1 PROMPT "Nomeia Totalizadores Parciais     "
         @ lci+9,cci+1 PROMPT "Simbolo da Moeda Corrente         "
*        @ lci+9,cci+1 PROMPT "Datas                             "
         @ lci+10,cci+1 PROMPT "Informaá‰es Variadas              "
         @ lci+11,cci+1 PROMPT "Al°quota Tribut†ria               "
         @ lci+12,cci+1 PROMPT "Recebimentos NÑo sujeitos ao ICMS "
*        @ lci+13,cci+1 PROMPT "Autenticaá∆o                      "
*        @ lci+14,cci+1 PROMPT "Acionamento de Gaveta             "
*        @ lci+15,cci+1 PROMPT "Imprime cheque                    "
*        @ lci+16,cci+1 PROMPT "Dados Da Ultima Reduá∆o           "
*        @ lci+17,cci+1 PROMPT "Leitura X Pela Serial             "
*        @ lci+18,cci+1 PROMPT "Leitura das Formas de Pagamento   "
         @ lci+13,cci+1 PROMPT "Sai do Sistema                    "

         MENU TO vn_menu

         tamanho_de_retorno := 0
         vc_buffer := inicio_protocolo := chr(27) + chr(251)         /* abertura   protocolo */
         fim_protocolo_driver := "|"+ chr(27)    /* fechamento protocolo */

// sequencia_retorno  := ""
         IF LASTKEY() = 27
                 fclose(numero_abertura_porta) /* fecha porta COMx: */
                 fim('"SIAC"-Sistema Integrado de Automaáao Comercial ('+mversao+')')
                 numero_abertura_porta := fopen("COM1", FO_READWRITE + FO_COMPAT)
         ENDIF

         do CASE
            CASE vn_menu = 1
                 outro_04_menu()
            CASE vn_menu = 2
                 outro_05_menu()
            CASE vn_menu = 3
                 sac11()
            CASE vn_menu = 4
                 outro_03_menu()
            CASE vn_menu = 5
                 outro_02_menu()
            CASE vn_menu = 6
                 vc_buffer += "18" + fim_protocolo_driver
                 com_c_imp(vc_buffer,tamanho_de_retorno)
            CASE vn_menu = 7
                 vc_buffer += "18" + fim_protocolo_driver
                 com_c_imp(vc_buffer,tamanho_de_retorno)

            CASE vn_menu = 8
                 resp := space(1)
                 resp := mensagem1("Vocà deseja Truncar ou Arredondar?",'T','T,R')
                 set color to
                 READ
                 IF LASTKEY() = 27
                        LOOP
                 ENDIF

                 IF resp = "T"
        	   tamanho_de_retorno := 0
          	   prepara_string := inicio_protocolo + "39|0" + fim_protocolo_driver
                   com_c_imp(prepara_string,tamanho_de_retorno)
                 ENDIF
                 IF resp = "A"
        	   tamanho_de_retorno := 0
                   prepara_string := inicio_protocolo + "39|1" + fim_protocolo_driver
                   com_c_imp(prepara_string,tamanho_de_retorno)
                 ENDIF
/*
            CASE vn_menu = 8
                 vc_buffer := chr(27) + chr(251)+"40|#1|Conta da Xuxa      " + fim_protocolo_driver
                 tamanho_de_retorno := 0
                 prepara_string := NIL
                 string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
*/
            CASE vn_menu = 9
                 tamanho_de_retorno := 2
                 prepara_string := NIL
                 prepara_string := vc_buffer + "35|16" + fim_protocolo_driver
                 string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)

                 @ 23,30 SAY "Simbolo de moeda atual ˛" + string_devolvida + "˛"
                 altera_sn := string_devolvida
                 @ 23,54 get altera_sn
                 READ
                 IF altera_sn # string_devolvida
                    prepara_string := NIL
                    prepara_string := vc_buffer + "01|"+ altera_sn + fim_protocolo_driver
                    com_c_imp(prepara_string,tamanho_de_retorno)
                 ENDIF
/*
            CASE vn_menu = 9

                 tamanho_de_retorno := 12
                 prepara_string := chr(27) + chr(251)+ + "35|23" + fim_protocolo_driver
                 com_c_imp(prepara_string,tamanho_de_retorno)
*/

            CASE vn_menu = 10
                 outro_01_menu()
            CASE vn_menu = 11
                 outro_06_menu()
            CASE vn_menu = 12
                 outro_07_menu()
/*
            CASE vn_menu = 13   \\autenticacao
                 mensagem("Insira o Documento a ser autenticado e pressione, <ENTER>!")
                 INKEY(0)
                 IF lastkey() = 13
         	    set color to
         	    tamanho_de_retorno := 0
                    prepara_string := NIL
         	    prepara_string := inicio_protocolo + "16" + fim_protocolo_driver
                    string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                 ENDIF
        /* Acionamento da Gaveta */

            CASE vn_menu = 14   // ACIONAMNETO DE GAVETA
        	 tamanho_de_retorno := 0
                 prepara_string := NIL
                 prepara_string := inicio_protocolo + "22|" + chr(255) + fim_protocolo_driver
                 string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
            CASE vn_menu = 15   // IMPRIME CHEQUES
                 tamanho_de_retorno := 0
                 prepara_string := NIL
                 prepara_string := inicio_protocolo +"57|001|00000000010055|Jefferson Wendhausen Mendes Ltda.            |                   Curitiba|10| Fevereiro|  98|Test eTes teTes teTesteT" + fim_protocolo_driver
                 string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)

            CASE vn_menu = 16   // DADOS DA ULTIMA REDUCAO Z
                 tamanho_de_retorno := 308
                 prepara_string := NIL
                 prepara_string := inicio_protocolo +"62|55" + fim_protocolo_driver
                 string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
            CASE vn_menu = 17
                 tamanho_de_retorno := 2500
                 prepara_string := NIL
                 prepara_string := inicio_protocolo +"69" + fim_protocolo_driver
                 string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
            CASE vn_menu = 18
                 tamanho_de_retorno := 1550
                 prepara_string := NIL
                 prepara_string := inicio_protocolo +"35|34" + fim_protocolo_driver
                 string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
*/
            CASE vn_menu = 13
                 fclose(numero_abertura_porta) /* fecha porta COMx: */
                 fim('"SIAC"-Sistema Integrado de Automaáao Comercial ('+mversao+')')
                 numero_abertura_porta := fopen("COM1", FO_READWRITE + FO_COMPAT)
         endCASE
end
RETURN

******************************  F I M  *********************************

static FUNCTION nomeia_totalizadores(linhas_totalizadores)

 FOR contador1 := 1 TO 9
     totalizadores[contador1][1] := substr(linhas_totalizadores,((contador1*19)-18),19)
     totalizadores[contador1][2] := "#"+str(contador1,1,0)
 NEXT contador1
 FOR contador1 := 1 TO 9
     @ 10+contador1,30 SAY totalizadores[contador1][2] get totalizadores[contador1][1]
 NEXT contador1
 READ

RETURN NIL
******************************  F I M  *********************************

static FUNCTION Decodifica_Retorno(ACK,ST1,ST2)

   Bit_st1:=Bit_st2:=Bit_ack:=0
   Bit_ack=Val(ACK)
   Bit_st1=Val(ST1)
   Bit_st2=Val(ST2)

//Tratamento do Ack ou NAk
  IF Bit_ack # 6
   ? "Problemas ao Enviar o Comando a Impressora Devolveu 21"
  end if

//Aqui Ç Feito o Tratamento do St1 subtraindo  o Byte anterior pelo Seguinte
  IF Bit_st1 >= 128;? ST1_BIT_7 ;Bit_st1=Bit_st1-128 ;end if
  IF Bit_st1 >= 64 ;? ST1_BIT_6 ;Bit_st1=Bit_st1-64  ;end if
  IF Bit_st1 >= 32 ;? ST1_BIT_5 ;Bit_st1=Bit_st1-32  ;end if
  IF Bit_st1 >= 16 ;? ST1_BIT_4 ;Bit_st1=Bit_st1-16  ;end if
  IF Bit_st1 >= 8  ;? ST1_BIT_3 ;Bit_st1=Bit_st1-8   ;end if
  IF Bit_st1 >= 4  ;? ST1_BIT_2 ;Bit_st1=Bit_st1-4   ;end if
  IF Bit_st1 >= 2  ;? ST1_BIT_1 ;Bit_st1=Bit_st1-2   ;end if
  IF Bit_st1 >= 1  ;? ST1_BIT_0                      ;end if
//Aqui Ç Feito o Tratamento do St2 subtraindo  o Byte anterior pelo Seguinte
  IF Bit_st2 >= 128;? ST2_BIT_7 ;Bit_st2=Bit_st2-128 ;end if
  IF Bit_st2 >= 64 ;? ST2_BIT_6 ;Bit_st2=Bit_st2-64  ;end if
  IF Bit_st2 >= 32 ;? ST2_BIT_5 ;Bit_st2=Bit_st2-32  ;end if
  IF Bit_st2 >= 16 ;? ST2_BIT_4 ;Bit_st2=Bit_st2-16  ;end if
  IF Bit_st2 >= 8  ;? ST2_BIT_3 ;Bit_st2=Bit_st2-8   ;end if
  IF Bit_st2 >= 4  ;? ST2_BIT_2 ;Bit_st2=Bit_st2-4   ;end if
  IF Bit_st2 >= 2  ;? ST2_BIT_1 ;Bit_st2=Bit_st2-2   ;end if
  IF Bit_st2 >= 1  ;? ST1_BIT_0                      ;end if

RETURN NIL

************************ F I M ************************************

*static FUNCTION com_c_imp(buffer_a_ser_enviado,tam_a_ser_ret)
FUNCTION com_c_imp(buffer_a_ser_enviado,tam_a_ser_ret)

 /* evia sequencia de bytes para impressora */

 fwrite(numero_abertura_porta, @buffer_a_ser_enviado, len(buffer_a_ser_enviado))

 /* pega o retorno da impressora NAK/ACK ST1 ST2 */

 retorno_impressora := ack := nak := st1 := st2 := space(1)

 FOR contador1 := 1 TO 3

     fREAD(numero_abertura_porta,@retorno_impressora,1)

     do CASE
        CASE contador1 = 1
             do CASE
                CASE asc(retorno_impressora) = 21 /* retorno em caracter 21d=15h=NAK */
                     atencao("Atencao... a impressora retornou 21d=15h=NAK   [Programa Abortado]")
*                     ?
*                     ? "NAK"
*                     ?
*                     ? "Programa abortado!"
*                     ?
//                    fclose(numero_abertura_porta)
//                     quit
//                     executou_sn := .f.
                CASE asc(retorno_impressora) = 06
                     ack := transform(asc(retorno_impressora),"99")
                otherwise
*                    CLEAR
*                    ?
                     atencao("Atencao... provavelmente o driver nao foi carregado !  *** Programa Abortado ! ***")
*                    ? "Programa abortado!"

*                    ?
                     fclose(numero_abertura_porta)
                     quit
//                     executou_sn := .f.
             endCASE
        CASE contador1 = 2
             st1 := transform(asc(retorno_impressora),"999")
             //  st1 := asc(retorno_impressora)
        CASE contador1 = 3
             st2 := transform(asc(retorno_impressora),"999")
             //st2 := asc(retorno_impressora)
     endCASE

 NEXT contador1

 /* pega sequencia de retorno caso necess†rio */

 sequencia_retorno := ""
 FOR contador2 := 1 TO tam_a_ser_ret
    fREAD(numero_abertura_porta, @retorno_impressora, 1)
    sequencia_retorno += retorno_impressora
 NEXT contadorcci+1


 Decodifica_Retorno(ack,st1,st2)

 IF asc(retorno_impressora) # 21


*   atencao("ACK = [" + ack + "]   ST1 = [" + st1 + "]   ST2 = ["+ st2 + "]  | Retorno = [" + sequencia_retorno + "]")

*   ?
*   ? "ACK = [" + ack + "]   ST1 = [" + st1 + "]   ST2 = ["+ st2 + "]"
*   ? "Retorno = [" + sequencia_retorno + "]"
*   ?
*   ?
*   wait "Tecle algo para continuar"

 ENDIF

RETURN sequencia_retorno /* executou_sn */

************************** F I M ****************************************

static FUNCTION outro_01_menu()

LOCAL tela:=SAVESCREEN(00,00,24,79)

WHILE .t.
        botao(01,01,19,35)
        SETCOLOR('N/W,W+/N,X')
        vn_menu2 := 0
        @  2,2 PROMPT "N£mero de SÇrie                  "
        @  3,2 PROMPT "Vers∆o do Firmware               "
        @  4,2 PROMPT "CGC/IE                           "
        @  5,2 PROMPT "Grande Total                     "
        @  6,2 PROMPT "N£mero de Cancelamentos          "
        @  7,2 PROMPT "N£mero de Descontos              "
        @  8,2 PROMPT "Contador Sequàncial              "
        @  9,2 PROMPT "N£mero de Operaá‰es N∆o Fiscais  "
        @ 10,2 PROMPT "N£mero de Cupons Cancelados      "
        @ 11,2 PROMPT "N£mero de reduá‰es               "
        @ 12,2 PROMPT "N£mero de intervená‰es           "
        @ 13,2 PROMPT "N£mero de Substituiá‰es de Prop. "
        @ 14,2 PROMPT "N£mero do Ultimo Iten Vendido    "
        @ 15,2 PROMPT "Clichà do Propriet†rio           "
        @ 16,2 PROMPT "Retorna Byte de Vinculaá∆o ISS   "
        @ 17,2 PROMPT "Numero do Caixa                  "
        @ 18,2 PROMPT "Numero da Loja                   "

        MENU TO vn_menu2

        prepara_string := NIL

        do CASE
           CASE vn_menu2 = 0
                RESTSCREEN(00,00,24,79,tela)
                exit
           CASE vn_menu2 = 1
                tamanho_de_retorno := 14
                prepara_string := inicio_protocolo + "35|00" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("N£mero de SÇrio",string_devolvida)
           CASE vn_menu2 = 2
                tamanho_de_retorno := 4
                prepara_string := inicio_protocolo + "35|01" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Vers∆o do Firmware",string_devolvida)
           CASE vn_menu2 = 3
                tamanho_de_retorno := 33
                prepara_string := inicio_protocolo + "35|02" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("CGC/IE",string_devolvida)
           CASE vn_menu2 = 4
                tamanho_de_retorno := 18
                prepara_string := inicio_protocolo + "35|03" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Grande Total",string_devolvida)
           CASE vn_menu2 = 5
                tamanho_de_retorno := 14
                prepara_string := inicio_protocolo + "35|04" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Cancelamentos",string_devolvida)
           CASE vn_menu2 = 6

                tamanho_de_retorno := 14
                prepara_string := inicio_protocolo + "35|05" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Descontos",string_devolvida)
           CASE vn_menu2 = 7
                tamanho_de_retorno := 6
                prepara_string := inicio_protocolo + "35|06" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Contador Sequencial",string_devolvida)
           CASE vn_menu2 = 8
                tamanho_de_retorno := 6
                prepara_string := inicio_protocolo + "35|07" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Operaá‰es n∆o Fiscais",string_devolvida)
           CASE vn_menu2 = 9
                tamanho_de_retorno := 4
                prepara_string := inicio_protocolo + "35|08" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Cupons Cancelados",string_devolvida)
           CASE vn_menu2 = 10
                tamanho_de_retorno := 4
                prepara_string := inicio_protocolo + "35|09" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Reduá‰es",string_devolvida)
           CASE vn_menu2 = 11
                tamanho_de_retorno := 4
                prepara_string := inicio_protocolo + "35|10" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Intervená‰es",string_devolvida)
           CASE vn_menu2 = 12
                tamanho_de_retorno := 4
                prepara_string := inicio_protocolo + "35|11" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Subst.Propriet†rio",string_devolvida)
           CASE vn_menu2 = 13
                tamanho_de_retorno := 4
                prepara_string := inicio_protocolo + "35|12" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Èltimo item vendido",string_devolvida)
           CASE vn_menu2 = 14
                tamanho_de_retorno := 144
                prepara_string := inicio_protocolo + "35|13" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Èltimo item vendido",string_devolvida)
           CASE vn_menu2 = 15
                tamanho_de_retorno := 2
                prepara_string := inicio_protocolo + "35|29" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                x=Transform(Asc(string_devolvida),"999")
                mostra_informacao("Èltimo item vendido",x)
           CASE vn_menu2 = 16
                tamanho_de_retorno := 4
                prepara_string := inicio_protocolo + "35|14" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Numero do Caixa",string_devolvida)
           CASE vn_menu2 = 17
                tamanho_de_retorno := 4
                prepara_string := inicio_protocolo + "35|15" + fim_protocolo_driver
                string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                mostra_informacao("Numero da Loja",string_devolvida)

        endCASE

 ENDDO

RETURN NIL

************************** F I M ****************************************

static FUNCTION outro_02_menu()

LOCAL tela:=SAVESCREEN(00,00,24,79),mopcao:=' '

WHILE .t.
        botao(01,01,06,40)
        SETCOLOR('N/W,W+/N,X')
        vn_menu3 := 0
        @  2,2 PROMPT "Letura X                              "
        @  3,2 PROMPT "Reduá∆o Z                             "
        @  4,2 PROMPT "Leitura da Mem¢ria Fiscal - Datas     "
        @  5,2 PROMPT "Leitura da Mem¢ria Fiscal - Intervalo "
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
                        prepara_string := inicio_protocolo + "06" + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                CASE vn_menu3 = 2
                        mensagem('Confirma a impressao da LEITURA "Z" [s/N]:')
                        mopcao := 'N'
                        @ 23,COL()+1 GET mopcao PICT '@!' VALID mopcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. mopcao = 'N'
                                RESTSCREEN(00,00,24,79,tela)
                                LOOP
                        ENDIF
                        prepara_string := inicio_protocolo + "05" + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                CASE vn_menu3 = 3
                        SET CENTURY OFF
                        dta_inicio := dta_fim := ctod("  /  /  ")
                        botao(14,10,17,50)
                        @ 15,11 SAY "Data de inicio " get dta_inicio
                        @ 16,11 SAY "Data de fim    " get dta_fim
                        READ
                        mensagem('Confirma a impressao da MEMORIA FISCAL [s/N]:')
                        mopcao := 'N'
                        @ 23,COL()+1 GET mopcao PICT '@!' VALID mopcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. mopcao = 'N'
                                RESTSCREEN(00,00,24,79,tela)
                                LOOP
                        ENDIF
                        dta_inicio := strtran(dtoc(dta_inicio),"/")
                        dta_fim    := strtran(dtoc(dta_fim),"/")

                        prepara_string := inicio_protocolo + "08|"+dta_inicio+"|"+dta_fim+"|I|"+fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                        SET CENTURY ON
                CASE vn_menu3 = 4
                        ini_inter := fim_inter := 0
                        botao(14,10,17,50)
                        @ 15,11 SAY "Reduá∆o Inicio " get ini_inter picture "9999"
                        @ 16,11 SAY "Reduá∆o fim    " get fim_inter picture "9999"
                        READ
                        mensagem('Confirma a impressao da MEMORIA FISCAL [s/N]:')
                        mopcao := 'N'
                        @ 23,COL()+1 GET mopcao PICT '@!' VALID mopcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. mopcao = 'N'
                                RESTSCREEN(00,00,24,79,tela)
                                LOOP
                        ENDIF

/*                prepara_string := inicio_protocolo + "08|"+chr(30)+chr(30)+"|"+;
                                strzero(ini_inter,4,0)+"|"+chr(30)+chr(30)+"|"+;
                                strzero(fim_inter,4,0)+"|I|"+fim_protocolo_driver
*/
                        prepara_string := inicio_protocolo + "08|00|"+;
                                strzero(ini_inter,4,0)+"|00|"+;
                                strzero(fim_inter,4,0)+"|I|"+fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)

        ENDCASE

ENDDO

RETURN NIL

************************** F I M ****************************************

static FUNCTION outro_03_menu()

LOCAL tela:=SAVESCREEN(00,00,24,79)

WHILE .t.
        botao(01,01,06,39)
        SETCOLOR('N/W,W+/N,X')
        vn_menu4 := 0
        @  2,2 PROMPT "Intervená∆o TÇcnica                  "
        @  3,2 PROMPT "Eprom Conectada                      "
        @  4,2 PROMPT "Truncamento                          "
        @  5,2 PROMPT "Vinculaá∆o ao ISS                    "
        MENU TO vn_menu4

        prepara_string := NIL
        tamanho_de_retorno := 1

        do CASE
                CASE vn_menu4 = 0
                        RESTSCREEN(00,00,24,79,tela)
                        exit
                CASE vn_menu4 = 1
                        prepara_string := inicio_protocolo + "35|20" + fim_protocolo_driver
                        string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                        string_devolvida := if(string_devolvida = "U","MODO NORMAL","MODO INTERVENÄ«O TêCNICA")
                        mostra_informacao("Estado da Impressora",string_devolvida)
                CASE vn_menu4 = 2
                        prepara_string := inicio_protocolo + "35|21" + fim_protocolo_driver
                        string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                        string_devolvida := if(string_devolvida = "U","EPROM CONECTADA","EPROM DESCONECTADA")
                        mostra_informacao("Estado da Impressora",string_devolvida)
                CASE vn_menu4 = 3
                        prepara_string := inicio_protocolo + "35|28" + fim_protocolo_driver
                        string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                        string_devolvida := if(string_devolvida = CHR(255),"ARREDONDAMENTO","TRUNCAMENTO")
                        mostra_informacao("Estado da Impressora",string_devolvida)

                        /* Vinculando ao ISS */

                CASE vn_menu4 = 4
                     tamanho_de_retorno := 06
                     prepara_string := inicio_protocolo + "35|23" + fim_protocolo_driver
                     string_dev := com_c_imp(prepara_string,tamanho_de_retorno)
                     prepara_string := NIL
                     tamanho_de_retorno := 06
                     prepara_string := vc_buffer + "35|27" + fim_protocolo_driver
                     string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                         do CASE
                            CASE string_devolvida = string_dev
                               set color TO g*/n
                               @ 10,02 SAY "Obs.: Esta opá∆o n∆o pode ser utilizada, pois j† houve movimentaá∆o no dia..."
                               set color to
                               @ 12,02 SAY "Pressione qualquer tecla para continuar..."
                               Wait""
                             exit
                         endCASE
                     qual_tot := 0
                     @ 07,05 SAY "Qual totalizador (01/16) deseja vincular ao ISS ?" get qual_tot pict "99"
                     READ
                     do CASE
                         CASE qual_tot = 01
                             tamanho_de_retorno := 0
                             prepara_string := inicio_protocolo + "38|1000000000000000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 02
                             prepara_string := inicio_protocolo + "38|0100000000000000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 03
                             prepara_string := inicio_protocolo + "38|0010000000000000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 04
                             prepara_string := inicio_protocolo + "38|0001000000000000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 05
                             prepara_string := inicio_protocolo + "38|0000100000000000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 06
                             prepara_string := inicio_protocolo + "38|0000010000000000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 07
                             prepara_string := inicio_protocolo + "38|0000001000000000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 08
                             prepara_string := inicio_protocolo + "38|0000000100000000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 09
                             prepara_string := inicio_protocolo + "38|0000000010000000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 10
                             prepara_string := inicio_protocolo + "38|0000000001000000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 11
                             prepara_string := inicio_protocolo + "38|0000000000100000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 12
                             prepara_string := inicio_protocolo + "38|0000000000010000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 13
                             prepara_string := inicio_protocolo + "38|0000000000001000" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 14
                             prepara_string := inicio_protocolo + "38|0000000000000100" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 15
                             prepara_string := inicio_protocolo + "38|0000000000000010" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                         CASE qual_tot = 16
                             prepara_string := inicio_protocolo + "38|0000000000000001" + fim_protocolo_driver
                             com_c_imp(prepara_string,tamanho_de_retorno)
                     endCASE
        ENDCASE
ENDDO
RETURN NIL

************************** F I M ****************************************

static FUNCTION outro_05_menu()

LOCAL tela:=SAVESCREEN(00,00,24,79)

WHILE .t.
        botao(01,01,05,39)
        SETCOLOR('N/W,W+/N,X')
        vn_menu6 := 0
        @  2,2 PROMPT "Abre Cupom N∆o Fiscal                "
        @  3,2 PROMPT "Imprime Mensagem                     "
        @  4,2 PROMPT "Fecha Cupom N∆o Fiscal               "
        MENU TO vn_menu6

        prepara_string := NIL
        tamanho_de_retorno := 0

        do CASE
                CASE vn_menu6 = 0
                        RESTSCREEN(00,00,24,79,tela)
                        exit
                CASE vn_menu6 = 1
                        prepara_string := inicio_protocolo + "20" + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                CASE vn_menu6 = 2
                        CLEAR
                        mensagem := space(48)
                        @ 0,0 SAY "Entre com a mensagem" get mensagem
                        READ

                        mensagem := alltrim(mensagem) + chr(10) // Ç obrigat¢rio usar

                        /* inicia negrito*/

                        prepara_string := inicio_protocolo + "20|\" + chr(27) + chr(69) + mensagem + "\" + chr(27) + chr(70) + mensagem + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)

                CASE vn_menu6 = 3

                        /* fecha negrito*/

                        prepara_string := inicio_protocolo + "21" + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
        ENDCASE
 ENDDO
RETURN NIL

************************** F I M ****************************************

static FUNCTION outro_04_menu()

LOCAL tela:=SAVESCREEN(00,00,24,79)

WHILE .t.
        botao(01,01,12,46,,' Menu Cupom Fiscal ')
        SETCOLOR('N/W,W+/N,X')
        vn_menu5 := 0
        @  2,2 PROMPT "Caixa                                       "
        @  3,2 PROMPT "Venda de Produtos                           "
*       @  3,2 PROMPT "Abre Cupom Fiscal                           "
        @  4,2 PROMPT "Cancela Cupom Fiscal - Sem Vendas Efetuadas "
        @  5,2 PROMPT "Cancela Cupom Fiscal - Com Vendas Efetuadas "
*       @  6,2 PROMPT "Vende Item / Gasolina                       "
*       @  7,2 PROMPT "Vende Item / Normal                         "
*       @  8,2 PROMPT "Vende Item / Configuravel                   "
        @  6,2 PROMPT "Cancela Ultimo Item Vendido                 "
        @  7,2 PROMPT "Cancela Item GenÇrico                       "
        @  8,2 PROMPT "Fecha Cupom Fiscal sem Formas de Pagamento  "
        @  9,2 PROMPT "Fecha Cupom Fiscal com Formas de Pagamento  "
        @ 10,2 PROMPT "Retorna o Subtotal                          "
        @ 11,2 PROMPT "N£mero do Cupom                             "

        MENU TO vn_menu5

        prepara_string := NIL
        tamanho_de_retorno := 0

        do CASE
                CASE vn_menu5 = 0
                        RESTSCREEN(00,00,24,79,tela)
                             exit
                CASE vn_menu5 = 1        // CAIXA
                        bemaped()
                CASE vn_menu5 = 2       // VENDA DE PRODUTO
                        sac23ecf()
*                       com_c_imp(prepara_string,tamanho_de_retorno)
*               CASE vn_menu5 = 2       // ABRE CUPOM FISCAL
*                       prepara_string := inicio_protocolo + "00" + fim_protocolo_driver
*                       com_c_imp(prepara_string,tamanho_de_retorno)
                CASE vn_menu5 = 3        // CANCELA CUPOM FISCAL SEM VENDA
                        cancup_s()
*                       prepara_string := inicio_protocolo + "10|0000|00000000000000|A" + fim_protocolo_driver
*                       com_c_imp(prepara_string,tamanho_de_retorno)
                CASE vn_menu5 = 4        // CANCELA CUPOM FISCAL COM VENDA
                        cancup_c()
*                       prepara_string := inicio_protocolo + "10|0000|00000000000000|A" + fim_protocolo_driver
*                       com_c_imp(prepara_string,tamanho_de_retorno)
*                       prepara_string := inicio_protocolo + "14" + fim_protocolo_driver
*                       com_c_imp(prepara_string,tamanho_de_retorno)
/*
                CASE vn_menu5 = 5        // VENDA DE GASOLINA
                        prepara_string := inicio_protocolo + "56|0000000000001|GASOLINA 01234567890123456789|FF|0001000|00000788|0000" + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                CASE vn_menu5 = 6        // VENDA NORMAL
                        prepara_string := inicio_protocolo + "09|0000000000002|Bala de Goma Xuxa            |FF|0001|00000005|0000" + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                CASE vn_menu5 = 7        // VENDA CONFIGURAVEL
                        declare venda_item[2][6]
                        venda_item[1][1] := space(13) ; venda_item[2][1] := "C¢digo"
                        venda_item[1][2] := space(29) ; venda_item[2][2] := "Descriá∆o"
                        venda_item[1][3] := space(2)  ; venda_item[2][3] := "Al°quota"
                        venda_item[1][4] := 0         ; venda_item[2][4] := "Quantidade"
                        venda_item[1][5] := 0         ; venda_item[2][5] := "Valor Unit†rio"
                        venda_item[1][6] := 0         ; venda_item[2][6] := "Desconto"

                        CLEAR

                        @ 0,0 SAY "Informe os dados para venda"
                        @ 2,0 SAY venda_item[2][1] + space(3) get venda_item[1][1]
                        @ 3,0 SAY venda_item[2][2]            get venda_item[1][2]
                        @ 4,0 SAY venda_item[2][3] + space(1) get venda_item[1][3] picture "@!"
                        READ

                        @ 6,0 SAY "Escolha as opá‰es a baixo - Use as setas direcionais <- ->, ENTER Seleciona"
                        @ 8,0 SAY "Quantidade - [Inteira] Fracionaria"
                        escolha_quantidade := escolha_desconto := .t.
                        fraciona_valor := .f.
                        setcursor(0)
                        do WHILE .t.
                           tecla := inkey(0)
                           do CASE
                              CASE tecla == 4
                                   @ 8,0 SAY "Quantidade -  Inteira [Fracionaria]"
                                   escolha_quantidade := .f.
                              CASE tecla == 19
                                   @ 8,0 SAY "Quantidade - [Inteira] Fracionaria "
                                   escolha_quantidade := .t.
                              CASE tecla == 13
                                   exit
                           endCASE
                        ENDDO
                        @ 9,0 SAY "Desconto - [Por Percentual] Por Valor"
                        do WHILE .t.
                           tecla := inkey(0)
                           do CASE
                              CASE tecla == 4
                                   @ 9,0 SAY "Desconto -  Por Percentual [Por Valor]"
                                   escolha_desconto := .f.
                              CASE tecla == 19
                                   @ 9,0 SAY "Desconto - [Por Percentual] Por Valor "
                                   escolha_desconto := .t.
                              CASE tecla == 13
                                   exit
                           endCASE
                        ENDDO
                        @ 10,0 SAY "Fraciona valor -  Sim [N∆o]"
                        do WHILE .t.
                           tecla := inkey(0)
                           do CASE
                              CASE tecla == 4
                                   @ 10,0 SAY "Fraciona valor -  Sim [N∆o]"
                                   fraciona_valor := .f.
                              CASE tecla == 19
                                   @ 10,0 SAY "Fraciona valor - [Sim] N∆o "
                                   fraciona_valor := .t.
                              CASE tecla == 13
                                   exit
                           endCASE
                        ENDDO
                        setcursor(1)
                        @ 11,0 SAY venda_item[2][4] + " " + if(escolha_quantidade,"Inteira","Fracionaria")    get venda_item[1][4] picture if(escolha_quantidade,"9999","9999,999")
                        @ 12,0 SAY venda_item[2][5] + " " get venda_item[1][5] picture if(fraciona_valor,"99999,999","999999,99")
                        @ 13,0 SAY venda_item[2][6] + " " + if(escolha_desconto,"por Percentual","por Valor") get venda_item[1][6] picture if(escolha_desconto,"99,99","999999,99")
                        READ

                        IF isdigit(substr(venda_item[1][1],1,1))
                           venda_item[1][1] := strzero(val(venda_item[1][1]),13,0)
                        ENDIF

                        venda_item[1][4] := if(escolha_quantidade,strzero(venda_item[1][4],4,0),strzero(venda_item[1][4],7,0))

                        venda_item[1][5] := strzero(venda_item[1][5],8,0)

                        IF fraciona_valor
                           prepara_string := inicio_protocolo + "56|" + venda_item[1][1] +"|"+venda_item[1][2] +"|"+venda_item[1][3]+"|"+venda_item[1][4]+"|"+venda_item[1][5]+"|0000" + fim_protocolo_driver
                           com_c_imp(prepara_string,tamanho_de_retorno)
                        else
                           prepara_string := inicio_protocolo + "09|" + venda_item[1][1] +"|"+venda_item[1][2] +"|"+venda_item[1][3]+"|"+venda_item[1][4]+"|"+venda_item[1][5]+"|0000" + fim_protocolo_driver
                           com_c_imp(prepara_string,tamanho_de_retorno)
                        ENDIF
*/
                CASE vn_menu5 = 5        // CANCELA ULTIMO ITEM VENDIDO
                        canultitem()
*                        prepara_string := inicio_protocolo + "13" + fim_protocolo_driver
*                        com_c_imp(prepara_string,tamanho_de_retorno)
                CASE vn_menu5 = 6        // CANCELA ITEM GENERICO
                        can_item()
/*
                        item_a_ser_cancelado := space(4)
                        @ 20,0 SAY "N£mero do item a ser cancelado" get item_a_ser_cancelado picture "9999"
                        READ

                        item_a_ser_cancelado := strzero(val(item_a_ser_cancelado))

                        prepara_string := inicio_protocolo + "31|"+ item_a_ser_cancelado + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
*/
                CASE vn_menu5 = 7        // FECHA CUPOM FISCAL SEM FORMA DE PAGAMENTO

                        prepara_string := inicio_protocolo + "10|0000|00000000001000|A|Muito Obrigado, Volte sempre" + chr(10) +  fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)

                CASE vn_menu5 = 8       // INICIA FECHAMENTO DE CUPOM COM FORMAS DE PAGAMENTO */

                        prepara_string := inicio_protocolo + "32|A|0000" +  fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                        /* Efetua Forma de Pagamento */       /* 1234567890123456789012        12345678901234*/
                        prepara_string := inicio_protocolo + "33|Dinheiro              |00000000000015" +  fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                        prepara_string := inicio_protocolo + "34|Volte sempre" + chr(13) + chr(10) + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                CASE vn_menu5 = 9        // RETORNA SUBTOTAL DO CUPOM
                        tamanho_de_retorno := 14
                        prepara_string := inicio_protocolo + "29" + fim_protocolo_driver
                        string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                        mostra_informacao("Subtotal" ,string_devolvida)
                CASE vn_menu5 = 10       // NUMERO DO CUPOM
                        tamanho_de_retorno := 6
                        prepara_string := inicio_protocolo + "30" + fim_protocolo_driver
                        string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                        mostra_informacao("N£mero do Cupom Aberto" ,string_devolvida)

        ENDCASE

ENDDO
RETURN NIL
************************** F I M ****************************************

static FUNCTION mostra_informacao( mensagem, conteudo )

atencao(mensagem + " ˛" + conteudo + "˛")
RETURN NIL
************************** F I M ****************************************

static FUNCTION outro_06_menu()

LOCAL tela:=SAVESCREEN(00,00,24,79),maliquota:=' '

WHILE .t.
        botao(01,01,04,46)
        SETCOLOR('N/W,W+/N,X')
        vn_menu7 := 0
        @  2,2 PROMPT "Retorno de Al°quotas                        "
        @  3,2 PROMPT "Adiá∆o de Al°quota Tribut†ria               "
        MENU TO vn_menu7

        prepara_string := NIL
        tamanho_de_retorno := 0

        do CASE
                CASE vn_menu7 = 0
                        RESTSCREEN(00,00,24,79,tela)
                        exit
                CASE vn_menu7 = 1
                        tamanho_de_retorno := 66
                        prepara_string := inicio_protocolo + "26" + fim_protocolo_driver
                        string_devolvida := com_c_imp(prepara_string,tamanho_de_retorno)
                        declare aliquotas[2][16]
                        FOR contador2 := 1 TO 16
                                maliquota := maliquota +' | '+"T"+strzero(contador2,2,0) + " - " + stuff(substr(string_devolvida,((contador2*4)-3)+2,4),3,0,",")
                        NEXT
                        atencao(maliquota,0)
*                       wait "Tecle algo para continuar"

                CASE vn_menu7 = 2
                        aliquota_a_ser_incluida := space(4)
                        @ 20,00 SAY "Informe o valor da al°quota a ser cadastrada" get aliquota_a_ser_incluida picture "9999"
                        READ
                        mensagem('Confirma a inclussao da ALIQUOTA [s/N]:')
                        mopcao := 'N'
                        @ 23,COL()+1 GET mopcao PICT '@!' VALID mopcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. mopcao = 'N'
                                RESTSCREEN(00,00,24,79,tela)
                                LOOP
                        ENDIF
                        prepara_string := inicio_protocolo + "07|"+ aliquota_a_ser_incluida + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
        ENDCASE

ENDDO
RETURN NIL
************************** F I M ****************************************

static FUNCTION outro_07_menu()

LOCAL tela:=SAVESCREEN(00,00,24,79)

WHILE .t.
        botao(01,01,04,40)
        SETCOLOR('N/W,W+/N,X')
        vn_menu7 := 0
        @  2,2 PROMPT "Sangria                               "
        @  3,2 PROMPT "Suprimento                            "
*        @  4,2 PROMPT "Recebimento em Totalizador Parcial    "
        MENU TO vn_menu7

        prepara_string := NIL

        do CASE
                CASE vn_menu7 = 0
                        RESTSCREEN(00,00,24,79,tela)
                        exit
                CASE vn_menu7 = 1
                        valor_temp := 0
                        @ 23,0 SAY "Informe o valor da sangria" get valor_temp picture "9999999,99"
                        READ
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                        prepara_string := inicio_protocolo + "25|SA|"+strzero(valor_temp,14,0) + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                CASE vn_menu7 = 2
                        valor_temp := 0
                        @ 23,0 SAY "Informe o valor da suprimento" get valor_temp picture "9999999,99"
                        READ
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                        prepara_string := inicio_protocolo + "25|SU|"+strzero(valor_temp,14,0) + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
/*
                CASE vn_menu7 = 3
                        qual_totalizador := valor_temp := 0
                        @ 23,0 SAY "N£mero do Totalizador, de 1 a 9" get qual_totalizador picture "9" valid qual_totalizador >= 1 .and. qual_totalizador <= 9
                        @ 23,COL()+2 SAY "Informe o valor do recebimento " get valor_temp picture "9999999,99"
                        READ
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                        prepara_string := inicio_protocolo + "25|#"+str(qual_totalizador,1,0)+"|"+strzero(valor_temp,14,0) + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
*/
        ENDCASE

ENDDO
RETURN NIL
************************** F I M ****************************
* CANCELA CUPOM SEM VENDAS
**************************
FUNCTION cancup_s

prepara_string := inicio_protocolo + "10|0000|00000000000000|A" + fim_protocolo_driver
com_c_imp(prepara_string,tamanho_de_retorno)
RETUR NIL
************************** F I M ****************************
* CANCELA CUPOM COM VENDAS
**************************
FUNCTION cancup_c

prepara_string := inicio_protocolo + "10|0000|00000000000000|A" + fim_protocolo_driver
com_c_imp(prepara_string,tamanho_de_retorno)
prepara_string := inicio_protocolo + "14" + fim_protocolo_driver
com_c_imp(prepara_string,tamanho_de_retorno)
RETUR NIL
************************** F I M ****************************
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

prepara_string := inicio_protocolo + "13" + fim_protocolo_driver
com_c_imp(prepara_string,tamanho_de_retorno)
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
item_a_ser_cancelado := strzero(val(item_a_ser_cancelado))
prepara_string := inicio_protocolo + "31|"+ item_a_ser_cancelado + fim_protocolo_driver
com_c_imp(prepara_string,tamanho_de_retorno)
RESTSCREEN(00,00,24,79,telai)
RETUR NIL

