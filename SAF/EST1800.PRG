*------------------------------------------------------------------------
* Nome do programa:     EST1800.PRG
* Data:                 26/04/95
* Autor:                Softer -> ADOLFO
* Funcao:               REEMISSAO DE PEDIDOS
*                       Controle de Estoque Comercial
* Comentario:          
*------------------------------------------------------------------------
#include "FILEIO.CH"

SELECT 0
FABREA("MOVTOSH",.F.,gDiret)
cIndice1 = gDiret + "MOVHX1" 
cIndice2 = gDiret + "MOVHX2" 
cIndice3 = gDiret + "MOVHX3" 
SET INDEX TO &cIndice1,&cIndice2,&cIndice3

SELECT 0
FABREA("MOVTOS",.F.,gDiret)
cIndice1 = gDiret + "MOVX1" 
cIndice2 = gDiret + "MOVX2" 
SET INDEX TO &cIndice1,&cIndice2
SET ORDER TO 2

SELECT 0
FABREA("LOCALEST",.F.,gDiret)
cIndice1 = gDiret + "LOCAX1" 
cIndice2 = gDiret + "LOCAX2" 
SET INDEX TO &cIndice1,&cIndice2
SET ORDER TO 1

SELECT 0
FABREA("CLIENTES",.F.,gDiret)
cIndice1 = gDiret + "CLICODX1" 
cIndice2 = gDiret + "CLIFANX2" 
cIndice3 = gDiret + "CLICPFX3" 
SET INDEX TO &cIndice1,&cIndice2,&cIndice3
SET ORDER TO 2
GO TOP

SELECT 0
FABREA("TB_PAGTO",.F.)
SET INDEX TO TBPGX1,TBPGX2

SELECT 0
FABREA("ITENS",.F.,gDiret)
cIndice1 = gDiret + "ITENX1" 
cIndice2 = gDiret + "ITENX2" 
cIndice3 = gDiret + "ITENX3" 
cIndice4 = gDiret + "ITENX4" 
SET INDEX TO &cIndice1,&cIndice2,&cIndice3,&cIndice4
SET ORDER TO 2
GO TOP

SELECT 0
FABREA("FUNCIONA",.F.)
SET INDEX TO FUNCODX1,FUNNOMX2
SET ORDER TO 2
GO TOP

SELECT 0
FABREA("TIPODOC",.F.)
SET INDEX TO TIPCODX1

*** TELA
RESTORE SCREEN FROM tLimpa
@ 01,01 SAY PROCNAME() 
@ 01,29 SAY "*** Emite Cupom fis.**"

nNumerodoc = 0
DO WHILE .T.

   MOLD(02,09,05,70)
   EST1610F(04,11,STRZERO(nNumerodoc,6),.NOT. .T.)
   @ 03,11 SAY "Pedido:" GET nNumeroDoc PICT "999999" VALID EST1610F(04,11,STRZERO(nNumerodoc,6),.NOT. .T.)

   MENSAGEM("Digite numero do Pedido.  <ESC>Desiste")
   READ
   LIMPA()

*** CONVERTE PARA CARACTER
   cNumeroDoc = STRZERO(nNumeroDoc,6)

*** ESC?
   IF LASTKEY() = 27
      EXIT
   ENDIF

*** VERIFICA SE EXISTE ESTE PEDIDO
   lEncomenda = .F.

   SELECT MOVTOSH
   SET ORDER TO 2
   SEEK "20" + cNumeroDoc SOFTSEEK
   IF DATACONH <> CTOD("  /  /  ")
      lEncomenda = .T.
   ENDIF

*** CADASTRADO?
   IF NUMERODOC <> cNumerodoc
      SELECT MOVTOSH
      SET ORDER TO 2
      SEEK "29" + cNumeroDoc SOFTSEEK

      IF NUMERODOC = cNumerodoc
         TECLA("Pedido Cancelado.  [ENTER]")
         LIMPA()
         LOOP
      ELSE
         TECLA("Pedido nao cadastrado.  [ENTER]")
         LIMPA()
         LOOP
      ENDIF

   ENDIF

   SELECT CLIENTES
   SET ORDER TO 1
   SEEK MOVTOSH->CLIE_FORNE
   SET ORDER TO 2
   SELECT MOVTOSH

*** MOSTRA OUTROS DADOS
   @ 03,30 SAY "Cliente " + MOVTOSH->CLIE_FORNE+" Data    " + DTOC(MOVTOSH->DTEMISSAO)
   cCodcli = MOVTOSH->CLIE_FORNE
   @ 04,30 SAY CLIENTES->FANTASIA

*** MOSTRA PEDIDO NA TELA

   aCampos:={}
   aTit   :={}
   aMask  :={}
   cArqMostra  = ""

  IF EST1800A() = .F.
     LOOP
  ENDIF 

*** CHANTAI
//    IF EST1800B() = .F.
//       LOOP
//    ENDIF
   
      SAVE SCREEN TO tREC4600
      SELECT ARQMOSTRA
      GO TOP
      @ 06,00 CLEAR TO 22,79
      MENSAGEM("Confirma impressao? [ENTER]Sim   <ESC>Nao")
      DBEDIT(06,00,22,79,aCampos,"",aMask,aTit)
      USE
      FERASE(cArqMostra)
      LIMPA()
      RESTORE SCREEN FROM tREC4600

      IF LASTKEY() = 13
         
         lImpfisc = .F.

         DO WHILE lImpfisc = .F.

            fSerial := FOPEN(gComfisc,FO_READWRITE+FO_COMPAT)

** TESTO SE A IMPRESSORA ESTA' FUNCIONANDO

            IF FERROR() <> 0
               FOR I = 1 TO 3
                  TONE(600,3)
               NEXT
               aOptions := {"Tenta Novamente","Desiste"}
               nChoice := ALERT("Problemas de Comunicacao (Impressora Fiscal)",aOptions)   
               DO CASE
               CASE nChoice == 1               
                  lImpfisc = .F.
                  LOOP
               CASE nChoice == 2                
                  lImpfisc = .F.
                  EXIT
               ENDCASE
            ELSE
               lImpfisc = .T.
               EXIT
            ENDIF
         ENDDO
         
         IF lImpfisc = .F.
            RESTORE SCREEN FROM tREC4600
            LIMPA()
            TECLA("Cupom N„o Impresso")
            LIMPA()
            LOOP   
         ENDIF

*** PEGO NUMERACAO DO PROXIMO CUPOM FISCAL PARA GRAVAR NO HEADPED

         cCupomfisc = SPACE(06)
         BUFFER = CHR(27)+CHR(251)+"30"+CHR(27)

** ERRO DA IMPRESSORA

         IF FEROIMP(BUFFER) = .F.
            FCLOSE(fSerial)
            RESTORE SCREEN FROM tREC4600
            LIMPA()
            TECLA("Cupom N„o Impresso")
            LIMPA()
            LOOP   
         ENDIF

         BUFRX = SPACE(01)
         BUFTOT = "" 

         FOR I = 1 TO 6
            FREAD(fSerial,@BUFRX,1)
            BUFTOT = BUFTOT + BUFRX   
         NEXT               

         cCupomfisc = STRZERO(VAL(BUFTOT)+1,6)

         TONE(600,3)
         LIMPA()
         MENSAGEM("SEQUENCIAL -> "+cCupomfisc)

*** ABERTURA DO CUPOM FISCAL         

         BUFFER = CHR(27)+CHR(251)+"00|"+CHR(27)
         IF FEROIMP(BUFFER) = .F.
            FCLOSE(fSerial)
            RESTORE SCREEN FROM tREC4600
            LIMPA()
            TECLA("Cupom N„o Impresso")
            LIMPA()
            LOOP   
         ENDIF

** AGORA EU IMPRIMO OS ITENS DO PEDIDO

         SELECT MOVTOS
         SET ORDER TO 2
         SEEK "20"+MOVTOSH->CLIE_FORNE+MOVTOSH->NUMERODOC SOFTSEEK

         DO WHILE MOVTOSH->CLIE_FORNE = MOVTOS->CLIE_FORNE .AND. MOVTOSH->NUMERODOC=MOVTOS->NUMERODOC .AND. .NOT. EOF()

            IF CODMOV <> "20"   
               SKIP
               LOOP
            ENDIF

            cItem = MOVTOS->ITEM
            nQtditem = QUANTIDADE
            nValorit = VALOR

            IF nQtditem > 0
               cCodigo = SPACE(13)
               cDescri = SPACE(29)
               cAliquo = SPACE(02)
               cQuanti = SPACE(07)
               cValuni = SPACE(08)
               cDescpe = SPACE(04)  
                
               cCodigo = STRZERO(VAL(cItem),13)
               SELECT ITENS
               SET ORDER TO 1
               SEEK cItem
               nValorit = ITENS->PRCVENDA
               SELECT MOVTOS
               cDescri = SUBSTR(ITENS->DESCRICAO,1,29)
               cAliquo = STRZERO(ITENS->ALIQICMS,2)                  
               nInt = INT(nQtditem)
               nRest = VAL(RIGHT(STR(nQtditem,9,3),3))
               cQuanti = STRZERO(nInt,4)+STRZERO(nRest,3)
               nInt = INT(nValorit)
               nRest = VAL(RIGHT(STR(nValorit,9,2),2))                  
               cValuni = STRZERO(nInt,6)+STRZERO(nRest,2)
               cDescpe = "0000"
                     
               BUFFER = CHR(27)+CHR(251)+"09|"+cCodigo+"|"+cDescri+"|"+cAliquo+"|"+cQuanti+"|"+cValuni+"|"+cDescpe+"|"+CHR(27)
               IF FEROIMP(BUFFER) = .F.
                  FCLOSE(fSerial)
                  RESTORE SCREEN FROM tREC4600
                  LIMPA()
                  TECLA("Cupom N„o Impresso")
                  LIMPA()
                  EXIT
               ENDIF

            ENDIF

            SELECT MOVTOS
            SKIP
         
         ENDDO  

*** IMPRIMO AGORA O FECHAMENTO DO CUPOM

         SELECT MOVTOSH
   
         cMens = SPACE(04)
         nInt = INT(VALORTOTAL)
         nRest = VAL(RIGHT(STR(VALORTOTAL,9,2),2))
         cValtotpag = STRZERO(nInt,12)+STRZERO(nRest,2)
         cAcreoudesc = "a"
         cValacreoudesc = "00000000000000"
         cMensagem = "                       "

*** VERIFICO SE HOUVE DESCONTO OU ACRESCIMO         

            nAcreoudesc = DESCONTO

            IF nAcreoudesc <> 0
               IF nAcreoudesc > 0
                  cMens = "DESC"               
                  cAcreoudesc = "d"
               ELSE
                  nAcreoudesc = nAcreoudesc * (-1)
                  cMens = "SVC."                              
                  cAcreoudesc = "a"                  
               ENDIF
               nInt = INT(nAcreoudesc)
               nRest = VAL(RIGHT(STR(nAcreoudesc,9,2),2))
               cValacreoudesc = STRZERO(nInt,12)+STRZERO(nRest,2)
            ENDIF

         cMens = SPACE(04)
         nInt = INT(VALORTOTAL)
         nRest = VAL(RIGHT(STR(VALORTOTAL,9,2),2))
         cValtotpag = STRZERO(nInt,12)+STRZERO(nRest,2)
         cMensagem = "                       "
   
         BUFFER = CHR(27)+CHR(251)+"10|"+cMens+"|"+cValtotpag+"|"+cAcreoudesc+"|"+cValacreoudesc+"|"+cMensagem+CHR(13)+"|"+CHR(27)
         IF FEROIMP(BUFFER) = .F.
            FCLOSE(fSerial)
            RESTORE SCREEN FROM tREC4600
            LIMPA()
            TECLA("Cupom N„o Impresso")
            LIMPA()
            LOOP   
         ENDIF

** ENCERRO ENVIO PARA A IMPRESSORA

         FCLOSE(fSerial)

** AGORA GRAVO QUE FOI EMITIDO CUPOM FISCAL

         SELECT MOVTOSH
         FCHANGE()
         REPLACE NUMSERIE WITH cCupomfisc
         REPLACE ADICIONAL WITH "I"
         FUNLOCK()
         
   ENDIF


ENDDO

SELECT LOCALEST
USE
SELECT FUNCIONA
USE
SELECT CLIENTES
USE
SELECT ITENS
USE
SELECT TB_PAGTO 
USE 
SELECT TIPODOC
USE
SELECT MOVTOS
USE
SELECT MOVTOSH
USE

RETURN

*****************************
FUNCTION EST1800A()
*****************************
** MONTA PEDIDO NA TELA

*** POSICIONA EM MOVTOS
   SELECT MOVTOS
   SET ORDER TO 2
   SEEK "20"+MOVTOSH->CLIE_FORNE+MOVTOSH->NUMERODOC SOFTSEEK
   
   IF .NOT. EOF()

      cHora = TIME() 
      cHora = SUBSTR(cHora,4,2) + SUBSTR(cHora,7,2)
      cArqMostra = gDiret + RIGHT(DTOS(DATE()),4)+cHora + ".DBF"
   
      aEstrutura:={}
      AADD(aEstrutura,{"MOSTRA","C",79,0})
      DBCREATE(cArqMostra,aEstrutura)

      aCampos:={}
      aTit   :={}
      aMask  :={}

      AADD(aCampos,"MOSTRA")
      AADD(aTit,"Consulta em Tela")
      AADD(aMask,.T.)

      SELECT 0
      USE &cArqMostra ALIAS ARQMOSTRA EXCLUSIVE
            
      SELECT CLIENTES
      SET ORDER TO 1
      SEEK MOVTOSH->CLIE_FORNE
      SET ORDER TO 2
      
      SELECT LOCALEST
      SET ORDER TO 1
      SEEK MOVTOSH->LOCALESTOQ

      SELECT TB_PAGTO
      SET ORDER TO 1
      SEEK MOVTOSH->FORMAPGTO
      SET ORDER TO 2

      SELECT ARQMOSTRA
      APPEND BLANK
      REPLACE MOSTRA WITH "+-----------------------------------------------------------------------------+"
      APPEND BLANK
      REPLACE MOSTRA WITH "|Forma de pagamento "+TB_PAGTO->TBDESCRI+"Emissao: "+DTOC(MOVTOSH->DTEMISSAO)+" Vencimento: "+DTOC(MOVTOSH->DATA1)+"|"
      APPEND BLANK
      REPLACE MOSTRA WITH "|-------+---+----+--------------------------------+---------+--------+--------|"
      APPEND BLANK
      REPLACE MOSTRA WITH "|  Cod  |Qt.| Un | Discriminacao                  |Embalagem| Vl. Un.|  Total |"
      APPEND BLANK
      REPLACE MOSTRA WITH "|-------+---+----+--------------------------------+---------+--------+--------|"

      SELECT MOVTOS
      DO WHILE MOVTOSH->CLIE_FORNE = MOVTOS->CLIE_FORNE .AND. MOVTOSH->NUMERODOC=MOVTOS->NUMERODOC .AND. .NOT. EOF()

         IF CODMOV <> "20"   
            SKIP
            LOOP
         ENDIF

         IF QUANTIDADE > 0            
            SELECT ITENS
            SET ORDER TO 1
            SEEK MOVTOS->ITEM
            SELECT ARQMOSTRA
            APPEND BLANK
            REPLACE MOSTRA WITH "| "+MOVTOS->ITEM+" |"+STRZERO(MOVTOS->QUANTIDADE,3)+"| "+ITENS->UNIDADE+" | "+LEFT(ITENS->DESCRICAO,30)+" |"+LEFT(ITENS->REFERENCIA,9)+"| "+TRANSFORM(MOVTOS->VALOR,"@E 999.99")+" |"+TRANSFORM(MOVTOS->VALOR*MOVTOS->QUANTIDADE,"@E 9,999.99")+"|"
            SELECT MOVTOS   
         ENDIF

*** PROXIMO
         SELECT MOVTOS
         SKIP
               
      ENDDO

      IF MOVTOSH->VALORTOTAL > 0
         SELECT ARQMOSTRA
         APPEND BLANK
         REPLACE MOSTRA WITH "|-------+---+----+--------------------------------+--+------+--------+--------|"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    |        T O T A L       |"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    +------------------------|"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    |    "+TRANSFORM(MOVTOSH->VALORTOTAL+MOVTOSH->DESCONTO,"@E 999,999.99")+"          |"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    +------------------------|"

*** HOUVE DESCONTO?
         IF MOVTOSH->DESCONTO > 0
            APPEND BLANK
            REPLACE MOSTRA WITH "|                                                    |     D E S C O N T O    |"
            APPEND BLANK
            REPLACE MOSTRA WITH "|                                                    +------------------------|"
            APPEND BLANK
            REPLACE MOSTRA WITH "|                                                    |    "+TRANSFORM(MOVTOSH->DESCONTO,"@E 999,999.99")+"          |"
            APPEND BLANK
            REPLACE MOSTRA WITH "|                                                    +------------------------|"
         ENDIF

         APPEND BLANK        
         REPLACE MOSTRA WITH "|                                                    |    A    P A G A R      |"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    +------------------------|"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    |    "+TRANSFORM(MOVTOSH->VALORTOTAL,"@E 999,999.99")+"          |"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    +------------------------|"
         SELECT FUNCIONA
         SET ORDER TO 1
         SEEK MOVTOSH->VENDEDOR
         SET ORDER TO 2
         SELECT ARQMOSTRA
         APPEND BLANK   
         REPLACE MOSTRA WITH "| Vendedor :  "+FUNCIONA->NOMEFUNC+"Comprador: "+CLIENTES->FANTASIA+"        |"
         APPEND BLANK
         REPLACE MOSTRA WITH "+-----------------------------------------------------------------------------+"
      ENDIF
   ELSE
      LIMPA()
      TECLA("Nenhum Item Para Este Pedido.")
      LIMPA()
      RETURN .F.
   ENDIF

RETURN .T.

*****************************
FUNCTION EST1800B()
*****************************
** MONTA PEDIDO NA TELA (CHANTAI)

*** POSICIONA EM MOVTOS
   SELECT MOVTOS
   SET ORDER TO 2
   SEEK "20"+MOVTOSH->CLIE_FORNE+MOVTOSH->NUMERODOC SOFTSEEK
   
   IF .NOT. EOF()

      cHora = TIME() 
      cHora = SUBSTR(cHora,4,2) + SUBSTR(cHora,7,2)
      cArqMostra = gDiret + RIGHT(DTOS(DATE()),4)+cHora + ".DBF"
   
      aEstrutura:={}
      AADD(aEstrutura,{"MOSTRA","C",79,0})
      DBCREATE(cArqMostra,aEstrutura)

      aCampos:={}
      aTit   :={}
      aMask  :={}

      AADD(aCampos,"MOSTRA")
      AADD(aTit,"Consulta em Tela")
      AADD(aMask,.T.)

      SELECT 0
      USE &cArqMostra ALIAS ARQMOSTRA EXCLUSIVE
            
      SELECT CLIENTES
      SET ORDER TO 1
      SEEK MOVTOSH->CLIE_FORNE
      SET ORDER TO 2
      
      SELECT LOCALEST
      SET ORDER TO 1
      SEEK MOVTOSH->LOCALESTOQ

      SELECT TB_PAGTO
      SET ORDER TO 1
      SEEK MOVTOSH->FORMAPGTO
      SET ORDER TO 2

      SELECT ARQMOSTRA
      APPEND BLANK
      REPLACE MOSTRA WITH "+-----------------------------------------------------------------------------+"
      APPEND BLANK
      REPLACE MOSTRA WITH "|Forma de pagamento "+TB_PAGTO->TBDESCRI+"Emissao: "+DTOC(MOVTOSH->DTEMISSAO)+" Vencimento: "+DTOC(MOVTOSH->DATA1)+"|"
      APPEND BLANK
      REPLACE MOSTRA WITH "|-----+----------+--+--------------------------+---+------+---+------+--------|"
      APPEND BLANK
      REPLACE MOSTRA WITH "| Item| Codigo   |Un| Discriminacao            |Emb|Refer.|Qtd|P.Unit| Total  |"
      APPEND BLANK
      REPLACE MOSTRA WITH "|-----+----------+--+--------------------------+---+------+---+------+--------|"

      SELECT MOVTOS
      DO WHILE MOVTOSH->CLIE_FORNE = MOVTOS->CLIE_FORNE .AND. MOVTOSH->NUMERODOC=MOVTOS->NUMERODOC .AND. .NOT. EOF()

         IF CODMOV <> "20"   
            SKIP
            LOOP
         ENDIF

         IF QUANTIDADE > 0            
            SELECT ITENS
            SET ORDER TO 1
            SEEK MOVTOS->ITEM
            SELECT ARQMOSTRA
            APPEND BLANK
            REPLACE MOSTRA WITH "|"+MOVTOS->ITEM+"|"+LEFT(ITENS->REFERENCIA,10)+"|"+ITENS->UNIDADE+"| "+LEFT(ITENS->DESCRICAO,25)+"|"+STR(ITENS->QTDUNID,3)+"|"+RIGHT(FCONVREF(MOVTOS->VALOR),6)+"|"+TRANSFORM(MOVTOS->QUANTIDADE,"999.99")+"|"+TRANSFORM(MOVTOS->REFERENCIA,"@E 999.99")+"|"+TRANSFORM(MOVTOS->CUSTTOT,"@E 9,999.99")+"|"
            SELECT MOVTOS   
         ENDIF

*** PROXIMO
         SELECT MOVTOS
         SKIP
               
      ENDDO

      IF MOVTOSH->VALORTOTAL > 0
         SELECT ARQMOSTRA
         APPEND BLANK
         REPLACE MOSTRA WITH "|-----+----------+--+--------------------------+---+-+----+---+------+--------|"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    |        T O T A L       |"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    +------------------------|"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    |    "+TRANSFORM(MOVTOSH->CUSTTOT,"@E 999,999.99")+"          |"
         APPEND BLANK
         REPLACE MOSTRA WITH "|                                                    +------------------------|"
         SELECT FUNCIONA
         SET ORDER TO 1
         SEEK MOVTOSH->VENDEDOR
         SET ORDER TO 2
         SELECT ARQMOSTRA
         APPEND BLANK   
         REPLACE MOSTRA WITH "| Vendedor :  "+FUNCIONA->NOMEFUNC+"Comprador: "+CLIENTES->FANTASIA+"        |"
         APPEND BLANK
         REPLACE MOSTRA WITH "+-----------------------------------------------------------------------------+"
         APPEND BLANK
         REPLACE MOSTRA WITH FCONVREF(MOVTOSH->VALORTOTAL)

      ENDIF
   ELSE
      LIMPA()
      TECLA("Nenhum Item Para Este Pedido.")
      LIMPA()
      RETURN .F.
   ENDIF

RETURN .T.


