*-----------------------------------------------------------------------
* Nome do programa:     INTPROC.PRG
* Data:                 27/09/95
* Autor:                Softer
* Funcao:               Rotinas do Integrador
*                       
* Comentario:          
*-----------------------------------------------------------------------
#include "FILEIO.CH"
FUNCTION DECRIPTO
*
*                                   Descriptografa Senha
PARAMETERS pSenha

*---- CRIPTOGRAFIA
LOCAL cDecSenha := ""
FOR nVar = 1 TO LEN(TRIM(pSenha))
   cVar = SUBSTR(pSenha,nVar,1)
   cDecSenha = cDecSenha + CHR(ASC(cVar)-24)
NEXT
cDecSenha = cDecSenha + SPACE(LEN(pSenha)-LEN(cDecSenha))
RETURN cDecSenha
*----------------------------------------------------------------------
FUNCTION DIGISENHA
LOCAL lRetorno
PARAMETERS cSenha 
*
*                                Digitacao das senhas
*
IF LASTKEY() = 5
   RETURN .T.
ENDIF

oGet := GETLIST
GETLIST := {}

SAVE SCREEN TO tSenha
STORE SPACE(08) TO cSenha1,cSenha2

LIMPA()

MENSAGEM("Digite a Senha")
DO PASSWORD WITH cSenha1, 8, 8, 15, 27 

lEsc = .F.
IF LASTKEY() <> 27

   MENSAGEM("Re-digite a senha")
   DO PASSWORD WITH cSenha2, 8, 8, 24, 27 

   IF LASTKEY() <> 27

      IF cSenha1 <> cSenha2
         TECLA("Senhas digitadas sao diferentes-Senha nao aceita." + ;
            "Tecle [ENTER]")
         LIMPA()
         lRetorno = .F.
      ENDIF

   ELSE
      lEsc = .T.
   ENDIF
   cSenhafunc = CRIPTO(cSenha1)
   lRetorno = .T.
ELSE
   lEsc = .T.
ENDIF

IF lEsc
   TECLA("[ESC]Detectado-Nova Senha nao aceita." + ;
      "Tecle [ENTER]")
   LIMPA()
   lRetorno = .F.
ENDIF

RESTORE SCREEN FROM tSenha

GETLIST := oGet

RETURN lRetorno
*--------------------------------------------------------------------
FUNCTION CRIPTO
*
*                                      Criptografa Senha
PARAMETERS pSenha

*---- CRIPTOGRAFIA
LOCAL cSenha   := ""
FOR nVar = 1 TO LEN(TRIM(pSenha))
   cVar = SUBSTR(pSenha,nVar,1)
   cSenha = cSenha + CHR(ASC(cVar)+24)
NEXT
RETURN cSenha
*****************************
FUNCTION COMANDO
*                          Display mensagem centralizado na linha 24 tela
*==========================
PRIVATE cMensagem,nPos
PARAMETERS cMensagem,nLin

SET KEY 287 TO IND_SOFTER // ALT+S PARA TELA SOFTER

IF nLin = NIL
   nLin = 24
ENDIF

cMensagem = TRIM(cMensagem)
nPos = CENTER(cMensagem,80)
SETCOLOR(gCormenu)
@ nLin,IIF(nPos<15,15,nPos) SAY cMensagem
RETURN .T.
*---------------------------------------------fim COMANDO----------------
FUNCTION F_CGCCPF(nLin,nCol,cCgccpf)
*
*                             CALCULO CGC/CPF AUTOMATICO
*

DO CASE
   CASE LEN(ALLTRIM(cCgccpf)) = 14
      PRIVATE D[12],I,DF1,DF2,DF3
      FOR I=1 TO 12
         D[I]=VAL(SUBSTR(cCgccpf,I,1))
      NEXT
*---- TESTE DO PRIMEIRO DIGITO (UNIDADE)
      DF1=5*D[1]+4*D[2]+3*D[3]+2*D[4]+9*D[5]+8*D[6]+7*D[7]+6*D[8];
          +5*D[9]+4*D[10]+3*D[11]+2*D[12]
      DF2=DF1/11
      DF3=INT(DF2)*11
      RESTO1=DF1-DF3
      IF resto1=0 .OR. resto1=1
         pridig=0
      ELSE
         pridig=11-resto1
      ENDIF
*---- TESTE DO SEGUNDO DIGITO (DEZENA)
      DF1=6*D[1]+5*D[2]+4*D[3]+3*D[4]+2*D[5]+9*D[6]+8*D[7]+7*D[8];
          +6*D[9]+5*D[10]+4*D[11]+3*D[12]+2*PRIDIG
      DF2=DF1/11
      DF3=INT(DF2)*11
      resto2=DF1-DF3
      IF resto2=0 .OR. resto2=1
         segdig=0
       ELSE
         segdig=11-resto2
      ENDIF
      IF pridig <> VAL(SUBSTR(cCgccpf,13,1)) .OR. segdig <> VAL(SUBSTR(cCgccpf,14,1))
         LIMPA()
         TONE(650,4)
         TECLA("C.G.C. Invalido! - Tecle [ENTER]")
         lRetorn =.F.
      ELSE  
         LIMPA()
*         @ nLin,nCol  SAY SPACE(18)
*         GETLIST[2,3] := "@R 999.999.999-99"
         lRetorn = .T.
      ENDIF
   CASE LEN(ALLTRIM(cCgccpf)) = 0
      LIMPA()
      @ nLin,nCol SAY SPACE(18)
      lRetorn = .T.
   CASE LEN(ALLTRIM(cCgccpf)) = 11
      PRIVATE VAR,PESO,SOMA,VETOR,I,DIGNCPF,DIGPESO
      PRIVATE RESTO,PRIMDIG,SEGDIG
      VAR = LEN(SUBSTR(cCgccpf,1,9))
      PESO = '098765432'
      SOMA = 0
      DECLARE VETOR[VAR]
      FOR I = 1 TO VAR
         DIGNCPF = (VAL(SUBSTR(cCgccpf,I,1)))
         DIGPESO = (VAL(SUBSTR(PESO,I,1)))
         IF I = 1 .AND. DIGPESO = 0
            DIGPESO = 10
         ENDIF
         VETOR[I] = DIGNCPF * DIGPESO
         SOMA = SOMA + VETOR[I]
      NEXT
      RESTO = SOMA%11
      PRIMDIG = 11 - RESTO
      IF RESTO = 1 .OR. RESTO = 0
         PRIMDIG = 0
      ENDIF
      cCgccpf =  cCgccpf + STR(PRIMDIG,1)
      VAR = LEN(ALLTRIM(cCgccpf))
      PESO = '1098765432'
      SOMA = 0
      DECLARE VETOR[VAR]
      FOR I = 1 TO VAR
         DIGNCPF = (VAL(SUBSTR(cCgccpf,I,1)))
         DIGPESO = (VAL(SUBSTR(PESO,I,1)))
         IF I = 1 .AND. DIGPESO = 1
            DIGPESO = 11
         ENDIF
         IF I = 2 .AND. DIGPESO = 0
            DIGPESO = 10
         ENDIF
         VETOR[I] = DIGNCPF * DIGPESO
         SOMA = SOMA + VETOR[I]
      NEXT
      RESTO = SOMA%11
      SEGDIG = 11 - RESTO
      IF RESTO = 1 .OR. RESTO = 0
         SEGDIG = 0
      ENDIF
      IF PRIMDIG <> VAL(SUBSTR(cCgccpf,10,1)) .OR. SEGDIG <> VAL(SUBSTR(cCgccpf,11,1))
         LIMPA()
         TONE(650,4)
         TECLA("C.P.F. Invalido! - Tecle [ENTER]" )
         lRetorn =.F.
      ELSE  
         LIMPA()
*         @ nLin,nCol  SAY SPACE(18)
*         GETLIST[2,3] := "@R 999.999.999-99"
         lRetorn = .T.
      ENDIF
   OTHERWISE
      LIMPA()
      TECLA("C.G.C. ou C.P.F. Invalido! - Tecle [ENTER]")
      @ nLin,nCol  SAY SPACE(18)
      lRetorn = .F.
ENDCASE
RETURN lRetorn
*---------------------------------------------fim F_CGCCPF-------------

*** POSSIBILITA VOLTAR A OPCAO ANTERIOR MESMO QUE O VALID ATUAL ESTEJA .F. DES-
*** DE QUE PRESSIONADO SETA PARA ESQUERDA OU PARA CIMA
FUNCTION INTVOLTA(plParam)
LOCAL lRetorno:=.F.
IF .NOT. plParam 
   IF LASTKEY() = 5 .OR. LASTKEY() = 19
      lRetorno = .T.
   ENDIF
ELSE
   lRetorno := plParam
ENDIF
RETURN lRetorno

*** FUNCTION CEC1200I
**  TESTE DE CGC/CPF JA CADASTRADO
*
FUNCTION CEC1200I(cCgcCpf,cCodCli)
LOCAL nReg,nOrder,lValido

lValido = .T.
IF LEN(TRIM(cCgcCpf)) <> 0

*** PEGA REGISTRO CORRENTE
   nReg = RECNO()

*** INDICE
   nOrder = INDEXORD()
   SET ORDER TO 3

*** PROCURA CGC
   SELECT FORNECE
   SEEK cCgcCpF

*** ACHOU?
   IF .NOT. EOF()

*** O CODIGO DO CLIENTE ENCONTRADO E' DIFERENTE DO QUE ESTA' SE 
*** CADASTRANDO?
      IF cCodFor <> CODFOR
         tCec1200i=SAVESCREEN(23,00,34,79)
         LIMPA()
         TONE(650,4)
         TECLA("Cgc/Cpf J  Cadastrado para o Cliente C¢digo " + CODFOR+ ;
            ". [ENTER]")  
         RESTSCREEN(23,00,34,79,tCec1200i)
         lValido = .F.
      ENDIF
   ENDIF
*** RETORNA 
   SET ORDER TO nOrder
   GOTO nReg
ENDIF
RETURN lValido
*----------------------------------------FIM CEC1200I-----------------

PROCEDURE SITPAGAR
PARAMETERS pSituacao
DO CASE
CASE pSituacao = "B"
   RETURN "COBRANCA BANCARIA"
CASE pSituacao = "C"
   RETURN "CARTEIRA"
CASE pSituacao = "D"
   RETURN "`A PRAZO"
CASE pSituacao = "E"
   RETURN "TITULO ESTIMADO"
CASE pSituacao = "F"
   RETURN "FIXA"
CASE pSituacao = "J"
   RETURN "COBRANCA JUDICIAL"
CASE pSituacao = "P"
   RETURN "PAGAMENTO PARCIAL"
CASE pSituacao = "U"
   RETURN "CAUCAO"
CASE pSituacao = "L"
   RETURN "LIQUIDADO"
ENDCASE
RETURN ""


PROCEDURE SITRECEB
PARAMETERS pSituacao
DO CASE
CASE pSituacao = "B"
   RETURN "COBRANCA BANCARIA"
CASE pSituacao = "C"
   RETURN "CARTEIRA"
CASE pSituacao = "D"
   RETURN "`A PRAZO"
CASE pSituacao = "E"
   RETURN "TITULO ESTIMADO"
CASE pSituacao = "F"
   RETURN "FIXA"
CASE pSituacao = "J"
   RETURN "COBRANCA JUDICIAL"
CASE pSituacao = "P"
   RETURN "PAGAMENTO PARCIAL"
CASE pSituacao = "U"
   RETURN "CAUCAO"
CASE pSituacao = "L"
   RETURN "LIQUIDADO"
ENDCASE
RETURN ""
**********************************************************************
*
*  Errorsys.prg      
*
*  Sistema de Erros Clipper
*
*  Copyright (c) 1997  Softer Consultoria e Sistemas Ltda.
*  All rights reserved.
*
*  Compile: /m /n /w
*
*  Nota:    Esta Rotina Nao pode ser renomeada ! Ela e' chamada
*           automaticamente pelas aplicacoes Clipper durante a 
*           inicializacao para definir o handler de erros Default
*
***********************************************************************/
*
#include "error.ch"

PROCEDURE ERRORSYS()

   ERRORBLOCK( { |eObj| DEFERROR(eObj) } )




RETURN
*
***********************************************************************
*  DEFERROR() - Funcao de Manipulacao de Erros Substituta
*               Retorna 3 Valores de Retorno Validos
*                 .T. == Tentar Novamente
*                 .F. == Ignorar Erro
*                 exp == Valor a ser substituido quando CANSUBSTITUTE
*                        for .T.
************************************************************************/
FUNCTION DEFERROR(eObj)
*
LOCAL i, cErrMessage, aOptions := { "Abortar" }
LOCAL nChoice 

// Evita divisao por Error Zero, registrando o erro
IF ( eObj:GENCODE == EG_ZERODIV )

   LOGITEM(eObj)
   RETURN (0)

ENDIF

// Evita ERRO POR IMPRESSOARA DESLIGADA
IF ( eObj:GENCODE == EG_PRINT )

   DO WHILE .T.
      IF .NOT. ISPRINTER() 
         FOR IMP = 1 TO 3
            TONE(600,3)
         NEXT
         SET DEVICE TO SCREEN
         LIMPA()
         SETBLINK(.T.)
         TECLA("Verifique Impressora e Tecle [ENTER]. <ESC>Imprime em Disco")
         SETBLINK(.F.)
         LIMPA()
         SET DEVICE TO PRINT
         IF LASTKEY() = 27
            cNetName = NETNAME()
            IF LEN(TRIM(cNetName)) = 0
               IF FILE("INTEGRA.PRN")
                  FERASE("INTEGRA.PRN")
               ENDIF   
               SET PRINTER TO INTEGRA.PRN
            ELSE
               cNetName = TRIM(LEFT(cNetName,8))+".PRN"
               IF FILE(cNetName)
                  FERASE(cNetname)
               ENDIF   
               SET PRINTER TO &cNetName
            ENDIF
            EXIT
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO      
   RETURN .T.

ENDIF

// Impede que violacoes de compartilhamento do DOS causem problemas
IF ( eObj:GENCODE == EG_OPEN .AND. eObj:OSCODE == 32 .AND. eObj:CANDEFAULT )

   NETERR(.T.)
   RETURN (.F.)

ENDIF

// Faz o mesmo em relacao a APPEND FAILURE
IF ( eObj:GENCODE == EG_APPENDLOCK .AND. eObj:CANDEFAULT )

   NETERR(.T.)
   RETURN (.F.)

ENDIF

// Manipulacao de Erros do DOS
IF !EMPTY(eObj:OSCODE)

   LOGITEM(eObj)          //  Registra o Erro
   ALERT(ERRORMESSAGE(eObj) + ;
      "; Erro do DOS " + LTRIM(STR(eObj:OSCODE)),aOptions)
   ERRORLEVEL(1)
   BREAK(eObj)
ELSE

   LOGITEM(eObj)
   IF eObj:CANRETRY
      AADD(aOptions, "Tentar Novamente")
   ENDIF
         
   IF eObj:CANDEFAULT
      AADD(aOptions, "Default")
   ENDIF

// Exibe a janela de Notificacao de Erro
   nChoice := ALERT(ERRORMESSAGE(eObj), aOptions)

   IF aOptions[nChoice] == "Abortar"
      ERRORLEVEL(1)
      BREAK(eObj)
   ELSEIF aOptions[nChoice] == "Tentar Novamente"
      RETURN(.T.)
   ELSEIF aOptions[nChoice] == "Default"
      RETURN(.F.)
   ELSE
      ERRORLEVEL(1)
      BREAK(eObj)
   ENDIF
ENDIF
RETURN .F.
************************************************************************
*  ErrorMessage() - Funcao de Criacao de Mensagens de Erro
*                   Extrai informacoes da variavel de instancia,
*                   que serao exibidas na funcao ALERT()

FUNCTION ERRORMESSAGE(eObj)
LOCAL cMessage := "Erro. "


//Informacoes do Sub-Sistema do Clipper

IF VALTYPE(eObj:SubSystem) == "C"
   cMessage += TRIM(eObj:SubSystem)

   IF VALTYPE(eObj:SubCode) == "N"
      cMessage += "/" + LTRIM(STR(eObj:SubCode))
   ENDIF

ENDIF

 
IF ( VALTYPE(eObj:Description) == "C" )
   cMessage += ";" + eObj:Description
ENDIF


IF LEN(eObj:Filename) > 0
   cMessage += ": " + TRIM(eObj:Filename)
ENDIF

IF LEN(eObj:Operation) > 0
   cMessage += (": " + eObj:Operation)
ENDIF

RETURN (cMessage)
*-----------------------------------------------------------------------
*     LOGITEM(eObj) - Funcao para Registro de Erros
*
*
FUNCTION LOGITEM(eObj)
LOCAL nProc := 3, lConsole
lConsole := SET(_SET_CONSOLE, .F. )
*
SET PRINTER TO LOG.TXT ADDITIVE
SET PRINTER ON
*
?
? "+++"
? "Erro", DATE(), TIME()
? "Args  ", IF(eObj:Args == NIL, "NIL", eObj:Args)
? "CanDefault", IF(eObj:CanDefault, "SIM", "NAO")
? "CanRetry  ", IF(eObj:CanRetry, "SIM", "NAO")
? "CanSubstitute", IF(eObj:CanSubstitute, "SIM", "NAO")
? "Cargo ", eObj:Cargo
? "Descricao  ", eObj:Description
? "Arquivo  ", eObj:Filename
? "GenCode ", STR(eObj:GenCode)
? "Operacao ", eObj:Operation
? "Cod.S.Oper. ", STR(eObj:OsCode)
? "Grau ", STR(eObj:Severity)
? "SubCode ", STR(eObj:SubCode)
? "SubSystem ", eObj:SubSystem
? "Tentativas ", STR(eObj:Tries)
? "---"
?
DO WHILE LEN(PROCNAME(nProc)) > 0
   ? "Chamada de", TRIM(PROCNAME(nProc)), ;
     "Linha: ", LTRIM(STR(PROCLINE(nProc)))
     nProc++
ENDDO

? "*------------------------------*"
? "*------------------------------*"

SET PRINTER OFF
SET PRINTER TO
RETURN NIL
*------------------------------------------fim TEMPOLIQUI----------------
FUNCTION DIASEMAN(nDia)
*                          Retorna o dia da semana
*==========================
LOCAL cDiaSeman

SET KEY 287 TO IND_SOFTER // ALT+S PARA TELA SOFTER

DO CASE
CASE nDia = 1
   cDiaSeman = "Domingo      "
CASE nDia = 2
   cDiaSeman = "Segunda-Feira"
CASE nDia = 3
   cDiaSeman = "Terca-Feira  "
CASE nDia = 4
   cDiaSeman = "Quarta-Feira "
CASE nDia = 5
   cDiaSeman = "Quinta-Feira "
CASE nDia = 6
   cDiaSeman = "Sexta-Feira  "
CASE nDia = 7
   cDiaSeman = "Sabado       "
OTHERWISE
   cDiaSeman = "Dia Invalido "
ENDCASE
RETURN cDiaSeman

*----------------------------------------------------------------------
FUNCTION TESTACGC(cgc)
*==========================
PRIVATE D[12],I,DF1,DF2,DF3

SET KEY 287 TO IND_SOFTER // ALT+S PARA TELA SOFTER

FOR I=1 TO 12
   D[I]=VAL(SUBSTR(cgc,I,1))
NEXT
***TESTE DO PRIMEIRO DIGITO (UNIDADE)
DF1=5*D[1]+4*D[2]+3*D[3]+2*D[4]+9*D[5]+8*D[6]+7*D[7]+6*D[8];
    +5*D[9]+4*D[10]+3*D[11]+2*D[12]
DF2=DF1/11
DF3=INT(DF2)*11
RESTO1=DF1-DF3
IF resto1=0 .OR. resto1=1
   pridig=0
 ELSE
   pridig=11-resto1
ENDIF
***TESTE DO SEGUNDO DIGITO (DEZENA)
DF1=6*D[1]+5*D[2]+4*D[3]+3*D[4]+2*D[5]+9*D[6]+8*D[7]+7*D[8];
    +6*D[9]+5*D[10]+4*D[11]+3*D[12]+2*PRIDIG
DF2=DF1/11
DF3=INT(DF2)*11
resto2=DF1-DF3
IF resto2=0 .OR. resto2=1
   segdig=0
 ELSE
   segdig=11-resto2
ENDIF
IF pridig <> VAL(SUBSTR(CGC,13,1)) .OR. segdig <> VAL(SUBSTR(cgc,14,1))
   @ 23, 13 CLEAR
   @ 23, 23 SAY "C.G.C. Invalido!"
   INKEY(0)
   @ 23,13 CLEAR 
   RETURN(.F.)
 ELSE  
   RETURN(.T.)
ENDIF

***************************
FUNCTION FPORT(lInclusao,nLinesc,nColesc)
***************************
* COLOCA PORTADOR CONFORME  CADASTRO DE CLIENTES
*

IF lInclusao
   nPortador = VAL(CLIENTES->PORTADOR)
   PAG2300C(nLinesc,nColesc+4,STRZERO(nPortador,3),.F.,.T.)
   @ nLinesc,nColesc SAY nPortador PICT "999"
ENDIF

RETURN .T.

*** FUNCTION CAI1000H
**  CALCULO DO SALDO DO ITEM EM ESTOQUE
*
FUNCTION CAI1000H
nSaldag = 0
nSaldag = (QTDENTUF + QTDAJE + QTDTRANE + QTDENTMC) - (QTDSAIMC + QTDAJS + QTDTRANSAI)
RETURN nSaldag


********************************************
FUNCTION FACESSO(cPrograma,lAcesso,cAutor)
********************************************

IF DBF() = NIL
   lDbf  = .T.
   cAlias= ALIAS()
ELSE
   lDbf  = .F.
ENDIF

cProg  = cPrograma
lAchou = .F.

SELECT 0
FABREA("ACESSFUN",.F.)
SET INDEX TO ACX1
IF lAcesso = .T. .AND. cAutor <> NIL
   SEEK cAutor+cProg
ELSE
   SEEK gCodFunc+cProg
ENDIF
IF .NOT. EOF()
   lAchou = .T.
ENDIF

SELECT ACESSFUN
USE

IF lAchou .OR. gCodFunc = "99"
   IF lAcesso = .T.
      IF lDbf
         SELECT &cAlias
      ENDIF
      RETURN .T.
   ELSE
      DO &cProg
   ENDIF
ELSE
   IF lAcesso = .T.
      IF lDbf
         SELECT &cAlias
      ENDIF
      RETURN .F.
   ELSE
      FNEGADO()
   ENDIF
ENDIF

IF lDbf
   SELECT &cAlias
ENDIF

RETURN .T.

****************************
FUNCTION FNEGADO()
****************************

FOR A = 1 TO 3
   TONE(600,3)
NEXT

ALERT("Acesso Negado Para Este Usuario!!!")
   
RETURN .T.

****************************************
PROCEDURE MONTACESSO(prog)
****************************************

*---Seleciona arquivos
cArq = ALIAS()

IF .NOT. FILE("ACESSO.DBF")
   aEstacesso:={}
   AADD(aEstacesso,{"PROGRAMA","C",10,0})
   AADD(aEstacesso,{"DESCRICAO","C",40,0})
   DBCREATE("ACESSO.DBF",aEstacesso)
ENDIF

SELECT 0
USE ACESSO EXCLUSIVE
IF .NOT. FILE("ACESSO.NTX")
   INDEX ON PROGRAMA TO ACESSO
ELSE   
   SET INDEX TO ACESSO
ENDIF

GO TOP

*---Define variaveis
cPrograma = TAMANHO(prog,10)
cDescricao = SPACE(40)

SELECT ACESSO
SEEK cPrograma

IF .NOT. EOF()
   FOR A = 1 TO 3
      TONE(600,3)
   NEXT   
   TECLA("Acesso Ja' Cadastrado.[ENTER]Altera.<ESC>Desiste.")
   IF LASTKEY() = 27
      SELECT ACESSO
      USE
      IF LEN(ALLTRIM(cArq)) <> 0
         SELECT &cArq
      ENDIF
      RETURN .T.
   ENDIF
   cDescricao = DESCRICAO
ENDIF   

*---Rotina Principal

aGet := GETLIST
GETLIST := {}

nCursor = SETCURSOR()
SETCURSOR(1)

DO WHILE .T.

   SAVE SCREEN TO tAcesso
   
   MOLD(10,10,16,70)

   @ 12,12 SAY "Programa..:" GET cPrograma PICT "@!"
   @ 14,12 SAY "Descricao.: "+cDescricao 
   READ

   IF LASTKEY() = 27
      EXIT
   ENDIF

   SELECT ACESSO
   SEEK cPrograma
   cDescricao = DESCRICAO

   @ 14,12 SAY "Descricao.:" GET cDescricao PICT "@!" VALID cDescricao <> SPACE(40) 
   READ

   IF LASTKEY() = 27
      LOOP
   ENDIF

   SELECT ACESSO
   SEEK cPrograma
   IF EOF()
      FAPPEND()
      REPLACE PROGRAMA WITH cPrograma
   ELSE
      FCHANGE()
   ENDIF
   REPLACE DESCRICAO WITH cDescricao
   FUNLOCK()
   
   EXIT
ENDDO

SETCURSOR(nCursor)

SELECT ACESSO
USE

*--- Restaura a Tela
RESTORE SCREEN FROM tAcesso

*** RESTAURA GET

GETLIST := aGet

IF LEN(ALLTRIM(cArq)) <> 0
   SELECT &cArq
ENDIF

RETURN

********************
PROCEDURE MODACESSO
********************

SELECT ACESTEMP
GO BOTTOM
SKIP -14
KEYBOARD REPLICATE(CHR(04),4)
DBEDIT(04,44,20,77,aCam3400,"RES3400BA",aMas3400,aTit3400,.T., ;
   .T.,.T.,.T.)

*** FUNCTION RES3400BA
**  DBEDIT COM ATUALIZACAO DOS PRECOS
*
FUNCTION RES3400BA
PARAMETERS p_modo
STATIC cProcura := "                              "

*** VALOR INICIAL DE RETORNO
p_retorno = 1

*** QUAL A TECLA?
nLastKey = LASTKEY()
cTeclada = UPPER(CHR(nLastKey))

DO CASE
CASE nLastKey = 19 //ESQUERDA
   KEYBOARD REPLICATE(CHR(04),1)
CASE nLastKey = 1  //HOME
   KEYBOARD REPLICATE(CHR(04),4)
CASE nLastKey = 27 //ESC?
   p_retorno = 0
CASE nLastkey=13

   IF ACESSO = "N"
      cAcesso = "S"
   ELSE
      cAcesso = "N"
   ENDIF

*** ATUALIZA QUANTIDADE
   FCHANGE()
   REPLACE ACESSO  WITH cAcesso
   FUNLOCK()

   KEYBOARD CHR(24)
   p_retorno = 2

ENDCASE

RETURN p_retorno

*** FUNCTION LAB1500B
**  CALCULA VALIDADE DA DATA
*
FUNCTION LAB1500B
LOCAL nHora,nMinuto
PARAMETERS pcData

nHora = 0
nMinuto = 0
LAB1500C(pcData,@nHora,@nMinuto)

IF (nHora >= 0 .AND. nHora <= 24) .AND. (nMinuto >= 0 .AND. nMinuto <= 59) ;
    .OR. (nHora = 24 .AND. nMinuto = 0)
   RETURN .T.
ELSE
   LIMPA()
   TECLA("Hora Invalida!")
   RETURN .F.    
ENDIF

*** FUNCTION LAB1500C
**  RETIRA VALORES DA HORA E MINUTO
*
FUNCTION LAB1500C
PARAMETERS cString,nHora,nMinuto
nHora   = VAL(LEFT(cString,2))
nMinuto = VAL(SUBSTR(cString,4,2))
RETURN

FUNCTION TAMANHO
PARAMETERS pVar,pTam
LOCAL cVar

*** COMPLETA TAMANHO COM ESPACOS
IF LEN(pVar) < pTam
   pVar = pVar + SPACE(pTam-LEN(pVar))
ENDIF   

RETURN LEFT(pVar,pTam)

**************************
FUNCTION FCONVREF(nValor)
**************************

cInteiro = STRZERO(INT(nValor),8)
cResto   = STRZERO(VAL(RIGHT(STR(nValor,10,2),2)),2)

cRetorno = cInteiro+cResto

RETURN cRetorno

FUNCTION SIMNAO
*==========================
*        CONFIRMACAO OU RECUSA DE UMA TECLA DIGITADA           
*==========================
cCorAnt = SETCOLOR()
SAVE SCREEN TO tTelnao

LIMPA()

SETCOLOR("W+/R,GR+/N,,,GR+/N")
MOLD(19,22,21,55)

nSimNao = 2
@ 20,28 SAY    "Confirme       ou"
@ 20,38 PROMPT "Sim" MESSAGE " Confirma "
@ 20,47 PROMPT "N„o" MESSAGE "  Recusa  "
MENU TO nSimNao

IF nSimNao = 1
   lSimNao = .T.
ELSE
   lSimNao = .F.   
ENDIF

SETCOLOR(cCorAnt)
RESTORE SCREEN FROM tTelNao
FLIMPA()
RETURN lSimNao


*** PROCEDURE REC0000F
**  AVALIA GET DO LOCAL
*
PROCEDURE REC0000F
PARAMETERS nLinha,nColuna,cCod,lObrigatorio

*** VALOR INICIAL DO RETORNO
lRetorno = .F.

*** CHAMA CONSULTA
IF LASTKEY() = 9
   SELECT 0
   FABREA("TITLREC",.F.,gDiret)
   cIndice1 = gDiret + "RECNUMX1" 
   cIndice2 = gDiret + "RECNUMX2" 
   SET INDEX TO &cIndice1,&cIndice2
   SET ORDER TO 2
   SAVE SCREEN TO telahist
   cCod = SUBSTR(F_10("TITLREC"),1,6)
   nParcela = VAL(SUBSTR(F_10("TITLREC"),7,2))
   SELECT TITLREC
   USE
   RESTORE SCREEN FROM telahist

*** ESC?
   IF LASTKEY() <> 27
      KEYBOARD cCod  
   ENDIF
ELSEIF LASTKEY()=5 .OR. LASTKEY() = 19 //SETAS PARA CIMA E PARA A ESQUERDA
   lRetorno = .T.
ELSE

*** CHAMA SEEK ESPERTO
   lRetorno = .T.

ENDIF

RETURN lRetorno

******************************
FUNCTION PEGASENHA()
******************************

lValidafunc = .F.

SELECT 0
FABREA("FUNCIONA",.F.)
SET INDEX TO FUNCODX1
GO TOP

DO WHILE .NOT. EOF()

   cArqteste = "C"+STRZERO(VAL(CODIFUNC),2)+".DBF"

   IF FILE(cArqteste)
      SELECT 0
      USE &cArqteste EXCLUSIVE
      IF .NOT. NETERR()
         IF FUNCIONA->PROBLEMA = .T.
            IF LEN(ALLTRIM(ARQUIPROB)) <> 0
               nProblema++
               lProblema = .T.
               AADD(cFuncpro,FUNCIONA->CODIFUNC)
               AADD(cArqupro,ALLTRIM(ARQUIPROB))
            ENDIF
         ENDIF
         USE
      ENDIF
   ENDIF

   SELECT FUNCIONA
   SKIP

ENDDO

*---- TENTA PEGAR SENHAS
FOR I = 1 TO 3

   nCodfunc   = 0
   
   @ 23,13 SAY "Digite Seu Codigo: " GET nCodfunc PICT "99" ;
                                         VALID nCodfunc <> 0
   READ

***VERIFICO SE ESTE FUNCIONARIO JA ESTA ACESSANDO O SISTEMA

   cArqteste = "C"+STRZERO(nCodfunc,2)+".DBF"
   IF FILE(cArqteste)
      SELECT 0
      USE &cArqteste EXCLUSIVE
      IF NETERR()
         FOR J = 1 TO 3
            TONE(600,3)   
         NEXT
         LIMPA()
         ALERT("Este Usuario esta' Acessando o Sistema")
         LIMPA()
         LOOP
      ELSE
         USE
      ENDIF   

   ENDIF
*----   
   LIMPA()

   IF LASTKEY() = 27
      lValidafunc = .F.
      EXIT
   ENDIF

*--Converte para String
   cCodfunc = STRZERO(nCodfunc,2)

*---- ACESSA FUNCIONARIO
   SELECT FUNCIONA
   SEEK cCodfunc
      
   IF EOF() 
      IF cCodfunc <> "99"
         LIMPA()
         TECLA("Codigo N„o Cadastrado - TECLE [ENTER]")
         LIMPA()
         LOOP
      ELSE
         nDia = DAY(DATE())
         nMes = MONTH(DATE())
         cDecSenha = STRZERO(nDia+300,3) + STRZERO( nMes * nMes,3)
      ENDIF      
   ELSE
      cDecSenha = DECRIPTO(SENHAFUNC)
   ENDIF

*---- DESCRIPTOGRAFA A SENHA
   cData=DTOS(DATE())

*---- DIGITA A SENHA DO FUNCION RIO
   cSenha = SPACE(08)

   @ 23,13 SAY "Digite Sua Senha, "+NOMEFUNC
   DO PASSWORD WITH cSenha, 8, 23, 45, 28 
   LIMPA()

   IF LASTKEY() = 27
      lValidafunc = .F.
      EXIT
   ENDIF

   IF .NOT. ALLTRIM(cSenha) == ALLTRIM(cDecSenha)
      TONE(800,4)
      TECLA("Senha Invalida !!!")
      LIMPA()
      LOOP
   ELSE
      IF cSenha <> SPACE(08)
         lValidafunc = .T.
         EXIT
      ENDIF
   ENDIF   
   
NEXT

gNomefunc = FUNCIONA->NOMEFUNC
gCodfunc  = cCodfunc

IF lValidafunc
   SELECT FUNCIONA
   FCHANGE()
   REPLACE PROBLEMA WITH .T.
   FUNLOCK()
ENDIF   

*---- FECHA FUNCIONA
SELEC FUNCIONA
USE

RETURN lValidafunc

*************************
PROCEDURE PEGAEMP
*************************

*---- SELECIONA EMPRESA
SELECT 0
FABREA("EMPRESAS",.F.)
SET INDEX TO EMPCODX1

DO WHILE .T.

*---- Sem Parametro de Codigo de Empresa
   nCodigo = 0
   IF pCodigo = NIL
      @ 23,15 SAY "Digite o Codigo da Empresa " GET nCodigo PICT "99" ;
                   VALID !EMPTY(nCodigo)
      READ
      COMANDO("[ESC] Desiste")
      
      LIMPA()
      
*---- ESC?
      IF LASTKEY() = 27
         LOOP
      ENDIF

*---- TRANSFORMA STRING 
      cCodigo = STRZERO(nCodigo,2)
   ELSE
   
*---- CONVERTE PARA DOIS BYTES O PARAMETRO PASSADO
      nCodigo = VAL(pCodigo)
      cCodigo = STRZERO(nCodigo,2)
      gCodEmp = cCodigo
   ENDIF

*---- PROCURA EMPRESA
   SELECT EMPRESAS
   SEEK cCodigo

   IF EOF()   
      LIMPA()
      TECLA("Empresa N„o Cadastrada -  Pressione[ENTER]")
      LIMPA()
      cCodigo = NIL
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO

*---- PREPARA VARIAVEIS GERAIS E LIBERA EMPRESAS
gCodEmp  = CODIGO
gEmpresa = DECRIPTO(FANTASIA)
gDiret   = ".\" + gCodEmp + "\"

gContcaixa = "N"
gCaixaab   = .F.
gTurncaixa =  0
gDt_Base   = DATE()

gForma = 1
gNumeraut = .T.
gAreaenc = "001"
SELECT 0
FABREA("PARAMS",.F.,gDiret)
IF FORMAIMP <> 0
   gForma = FORMAIMP
ENDIF

IF AREAENC <> SPACE(03)
   gAreaenc = AREAENC
ENDIF

IF NUMERAUT = "N"
   gNumeraut = .F.
ENDIF

IF EMPRESAS->CONTCAIXA = "S"
   gContcaixa = "S"
   gDt_Base   = DTTURATU
   IF SITUATUR = "A"
      gCaixaab   = .T.
   ELSE
      gCaixaab   = .F.
   ENDIF
   gTurncaixa =  TURNOATU
   
ENDIF
gComfisc = COMIMP

SELECT PARAMS
USE

SELECT EMPRESAS

SAVE SCREEN TO tLimpa
gNfonte = VAL(CONTA)
DO CASE
CASE VAL(IMPRESSORA) = 1
   iComp     = CHR(15)
   iNorm     = CHR(18)
   iPadrao   = CHR(27)+"@"
   iExpandi  = CHR(14)
   iExpandf  = CHR(20)
   iNormbema = CHR(27)+"M"
   iEjeta    = CHR(18)
CASE VAL(IMPRESSORA) = 2
   iComp     = CHR(27)+CHR(40)+CHR(115)+'16,67'+CHR(72)
   iNorm     = CHR(27)+CHR(40)+CHR(115)+'12'+CHR(72)
   iEjeta    = CHR(27)+'E'
   iPadrao   = CHR(27)+"@"
   iExpandi  = CHR(14)
   iExpandf  = CHR(20)
   iNormbema = CHR(27)+"M"
ENDCASE

IF FORPEMP = "S"
   gDforn = gDiret
ELSE
   gDforn = ""
ENDIF

IF CLIPEMP = "S"
   gDclie = gDiret
ELSE
   gDclie = ""
ENDIF

SELECT EMPRESAS
USE

RETURN

***********************
PROCEDURE PEGAFONTE
***********************

DO CASE
CASE gNfonte = 0
   FONTSELECT(1)
CASE gNfonte = 1
   IF "" <> FILESEEK("BOXROUND.016")
      FONTLOAD("BOXROUND.016", 2)
      FONTSELECT(2,2)
   ENDIF
CASE gNfonte = 2
   IF "" <> FILESEEK("CNTDOWN.016")
      FONTLOAD("CNTDOWN.016", 2)
      FONTSELECT(2,2)
   ENDIF
CASE gNfonte = 3
   IF "" <> FILESEEK("ITALIC.016")
      FONTLOAD("ITALIC.016", 2)
      FONTSELECT(2,2)
   ENDIF
CASE gNfonte = 4
   IF "" <> FILESEEK("OCR.016")
      FONTLOAD("OCR.016", 2)
      FONTSELECT(2,2)
   ENDIF
CASE gNfonte = 5
   IF "" <> FILESEEK("ROM8PIX.016")
      FONTLOAD("ROM8PIX.016", 2)
      FONTSELECT(2,2)
   ENDIF
CASE gNfonte = 6
   IF "" <> FILESEEK("THIN.016")
      FONTLOAD("THIN.016", 2)                        
      FONTSELECT(2,2)
   ENDIF
ENDCASE

RETURN

*************************
PROCEDURE TESTOINDICES
*************************


IF lProblema 

   SAVE SCREEN TO tProblema

   FOR J = 1 TO 3
      TONE(600,3)
   NEXT

   MOLD(04,13,11+LEN(cFuncpro),68)
   @ 05,21 SAY "Necessario Reindexar os Arquivos Abaixo" COLOR "W/N"
   @ 06,19 SAY "Pois Usuarios Sairam Ilegalmente do Sistema" COLOR "W/N"
   @ 07,14 SAY "------------------------------------------------------"  
   @ 08,16 SAY "Usuario  Arquivo a Reindexar"
   @ 09,16 SAY "=======  =========================================="
   FOR I = 1 TO LEN(cFuncpro)
      @ 09+I,19 SAY cFuncpro[I]
      @ 09+I,25 SAY cArqupro[I]
   NEXT
   @ 11+LEN(cFuncpro),21 SAY " TECLE <ENTER> Para Iniciar,[ESC]Desiste " COLOR "W/N"
   INKEY(0)

   IF LASTKEY() = 27
      CLOSE ALL
      SET COLOR TO
      CLEAR
      SET CONSOLE ON
      ?
      ? "Lembre-se de Reindexar os Arquivos..."
      ? "Obrigado."
      ?
      QUIT
   ENDIF         

*---- PROCEDIMENTO

   lErro = .F.

   FOR I = 1 TO LEN(cFuncpro)

      DO CASE
/*
      CASE cArqupro[I] = "ACESSFUN"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE ACESSFUN EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE
         FERASE("ACX1.NTX")
         SELECT 0
         FABREA("ACESSFUN",.T.)
         USE
      CASE cArqupro[I] = "ACESSO"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE ACESSO EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE
         FERASE("ACESSO.NTX")
         SELECT 0
         USE ACESSO EXCLUSIVE
         INDEX ON PROGRAMA TO ACESSO
         USE
      CASE cArqupro[I] = "FUNCIONA"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE FUNCIONA EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         FERASE("FUNCODX1.NTX")
         FERASE("FUNNOMX2.NTX")
         SELECT 0
         FABREA("FUNCIONA",.T.)
         USE
      CASE cArqupro[I] = "COMPPED"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE COMPPED EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         FERASE("COMCODX1.NTX")
         FERASE("COMDESX2.NTX")
         SELECT 0
         FABREA("COMPPED",.T.)
         USE
      CASE cArqupro[I] = "EMPRESAS"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE EMPRESAS EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         FERASE("EMPCODX1.NTX")
         SELECT 0
         FABREA("EMPRESAS",.T.)
         USE
      CASE cArqupro[I] = "GRFATURA"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE GRFATURA EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         FERASE("GRFX1.NTX")
         SELECT 0
         FABREA("GRFATURA",.T.)
         USE
      CASE cArqupro[I] = "HISTPADR"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE HISTPADR EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         FERASE("HISX1.NTX")
         FERASE("HISX2.NTX")
         SELECT 0
         FABREA("HISTPADR",.T.)
         USE
      CASE cArqupro[I] = "MOEDAS"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE MOEDAS EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         FERASE("MOEX1.NTX")
         SELECT 0
         FABREA("MOEDAS",.T.)
         USE
      CASE cArqupro[I] = "NATOPER"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE NATOPER EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         FERASE("NATCOFX1.NTX")
         FERASE("NATDESX2.NTX")
         SELECT 0
         FABREA("NATOPER",.T.)
         USE
      CASE cArqupro[I] = "OPCONTAB"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE OPCONTAB EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         FERASE("OPCONTX1.NTX")
         FERASE("OPCONTX2.NTX")
         SELECT 0
         FABREA("OPCONTAB",.T.)
         USE
      CASE cArqupro[I] = "TB_PAGTO"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE TB_PAGTO EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         FERASE("TBPGX1.NTX")
         FERASE("TBPGX2.NTX")
         SELECT 0
         FABREA("TB_PAGTO",.T.)
         USE
      CASE cArqupro[I] = "TIPODOC"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         SELECT 0
         USE TIPODOC EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         FERASE("TIPCODX1.NTX")
         FERASE("TIPDESX2.NTX")
         SELECT 0
         FABREA("TIPODOC",.T.)
         USE
      CASE cArqupro[I] = "GRUPOS"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret+"GRUPOS"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret+"GRUPX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret+"GRUPX2.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("GRUPOS",.T.,gDiret)
         USE
*/
      CASE cArqupro[I] = "MOVTOS"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "MOVTOS"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "MOVX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "MOVX2.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("MOVTOS",.T.,gDiret)
         USE
      CASE cArqupro[I] = "ESTOQUE"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "ESTOQUE"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "ESTX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "ESTX2.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("ESTOQUE",.T.,gDiret)
         USE
      CASE cArqupro[I] = "NOTAFISH"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "NOTAFISH"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "NOTHX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "NOTHX2.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "NOTHX3.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("NOTAFISH",.T.,gDiret)
         USE
      CASE cArqupro[I] = "NOTAFIS"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "NOTAFIS"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "NOTX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "NOTX2.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("NOTAFIS",.T.,gDiret)
         USE
      CASE cArqupro[I] = "MOVTOSH"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "MOVTOSH"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "MOVHX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "MOVHX2.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "MOVHX3.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("MOVTOSH",.T.,gDiret)
         USE
      CASE cArqupro[I] = "TITLPAG"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "TITLPAG"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "PAGNUMX1.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("TITLPAG",.T.,gDiret)
         USE
      CASE cArqupro[I] = "TITLREC"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "TITLREC"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "RECNUMX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "RECNUMX2.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("TITLREC",.T.,gDiret)
         USE
      CASE cArqupro[I] = "CAIXA"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "CAIXA"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "CAIX1.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("CAIXA",.T.,gDiret)
         USE
      CASE cArqupro[I] = "CAIXAR"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "CAIXAR"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "CAIRX1.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("CAIXAR",.T.,gDiret)
         USE
      CASE cArqupro[I] = "CAIRDIA"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "CAIRDIA"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "CDRX1.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("CAIRDIA",.T.,gDiret)
         USE
      CASE cArqupro[I] = "CLIPGTO"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "CLIPGTO"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDclie + "CLPGX1.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("CLIPGTO",.T.,gDclie)
         USE
      CASE cArqupro[I] = "CLIENTES"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "CLIENTES"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "CLICODX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "CLICPFX3.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "CLIFANX2.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "CLIFONX4.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("CLIENTES",.T.,gDclie)
         USE
/*
      CASE cArqupro[I] = "TRANSPOR"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "TRANSPOR"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "CODTRAX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "NOMTRAX2.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("TRANSPOR",.T.,gDiret)
         USE
      CASE cArqupro[I] = "PORTADOR"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "PORTADOR"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "PORCODX1.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("PORTADOR",.T.,gDiret)
         USE
      CASE cArqupro[I] = "FORNECE"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre =  gDforn + "FORNECE"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDforn + "FORCODX1.NTX"
         FERASE(cArqera)
         cArqera = gDforn + "FORCPFX3.NTX"
         FERASE(cArqera)
         cArqera = gDforn + "FORFANX2.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("FORNECE",.T.,gDforn)
         USE
      CASE cArqupro[I] = "ITENS"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "ITENS"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "ITENX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "ITENX2.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "ITENX3.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "ITENX4.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("ITENS",.T.,gDiret)
         USE
      CASE cArqupro[I] = "SALDO"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "SALDO"   
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "SALX1.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("SALDO",.T.,gDiret)
         USE
      CASE cArqupro[I] = "COMPRAS"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "COMPRAS"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "COMX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "COMX2.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "COMX3.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("COMPRAS",.T.,gDiret)
         USE
      CASE cArqupro[I] = "LOCALEST"
** VERIFICO SE ARQUIVO ESTA SENDO USADO
         cArqabre = gDiret + "LOCALEST"
         SELECT 0
         USE &cArqabre EXCLUSIVE
         IF NETERR()
            MOLD(12,13,14,68)
            @ 13,17 SAY "***  Necessario Usuarios Sairem do Sistema ***" COLOR "W/N*"
            INKEY(0)
            lErro = .T.
            EXIT
         ENDIF
         USE   
         cArqera = gDiret + "LOCAX1.NTX"
         FERASE(cArqera)
         cArqera = gDiret + "LOCAX2.NTX"
         FERASE(cArqera)
         SELECT 0
         FABREA("LOCALEST",.T.,gDiret)
         USE
*/
      ENDCASE

   NEXT

   RESTORE SCREEN FROM tProblema  

   IF lErro 
      CLOSE ALL
      SET COLOR TO
      CLEAR
      SET CONSOLE ON
      ?
      ? "Lembre-se de Reindexar os Arquivos..."
      ? "Obrigado."
      ?
      QUIT   
   ENDIF

   SELECT 0
   FABREA("FUNCIONA",.F.)
   SET INDEX TO FUNCODX1
   GO TOP

   DO WHILE .NOT. EOF()

      IF CODIFUNC <> gCodfunc
         FCHANGE()
         REPLACE PROBLEMA WITH .F.
         FUNLOCK()
      ENDIF

      cArqteste = "C"+STRZERO(VAL(CODIFUNC),2)+".DBF"

      IF FILE(cArqteste)
         SELECT 0
         USE &cArqteste EXCLUSIVE
         IF .NOT. NETERR()
            IF PROBLEMA = .T.
               FCHANGE()
               REPLACE PROBLEMA WITH .F.
               REPLACE ARQUIPROB WITH SPACE(80)
               FUNLOCK()
            ENDIF
            USE
         ENDIF
      ENDIF

      SELECT FUNCIONA
      SKIP

   ENDDO

   SELECT FUNCIONA
   USE

   LIMPA()
   TECLA("Indices Refeitos - Tecle [ENTER]")
   LIMPA()
   
ENDIF

RETURN


*** FUNCTION EST1100A
**  DBEDIT SOMENTE PARA MOSTRAR 
*
FUNCTION EST1100A
RETURN 0

*** FUNCTION EST1100C
**  CONSULTA DOS ITENS ANTERIORES
*
FUNCTION EST1100C
*** CHAMA DBEDIT
SAVE SCREEN TO tEST1100c
SELECT SAITEMP
MENSAGEM("<ESC>Ao terminar consulta")
DBEDIT(08,01,16,78,aCam1100,"",aMas1100,aTit1100,.T., ;
   .T.,.T.,.T.)
RESTORE SCREEN FROM tEST1100c
RETURN .T.

PROCEDURE EST1100E

*** CABECALHO
cHoraPedi = DTOC(DATE()) + " * " + TIME() 
TESTAPRINT()   
SET DEVICE TO PRINTER

nLinRel=1

@ nLinRel,000 SAY iComp

@ nLinRel,000 SAY REPLiCATE("-",101)
nLinRel++
@ nLinRel,001 SAY EMPRESAS->RAZAO
nLinRel++
@ nLinRel,001 SAY EMPRESAS->ENDERECO
nLinRel++
@ nLinRel,001 SAY EMPRESAS->BAIRRO + " " + TRIM(EMPRESAS->CIDADE) + " " + ;
   EMPRESAS->ESTADO
nLinRel++
@ nLinRel,001 SAY EMPRESAS->TEL
nLinRel++
@ nLinRel,001 SAY "FAX: " + cTelAux
@ nLinRel,053 SAY "Data: " + cHoraPedi
nLinRel++

@ nLinRel,000 SAY REPLiCATE("-",101)
nLinRel++

@ nLinRel,019 SAY "Consulta de Precos/Orcamento"
nLinRel++

@ nLinRel,000 SAY REPLiCATE("-",101)
nLinRel++

@ nLinRel,001 SAY "CLiente...........:"
@ nLinRel,021 SAY cRazao
nLinRel++

nLinRel++

@ nLinRel,001 SAY "Item"
@ nLinRel,069 SAY "Qtde"
@ nLinRel,078 SAY "Unitario"
@ nLinRel,096 SAY "Total"
nLinRel++

nValorTot = 0 

*** DETALHE
SELECT 0
FABREA("ESTOQUE",.F.,gDiret)
cIndice1 = gDiret + "ESTX1" 
SET INDEX TO &cIndice1

SELECT SAITEMP
GO TOP
DO WHILE .NOT. EOF()
            
   SELECT ESTOQUE
   SEEK SAITEMP->LOCALARMAZ + SAITEMP->ITEM
   IF .NOT. EOF() .AND. CAI1000H() >= SAITEMP->QUANTIDADE - ;
      SAITEMP->QUANTIDANT

      IF SAITEMP->QUANTIDADE > 0

         @ nLinRel,000 SAY ITEM PICT "99999"

         SELECT ITENS
         SET ORDER TO 1
         SEEK SAITEMP->ITEM
         SET ORDER TO 2

         @ nLinRel,006 SAY SAITEMP->DESCRICAO
         @ nLinRel,047 SAY ITENS->UNIDADE
         @ nLinRel,050 SAY ITENS->REFERENCIA
         @ nLinRel,066 SAY SAITEMP->QUANTIDADE PICT "999999"
         @ nLinRel,073 SAY SAITEMP->PRCVENDA PICT "@E 99,999,999.99"
         @ nLinRel,087 SAY SAITEMP->PRCVENDA*SAITEMP->QUANTIDADE;
            PICT "@E 999,999,999.99"
         nLinRel++

      ENDIF

*** INCREMENTA TOTAL
      nValorTot = nValorTot + SAITEMP->PRCVENDA * SAITEMP->QUANTIDADE
   ENDIF

*** PROXIMO
   SELECT SAITEMP
   SKIP
               
ENDDO

SELECT ESTOQUE
USE

@ nLinRel,030 SAY "T O T A L"
@ nLinRel,087 SAY nValorTot PICT "@E 999,999,999.99"
nLinRel++

@ nLinRel,001 SAY "Vendedor : "

SELECT FUNCIONA
SET ORDER TO 1
SEEK STRZERO(nCodVend,2)
SET ORDER TO 2
@ nLinRel,013 SAY FUNCIONA->NOMEFUNC
nLinRel++

@ nLinRel,000 SAY REPLiCATE("-",101)
nLinRel++
@ 01,000 SAY iNorm
SET DEVICE TO SCREEN

RETURN

*** PROCEDURE EST1100I
**  IMPRIME RELATORIO
*
PROCEDURE EST1100I
PARAMETERS cNumero,cCodcli

*** POSICIONA EM MOVTOSH
lFaz = .T.
SELECT MOVTOSH
SET ORDER TO 2
SEEK "20"+cNumero+cCodcli
IF EOF()
   TECLA("Pedido nao cadastrado")
   lFaz = .F.
ELSE 

*** POSICIONA EM MOVTOS
   SELECT MOVTOS
   SET ORDER TO 2
   SEEK "20"+MOVTOSH->CLIE_FORNE+MOVTOSH->NUMERODOC SOFTSEEK
   
   IF .NOT. EOF()
            
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

*** CABECALHO
      cHoraPedi = DTOC(DATE()) + " * " + TIME() 
      TESTAPRINT()
      SET DEVICE TO PRINT
      nLinRel = PROW()
      @ nLinRel,000 SAY iComp
      gColuna = 1
      nLinRel++

      @ nLinRel,000 SAY "+---------------------------------------------------------------------------------------------------------------------------------+"
      nLinRel++

      @ nLinRel,000 SAY "|"
      @ nLinRel,002 SAY gEmpresa
      @ nLinRel,057 SAY "P E D I D O"
      @ nLinRel,108 SAY "No."
      @ nLinRel,119 SAY MOVTOSH->NUMERODOC PICT "999999"
      @ nLinRel,130 SAY "|"
      nLinRel++

      @ nLinRel,000 SAY "|"
      @ nLinRel,002 SAY LOCALEST->LO_DESCR

      @ nLinRel,130 SAY "|"
      nLinRel++

      @ nLinRel,000 SAY "|---------------------------------------------------------------------------------------------------------------------------------|"
      nLinRel++

      @ nLinRel,000 SAY "|  Data "
      @ nLinRel,008 SAY cHoraPedi 
      @ nLinRel,080 SAY "Emissao: "+DTOC(MOVTOSH->DTEMISSAO)
      @ nLinRel,130 SAY "|"
      nLinRel++

      @ nLinRel,000 SAY "|---------------------------------------------------------------------------------------------------------------------------------|"
      nLinRel++

      @ nLinRel,000 SAY "|  Cliente "
      @ nLinRel,012 SAY CLIENTES->CODCLI + " " + CLIENTES->RAZAO
      @ nLinRel,091 SAY "|"
      @ nLinRel,130 SAY "|"
      nLinRel++ 

      @ nLinRel,000 SAY "|  Ender."
      @ nLinRel,012 SAY CLIENTES->ENDERECO + " " + CLIENTES->BAIRRO
      @ nLinRel,091 SAY "|  Pedido para Emissao de Nota-Fiscal  |"
      nLinRel++

      @ nLinRel,000 SAY "|  C.G.C"
 
*--- Cgc ou Cpf
      IF LEN(ALLTRIM(CLIENTES->CGCCPF)) = 11 
         @ nLinRel,012 SAY CLIENTES->CGCCPF  PICT "@R 999.999.999-99"
      ELSE
         @ nLinRel,012 SAY CLIENTES->CGCCPF  PICT "@R 99.999.999/9999-99"
      ENDIF
 
      @ nLinRel,032 SAY "Ins.Est" 
      @ nLinRel,040 SAY CLIENTES->IEST
      @ nLinRel,063 SAY CLIENTES->CIDADE
      @ nLinRel,088 SAY CLIENTES->ESTADO
      @ nLinRel,091 SAY "|         Nao Tem Valor Fiscal         |"
      @ nLinRel,091 SAY "          Nao Tem Valor Fiscal          "
      nLinRel++

      @ nLinRel,000 SAY "|-------+--------+----+----+---------------------------------------+----------------+------------------------+--------------------|"
      nLinRel++

      @ nLinRel,000 SAY "|  Cod  | Quant. | Un | Fp | Discriminacao                         |   Embalagem    |      P. Unitario       |        Total       |"
      nLinRel++

      @ nLinRel,000 SAY "|-------+--------+----+----+---------------------------------------+----------------+------------------------+--------------------|"
      nLinRel++

      nPesLiq = 0
      nPesBru = 0
      nQtdit  = 0
            
      SELECT MOVTOS
      DO WHILE MOVTOSH->CLIE_FORNE = MOVTOS->CLIE_FORNE .AND. MOVTOSH->NUMERODOC=MOVTOS->NUMERODOC .AND. .NOT. EOF()

         IF CODMOV <> "20"   
            SKIP
            LOOP
         ENDIF

         IF MOVTOS->QUANTIDADE > 0            

            @ nLinRel,000 SAY "|"
            @ nLinRel,002 SAY MOVTOS->ITEM PICT "99999"
            @ nLinRel,008 SAY "|"
            @ nLinRel,009 SAY MOVTOS->QUANTIDADE PICT "9999999"

            SELECT ITENS
            SET ORDER TO 1
            SEEK MOVTOS->ITEM
            SET ORDER TO 2
 
            @ nLinRel,017 SAY "|"               
            @ nLinRel,019 SAY ITENS->UNIDADE
            @ nLinRel,022 SAY "|"
            @ nLinRel,024 SAY MOVTOS->FORMAPGTO
            @ nLinRel,027 SAY "|"
            @ nLinRel,029 SAY LEFT(ITENS->DESCRICAO,37)
            @ nLinRel,067 SAY "|"
            @ nLinRel,068 SAY ITENS->REFERENCIA
            @ nLinRel,084 SAY "|"
            @ nLinRel,089 SAY MOVTOS->VALOR PICT "@E 999,999,999.99"
            @ nLinRel,109 SAY "|"
            @ nLinRel,111 SAY MOVTOS->VALOR*MOVTOS->QUANTIDADE;
               PICT "@E 9,999,999,999.99"
            nQtdit  = nQtdit + MOVTOS->QUANTIDADE
            @ nLinRel,130 SAY "|"
            @ nLinRel,131 SAY IIF(ITENS->GENERO="S","S","")
            nLinRel++

            nPesLiq = nPesLiq + ITENS->PESLIQUIDO * MOVTOS->QUANTIDADE
            nPesBru = nPesBru + ITENS->PESBRUTO   * MOVTOS->QUANTIDADE

         ENDIF

*** PROXIMO
         SELECT MOVTOS
         SKIP
               
      ENDDO

      IF MOVTOSH->VALORTOTAL > 0
         @ nLinRel,000 SAY "|-------+--------+----+----+---------------------------------------+----------------+------------------------+--------------------|"
         nLinRel++
         @ nLinRel,000 SAY "|Tot it.:                                                                           |                        |                    |"
         @ nLinRel,010 SAY nQtdit            PICT "999999"
         nLinRel++
         @ nLinRel,000 SAY "| Obs:                                                                              |        T O T A L       |"
         @ nLinRel,111 SAY MOVTOSH->VALORTOTAL+MOVTOSH->DESCONTO ;
            PICT "@E 9,999,999,999.99"
         @ nLinRel,130 SAY "|"
         nLinRel++

         @ nLinRel,000 SAY "|                                                                                   +---------------------------------------------|"
         nLinRel++

*** HOUVE DESCONTO?
         IF MOVTOSH->DESCONTO > 0
            @ nLinRel,000 SAY "|                                                                                   |     D E S C O N T O    |"
            @ nLinRel,115 SAY MOVTOSH->DESCONTO PICT "@E 9,999,999.99"
            @ nLinRel,130 SAY "|"
            nLinRel++

            @ nLinRel,000 SAY "|                                                                                   +---------------------------------------------|"
            nLinRel++
         ENDIF

         @ nLinRel,000 SAY "|                                                                                   |    A    P A G A R      |"
         @ nLinRel,111 SAY MOVTOSH->VALORTOTAL PICT ;
            "@E 9,999,999,999.99"
         @ nLinRel,130 SAY "|"
         nLinRel++

         @ nLinRel,000 SAY "|                                                                                   +---------------------------------------------|"

         cPesox = "PESOS.."
         IF nPesLiq > 0
            cPesox = cPesox + "Liq"+ALLTRIM(TRANSFORM(nPesLiq,;
               "@E 999999.999")) + " "
         ENDIF
         IF nPesBru > 0
            cPesox = cPesox + "Bruto:" + ALLTRIM(TRANSFORM(nPesBru, ;
            "@E 999999.999"))
         ENDIF
         @ nLinRel,002 SAY cPesox
         nLinRel++

         SELECT FUNCIONA
         SET ORDER TO 1
         SEEK MOVTOSH->VENDEDOR
         SET ORDER TO 2
         
         @ nLinRel,000 SAY "| Vendedor :  "
         @ nLinRel,014 SAY FUNCIONA->NOMEFUNC
         @ nLinRel,066 SAY "Comprador:"
         @ nLinRel,078 SAY CLIENTES->FANTASIA
         @ nLinRel,130 SAY "|"            
         nLinRel++

         @ nLinRel,000 SAY "+---------------------------------------------------------------------------------------------------------------------------------+"
         nLinRel++

*** DESCREVO FORMA DE PAGAMENTO
         SET DEVI TO SCREEN
         SELECT 0
         FABREA("TITLREC",.F.,gDiret)
         cIndice1 = gDiret + "RECNUMX1" 
         SET INDEX TO &cIndice1
         SEEK CLIENTES->CODCLI+STRZERO(nNumerodoc,6) SOFTSEEK
         SET DEVI TO PRINT


         @ nLinRel,001 SAY "Forma Pg. N§ Titulo/Pc Dt.Vencto     Valor "
         nLinRel++
         @ nLinRel,001 SAY "-------------------------------------------"
         nLinRel++

         DO WHILE CLIENTES->CODCLI = CODCLI .AND. NUMERODOC = STRZERO(nNumerodoc,6)

            IF "A Vista" $ OBS
               @ nLinRel,001 SAY "A Vista"
            ELSE
               @ nLinRel,001 SAY "A Prazo"            
            ENDIF   
               @ nLinRel,14 SAY NUMERODOC PICT "999999"
               @ nLinRel,20 SAY "/"+PARCELA
               @ nLinRel,25 SAY DTVENCTO PICT "@D"
               @ nLinRel,34 SAY VALTIT PICT "@E 99,999.99"
               nLinRel++
            SKIP
         ENDDO

         SELECT TITLREC
         USE

      ELSE
         @ nLinRel,000 SAY "+---------------------------------------------------------------------------------------------------------------------------------+"
         nLinRel++
         @ nLinRel,000 SAY "| P E D I D O  C O M  V A L O R  Z E R O                                            |     D E S C O N T O    |                    |"
         nLinRel++
         @ nLinRel,000 SAY "+---------------------------------------------------------------------------------------------------------------------------------+"
      ENDIF

      nLinRel++
      nLinRel++
      nLinRel++

*** IMPRIME LINHAS C/ SALTO 
      SELECT LOCALEST
      SET ORDER TO 1
      SEEK gCodEmp
         
      @ 000,000 SAY iEjeta
      SET DEVICE TO SCREEN

*** LIMPA MENSAGEM
      LIMPA()
      
   ENDIF      

ENDIF

RETURN

*** PROCEDURE EST1100K
**  AUTORIZACAO
*
FUNCTION EST1100K

lSenha = .F.

SELECT FUNCIONA
nRegVendedor = RECNO()
SAVE SCREEN TO tTelDesc

*** TELA
@ 06,19 CLEAR TO 14,57
MOLD(06,19,14,57)
@ 08,22 SAY "Desconto/Muda Vncimento"
@ 10,22 SAY "Autorizador ..:"
@ 12,22 SAY "Senha.........:"

DO WHILE .T.

*** PEDE CODIGO DO AUTORIZADOR
   nAutoriz = 0
   IF .NOT. INT1400DA(@nAutoriz,10,38)
      LOOP
   ENDIF   

*** ESC?
   IF LASTKEY() = 27
      EXIT
   ENDIF

*** PROCURA NO ARQUIVO DE VENDEDORES
   SELECT FUNCIONA
   SET ORDER TO 1
   SEEK STRZERO(nAutoriz,2)
   SET ORDER TO 2
   IF EOF()
      TECLA("Usuario nao cadastrado.  Tecle [ENTER]")
      LIMPA()
      LOOP
   ENDIF
   cSenhaAutoriz = DECRIPTO(SENHAFUNC)

*** TESTA NIVELFUNC
   IF FACESSO("NIVELDESC",.T.,STRZERO(nAutoriz,2)) = .F.
      FOR P = 1 TO 3
         TONE(600,3)
      NEXT
      LIMPA()
      TECLA("Nao Autorizado.  [ENTER]")
      LIMPA()
   ELSE

*** PEDE SENHA
      FOR XI = 1 TO 3
         cSenha = SPACE(08)
         DO PASSWORD WITH cSenha,8,12,38,.T.
         IF LASTKEY() = 27
            EXIT
         ENDIF
         IF cSenha = cSenhaAutoriz
            lSenha = .T.
            EXIT
         ELSE
            TECLA("Senha Invalida.")
            LIMPA()
            LOOP
         ENDIF
      NEXT
   
   ENDIF

*** FORCA SAIDA
   EXIT

ENDDO

*** SENHA VALIDA?
RESTORE SCREEN FROM tTelDesc

SELECT FUNCIONA
GOTO nRegVendedor

RETURN lSenha

*** FUNCTION CEC2200G
**  MOSTRA SITUACAO DO CLIENTE
*
PROCEDURE CEC2200G

nSaldo = 0
nSaldo = CLIENTES->LIM_CRED - CLIENTES->SALDOATUAL
      
@ 19,64 SAY nSaldo PICT "@E 99,999,999.99"
LIMPA()

RETURN nSaldo

*** PROCEDURE CEC2900I
**  IMPRIME RELATORIO
*
PROCEDURE CEC2900I
PARAMETERS cNumero,cCodcli

*** POSICIONA EM MOVTOSH
lFaz = .T.
SELECT MOVTOSH
SET ORDER TO 2
SEEK "20" + cNumero + cCodcli
IF EOF()
   TECLA("Pedido nao cadastrado")
   lFaz = .F.
ELSE 

*** POSICIONA EM MOVTOS
   SELECT MOVTOS
   SET ORDER TO 2
   SEEK "20"+MOVTOSH->CLIE_FORNE+MOVTOSH->NUMERODOC SOFTSEEK

   IF .NOT. EOF()
            
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

*** CABECALHO
      cHoraPedi = DTOC(DATE()) + " * " + TIME() 
      TESTAPRINT()
      SET DEVICE TO PRINT
      nLinRel = PROW()
      @ PROW(),000 SAY iComp
      gColuna = 1
      nLinRel++

      @ nLinRel,000 SAY "+---------------------------------------------------------------------------------------------------------------------------------+"
      nLinRel++
      @ nLinRel,000 SAY "|"
      @ nLinRel,002 SAY gEmpresa
      @ nLinRel,057 SAY "P E D I D O"
      @ nLinRel,108 SAY "No."
      @ nLinRel,119 SAY MOVTOSH->NUMERODOC PICT "99999999"
      @ nLinRel,130 SAY "|"
      nLinRel++

      @ nLinRel,000 SAY "|"
      @ nLinRel,002 SAY LOCALEST->LO_DESCR

      @ nLinRel,130 SAY "|"
      nLinRel++

      @ nLinRel,000 SAY "|---------------------------------------------------------------------------------------------------------------------------------|"
      nLinRel++

      @ nLinRel,000 SAY "|  Data "
      @ nLinRel,008 SAY cHoraPedi 
      @ nLinRel,042 SAY "Forma de pagamento"
 
      @ nLinRel,063 SAY TB_PAGTO->TBDESCRI
      @ nLinRel,112 SAY "Vencto"
      @ nLinRel,121 SAY MOVTOSH->DATA1 PICT "@D"
      @ nLinRel,130 SAY "|"
      nLinRel++
      @ nLinRel,000 SAY "|---------------------------------------------------------------------------------------------------------------------------------|"
      nLinRel++

      @ nLinRel,000 SAY "|  CLiente "
      @ nLinRel,012 SAY CLiENTES->CODCLI + " " + CLiENTES->RAZAO+" Fone: "+ALLTRIM(CLIENTES->PREFIXO)+"-"+ALLTRIM(CLIENTES->TEL)
      @ nLinRel,091 SAY "|"
      @ nLinRel,130 SAY "|"
      nLinRel++ 

      @ nLinRel,000 SAY "|  Ender."
      @ nLinRel,012 SAY CLiENTES->ENDERECO + " " + CLiENTES->BAIRRO
      @ nLinRel,091 SAY "|  Pedido para Emissao de Nota-Fiscal  |"
      nLinRel++

      @ nLinRel,000 SAY "|  C.G.C"
 
*--- Cgc ou Cpf
      IF LEN(ALLTRIM(CLiENTES->CGCCPF)) = 11 
         @ nLinRel,012 SAY CLiENTES->CGCCPF  PICT "@R 999.999.999-99"
      ELSE
         @ nLinRel,012 SAY CLiENTES->CGCCPF  PICT "@R 99.999.999/9999-99"
      ENDIF
 
      @ nLinRel,032 SAY "Ins.Est" 
      @ nLinRel,040 SAY CLiENTES->IEST
      @ nLinRel,063 SAY CLiENTES->CIDADE
      @ nLinRel,088 SAY CLiENTES->ESTADO
      @ nLinRel,091 SAY "|         Nao Tem Valor Fiscal         |"
      @ nLinRel,091 SAY "          Nao Tem Valor Fiscal          "
      nLinRel++

      @ nLinRel,000 SAY "|-------+-----------+----+-----------------------------------+------------------+----------------+--------+----------+------------|"
      nLinRel++

      @ nLinRel,000 SAY "| Item  | Codigo    | Un | Discriminacao                     |     Embalagem    |   Referencia   | Quant. | P. Unit. |   Total    |"
      nLinRel++

      @ nLinRel,000 SAY "|-------+-----------+----+-----------------------------------+------------------+----------------+--------+----------+------------|"
      nLinRel++

      nPesLiq = 0
      nPesBru = 0
      SET ORDER TO 2
      SEEK "20"+MOVTOSH->CLIE_FORNE+MOVTOSH->NUMERODOC SOFTSEEK            

      SELECT MOVTOS
      DO WHILE MOVTOSH->CLIE_FORNE = MOVTOS->CLIE_FORNE .AND. MOVTOSH->NUMERODOC=MOVTOS->NUMERODOC .AND. .NOT. EOF()

         IF CODMOV <> "20"   
            SKIP
            LOOP
         ENDIF

         IF MOVTOS->QUANTIDADE > 0            

            @ nLinRel,000 SAY "|       |           |    |                                   |                  |                |        |          |            |"

            SELECT ITENS
            SET ORDER TO 1
            SEEK MOVTOS->ITEM
            SET ORDER TO 2

            @ nLinRel,002 SAY MOVTOS->ITEM PICT "99999" 
            @ nLinRel,009 SAY ITENS->REFERENCIA PICT "@S13"
            @ nLinRel,022 SAY ITENS->UNIDADE
            @ nLinRel,027 SAY LEFT(ITENS->DESCRICAO,33)
            @ nLinRel,063 SAY ITENS->QTDUNID
            @ nLinRel,082 SAY FCONVREF(MOVTOS->VALOR)
            @ nLinRel,098 SAY MOVTOS->QUANTIDADE PICT "9999999"
            @ nLinRel,108 SAY MOVTOS->REFERENCIA PICT "@E 9,999.99"
            @ nLinRel,119 SAY MOVTOS->CUSTTOT PICT "@E 999,999.99"
            @ nLinRel,131 SAY IIF(ITENS->GENERO="S","S","")
            nLinRel++
            nPesLiq = nPesLiq + ITENS->PESLiQUIDO * MOVTOS->QUANTIDADE
            nPesBru = nPesBru + ITENS->PESBRUTO   * MOVTOS->QUANTIDADE

         ENDIF

*** PROXIMO
         SELECT MOVTOS
         SKIP
               
      ENDDO

      IF MOVTOSH->VALORTOTAL > 0
         @ nLinRel,000 SAY "|-------+-----------+--------+----+-----------------------------------+-------------+----+----------------+--+-------+------------|"
         nLinRel++

         @ nLinRel,000 SAY "| Obs: A Mercadoria Segue Por conta e Risco do                                      |        T O T A L       |                    |"
         @ nLinRel,117 SAY MOVTOSH->CUSTTOT PICT "@E 9,999,999.99"
         nLinRel++
         @ nLinRel,000 SAY "|      Cliente.                                                                     +------------------------+--------------------|"
         nLinRel++

         @ nLinRel,000 SAY "|                                                                                   |    A    P A G A R      |"
         @ nLinRel,117 SAY MOVTOSH->CUSTTOT PICT "@E 9,999,999.99"
         @ nLinRel,130 SAY "|"
         nLinRel++
         @ nLinRel,000 SAY "|                                                                                   +------------------------+--------------------|"
         cPesox = "PESOS.."
         IF nPesLiq > 0
            cPesox = cPesox + "Liq"+ALLTRIM(TRANSFORM(nPesLiq,;
               "@E 999999.999")) + " "
         ENDIF
         IF nPesBru > 0
            cPesox = cPesox + "Bruto:" + ALLTRIM(TRANSFORM(nPesBru, ;
            "@E 999999.999"))
         ENDIF
         @ nLinRel,002 SAY cPesox
         nLinRel++


         SELECT FUNCIONA
         SET ORDER TO 1
         SEEK MOVTOSH->VENDEDOR
         SET ORDER TO 2
           
         @ nLinRel,000 SAY "| Vendedor :  "
//         @ nLinRel,014 SAY FUNCIONA->NOMEFUNC
         @ nLinRel,066 SAY "Comprador:"
         @ nLinRel,078 SAY CLiENTES->FANTASIA
         @ nLinRel,130 SAY "|"            
         nLinRel++

         @ nLinRel,000 SAY "+---------------------------------------------------------------------------------------------------------------------------------+"
         nLinRel++
         @ nLinRel,000 SAY CHR(14) + FCONVREF(MOVTOSH->VALORTOTAL)
         nLinRel++
         nLinRel++

         @ nLinRel,000 SAY "-----------------------------------------------------------------------------------------------------------------------------------"
         nLinRel++

         @ nLinRel,000 SAY "+-------------------------------------------------------------------------------------------------------------- ESTOQUE ----------+"
         nLinRel++
      
      ELSE
         @ nLinRel,000 SAY "P E D I D O  C O M  V A L O R  Z E R O"

      ENDIF

      @ 000,000 SAY iEjeta

      SET DEVICE TO SCREEN

*** LIMPA MENSAGEM
      LIMPA()
      
   ENDIF      

ENDIF

RETURN


*** PROCEDURE EST1100M
**  IMPRIME RELATORIO
*
PROCEDURE EST1100M
PARAMETERS cNumero,cCodcli

*** POSICIONA EM MOVTOSH
lFaz = .T.
SELECT MOVTOSH
SET ORDER TO 2
SEEK "20"+cNumero+cCodcli
IF EOF()
   TECLA("Pedido nao cadastrado")
   lFaz = .F.
ELSE 

*** POSICIONA EM MOVTOS
   SELECT MOVTOS
   SET ORDER TO 2
   SEEK "20"+MOVTOSH->CLIE_FORNE+MOVTOSH->NUMERODOC SOFTSEEK
   
   IF .NOT. EOF()
            
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

*** CABECALHO
      cHoraPedi = DTOC(DATE()) + " * " + TIME() 
      TESTAPRINT()
      SET DEVICE TO PRINT
      nLinRel = PROW()
      cHor  = DTOC(DATE()) + " * " + SUBSTR(TIME(),1,5)
      gColuna = 1
      nLinRel++
      @ nLinRel,000 SAY " "
//      ETIQUETA()      
      @ PROW(),000 SAY iComp 
//      @ nLinRel,000 SAY iExpandi+PADC(gEmpresa,22)+iExpandf
      nLinRel++
      IF TB_PAGTO->SITUACAO = "L"
         @ nLinRel,003 SAY iExpandi+"* PAGAMENTO A VISTA *" +iExpandf
      ELSE
         @ nLinRel,003 SAY iExpandi+"* PAGAMENTO A PRAZO *" +iExpandf      
      ENDIF
      nLinRel++   
      @ nLinRel,000 SAY iNormbema
      nLinRel++   
      @ nLinRel,001 SAY "PEDIDO No"
      @ nLinRel,011 SAY MOVTOSH->NUMERODOC PICT "999999"
      @ nLinRel,028 SAY "DP. " + cHoraPedi 
      nLinRel++   
      @ nLinRel,000 SAY REPLICATE("-",052)
      nLinRel++
      @ nLinRel,001 SAY "Cliente"
      @ nLinRel,009 SAY CLIENTES->CODCLI + " " + SUBSTR(CLIENTES->RAZAO,1,30)
      nLinRel++ 
      @ nLinRel,001 SAY "Fone.:"+CLIENTES->TEL
      nLinRel++
      @ nLinRel,001 SAY "Ender."
      @ nLinRel,008 SAY CLIENTES->ENDERECO 
      nLinRel++
      @ nLinRel,001 SAY "Bairro " + CLIENTES->BAIRRO+" "+CLIENTES->CIDADE+" "+CLIENTES->ESTADO
      nLinRel++

*--- Cgc ou Cpf
      IF LEN(ALLTRIM(CLIENTES->CGCCPF)) = 11 
         @ nLinRel,001 SAY "CPF:"
         @ nLinRel,006 SAY CLIENTES->CGCCPF  PICT "@R 999.999.999-99"
      ELSE
         @ nLinRel,001 SAY "CGC:"
         @ nLinRel,006 SAY CLIENTES->CGCCPF  PICT "@R 99.999.999/9999-99"
      ENDIF
      @ nLinRel,025 SAY "Ins.Est" 
      @ nLinRel,033 SAY CLIENTES->IEST
      nLinRel++
      @ nLinRel,000 SAY REPLICATE("-",052)
      nLinRel++
      @ nLinRel,001 SAY "Cod.  Un Discriminacao    Qtd. Prc.Unit Sub-Total"
      nLinRel++
      @ nLinRel,000 SAY REPLICATE("-",052)
      nLinRel++

      SELECT MOVTOS
      DO WHILE MOVTOSH->CLIE_FORNE = MOVTOS->CLIE_FORNE .AND. MOVTOSH->NUMERODOC=MOVTOS->NUMERODOC .AND. .NOT. EOF()

         IF CODMOV <> "20"   
            SKIP
            LOOP
         ENDIF

         IF MOVTOS->QUANTIDADE > 0            

            SELECT ITENS
            SET ORDER TO 1
            SEEK MOVTOS->ITEM
            SELECT MOVTOS

            @ nLinRel,001 SAY MOVTOS->ITEM PICT "99999"
            @ nLinRel,007 SAY ITENS->UNIDADE PICT "!!"
            @ nLinRel,010 SAY LEFT(ITENS->DESCRICAO,37) PICT "@S15"
            @ nLinRel,026 SAY MOVTOS->QUANTIDADE PICT "@E 9,999"
            @ nLinRel,032 SAY MOVTOS->VALOR PICT "@E 9,999.99"
            @ nLinRel,041 SAY MOVTOS->VALOR*MOVTOS->QUANTIDADE;
               PICT "@E 99,999.99"
            nLinRel++

         ENDIF

*** PROXIMO
         SELECT MOVTOS
         SKIP
               
      ENDDO

      IF MOVTOSH->VALORTOTAL > 0
         @ nLinRel,039 SAY  REPLICATE("-",11)
         nLinRel++
         @ nLinRel,001 SAY  "Total Pedido....."
         @ nLinRel,040 SAY MOVTOSH->VALORTOTAL+MOVTOSH->DESCONTO ;
            PICT "@E 999,999.99"
         nLinRel++

*** HOUVE DESCONTO?
         IF MOVTOSH->DESCONTO > 0
            @ nLinRel,001 SAY "Desconto........."
            @ nLinRel,028 SAY "(-)" 
            @ nLinRel,040 SAY MOVTOSH->DESCONTO PICT "@E 999,999.99"
            nLinRel++
         ENDIF
 
         @ nLinRel,039 SAY  REPLICATE("-",11)
         nLinRel++
         @ nLinRel,001 SAY "Total a Pagar...."
         @ nLinRel,001 SAY "Total a Pagar...."
         @ nLinRel,040 SAY MOVTOSH->VALORTOTAL PICT ;
            "@E 999,999.99"
         @ nLinRel,040 SAY MOVTOSH->VALORTOTAL PICT ;
            "@E 999,999.99"
         nLinRel++
         @ nLinRel,000 SAY REPLICATE("-",052)
         nLinRel++

         SELECT FUNCIONA
         SET ORDER TO 1
         SEEK MOVTOSH->VENDEDOR
         SELECT MOVTOSH
         
         @ nLinRel,001 SAY "Vendedor : "+MOVTOSH->VENDEDOR+" "+FUNCIONA->NOMEFUNC
         nLinRel++
         @ nLinRel,000 SAY REPLICATE("-",052)
         nLinRel++

         nCentro = CENTER(">>>>>>>>>>>>>> SEM VALOR FISCAL <<<<<<<<<<<<<<",52)
         @ nLinRel,nCentro SAY ">>>>>>>>>>>>>> SEM VALOR FISCAL <<<<<<<<<<<<<<"
         nLinRel++

*** DESCREVO FORMA DE PAGAMENTO

         SELECT 0
         FABREA("TITLREC",.F.,gDiret)
         cIndice1 = gDiret + "RECNUMX1" 
         SET INDEX TO &cIndice1
         SEEK CLIENTES->CODCLI+STRZERO(nNumerodoc,6) SOFTSEEK

         @ nLinRel,001 SAY "Forma Pg. N§ Titulo/Pc Dt.Vencto     Valor "
         nLinRel++
         @ nLinRel,001 SAY "-------------------------------------------"
         nLinRel++
         lTemprazo = .F.
         DO WHILE CLIENTES->CODCLI = CODCLI .AND. NUMERODOC = STRZERO(nNumerodoc,6)

            IF "A Vista" $ OBS
               @ nLinRel,001 SAY "A Vista"
            ELSE
               @ nLinRel,001 SAY "A Prazo"            
               lTemprazo = .T.
            ENDIF   
               @ nLinRel,14 SAY NUMERODOC PICT "999999"
               @ nLinRel,20 SAY "/"+PARCELA
               @ nLinRel,25 SAY DTVENCTO PICT "@D"
               @ nLinRel,34 SAY VALTIT PICT "@E 99,999.99"
               nLinRel++
            SKIP
         ENDDO

         SELECT TITLREC
         USE

         IF lTemprazo

            nCentro = CENTER("Reconheco como liquida e certa a divida acima,",52)
            @ nLinRel,nCentro SAY "Reconheco como liquida e certa a divida acima,"
            nLinRel++
            nLinRel++
            nLinRel++
            nCentro = CENTER("-----------------------------------",52)
            @ nLinRel,nCentro SAY "-----------------------------------"
            nLinRel++
            nCentro = CENTER("Assinatura",52)
            @ nLinRel,nCentro SAY "Assinatura"
            nLinRel++
            nLinRel++

         ENDIF

         nLinRel++
         nCentro = CENTER("Confira a Mercadoria!!",52)
         @ nLinRel,nCentro SAY "Confira a Mercadoria!!"
         nLinRel++
         nCentro = CENTER("Nao Aceitamos Reclamacoes Posteriores",52)
         @ nLinRel,nCentro SAY "Nao Aceitamos Reclamacoes Posteriores"
         nLinRel++
      ELSE
         @ nLinRel,000 SAY REPLICATE("-",052)
         nLinRel++
         @ nLinRel,001 SAY "P E D I D O  C O M  V A L O R  Z E R O"
         nLinRel++
         @ nLinRel,000 SAY REPLICATE("-",052)
      ENDIF

      nLinRel = nLinRel + 15
      @ nLinRel,000 SAY iEjeta

      SET DEVICE TO SCREEN

*** LIMPA MENSAGEM
      LIMPA()
      
   ENDIF      

ENDIF

RETURN


*******************************
FUNCTION FFORMA(nForma)
*******************************

lRetorno = .T.

IF FACESSO("FFORMAAC",.T.,gCodfunc) = .T.
   SELECT TB_PAGTO
   SET ORDER TO 1
   SEEK STRZERO(nForma,2)
   IF SITUACAO <> "L"
      lRetorno = .F.
   ENDIF
ENDIF

IF lRetorno = .F.
   FOR FF = 1 TO 3
      TONE(600,3)
   NEXT
   LIMPA()
   TECLA("Funcionario So' Pode Digitar Pedidos A' Vista!")
   LIMPA()
ENDIF   
   
RETURN lRetorno 

*************************
FUNCTION FTESTEMIS()
*************************

IF gContcaixa = "S"
   IF dDtemissao <> gDt_Base
      IF FACESSO("NIVAPAGAR",.T.,gCodfunc) = .T.
         TONE(750,3)
         LIMPA()
         TECLA("Lancamento Nao Entra no Caixa. Data de Emissao Diferente do Caixa -> "+DTOC(gDt_Base))
         LIMPA()
         RETURN .T.
      ELSE
         TONE(750,3)
         LIMPA()
         TECLA("Data de Emissao Diferente da Abertura do Caixa -> "+DTOC(gDt_Base))
         LIMPA()
         RETURN .F.
      ENDIF
   ELSE
      RETURN .T.
   ENDIF
ELSE
   IF dDtemissao > DATE()
      TONE(750,3)
      LIMPA()
      TECLA("Data de Emissao Maior Que Data de Hoje!!")
      LIMPA()
      RETURN .F.
   ELSE
      RETURN .T.
   ENDIF
ENDIF

RETURN .T.

***********************
FUNCTION FTESTANUM()
***********************
* TESTA SE PODE NUMERACAO AUTOMATICA DO PEDIDO

IF nNumerodoc = 0 .AND. gNumeraut = .F.
   FOR I = 1 TO 3
      TONE(600,3)
   NEXT
   LIMPA()
   TECLA("Numero do Pedido Nao Pode Ser ZERO")   
   LIMPA()
   RETURN .F.
ENDIF

RETURN .T.   

*---------------------------------------------------------------------*
STATIC FUNCTION ERRBOX(cErrMsg, bOldHandler)
*
*                 cErrMsg = Mensagem de Erro
*                 bOldHandler = Tabela de Opcoes
*
LOCAL nChoice, i, nStrgLen
LOCAL cOldColor, cOldScreen

cOldColor := SETCOLOR("W/R+, W+/B")
cOldScreen := SAVESCREEN(10,0,14,79)

@ 10,00 TO 14, 79 DOUBLE
@ 11,01,13,78 BOX SPACE(09)
@ 11,02 SAY cErrMsg

nStrgLen := LEN(aOptions * 3)
AEVAL(aOptions, { |arr| nStrgLen += LEN(arr) } )
nStrgLen := (78 - nStrgLen) / 2

FOR i := 1 TO LEN(aOptions)
   @ 13, nStrgLen PROMPT aOptions[i]
   nStrgLen += LEN(aOptions[i] + 3)  
NEXT

nChoice := 1   
MENU TO nChoice
SETCOLOR(cOldColor)
RESTSCREEN(10,0,14,79, cOldScreen)

RETURN nChoice
*----------------------------------------------------Fim do programa--*

**************************
FUNCTION FEROIMP(cBuffer)
**************************
** FUNCAO QUE ENVIA PARA IMPRESSORA E RETORNA O ERRO 

lRetorno = .T.

DO WHILE .T.

   FWRITE(fSerial,@cBuffer,LEN(cBuffer))  // ENVIO PARA A IMPRESSORA

   BUFFER := ACK := NAK := ST1 := ST2 := SPACE(01)

   FOR CONTADOR1 := 1 TO 3

      FREAD(fSerial,@BUFFER,1)
   
      DO CASE
         CASE CONTADOR1 = 1
              DO CASE
                 CASE ASC(BUFFER) = 21 /* RETORNO EM CARACTER 21D=15H=nak */
                     LIMPA()
                     TECLA("Atencao... a impressora retornou 21d=15h=NAK")
                     LIMPA()
                     FCLOSE(fSerial)
                     RETURN .F.
                CASE ASC(BUFFER) = 06
                     ACK := TRANSFORM(ASC(BUFFER),"99")
                OTHERWISE
                     LIMPA()
                     TECLA("Atencao... provavelmente o driver nao foi carregado !")
                     LIMPA()
                     FCLOSE(fSerial)
                     RETURN .F.
             ENDCASE
         CASE CONTADOR1 = 2
             ST1 := TRANSFORM(ASC(BUFFER),"999")
         CASE CONTADOR1 = 3
             ST2 := TRANSFORM(ASC(BUFFER),"999")
      ENDCASE

   NEXT CONTADOR1

   Bit_st1:=Bit_st2:=Bit_ack:=0
   Bit_ack=Val(ACK)
   Bit_st1=Val(ST1)
   Bit_st2=Val(ST2)

   IF Bit_ack <> 6
      cErro= "Problemas ao Enviar o Comando a Impressora Devolveu 21"
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

** TUDO CERTO

   IF Bit_ack+Bit_st1+Bit_st2 = 6
      lRetorno = .T.
      EXIT
   ENDIF
      
   IF Bit_st1 >= 128
      cErro = "fim de papel"
      Bit_st1=Bit_st1-128 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st1 >= 64
      cErro = "pouco papel"
      Bit_st1=Bit_st1-64 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st1 >= 32
      cErro = "erro no rel¢gio"
      Bit_st1=Bit_st1-32 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st1 >= 16
      cErro = "impressora em erro"
      Bit_st1=Bit_st1-16 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st1 >= 8
      cErro = "primeiro dado de CMD nÆo foi ESC (1Bh)"
      Bit_st1=Bit_st1-8 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st1 >= 4
      cErro = "comando inexistente"      
      Bit_st1=Bit_st1-4 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st1 >= 2
      cErro = "cupom aberto"
      Bit_st1=Bit_st1-2 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st1 >= 1
      cErro = "n£mero de parƒmetros de CMD inv lido"
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

      
   IF Bit_st2 >= 128
      cErro = "tipo de parƒmetro de CMD inv lido"
      Bit_st2=Bit_st2-128 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st2 >= 64
      cErro = "Mem¢ria fiscal lotada"
      Bit_st2=Bit_st2-64 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF


   IF Bit_st2 >= 32
      cErro = "erro na Mem¢ria RAM CMOS nÆo vol til"
      Bit_st2=Bit_st2-32 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st2 >= 16
      cErro = "al¡quota nÆo programada"
      Bit_st2=Bit_st2-16 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st2 >= 8
      cErro = "capacidade de al¡quotas program veis lotada"
      Bit_st2=Bit_st2-8 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st2 >= 4
      cErro = "cancelamento nÆo permitido"
      Bit_st2=Bit_st2-4 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st2 >= 2
      cErro = "CGC/IE do propriet rio nÆo programados"
      Bit_st2=Bit_st2-2 
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

   IF Bit_st2 >= 1
      cErro = "comando nÆo executado"
      aOptions := {"Tenta Novamente","Desiste"}
      FOR NICA = 1 TO 3
          TONE(750,3)
      NEXT
      nChoice := ALERT(cErro,aOptions)
      DO CASE
      CASE nChoice == 1               
         lRetorno = .F.
         LOOP
      CASE nChoice == 2                
         lRetorno = .F.
         EXIT
      ENDCASE      
   ENDIF

ENDDO

RETURN lRetorno   

****************************************
FUNCTION FDESCRI()
****************************************

** RETORNA O NOME DA ITEM
LOCAL cDescri

SELECT ITENS
SET ORDER TO 1
SEEK ITEMTEMP->ITEM
SELECT ITEMTEMP
cDescri = ITENS->DESCRICAO

RETURN cDescri

**************************
FUNCTION FUNID()
**************************

@ 21,20 SAY ITENS->UNIDADE PICT "!!"

*** PROCURA SE JA' DIGITADO

SELECT ITEMTEMP
GO TOP

nQuantidade = 0

DO WHILE .NOT. EOF()

  IF STRZERO(nItem,5) = ITEM
     nQuantidade = QUANTIDADE 
     EXIT
  ENDIF

  SKIP

ENDDO

RETURN .T.

*********************
FUNCTION ABREGAV()
*********************
* ABRE GAVETA DE DINHEIRO

   fSerial := FOPEN(gComfisc,FO_READWRITE+FO_COMPAT)
   BUFFER = CHR(27)+CHR(251)+"22|1|"+CHR(27)
   IF FEROIMP(BUFFER) = .F.
      FCLOSE(fSerial)
      RETURN
   ENDIF

RETURN
*------------------------------------------------------------------------
FUNCTION LIMPA
*                          Limpa area de mensagens
*==========================

SETCOLOR(gCorsist)
@ 23,13 CLEAR
@ 24,50 SAY "DBINFORMATICA - 9975-9465"
SETCOLOR(gCormenu)
RETURN .T.
*------------------------------------------------------------------------
