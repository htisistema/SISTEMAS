***************************
* TELA DE APRESENTACAO
***************************
#include "inkey.ch"              // constantes de codigos das teclas

***************************
* TELA DE APRESENTACAO
***************************

FUNCTION tela(mtitulo,tip)
************************
LOCAL la,ca,lb,cb
IF tip = NIL
        la := 14
        ca := 00
        lb := 23
        cb := 79
        setcor(2)
        DEVPOS(la,ca);DEVOUT(PADC(mtitulo+' >>>'+mversao+'<<<',80))
ELSE
        la := 10
        ca := 00
        lb := 20
        cb := 79
        DEVPOS(la+9,ca+21);DEVOUT(PADC('CNPJ: 02.702.271/0001-14  - Insc.Mun.: 291.454-9',57))
ENDIF
SETCOLOR('N/W,X')
botao(la+1,ca,lb,cb)
*setcor(6)
setcor(5)
DEVPOS(la+02,ca+1);DEVOUT('⁄€€€⁄€€€ ⁄€€€€€€€ ⁄€€€€€€€')
DEVPOS(la+03,ca+1);DEVOUT('≥€€€≥€€€ ≥€€€ø€€€ ≥€€€ø€€€')
DEVPOS(la+04,ca+1);DEVOUT('≥€€€€€€€ ≥€€€€€€€ ≥€€€€€€Ÿ')
DEVPOS(la+05,ca+1);DEVOUT('≥€€€ø€€€ ≥€€€ø€€Ÿ ≥€€€ø€€€')
DEVPOS(la+06,ca+1);DEVOUT('≥€€€≥€€€ ≥€€€≥€€€ ≥€€€€€€€')
DEVPOS(la+07,ca+1);DEVOUT('¿ƒƒŸ¿ƒƒŸ ¿ƒƒŸ¿ƒƒŸ ¿ƒƒƒƒƒƒŸ')
DEVPOS(la+08,ca+1);DEVOUT(' SOLUCOES EM INFORMATICA  ')
SETCOLOR('N/W,X')
DEVPOS(la+02,ca+10);DEVOUT('⁄€€€€€€€')
DEVPOS(la+03,ca+10);DEVOUT('≥€€€ø€€€')
DEVPOS(la+04,ca+10);DEVOUT('≥€€€€€€€')
DEVPOS(la+05,ca+10);DEVOUT('≥€€€ø€€Ÿ')
DEVPOS(la+06,ca+10);DEVOUT('≥€€€≥€€€')
DEVPOS(la+07,ca+10);DEVOUT('¿ƒƒŸ¿ƒƒŸ')
setcor(2)
DEVPOS(la+2,ca+30);DEVOUT(PADC('>>> H R B   Solucoes em Informatica <<<',48),setcor(5))
DEVPOS(la+3,ca+30);DEVOUT(PADC('Rua Agenor Lopes, 292 - Sl.403 - Recife - PE.',48),setcor(2))
DEVPOS(la+4,ca+30);DEVOUT(PADC('Fone:(81)3325.6663 - (Tacaimbo)(81)3755.1489',48))
DEVPOS(la+5,ca+30);DEVOUT(PADC('Suporte: HÇlio:9975.5923 - 8835.1751',48))
DEVPOS(la+6,ca+30);DEVOUT(PADC('Edson: (81) 8835.1752 - Roberta:8835.1753',48))
DEVPOS(la+7,ca+30);DEVOUT(PADC('e-mail: helio@hrbinfo.com.br',48),setcor(5))
DEVPOS(la+8,ca+30);DEVOUT(PADC('Direito Copia (C) 1994-2006 da HRB Informatica',48),setcor(5))
IF tip <> NIL
        DEVPOS(la+9,ca+2);DEVOUT('SIAC - '+DTOC(directory('siac.exe') [1][3]),setcor(5))
        DEVPOS(la+9,ca+30);DEVOUT(PADC('CNPJ: 02.702.271/0001-14  - Insc.Mun.: 291.454-9',48))
ENDIF
RETURN .T.

********************************** F I M *********************************************
**********************************
* FUNCAO QUE ESCREVE POR EXTENSO
**********************************
*------------------------------
FUNCTION extenso(pValor,pMoeda)
//-----------------------------
// Objetivo..: Monta String c/Extenso de um Valor ** MENUSCULO **
// Parametros: pValor -> Valor
//             pMoeda -> Flag de Real
//---------------------------------------------------------------------------
LOCAL aGranP := {},;
      aGranS := {},;
      nTerno,;
      cNumero,;
      cNumInt,;
      cNumDec,;
      cTerno,;
      cExtTer,;
      cExtens,;
      nParte,;
      nGrand

// Verifica Parametros
pValor := ABS(pValor)
pMoeda := IF(pMoeda == NIL,.F.,pMoeda)

// Verifica Valor
IF pValor == 0
   RETURN ("")
ENDIF

// Pega Numero de Ternos do Numero
nTerno := INT((LENNUM(INT(pValor)) / 3))
nTerno := IF((nTerno * 3) != LENNUM(INT(pValor)),(nTerno + 1),nTerno)

// Divide Numero Inteiro/Decimal
cNumero := STRZERO(pValor,((nTerno * 3) + 3),2)
cNumInt := LEFT(cNumero,(nTerno * 3))
cNumDec := RIGHT(cNumero,2)

// Verifica Limite da Funcao
IF nTerno > 10
   RETURN ("VALOR NAO SUPORTADO PELA FUNCAO")
ENDIF

// Adiciona Grandesas
AADD(aGranP,"")
AADD(aGranP,"mil ")
AADD(aGranP,"milhoes ")
AADD(aGranP,"bilhoes ")
AADD(aGranP,"trilhoes ")
AADD(aGranP,"quatrilhoes ")
AADD(aGranP,"quintilhoes ")
AADD(aGranP,"sextilhoes ")
AADD(aGranP,"septilhoes ")
AADD(aGranP,"octilhoes ")
AADD(aGranP,"nonilhoes ")
AADD(aGranS,"")
AADD(aGranS,"mil ")
AADD(aGranS,"milhao ")
AADD(aGranS,"bilhao ")
AADD(aGranS,"trilhao ")
AADD(aGranS,"quatrilhao ")
AADD(aGranS,"quintilhao ")
AADD(aGranS,"sextilhao ")
AADD(aGranS,"septilhao ")
AADD(aGranS,"octilhao ")
AADD(aGranS,"nonilhao ")

// Laco de Extenso dos Ternos Inteiros
cExtens := ""
nGrand  := nTerno
FOR nParte := 1 TO (nTerno * 3) STEP 3

   // Verifica Terno NAO Nulo
   IF (cTerno := SUBSTR(cNumInt,nParte,3)) != "000"

      // Coloca "," ou "E" (Depois do 1o. Terno)
      IF nParte != 1
         cExtens := RTRIM(cExtens)
         cExtens += IF(RIGHT(cNumInt,2) == "00" .AND. nGrand == 1," e ",", ")
      ENDIF

      // Pega Extenso do Terno
      cExtTer := EscreveTerno(cTerno)

      // Coloca Grandeza
      cExtTer += IF(VAL(cTerno) == 1,aGranS[nGrand],aGranP[nGrand])

      // Concatena Extenso
      cExtens += cExtTer
   ENDIF
   nGrand --
NEXT

// Verifica se Coloca Moeda
IF pMoeda

   // Verifica se Coloca "DE"
   cExtens += IF(LEN(cNumInt) > 6 .AND. RIGHT(cNumInt,6) == "000000","de ","")

   // Coloca Moeda
   cExtens += IF(VAL(cNumInt) == 1,"real ","reais ")
ENDIF

// Extenso do Terno Decimal (centavos)
IF (cTerno := "0" + cNumDec) != "000"

   // Coloca "e"
   cExtTer := "e "

   // Pega Extenso
   cExtTer += EscreveTerno(cTerno)

   // Coloca "CENTAVOS"
   cExtTer += IF(VAL(cNumDec) == 1,"centavo","centavos")

   // Concatena Extenso
   cExtens += cExtTer
ENDIF
RETURN (RTRIM(cExtens))

//---------------------------
FUNCTION EscreveTerno(pTerno)
//---------------------------
LOCAL cCen  := SUBSTR(pTerno,1,1),;
      cDez  := SUBSTR(pTerno,2,1),;
      cUni  := SUBSTR(pTerno,3,1),;
      cCent := "",;
      cDeze := "",;
      cUnid := ""

// Centena
IF     cCen == "0"
   cCent := ""
ELSEIF cCen == "1"
   cCent := IF((cDez + cUni) == "00","cem ","cento ")
ELSEIF cCen == "2"
   cCent := "duzentos "
ELSEIF cCen == "3"
   cCent := "trezentos "
ELSEIF cCen == "4"
   cCent := "quatrocentos "
ELSEIF cCen == "5"
   cCent := "quinhentos "
ELSEIF cCen == "6"
   cCent := "seiscentos "
ELSEIF cCen == "7"
   cCent := "setecentos "
ELSEIF cCen == "8"
   cCent := "oitocentos "
ELSEIF cCen == "9"
   cCent := "novecentos "
ENDIF

// Verifica Colocacao do "E"
cCent += IF(!EMPTY(cCent) .AND. (cDez + cUni) != "00","e ","")

// Dezena
IF     cDez == "0"
   cDeze := ""
ELSEIF cDez == "1"
   IF     cUni == "0"
      cDeze := "dez "
   ELSEIF cUni == "1"
      cDeze := "onze "
   ELSEIF cUni == "2"
      cDeze := "doze "
   ELSEIF cUni == "3"
      cDeze := "treze "
   ELSEIF cUni == "4"
      cDeze := "quatorze "
   ELSEIF cUni == "5"
      cDeze := "quinze "
   ELSEIF cUni == "6"
      cDeze := "dezesseis "
   ELSEIF cUni == "7"
      cDeze := "dezesete "
   ELSEIF cUni == "8"
      cDeze := "dezoito "
   ELSEIF cUni == "9"
      cDeze := "dezenove "
   ENDIF
ELSEIF cDez == "2"
   cDeze := "vinte "
ELSEIF cDez == "3"
   cDeze := "trinta "
ELSEIF cDez == "4"
   cDeze := "quarenta "
ELSEIF cDez == "5"
   cDeze := "cinquenta "
ELSEIF cDez == "6"
   cDeze := "sessenta "
ELSEIF cDez == "7"
   cDeze := "setenta "
ELSEIF cDez == "8"
   cDeze := "oitenta "
ELSEIF cDez == "9"
   cDeze := "noventa "
ENDIF

// Verifica Colocacao do "E"
cDeze += IF(!EMPTY(cDeze) .AND. !cDez $ "01" .AND. cUni != "0","e ","")

// Unidade
IF     cUni == "0" .OR. cDez == "1"
   cUnid := ""
ELSEIF cUni == "1"
   cUnid := "um "
ELSEIF cUni == "2"
   cUnid := "dois "
ELSEIF cUni == "3"
   cUnid := "tres "
ELSEIF cUni == "4"
   cUnid := "quatro "
ELSEIF cUni == "5"
   cUnid := "cinco "
ELSEIF cUni == "6"
   cUnid := "seis "
ELSEIF cUni == "7"
   cUnid := "sete "
ELSEIF cUni == "8"
   cUnid := "oito "
ELSEIF cUni == "9"
   cUnid := "nove "
ENDIF
RETURN (cCent+cDeze+cUnid)

************************** f i m ******************************
*------------------------------
FUNCTION extenso1(pValor,pMoeda)
*------------------------------
*  Objetivo..: Monta String c/Extenso de um Valor *** MAISCULO ***
*  Parametros: pValor -> Valor
*              pMoeda -> Flag de Real
*----------------------------------------------------------------------------
LOCAL aGranP := {},;
      aGranS := {},;
      nTerno,;
      cNumero,;
      cNumInt,;
      cNumDec,;
      cTerno,;
      cExtTer,;
      cExtens,;
      nParte,;
      nGrand

*  Verifica Parametros
pValor := ABS(pValor)
pMoeda := IF(pMoeda == NIL,.F.,pMoeda)

*  Verifica Valor
IF pValor == 0
   RETURN ("")
ENDIF

*  Pega Numero de Ternos do Numero
nTerno := INT((LENNUM(INT(pValor)) / 3))
nTerno := IF((nTerno * 3) != LENNUM(INT(pValor)),(nTerno + 1),nTerno)

*  Divide Numero Inteiro/Decimal
cNumero := STRZERO(pValor,((nTerno * 3) + 3),2)
cNumInt := LEFT(cNumero,(nTerno * 3))
cNumDec := RIGHT(cNumero,2)

*  Verifica Limite da Funcao
IF nTerno > 10
   RETURN ("VALOR NAO SUPORTADO PELA FUNCAO")
ENDIF

*  Adiciona Grandesas
AADD(aGranP,"")
AADD(aGranP,"MIL ")
AADD(aGranP,"MILHOES ")
AADD(aGranP,"BILHOES ")
AADD(aGranP,"TRILHOES ")
AADD(aGranP,"QUATRILHOES ")
AADD(aGranP,"QUINTILHOES ")
AADD(aGranP,"SEXTILHOES ")
AADD(aGranP,"SEPTILHOES ")
AADD(aGranP,"OCTILHOES ")
AADD(aGranP,"NONILHOES ")
AADD(aGranS,"")
AADD(aGranS,"MIL ")
AADD(aGranS,"MILHAO ")
AADD(aGranS,"BILHAO ")
AADD(aGranS,"TRILHAO ")
AADD(aGranS,"QUATRILHAO ")
AADD(aGranS,"QUINTILHAO ")
AADD(aGranS,"SEXTILHAO ")
AADD(aGranS,"SEPTILHAO ")
AADD(aGranS,"OCTILHAO ")
AADD(aGranS,"NONILHAO ")

*  Laco de Extenso dos Ternos Inteiros
cExtens := ""
nGrand  := nTerno
FOR nParte := 1 TO (nTerno * 3) STEP 3

   *  Verifica Terno NAO Nulo
   IF (cTerno := SUBSTR(cNumInt,nParte,3)) != "000"

      *  Coloca "," ou "E" (Depois do 1o. Terno)
      IF nParte != 1
         cExtens := RTRIM(cExtens)
         cExtens += IF(RIGHT(cNumInt,2) == "00" .AND. nGrand == 1," E ",", ")
      ENDIF

      *  Pega Extenso do Terno
      cExtTer := Escreve1(cTerno)

      *  Coloca Grandeza
      cExtTer += IF(VAL(cTerno) == 1,aGranS[nGrand],aGranP[nGrand])

      *  Concatena Extenso
      cExtens += cExtTer
   ENDIF
   nGrand --
NEXT

*  Verifica se Coloca Moeda
IF pMoeda

   *  Verifica se Coloca "DE"
   cExtens += IF(LEN(cNumInt) > 6 .AND. RIGHT(cNumInt,6) == "000000","DE ","")

   *  Coloca Moeda
   cExtens += IF(VAL(cNumInt) == 1,"REAL ","REAIS ")
ENDIF

*  Extenso do Terno Decimal (CENTAVOS)
IF (cTerno := "0" + cNumDec) != "000"

   *  Coloca "E"
   cExtTer := "E "

   *  Pega Extenso
   cExtTer += Escreve1(cTerno)

   *  Coloca "CENTAVOS"
   cExtTer += IF(VAL(cNumDec) == 1,"CENTAVO","CENTAVOS")

   *  Concatena Extenso
   cExtens += cExtTer
ENDIF
RETURN (RTRIM(cExtens))

*----------------------------
FUNCTION Escreve1(pTerno)
*----------------------------
LOCAL cCen  := SUBSTR(pTerno,1,1),;
      cDez  := SUBSTR(pTerno,2,1),;
      cUni  := SUBSTR(pTerno,3,1),;
      cCent := "",;
      cDeze := "",;
      cUnid := ""

*  Centena
IF     cCen == "0"
   cCent := ""
ELSEIF cCen == "1"
   cCent := IF((cDez + cUni) == "00","CEM ","CENTO ")
ELSEIF cCen == "2"
   cCent := "DUZENTOS "
ELSEIF cCen == "3"
   cCent := "TREZENTOS "
ELSEIF cCen == "4"
   cCent := "QUATROCENTOS "
ELSEIF cCen == "5"
   cCent := "QUINHENTOS "
ELSEIF cCen == "6"
   cCent := "SEISCENTOS "
ELSEIF cCen == "7"
   cCent := "SETECENTOS "
ELSEIF cCen == "8"
   cCent := "OITOCENTOS "
ELSEIF cCen == "9"
   cCent := "NOVECENTOS "
ENDIF

*  Verifica Colocacao do "E"
cCent += IF(!EMPTY(cCent) .AND. (cDez + cUni) != "00","E ","")

*  Dezena
IF     cDez == "0"
   cDeze := ""
ELSEIF cDez == "1"
   IF     cUni == "0"
      cDeze := "DEZ "
   ELSEIF cUni == "1"
      cDeze := "ONZE "
   ELSEIF cUni == "2"
      cDeze := "DOZE "
   ELSEIF cUni == "3"
      cDeze := "TREZE "
   ELSEIF cUni == "4"
      cDeze := "QUATORZE "
   ELSEIF cUni == "5"
      cDeze := "QUINZE "
   ELSEIF cUni == "6"
      cDeze := "DEZESSEIS "
   ELSEIF cUni == "7"
      cDeze := "DEZESETE "
   ELSEIF cUni == "8"
      cDeze := "DEZOITO "
   ELSEIF cUni == "9"
      cDeze := "DEZENOVE "
   ENDIF
ELSEIF cDez == "2"
   cDeze := "VINTE "
ELSEIF cDez == "3"
   cDeze := "TRINTA "
ELSEIF cDez == "4"
   cDeze := "QUARENTA "
ELSEIF cDez == "5"
   cDeze := "CINQUENTA "
ELSEIF cDez == "6"
   cDeze := "SESSENTA "
ELSEIF cDez == "7"
   cDeze := "SETENTA "
ELSEIF cDez == "8"
   cDeze := "OITENTA "
ELSEIF cDez == "9"
   cDeze := "NOVENTA "
ENDIF

*  Verifica Colocacao do "E"
cDeze += IF(!EMPTY(cDeze) .AND. !cDez $ "01" .AND. cUni != "0","E ","")

*  Unidade
IF     cUni == "0" .OR. cDez == "1"
   cUnid := ""
ELSEIF cUni == "1"
   cUnid := "UM "
ELSEIF cUni == "2"
   cUnid := "DOIS "
ELSEIF cUni == "3"
   cUnid := "TRES "
ELSEIF cUni == "4"
   cUnid := "QUATRO "
ELSEIF cUni == "5"
   cUnid := "CINCO "
ELSEIF cUni == "6"
   cUnid := "SEIS "
ELSEIF cUni == "7"
   cUnid := "SETE "
ELSEIF cUni == "8"
   cUnid := "OITO "
ELSEIF cUni == "9"
   cUnid := "NOVE "
ENDIF
RETURN (cCent+cDeze+cUnid)
************************** f i m ******************************

FUNCTION atencao(men,tempo)
**********************

LOCAL linhas,i,lin,matencao,li,ci,la,ca

men := RTRIM(men)
IF LEN(men) > 60
        mpos := 60
        ci := 10
ELSEIF LEN(men) < LEN('>> PRESSIONE ALGUMA TECLA P/CONTINUAR <<')
        mpos := LEN('>> PRESSIONE ALGUMA TECLA P/CONTINUAR <<')
        ci := (80-LEN('>> PRESSIONE ALGUMA TECLA P/CONTINUAR <<')) / 2
ELSE
        mpos := LEN(men)
        ci := (80-LEN(men)) / 2
ENDIF
linhas := MLCOUNT(men,mpos)

li := 12
la := 12+linhas+2
ca := ci+1+mpos+1
matencao := SAVESCREEN(li,ci-2,la+1,ca)
SETCOLOR('gr+/r,x')
botao(li,ci,la,ca)
FOR i = 1 TO  linhas
        LIN := MEMOLINE(men,mpos,i,,)
        DEVPOS(li+i,ci+1);DEVOUT(PADC(RTRIM(LIN),mpos))
NEXT
SETCOLOR('w/r,x')
DEVPOS(li+i,ci+1);DEVOUT(PADC('>> PRESSIONE ALGUMA TECLA P/CONTINUAR <<',mpos))
setcor(1)
TONE(100,6)
TONE(1600,6)
IF tempo = NIL
        INKEY(7)
ELSE
        INKEY(tempo)
ENDIF
RESTSCREEN(li,ci-2,la+1,ca,matencao)
RETURN .F.
******************************* F I M ********************************
FUNCTION mensagem1(men,mop,mcond,linha)
**********************

LOCAL linhas,i,lin,matencao,li,ci,la,ca,mpos

men := RTRIM(men)
IF LEN(men) > 60
        mpos := 60
        ci := 10
ELSEIF LEN(men) < LEN('Opcao ['+mcond+']:')
        mpos := LEN('Opcao ['+mcond+']:')
        ci := (80-LEN('Opcao ['+mcond+']:')) / 2
ELSE
        mpos := LEN(men)
        ci := (80-LEN(men)) / 2
ENDIF
linhas := MLCOUNT(men,mpos)

li := 12
IF linha <> NIL
        IF linha + linhas + 2 > 24
                li := 24 - (linhas + 2)
        ELSE
                li := linha
        ENDIF
ENDIF
la := li+linhas+2
ca := ci+1+mpos+1
matencao := SAVESCREEN(li,ci-2,la+1,ca)
SETCOLOR('w+/b,w/b,x')
botao(li,ci,la,ca,,' ESCOLHA A OPCAO ')
FOR i = 1 TO  linhas
        LIN := MEMOLINE(men,mpos,i,,)
        DEVPOS(li+i,ci+1);DEVOUT(PADC(RTRIM(LIN),mpos))
NEXT
*setcor(1)
IF mop = NIL
        INKEY(0)
        RESTSCREEN(li,ci-2,la+1,ca,matencao)
        setcor(1)
        RETURN .T.
ELSE
        SETCOLOR('w+/b,b/w,x')
        DEVPOS(li+i,ci+1);DEVOUT('Opcao ['+mcond+']:')
        @ li+i,COL()+1 GET mop PICT '@!' VALID mop $ mcond
        READ
        setcor(1)
        IF LASTKEY() = 27
                RESTSCREEN(li,ci-2,la+1,ca,matencao)
                RETURN mop
        ELSE
                RESTSCREEN(li,ci-2,la+1,ca,matencao)
                RETURN mop
        ENDIF
ENDIF
setcor(1)
RESTSCREEN(li,ci-2,la+1,ca,matencao)
RETURN .F.
******************************* F I M ********************************
/*
FUNCTION mensagem1(men,mop,mcond,linha,coluna)
**********************

LOCAL linhas,i,lin,matencao,li,ci,la,ca

linhas := MLCOUNT(men,40)

li := 12
ci := 20
IF linha <> NIL
        IF linha + linhas + 2 > 24
                li := 24 - (linhas + 2)
        ELSE
                li := linha
        ENDIF
ENDIF
IF coluna <> NIL
        IF coluna + 42 > 79
                ci := 79 - 42
        ELSE
                ci := coluna
        ENDIF
ENDIF
la := li+linhas+2
ca := ci + 42
matencao := SAVESCREEN(li,ci-2,la+1,ca)
SETCOLOR('gr+/r,W+/r,x')
botao(li,ci,la,ca)
FOR i = 1 TO  linhas
        LIN := MEMOLINE(men,40,i,,)
        DEVPOS(li+i,ci+1);DEVOUT(PADR(RTRIM(LIN),40))
NEXT
*setcor(1)
IF mop = NIL
        INKEY(0)
        RESTSCREEN(li,ci-2,la+1,ca,matencao)
        setcor(1)
        RETURN .T.
ELSE
        SETCOLOR('w+/r,n/r,x')
        DEVPOS(li+i,ci+1);DEVOUT('Opcao ['+mcond+']:')
        @ li+i,COL()+1 GET mop PICT '@!' VALID mop $ mcond
        READ
        setcor(1)
        IF LASTKEY() = 27
                RESTSCREEN(li,ci-2,la+1,ca,matencao)
                RETURN mop
        ELSE
                RESTSCREEN(li,ci-2,la+1,ca,matencao)
                RETURN mop
        ENDIF
ENDIF
setcor(1)
RESTSCREEN(li,ci-2,la+1,ca,matencao)
RETURN .F.
*/
******************************* F I M ********************************
FUNCTION mensagem(men)
**********************

LOCAL cor

cor := SETCOLOR()

limpa(23,00,23,79)
setcor(2)
DEVPOS(23,00);DEVOUT('Mensagem: ')
setcor(1)
DEVPOS(23,COL());DEVOUT(men)
*TONE(1600,2)
RETURN NIL
******************************* F I M ****************************************
**************************
* LIMPA UMA AREA
**************************

FUNCTION limpa(li,ci,lb,cb)
***************************

@ li,ci CLEAR TO lb,cb

RETURN NIL
***************** F I M **************************
*********************
* CABECALHO DA JANELA
*********************

FUNCTION janela(li,cb,cab,p,lado)
****************

LOCAL mcor

mcor := SETCOLOR()
setcor(3)
IF p = NIL
        setcor(7,'*')
ELSE
        setcor(7)
ENDIF

IF lado = NIL
        DEVPOS(li,cb-(LEN(cab)+2));DEVOUT(cab)
ELSE
        DEVPOS(li,cb+2);DEVOUT(cab)
ENDIF
SETCOLOR(mcor)
RETURN NIL
***************************** F I M ********************************************
*****************
 FUNCTION quadro(par)
*****************

LOCAL cen_tro

cen_tro=40-(len(par)/2)
cen_tro=cen_tro-2
botao(12,cen_tro,14,cen_tro+len(par)+3)
@ 13,cen_tro+2 SAY par
RETURN .F.
*************************** F I M **************************************
**********
FUNCTION tecla
***************
IF LASTKEY()=27
        RETURN .T.
ELSE
        RETURN .F.
ENDIF
RETURN .F.

*************************** F I M **********************************
***************************
* CABECALHO RELATORIO
***************************

FUNCTION cabecalho(pag,tit,tipo,vprg,tam)
************************************

LOCAL mtraco2
IF tam = NIL
        mtraco2:=REPLI('=',80)
ELSE
        mtraco2:=REPLI('=',132)
ENDIF
WHILE .T.
        imprt('M','N')
        imprt('M','E')
        pitalico(.T.)
        DEVPOS(PROW(),00);DEVOUT(memp+resa)
        pitalico(.F.)
        imprt('M','N')
        DEVPOS(PROW()+1,00);DEVOUT(tit)
        DEVPOS(PROW(),00);DEVOUT(tit)
        IF tam = NIL
                DEVPOS(PROW(),00);DEVOUT(PADL(mdata_sis,80))
        ELSE
                DEVPOS(PROW(),00);DEVOUT(PADL(mdata_sis,132))
        ENDIF
        IF tipo <> NIL
                DEVPOS(PROW()+1,00);DEVOUT(tipo)
                DEVPOS(PROW(),00);DEVOUT(tipo)
        ELSE
                DEVPOS(PROW()+1,00);DEVOUT(REPLI('*',LEN(tit)))
        ENDIF
        IF tam = NIL
                DEVPOS(PROW(),00);DEVOUT(PADL('Pagina: '+ STRZERO(pag,3),80))
        ELSE
                DEVPOS(PROW(),00);DEVOUT(PADL('Pagina: '+ STRZERO(pag,3),132))
        ENDIF
        imprt('M','C',1)
        DEVPOS(PROW(),00);DEVOUT(mfi+rma)
        IF tam = NIL
                IF vprg = NIL
                        DEVPOS(PROW(),00);DEVOUT(PADL(sistema+' '+mversao,136))
                ELSE
                        DEVPOS(PROW(),00);DEVOUT(PADL(sistema+' '+mversao+'('+vprg+')',136))
                ENDIF
        ELSE
                IF vprg = NIL
                        DEVPOS(PROW(),00);DEVOUT(PADL(sistema+' '+mversao,220))
                ELSE
                        DEVPOS(PROW(),00);DEVOUT(PADL(sistema+' '+mversao+'('+vprg+')',220))
                ENDIF
        ENDIF
        imprt('M','N')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco2)
        RETURN NIL
ENDDO
RETURN NIL
******************************* F I M *********************************
******************************************
* FUNCAO PARA BLOQUEIO DE ARQUIVO EM REDE
******************************************

FUNCTION bloqarq
***********************

LOCAL vtela

IF FLOCK()
        RETURN (.T.)
ENDIF
vtela := SAVESCREEN(23,00,23,79)
mensagem('Aguarde... Tentando Bloquear o Arquivo !!!')
WHILE LASTKEY() <> 27
        IF FLOCK()
                RESTSCREEN(23,00,23,79,vtela)
                RETURN (.T.)
        ENDIF
        INKEY(1)
ENDDO
RESTSCREEN(23,00,23,79,vtela)
RETURN (.F.)
************************** F I M ***********************************
******************************************
* FUNCAO PARA BLOQUEAR REGISTRO EM REDE
******************************************

FUNCTION bloqreg
***********************

LOCAL vtela

IF RLOCK()
        RETURN (.T.)
ENDIF
vtela := SAVESCREEN(23,00,23,79)
mensagem('Aguarde um momento...Registro acessado p/outro usuario !!!')

WHILE LASTKEY() <> 27
        IF DBRLOCK()
                RESTSCREEN(23,00,23,79,vtela)
                RETURN (.T.)
        ENDIF
        INKEY(1)
ENDDO
RESTSCREEN(23,00,23,79,vtela)
RETURN (.F.)
****************************** F I M ***************************
****************************************************
* FUNCAO TENTA ADICIONA REGISTRO NO ARQUIVO EM REDE
*****************************************************

FUNCTION adireg
**********************

LOCAL vtela

APPEND BLANK
IF ! NETERR()
        RETURN .T.
ENDIF
vtela := SAVESCREEN(23,00,23,79)
mensagem('Aguarde...Tentando Acesso ao Arquivo !!!')
WHILE LASTKEY() <> 27
        APPEND BLANK
        IF ! NETERR()
                RESTSCREEN(23,00,23,79,vtela)
                RETURN .T.
        ENDIF
        INKEY(1)
ENDDO
RESTSCREEN(23,00,23,79,vtela)
RETURN .F.
*************************** F I M *********************************
********************************
* FUNCAO PARA ABRIR ARQUIVOS
********************************

FUNCTION abrarq(arq)
**********************

IF SELE(arq) <> 0
        RETURN .T.
ELSE
        RETURN .F.
ENDIF

*************************** F I M *********************************
***************************
* VER NIVEL DE ACESSO
***************************

FUNCTION nivel(nivel1,nivel2,nivel3,nivel4,nivel5)
***************************************************

WHILE .T.
        IF nivel_acess = nivel1 .OR. nivel_acess = nivel2 .OR. ;
           nivel_acess = nivel3 .OR. nivel_acess = nivel4 .OR. ;
           nivel_acess = nivel5
                RETURN .T.
        ELSE
                atencao('ACESSO NAO AUTORIZADO PARA ESTE AMBIENTE',2)
                IF ! aut_sen('Senha de Liberacao do Ambiente:')
                        RETURN .F.
                ELSE
                        RETURN .T.
                ENDIF
        ENDIF
ENDDO
RETURN .T.
*********************************** f i m **************************************
***************************************
* FUNCAO PARA INICIALIZACAO DO SISTEMA
***************************************

FUNCTION inicializa()
*************************

LOCAL mcont,msenha,mnivel,mcod_op,lci,cci,lba,cba,tela,msele,morde,mmen,mcor,mdir

mcont:=0
lci := 10
cci := 20
lba := 14
cba := 60
mmen := SAVESCREEN(24,00,24,79)
tela := SAVESCREEN(01,00,24,79)
mcor := SETCOLOR()
IF FILE('insercom.ins')
        RETURN NIL
ELSE
        WHILE .T.
                *************
                SELE('sen')
                ORDSETFOCUS(2)
                GO TOP
                *************

                SEEK '999'

                IF ! FOUND()
                        UNLOCK
                        CLOSE ALL
                        limpa(00,00,24,79)
                        setcor(6)
                        setcor(5)
                        DEVPOS(02,1);DEVOUT('⁄€€€⁄€€€ ⁄€€€€€€€ ⁄€€€€€€€')
                        DEVPOS(03,1);DEVOUT('≥€€€≥€€€ ≥€€€ø€€€ ≥€€€ø€€€')
                        DEVPOS(04,1);DEVOUT('≥€€€€€€€ ≥€€€€€€€ ≥€€€€€€Ÿ')
                        DEVPOS(05,1);DEVOUT('≥€€€ø€€€ ≥€€€ø€€Ÿ ≥€€€ø€€€')
                        DEVPOS(06,1);DEVOUT('≥€€€≥€€€ ≥€€€≥€€€ ≥€€€€€€€')
                        DEVPOS(07,1);DEVOUT('¿ƒƒŸ¿ƒƒŸ ¿ƒƒŸ¿ƒƒŸ ¿ƒƒƒƒƒƒŸ')
                        DEVPOS(08,1);DEVOUT(' SOLUCOES EM INFORMATICA  ')
                        SETCOLOR('N/W,X')
                        DEVPOS(02,10);DEVOUT('⁄€€€€€€€')
                        DEVPOS(03,10);DEVOUT('≥€€€ø€€€')
                        DEVPOS(04,10);DEVOUT('≥€€€€€€€')
                        DEVPOS(05,10);DEVOUT('≥€€€ø€€Ÿ')
                        DEVPOS(06,10);DEVOUT('≥€€€≥€€€')
                        DEVPOS(07,10);DEVOUT('¿ƒƒŸ¿ƒƒŸ')
                        setcor(2)
                        DEVPOS(2,30);DEVOUT(PADC('>>> H R B   Solucoes em Informatica <<<',48),setcor(5))
                        DEVPOS(3,30);DEVOUT(PADC('Rua Agenor Lopes, 292 - Sl.403 - Recife - PE.',48),setcor(2))
                        DEVPOS(4,30);DEVOUT(PADC('Fone:(81)3325.6663 - (Tacaimbo)(81)3755.1489',48))
                        DEVPOS(5,30);DEVOUT(PADC('Suporte: HÇlio:9975.5923 - 8835.1751',48))
                        DEVPOS(6,30);DEVOUT(PADC('Edson: (81) 8835.1752 - Roberta:8835.1753',48))
                        DEVPOS(7,30);DEVOUT(PADC('e-mail: helio@hrbinfo.com.br',48),setcor(5))
                        DEVPOS(8,30);DEVOUT(PADC('Direito Copia (C) 1994-2006 da HRB Informatica',48),setcor(5))
                        SETCOLOR('W+/n,N/w,X')
                        @ 10,29 SAY 'Copia Exclusiva: '
                        setcor(4)
                        DEVPOS(10,46);DEVOUT(memp+resa)
                        SETCOLOR('w+/n,N/w,X')
                        @ 11,29 SAY '___________________________________________'
                        @ 14,02 to 17,77 double
                        SETCOLOR('W+/n,N/w,X')
                        DEVPOS(15,04);DEVOUT('               C O P I A    I L E G A L   !!!                      ')
                        DEVPOS(16,04);DEVOUT('Para Adiquiri o Sistema Ligue (081) 424.4912, Desligue o Computador')
                        DEVPOS(20,00);DEVOUT('')
                        SETCOLOR(mcor)
                        UNLOCK
                        QUIT
                ENDIF
                WHILE mcont <= 3
                        setcor(1)
                        limpa(24,31,24,79)
                        DEVPOS(24,31);DEVOUT('Senha de Inicializaá∆o:')
                        msenha = ''
                        pegasenha(@msenha,24,55,setcor(3))
                        IF msenha = 'NAO'
                                LOOP
                        ENDIF
                        mcont ++
                        setcor(1)
                        IF EMPTY(msenha)
                                limpa(24,31,24,79)
                                DEVPOS(24,31);DEVOUT('Senha Incorreta Tente Novamente')
                                TONE(100,6)
                                TONE(1600,6)
                                INKEY(2)
                                IF mcont = 3
                                        UNLOCK
                                        CLOSE ALL
                                        limpa(00,00,24,79)
                                        setcor(5)
                                        DEVPOS(02,1);DEVOUT('⁄€€€⁄€€€ ⁄€€€€€€€ ⁄€€€€€€€')
                                        DEVPOS(03,1);DEVOUT('≥€€€≥€€€ ≥€€€ø€€€ ≥€€€ø€€€')
                                        DEVPOS(04,1);DEVOUT('≥€€€€€€€ ≥€€€€€€€ ≥€€€€€€Ÿ')
                                        DEVPOS(05,1);DEVOUT('≥€€€ø€€€ ≥€€€ø€€Ÿ ≥€€€ø€€€')
                                        DEVPOS(06,1);DEVOUT('≥€€€≥€€€ ≥€€€≥€€€ ≥€€€€€€€')
                                        DEVPOS(07,1);DEVOUT('¿ƒƒŸ¿ƒƒŸ ¿ƒƒŸ¿ƒƒŸ ¿ƒƒƒƒƒƒŸ')
                                        DEVPOS(08,1);DEVOUT(' SOLUCOES EM INFORMATICA  ')
                                        SETCOLOR('N/W,X')
                                        DEVPOS(02,10);DEVOUT('⁄€€€€€€€')
                                        DEVPOS(03,10);DEVOUT('≥€€€ø€€€')
                                        DEVPOS(04,10);DEVOUT('≥€€€€€€€')
                                        DEVPOS(05,10);DEVOUT('≥€€€ø€€Ÿ')
                                        DEVPOS(06,10);DEVOUT('≥€€€≥€€€')
                                        DEVPOS(07,10);DEVOUT('¿ƒƒŸ¿ƒƒŸ')
                                        setcor(2)
                                        DEVPOS(2,30);DEVOUT(PADC('>>> H R B   Solucoes em Informatica <<<',48),setcor(5))
                                        DEVPOS(3,30);DEVOUT(PADC('Rua Agenor Lopes, 292 - Sl.403 - Recife - PE.',48),setcor(2))
                                        DEVPOS(4,30);DEVOUT(PADC('Fone:(81)3325.6663 - (Tacaimbo)(81)3755.1489',48))
                                        DEVPOS(5,30);DEVOUT(PADC('Suporte: HÇlio:9975.5923 - 8835.1751',48))
                                        DEVPOS(6,30);DEVOUT(PADC('Edson: (81) 8835.1752 - Roberta:8835.1753',48))
                                        DEVPOS(7,30);DEVOUT(PADC('e-mail: helio@hrbinfo.com.br',48),setcor(5))
                                        DEVPOS(8,30);DEVOUT(PADC('Direito Copia (C) 1994-2006 da HRB Informatica',48),setcor(5))
                                        SETCOLOR('W+/n,N/w,X')
                                        @ 10,29 SAY 'Copia Exclusiva: '
                                        setcor(4)
                                        DEVPOS(10,46);DEVOUT(memp+resa)
                                        SETCOLOR('w+/n,N/w,X')
                                        @ 11,29 SAY '___________________________________________'
                                        @ 14,02 to 17,77 double
                                        SETCOLOR('W+/n,N/w,X')
                                        DEVPOS(15,04);DEVOUT('               C O P I A    I L E G A L   !!!                      ')
                                        DEVPOS(16,04);DEVOUT('Para Adiquiri o Sistema Ligue (081) 424.4912, Desligue o Computador')
                                        DEVPOS(20,00);DEVOUT('')
                                        SETCOLOR(mcor)
                                        QUIT
                                ENDIF
                                LOOP
                        ENDIF
                        IF msenha == 'ININSEC'
                                FCREATE('insercom.ins',1)
                                limpa(24,31,24,79)
                                RETURN .T.
                        ELSE
                                limpa(24,31,24,79)
                                DEVPOS(24,31);DEVOUT('Senha Incorreta Tente Novamente')
                                TONE(100,6)
                                TONE(1600,6)
                                INKEY(2)
                                IF mcont = 3
                                        UNLOCK
                                        CLOSE ALL
                                        limpa(00,00,24,79)
                                        setcor(6)
                                        setcor(5)
                                        DEVPOS(02,1);DEVOUT('⁄€€€⁄€€€ ⁄€€€€€€€ ⁄€€€€€€€')
                                        DEVPOS(03,1);DEVOUT('≥€€€≥€€€ ≥€€€ø€€€ ≥€€€ø€€€')
                                        DEVPOS(04,1);DEVOUT('≥€€€€€€€ ≥€€€€€€€ ≥€€€€€€Ÿ')
                                        DEVPOS(05,1);DEVOUT('≥€€€ø€€€ ≥€€€ø€€Ÿ ≥€€€ø€€€')
                                        DEVPOS(06,1);DEVOUT('≥€€€≥€€€ ≥€€€≥€€€ ≥€€€€€€€')
                                        DEVPOS(07,1);DEVOUT('¿ƒƒŸ¿ƒƒŸ ¿ƒƒŸ¿ƒƒŸ ¿ƒƒƒƒƒƒŸ')
                                        DEVPOS(08,1);DEVOUT(' SOLUCOES EM INFORMATICA  ')
                                        SETCOLOR('N/W,X')
                                        DEVPOS(02,10);DEVOUT('⁄€€€€€€€')
                                        DEVPOS(03,10);DEVOUT('≥€€€ø€€€')
                                        DEVPOS(04,10);DEVOUT('≥€€€€€€€')
                                        DEVPOS(05,10);DEVOUT('≥€€€ø€€Ÿ')
                                        DEVPOS(06,10);DEVOUT('≥€€€≥€€€')
                                        DEVPOS(07,10);DEVOUT('¿ƒƒŸ¿ƒƒŸ')
                                        setcor(2)
                                        DEVPOS(2,30);DEVOUT(PADC('>>> H R B   Solucoes em Informatica <<<',48),setcor(5))
                                        DEVPOS(3,30);DEVOUT(PADC('Rua Agenor Lopes, 292 - Sl.403 - Recife - PE.',48),setcor(2))
                                        DEVPOS(4,30);DEVOUT(PADC('Fone:(81)3325.6663 - (Tacaimbo)(81)3755.1489',48))
                                        DEVPOS(5,30);DEVOUT(PADC('Suporte: HÇlio:9975.5923 - 8835.1751',48))
                                        DEVPOS(6,30);DEVOUT(PADC('Edson: (81) 8835.1752 - Roberta:8835.1753',48))
                                        DEVPOS(7,30);DEVOUT(PADC('e-mail: helio@hrbinfo.com.br',48),setcor(5))
                                        DEVPOS(8,30);DEVOUT(PADC('Direito Copia (C) 1994-2006 da HRB Informatica',48),setcor(5))
                                        SETCOLOR('W+/n,N/w,X')
                                        @ 10,29 SAY 'Copia Exclusiva: '
                                        setcor(4)
                                        DEVPOS(10,46);DEVOUT(memp+resa)
                                        SETCOLOR('w+/n,N/w,X')
                                        @ 11,29 SAY '___________________________________________'
                                        @ 14,02 to 17,77 double
                                        SETCOLOR('W+/n,N/w,X')
                                        DEVPOS(15,04);DEVOUT('               C O P I A    I L E G A L   !!!                      ')
                                        DEVPOS(16,04);DEVOUT('Para Adiquiri o Sistema Ligue (081) 424.4912, Desligue o Computador')
                                        DEVPOS(20,00);DEVOUT('')
                                        SETCOLOR(mcor)
                                        UNLOCK
                                        QUIT
                                ENDIF
                                LOOP
                        ENDIF
                ENDDO
        ENDDO
ENDIF
********************************** f i m **************************************
**********************
* VERIFICA SENHAS
**********************

FUNCTION senha

LOCAL mcont,msenha,mdata_sen,mdata_aux

PUBLIC senha_acess,nivel_acess,cod_operado

mcont:=0
*-----------------------------------------------------
CLOSE ALL
IF ! ABRARQ('sen')
        IF ! abriarq('insopera','sen');RETURN NIL;ENDIF
ENDIF
IF ! ABRARQ('set')
        IF ! abriarq('safsetup','set');RETURN NIL;ENDIF
ENDIF
*-----------------------------------------------------
WHILE .T.
        *************
        SELE('sen')
        ORDSETFOCUS(1)
        *************

        WHILE mcont <= 3
                GO TOP
                setcor(1)
                DEVPOS(24,31);DEVOUT('Digite sua Senha:')
                msenha = ''
                pegasenha(@msenha,24,48,setcor(3))
                setcor(1)
                mcont ++
                IF msenha = 'NAO'
                        SETCOLOR(cor)
                        RETURN .F.
                ENDIF
                IF EMPTY(msenha)
                        limpa(24,31,24,79)
                        DEVPOS(24,31);DEVOUT('Senha Incorreta Tente Novamente')
                        TONE(100,6)
                        TONE(1600,6)
                        INKEY(2)
                        limpa(24,31,24,79)
                        IF mcont = 3
                                SETCOLOR(cor)
                                RETURN .F.
                        ENDIF
                        LOOP
                ENDIF
                IF SUBSTR(msenha,1,6) = SUBSTR(DTOC(DATE()),7,2)+SUBSTR(DTOC(DATE()),1,2)+SUBSTR(DTOC(DATE()),4,2)
                        IF VAL(SUBSTR(msenha,7)) = 0
                                mdata_aux :=SPACE(11)
                        ELSE
                                mdata_sen := DATE() + VAL(SUBSTR(msenha,7))
                                mdata_aux := SUBSTR(DTOC(mdata_sen),1,2)+SUBSTR(DTOC(mdata_sen),4,2)+SUBSTR(DTOC(mdata_sen),7,2) + SUBSTR(msenha,1,6)
                        ENDIF
                        *************
                        SELE('set')
                        *************
                        IF LASTREC() = 0
                                IF ! ADIREG()
                                        atencao('Nao foi possivel acessar o registro')
                                        RETURN NIL
                                ENDIF
                        ELSE
                                IF ! BLOQREG()
                                        atencao('Nao foi possivel acessar o registro')
                                        RETURN NIL
                                ENDIF
                        ENDIF
                        set->sysdatlm := mdata_aux
                        DBCOMMIT()
                        DBUNLOCK()
                        *************
                        SELE('sen')
                        ORDSETFOCUS(1)
                        *************
                        nivel_acess := sen->snivel
                        cod_operado := sen->scod_op
                        senha_acess := msenha
                        RETURN .T.
                ENDIF

                WHILE ! EOF()
                        IF msenha == DCRIPTO(ssenha)
                                EXIT
                        ENDIF
                        SKIP
                ENDDO
                IF ! EOF() .AND. msenha == DCRIPTO(ssenha)
                        IF BLOQREG()
                                sen-> sult_ent := mdata_sis
                                sen-> shora_ini := TIME()
                                nivel_acess := snivel
                                cod_operado := scod_op
                                senha_acess := msenha
                                COMMIT
                                UNLOCK
                                CLOSE sen
                                RETURN .T.
                        ELSE
                                atencao('Nao foi Possivel Acessar o Arquivo')
                                LOOP
                        ENDIF
                ELSE
                        limpa(24,31,24,79)
                        DEVPOS(24,31);DEVOUT('Senha Incorreta Tente Novamente')
                        TONE(100,6)
                        TONE(1600,6)
                        INKEY(2)
                        limpa(24,31,24,79)
                        IF mcont = 3
                                SETCOLOR(cor)
                                RETURN .F.
                        ENDIF
                        LOOP
                ENDIF

        ENDDO
ENDDO
********************************** f i m **************************************
**********************
* ALTERA A SENHA
**********************

FUNCTION alt_senha

LOCAL mcont,msenha,lci,cci,lba,cba,tela,msele,morde,mmen,mcor

mcont:=0
lci := 12
cci := 20
lba := 14
cba := 60
mmen := SAVESCREEN(23,00,23,79)
tela := SAVESCREEN(00,00,24,79)
mcor := SETCOLOR()
msele := SELE()
morde := INDEXORD()
*-----------------------------------------------------
IF ! ABRARQ('sen')
        IF ! abriarq('insopera','sen');RETURN NIL;ENDIF
ENDIF
*-----------------------------------------------------
WHILE .T.
        *************
        SELE('sen')
        ORDSETFOCUS(1)
        GO TOP
        *************

        SEEK CRIPTO(senha_acess)

        IF FOUND()
                IF BLOQREG()
                        sen-> sult_ent := mdata_sis
                        sen-> shora_fim := TIME()
                        nivel_acess := snivel
                        cod_operado := scod_op
                        senha_acess := msenha
                        COMMIT
                        UNLOCK
                        setcor(4)
                        DEVPOS(04,67);DEVOUT('Operador: '+cod_operado)
                        setcor(1)
*                       atencao('Operador: '+cod_operado+'-'+RTRIM(snome))
                ELSE
                        atencao('Nao foi Possivel Acessar o Arquivo')
                        LOOP
                ENDIF
        ENDIF

        WHILE .T.
                botao(lci,cci,lba,cba)
                SETCOLOR('N/W,X')
                DEVPOS(lci+1,cci+2);DEVOUT('Digite a Senha de Acesso:')
                msenha = ''
                pegasenha(@msenha,lci+1,COL()+1,setcor(1))
                setcor(1)
                IF msenha = 'NAO'
                        LOOP
                ENDIF
                mcont ++
                GO TOP
                IF EMPTY(msenha)
                        limpa(23,31,23,79)
                        DEVPOS(23,31);DEVOUT('Senha Incorreta Tente Novamente')
                        TONE(100,6)
                        TONE(1600,6)
                        INKEY(2)
                        limpa(23,31,23,79)
                        LOOP
                ENDIF

                WHILE ! EOF()
                        IF msenha == DCRIPTO(ssenha)
                                EXIT
                        ENDIF
                        SKIP
                ENDDO
                IF ! EOF() .AND. msenha == DCRIPTO(ssenha)
                        *************
                        SELE('sen')
                        ORDSETFOCUS(1)
                        *************
                        IF BLOQREG()
                                sen-> sult_ent := mdata_sis
                                sen-> shora_ini := TIME()
                                nivel_acess := snivel
                                cod_operado := scod_op
                                senha_acess := msenha
                                COMMIT
                                UNLOCK
                                RESTSCREEN(00,00,24,79,tela)
                                RESTSCREEN(23,00,23,79,mmen)
                                setcor(4)
                                DEVPOS(04,67);DEVOUT('Operador: '+cod_operado)
                                setcor(1)
                                atencao('Operador: '+cod_operado+'-'+RTRIM(snome))
                                SETCOLOR(mcor)
                                SELE(msele)
                                ORDSETFOCUS(morde)
                                RETURN .T.
                        ELSE
                                atencao('Nao foi Possivel Acessar o Arquivo')
                                LOOP
                        ENDIF
                ELSE
                        limpa(lci+1,cci+2,lci+1,cba-2)
                        DEVPOS(lci+2,cci+2);DEVOUT('Senha Incorreta Tente Novamente')
                        TONE(100,6)
                        TONE(1600,6)
                        INKEY(2)
                        LOOP
                ENDIF
        ENDDO
ENDDO
********************************** f i m **************************************
******************************************
* CRIACAO DO ARQUIVO DE SENHAS
******************************************

FUNCTION arqsenha(ntx)
*******************

LOCAL matsen:={},mdata_sis

mdata_sis := DATE()
IF ! FILE('\'+mdrive+'\insopera.dbf')
        matsen:={ }
        AADD(matsen,{'scod_op','C',3,0})
        AADD(matsen,{'snome','C',30,0})
        AADD(matsen,{'ssenha','C',8,0})
        AADD(matsen,{'sdata_cad','D',8,0})
        AADD(matsen,{'scomissao','N',6,2})
        AADD(matsen,{'scota','N',12,2})
        AADD(matsen,{'sult_ent','D',8,0})
        AADD(matsen,{'snivel','C',1,0})
        AADD(matsen,{'shora_ini','C',5,2})
        AADD(matsen,{'shora_fim','C',5,2})
        AADD(matsen,{'sult_e_c','D',8,0})
        AADD(matsen,{'sh_i_c','C',5,2})
        DBCREATE('\'+mdrive+'\insopera.dbf',matsen)
ENDIF
IF ! FILE('\'+mdrive+'\insopera.cdx')
        IF ! abriarq('insopera','sen','E');RETURN NIL;ENDIF
        INDEX ON ssenha TAG senha TO ('\'+mdrive+'\insopera')
        INDEX ON scod_op TAG codop TO ('\'+mdrive+'\insopera')
        CLOSE ALL
ENDIF
IF ! abriarq('insopera','sen');RETURN NIL;ENDIF
IF LASTREC() = 0
        IF ADIREG()
                sen-> scod_op := '999'
                sen-> snome := 'HRB SOLUCOES EM INFORMATICA'
                sen-> ssenha := CRIPTO('ISCI')
                sen-> sdata_cad := mdata_sis
                sen-> snivel := '1'
                COMMIT
                UNLOCK
        ELSE
                atencao('Nao foi Possivel Acessar o Arquivo')
                CLOSE sen
                RETURN .T.
        ENDIF
ENDIF
RETURN NIL
************************ F I M ************************************************************************

*********************************************
*** SENHA1.PRG:  Menu de Operadores
*********************************************

FUNCTION senha1
***************

LOCAL tela,tela1,mcod_ven,desc,lci,cci,lba,cba,li,ci,lb,cb,pode,point

lci := 07
cci := 19
lba := 18
cba := 76

li := 20
ci := 19
lb := 22
cb := 76

tela := SAVESCREEN(01,00,24,79)

*-----------------------------------------------------
CLOSE ALL
IF ! ABRARQ('sen')
        IF ! abriarq('insopera','sen');RETURN NIL;ENDIF
ENDIF
*-----------------------------------------------------
mcod_ven := 0
desc := SPACE(20)
setcor(3)
botao(lci,cci,lba,cba)
@ lci+1,cci+1 TO lci+1,cba-1
@ lci+1,cci+14 SAY CHR(194)
@ lba,cci+14 SAY CHR(193)
@ lci+2,cci+14 TO lba-1,cci+14
setcor(1)
@ lci,cci+1 SAY " Operadores "

DEVPOS(lci+2,cci+2);DEVOUT('Cod.:')
DEVPOS(lci+2,cci+16);DEVOUT('Operador:')
DEVPOS(lci+3,cci+2);DEVOUT('Cod.:')
DEVPOS(lci+3,cci+16);DEVOUT('Operador:')
DEVPOS(lci+4,cci+2);DEVOUT('Cod.:')
DEVPOS(lci+4,cci+16);DEVOUT('Operador:')
DEVPOS(lci+5,cci+2);DEVOUT('Cod.:')
DEVPOS(lci+5,cci+16);DEVOUT('Operador:')
DEVPOS(lci+6,cci+2);DEVOUT('Cod.:')
DEVPOS(lci+6,cci+16);DEVOUT('Operador:')
DEVPOS(lci+7,cci+2);DEVOUT('Cod.:')
DEVPOS(lci+7,cci+16);DEVOUT('Operador:')
DEVPOS(lci+8,cci+2);DEVOUT('Cod.:')
DEVPOS(lci+8,cci+16);DEVOUT('Operador:')
DEVPOS(lci+9,cci+2);DEVOUT('Cod.:')
DEVPOS(lci+9,cci+16);DEVOUT('Operador:')
DEVPOS(lci+10,cci+2);DEVOUT('Cod.:')
DEVPOS(lci+10,cci+16);DEVOUT('Operador:')

**************
SELE('sen')
ORDSETFOCUS(2)
**************

GO TOP

pode=.T.

WHILE .T.
        mensagem("<I>nclusÑo  <A>lteraáÑo  <E>xclusÑo  < ou > Consulta  <ESC> Retorna" )
        point=RECNO()
        FOR f=1 TO 9
                IF ! EOF() .AND. scod_op <> '999' .AND. scod_op <> '111'
                        setcor(3)
                        @ lci+1+f,cci+8 SAY scod_op
                        @ lci+1+f,cci+26 SAY snome
                        setcor(1)
                        SKIP
                ELSE
                        setcor(3)
                        @ lci+1+f,cci+8 SAY "   "
                        @ lci+1+f,cci+26 SAY SPACE(30)
                        setcor(1)
                ENDIF
        NEXT F
        IF EOF()
                pode=.F.
        ENDIF
        GO POINT
        INKEY(0)
        DO CASE
                CASE LASTKEY()=ASC("I") .OR. LASTKEY()=ASC("i")
                        IF ! nivel('1')
                                atencao('Acesso nao Autorizado !!!')
                                IF ! aut_sen('Senha de Liberacao de Ambiente:')
                                        UNLOCK
                                        LOOP
                                ENDIF
                        ENDIF
                        senha2()
                        GO TOP
                        SKIP
                CASE LASTKEY()=5
                        IF ! BOF()
                                SKIP -1
                                pode=.T.
                        ENDIF
                CASE LASTKEY()=24
                        IF ! EOF() .AND. pode=.T.
                                SKIP
                        ENDIF
                CASE LASTKEY()=ASC("A") .OR. LASTKEY()=ASC("a")
                        ****************************************************
                        * VERIFICAR NIVEL DE ACESSO DA SENHA
                        ****************************************************
                        IF ! nivel('1')
                                atencao('Acesso nao Autorizado !!!')
                                IF ! aut_sen('Senha de Liberacao de Ambiente:')
                                        UNLOCK
                                        LOOP
                                ENDIF
                        ENDIF
                        ****************************************************
                        setcor(1)
                        senha3()
                        GO TOP
                        SKIP
                CASE LASTKEY()=ASC("E") .OR. LASTKEY()=ASC("e")
                        IF ! nivel('1')
                                atencao('Acesso nao Autorizado !!!')
                                IF ! aut_sen('Senha de Liberacao de Ambiente:')
                                        UNLOCK
                                        LOOP
                                ENDIF
                        ENDIF
                        senha4()
                        GO TOP
                        SKIP
                CASE LASTKEY()=27
                        RESTSCREEN(01,00,24,79,tela)
                        CLOSE ALL
                        RETURN NIL
        ENDCASE
ENDDO
************************ F I M ************************************************************************
*************************
* MENU INCLUSAO DE SENHAS
*************************

FUNCTION senha2
***************

LOCAL opcao,mcod_vend,mnome,vsenha1,vsenha,mdata_cad,vnivel,mcomissao,mcota

PRIVATE lci,cci,lba,cba

lci:=10
cci:=23
lba:=18
cba:=72
tela:=SAVESCREEN(01,00,24,79)

WHILE .T.
        mcod_vend = 0
        mnome = SPACE(30)
        vsenha = ''
        vnivel = SPACE(1)
        mcomissao := 0
        opcao = 'S'
        mcota := 0
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        janela(lci,cba,' Inclusao ')
        mensagem('Preencha os Campos - <ESC> p/Retornar')
        DEVPOS(lci+1,cci+2);DEVOUT('Cod.Operador..:')
        DEVPOS(lci+2,cci+2);DEVOUT('Nome..........:')
        DEVPOS(lci+3,cci+2);DEVOUT('Nivel.........:')
        DEVPOS(lci+4,cci+2);DEVOUT('Comissao (%)..:')
        DEVPOS(lci+5,cci+2);DEVOUT('Cota R$.......:')
        DEVPOS(lci+6,cci+2);DEVOUT('Data..........:')
        DEVPOS(lci+7,cci+2);DEVOUT('Senha.........:')
        @ lci+1,cci+18 GET mcod_vend PICT '999'
        READ
        IF LASTKEY() = 27
                SET CURSOR ON
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF

        SET INTEN ON
        ***********
        SELE('sen')
        ORDSETFOCUS(2)
        ***********
        GO TOP

        SEEK STRZERO(mcod_vend,3)

        IF FOUND()
                atencao('Codigo ja Existe no cadastro !!!')
                LOOP
        ENDIF

        SET INTEN ON
        setcor(3)
        mdata_cad = mdata_sis
        DEVPOS(lci+1,cci+18);DEVOUT(STRZERO(mcod_vend,3))
        DEVPOS(lci+6,cci+18);DEVOUT(mdata_cad)
        setcor(1)
        @ lci+2,cci+18 GET mnome PICT '@!'
        @ lci+3,cci+18 GET vnivel PICT '9' VALID vnivel $ '1,2,3,4,5,6,7,8,9'
        @ lci+4,cci+18 GET mcomissao PICT '99.99'
        @ lci+5,cci+18 GET mcota PICT '99,999,999.99'
        READ

        IF LASTKEY() = 27
                SET CURSOR ON
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        ***********
        SELE('sen')
        ORDSETFOCUS(1)
        ***********
        WHILE .T.
                GO TOP
                vsenha = ''
                vsenha1 = ''
                mensagem('Digite sua Senha')
                pegasenha(@vsenha,lci+7,cci+18,setcor(3))
                setcor(1)
                IF LEN(vsenha) <= 1
                        LOOP
                ENDIF
                IF vsenha = 'NAO'
                        EXIT
                ENDIF
                IF EMPTY(vsenha)
                        LOOP
                ENDIF

                WHILE ! EOF()
                        IF vsenha == DCRIPTO(ssenha)
                                EXIT
                        ENDIF
                        SKIP
                ENDDO
                IF ! EOF() .AND. vsenha == DCRIPTO(ssenha)
                        atencao('Senha Invalida, Tente novamente')
                        LOOP
                ENDIF
                mensagem('Digite Novamente sua Senha')
                pegasenha(@vsenha1,lci+7,cci+18,setcor(3))
                setcor(1)
                IF LEN(vsenha1) <= 1
                        LOOP
                ENDIF
                IF vsenha1 = 'NAO'
                        EXIT
                ENDIF
                IF vsenha == vsenha1
                        mensagem('Confirma a Inclusao do Operador [S/n]:')
                        opcao := 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27
                                EXIT
                        ENDIF
                        IF opcao = 'N'
                                EXIT
                        ELSE
                                ADIREG()
                                sen-> scod_op := STRZERO(mcod_vend,3)
                                sen-> snome := mnome
                                sen-> ssenha := CRIPTO(vsenha)
                                sen-> sdata_cad := mdata_sis
                                sen-> snivel := vnivel
                                sen-> scomissao := mcomissao
                                sen-> scota := mcota
                                COMMIT
                                UNLOCK
                                EXIT
                        ENDIF
                ELSE
                        atencao('Senha Digitada Errada, Tente novamente !!!')
                        LOOP
                ENDIF
        ENDDO
ENDDO
RETURN NIL
*********************** f i m ************************************

*************************
* MENU ALTERACAO DE SENHAS
*************************

FUNCTION senha3
***************

LOCAL opcao,mcod_vend,mnome,vsenha1,vsenha,mdata_cad,vnivel,mcomissao,mcota

lci:=10
cci:=23
lba:=18
cba:=72
tela:=SAVESCREEN(01,00,24,79)

WHILE .T.
        mcod_vend = 0
        mnome = SPACE(30)
        vnivel = SPACE(1)
        opcao = SPACE(1)
        mcomissao := 0
        vsenha = ''
        vsenha1 = ''
        mcota := 0
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        janela(lci,cba,' Alteracao ')
        mensagem('Digite o Codigo do Operador a ser Alterado - <ESC> p/Abandonar')
        ***********
        SELE('sen')
        ORDSETFOCUS(2)
        ***********
        GO TOP

        DEVPOS(lci+1,cci+2);DEVOUT('Cod.Operador..:')
        DEVPOS(lci+2,cci+2);DEVOUT('Nome..........:')
        DEVPOS(lci+3,cci+2);DEVOUT('Nivel.........:')
        DEVPOS(lci+4,cci+2);DEVOUT('Comissao (%)..:')
        DEVPOS(lci+5,cci+2);DEVOUT('Cota R$.......:')
        DEVPOS(lci+6,cci+2);DEVOUT('Data..........:')
        DEVPOS(lci+7,cci+2);DEVOUT('Senha.........:')

        @ lci+1,cci+18 GET mcod_vend PICT '999'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF EMPTY(mcod_vend)
                LOOP
        ENDIF

        SEEK STRZERO(mcod_vend,3)

        IF ! FOUND()
                atencao('Codigo nao Cadastrado !!!')
                LOOP
        ENDIF
        IF ! BLOQREG()
                atencao('Nao foi possivel acessar o Arquivo !!!')
                LOOP
        ENDIF
        vsenha := ''
        mnome := snome
        vnivel := snivel
        mcomissao := scomissao
        mcota := scota
        setcor(3)
        DEVPOS(lci+1,cci+18);DEVOUT(STRZERO(mcod_vend,3))
        DEVPOS(lci+5,cci+18);DEVOUT(sdata_cad )
        setcor(1)
        @ lci+2,cci+18 GET mnome PICT '@!'
        @ lci+3,cci+18 GET vnivel PICT '9' VALID vnivel $ '1,2,3,4,5,6,7,8,9'
        @ lci+4,cci+18 GET mcomissao PICT '99.99'
        @ lci+5,cci+18 GET mcota PICT '99,999,999.99'
        READ
        IF LASTKEY() = 27
                UNLOCK
                LOOP
        ENDIF
        mensagem('Deseja alterar a senha [s/N]:')
        opcao := 'N'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27
                UNLOCK
                LOOP
        ENDIF
        IF opcao = 'S'
                WHILE .T.
                        vsenha = ''
                        vsenha1 = ''
                        mensagem('Digite sua Senha')
                        pegasenha(@vsenha,lci+7,cci+18,setcor(3))
                        setcor(1)
                        IF LEN(vsenha) <= 1
                                LOOP
                        ENDIF
                        IF vsenha = 'NAO'
                                vsenha := ''
                                EXIT
                        ENDIF
                        IF EMPTY(vsenha)
                                LOOP
                        ENDIF
                        mensagem('Digite Novamente sua Senha')
                        pegasenha(@vsenha1,lci+7,cci+18,setcor(3))
                        setcor(1)
                        IF LEN(vsenha1) <= 1
                                LOOP
                        ENDIF
                        IF vsenha1 = 'NAO'
                                vsenha := ''
                                EXIT
                        ENDIF
                        IF vsenha == vsenha1
                                EXIT
                        ELSE
                                atencao('Senha Digitada Errada, Tente novamente !!!')
                                LOOP
                        ENDIF
                ENDDO
        ENDIF
        mensagem('Confirma a Alteracao [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27
                UNLOCK
                LOOP
        ENDIF
        IF opcao = 'N'
                UNLOCK
                LOOP
        ELSE
                IF vsenha == ''
                        sen-> snome := mnome
                        sen-> snivel := vnivel
                        sen-> scomissao := mcomissao
                        sen-> scota := mcota
                ELSE
                        sen-> snome := mnome
                        sen-> snivel := vnivel
                        sen-> scomissao := mcomissao
                        sen-> ssenha := CRIPTO(vsenha)
                        sen-> scota := mcota
                ENDIF
                COMMIT
                UNLOCK
                LOOP
        ENDIF
ENDDO
RETURN NIL
*********************** f i m ************************************

*************************
* MENU EXCLUSAO DE SENHAS
*************************

FUNCTION senha4
***************

LOCAL opcao,mcod_vend,vsenha

lci:=10
cci:=23
lba:=18
cba:=72
tela:=SAVESCREEN(01,00,24,79)

WHILE .T.
        mcod_vend = 0
        mnome = SPACE(30)
        vsenha := SPACE(8)
        opcao = SPACE(1)
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        janela(lci,cba,' Exclusao ')
        mensagem('Digite o Codigo do Operador a ser Excluido - <ESC> p/Abandonar')
        ***********
        SELE('sen')
        ORDSETFOCUS(2)
        ***********
        GO TOP

        DEVPOS(lci+1,cci+2);DEVOUT('Cod.Operador..:')
        DEVPOS(lci+2,cci+2);DEVOUT('Nome..........:')
        DEVPOS(lci+3,cci+2);DEVOUT('Nivel.........:')
        DEVPOS(lci+4,cci+2);DEVOUT('Comissao (%)..:')
        DEVPOS(lci+5,cci+2);DEVOUT('Cota R$.......:')
        DEVPOS(lci+6,cci+2);DEVOUT('Data..........:')
        DEVPOS(lci+7,cci+2);DEVOUT('Senha.........:')

        @ lci+1,cci+18 GET mcod_vend PICT '999'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF EMPTY(mcod_vend)
                LOOP
        ENDIF

        SEEK STRZERO(mcod_vend,3)

        IF ! FOUND()
                atencao('Codigo nao Cadastrado !!!')
                LOOP
        ENDIF
        setcor(3)
        DEVPOS(lci+1,cci+18);DEVOUT(STRZERO(mcod_vend,3))
        DEVPOS(lci+2,cci+18);DEVOUT(snome)
        DEVPOS(lci+3,cci+18);DEVOUT(snivel)
        DEVPOS(lci+4,cci+18);DEVOUTPICT(scomissao,'99.99')
        DEVPOS(lci+5,cci+18);DEVOUTPICT(scota,'99,999,999.99')
        DEVPOS(lci+6,cci+18);DEVOUT(sdata_cad )
        DEVPOS(lci+7,cci+18);DEVOUT(vsenha)
        setcor(1)
        mensagem('Confirma a Exclusao [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF opcao = 'N'
                LOOP
        ELSE
                IF BLOQARQ()
                        DELE
                        COMMIT
                        UNLOCK
                        LOOP
                ELSE
                        atencao('Nao foi possivel acessar o Arquivo !!!')
                        LOOP
                ENDIF
        ENDIF
ENDDO
RETURN NIL
*********************** f i m ************************************
***************************
* FINALIZA OS SISTEMAS
***************************

FUNCTION fim(msist)
*************

LOCAL opcao,lci,cci,lba,cba,tela

lci := 05
cci := 25
lba := 20
cba := 53

tela := SAVESCREEN(00,00,24,79)
opcao := 0

WHILE .T.
        setcor(3)
        limpa(23,00,23,79)
*       botao(lci,cci,lba,cba)
        botao(lci,cci,lba,cba)
        DEVPOS(lci+1,cci+2);DEVOUT('ESCOLHA A OPCAO QUE DESEJA')
        @ lci+2,cci+1 TO lci+2,cba-1
        botao(lci+3,cci+1,lci+5,cba-1)
        botao(lci+6,cci+1,lci+8,cba-1)
        botao(lci+9,cci+1,lci+11,cba-1)
        botao(lci+12,cci+1,lci+14,cba-1)
        @ lci+4,cci+2 PROMPT '   Finalizar o sistema   ' MESSAGE '** Sai do sistema **'
        @ lci+7,cci+2 PROMPT ' Nao finalizar o sistema ' MESSAGE '** Retorna ao sistema **'
        @ lci+10,cci+2 PROMPT '     Deixar na senha     ' MESSAGE '** Deixa na senha **'
        @ lci+13,cci+2 PROMPT '    Protecao de tela     ' MESSAGE '** Protecao de tela **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                setcor(1)
                RETURN NIL
        ENDIF

        DO CASE
                CASE opcao = 1
                        botao(lci+3,cci+1,lci+5,cba-1,'*')
                        SET CURSOR ON
                        CLOSE ALL
                        *-----------------------------------------------------
                        IF ! ABRARQ('sen')
                                IF ! abriarq('insopera','sen');RETURN NIL;ENDIF
                        ENDIF
                        *-----------------------------------------------------
                        *************
                        SELE('sen')
                        ORDSETFOCUS(1)
                        GO TOP
                        *************

                        SEEK CRIPTO(senha_acess)

                        IF ! FOUND()
                                atencao('Operador foi Excluido !!!')
                        ELSE
                                IF BLOQREG()
                                        sen-> shora_fim := TIME()
                                        COMMIT
                                        UNLOCK
                                ELSE
                                        atencao('Nao foi possivel Acessar o Arquivo !!!')
                                        RETURN NIL
                                ENDIF
                        ENDIF
                        UNLOCK
                        CLOSE ALL
                        SETCOLOR('w/n')
                        limpa(00,00,24,79)
                        BANNER(00,RTRIM(memp),'b+/n,x')
                        BANNER(07,RTRIM(resa),'b+/n,x')
                        SETCOLOR('W+/N,X')
                        @ 14,02 SAY REPLI('ﬂ',LEN('Copia Exclusiva: '+memp+resa))
                        @ 15,02 SAY 'Copia Exclusiva: '
                        setcor(6)
                        DEVPOS(15,19);DEVOUT(memp+resa)
                        SETCOLOR('W+/N,X')
                        @ 16,02 SAY REPLI('‹',LEN('Copia Exclusiva: '+memp+resa))
                        botao(18,02,21,56)
                        DEVPOS(19,04);DEVOUT('A tentativa de COPIA deste Sistema pode acarretar')
                        DEVPOS(20,04);DEVOUT('Problemas irreversiveis para seu Computador')
                        DEVPOS(22,00);DEVOUT('')
                        SETCOLOR(cor1)
                        CHDIR('\'+mdrive)
                        CLEAR MEMORY
                        CLOSE ALL
                        QUIT
                CASE opcao = 2
                        botao(lci+6,cci+1,lci+8,cba-1,'*')
                        RESTSCREEN(00,00,24,79,tela)
                        setcor(1)
                        RETURN NIL
                CASE opcao = 3
                        botao(lci+9,cci+1,lci+11,cba-1,'*')
                        RESTSCREEN(00,00,24,79,tela)
                        setcor(1)
                        alt_senha()
                        RETURN NIL
                CASE opcao = 4
                        botao(lci+12,cci+1,lci+14,cba-1,'*')
                        SETCOLOR(cor)
                        limpa(00,00,24,79)
                        prot_tela()
                        RESTSCREEN(00,00,24,79,tela)
                        setcor(1)
                        RETURN NIL
       ENDCASE
ENDDO
RETURN NIL
***************************** F I M *********************************************
*****************************
* FUNCAO PARA CRIPTOGRAFA
*****************************

FUNCTION cripto(mexp)
*********************

LOCAL mletra:={},maux:={},msenha,i

IF LASTKEY() = 27
        RETURN .F.
ENDIF
IF LEN(mexp) < 2
        atencao('Senha Invalida !!!')
        RETURN .F.
ENDIF
IF LEN(mexp) > 8
        atencao('Senha Invalida !!!')
        RETURN .F.
ENDIF
msenha := ''
i:=0
FOR i = 1 TO LEN(mexp)
        AADD(mletra,SUBSTR(mexp,i,1))
NEXT
i:=0
FOR i = 1 TO LEN(mexp)
        IF mletra[i] = 'A'
                AADD(maux,CHR(189))
        ELSEIF mletra[i] = 'B'
                AADD(maux,CHR(184))
        ELSEIF mletra[i] = 'C'
                AADD(maux,CHR(154))
        ELSEIF mletra[i] = 'D'
                AADD(maux,CHR(181))
        ELSEIF mletra[i] = 'E'
                AADD(maux,CHR(228))
        ELSEIF mletra[i] = 'F'
                AADD(maux,CHR(230))
        ELSEIF mletra[i] = 'G'
                AADD(maux,CHR(232))
        ELSEIF mletra[i] = 'H'
                AADD(maux,CHR(218))
        ELSEIF mletra[i] = 'I'
                AADD(maux,CHR(204))
        ELSEIF mletra[i] = 'J'
                AADD(maux,CHR(236))
        ELSEIF mletra[i] = 'K'
                AADD(maux,CHR(231))
        ELSEIF mletra[i] = 'L'
                AADD(maux,CHR(245))
        ELSEIF mletra[i] = 'M'
                AADD(maux,CHR(225))
        ELSEIF mletra[i] = 'N'
                AADD(maux,CHR(237))
        ELSEIF mletra[i] = 'O'
                AADD(maux,CHR(224))
        ELSEIF mletra[i] = 'P'
                AADD(maux,CHR(208))
        ELSEIF mletra[i] = 'Q'
                AADD(maux,CHR(166))
        ELSEIF mletra[i] = 'R'
                AADD(maux,CHR(168))
        ELSEIF mletra[i] = 'S'
                AADD(maux,CHR(172))
        ELSEIF mletra[i] = 'T'
                AADD(maux,CHR(157))
        ELSEIF mletra[i] = 'U'
                AADD(maux,CHR(140))
        ELSEIF mletra[i] = 'W'
                AADD(maux,CHR(174))
        ELSEIF mletra[i] = 'V'
                AADD(maux,CHR(20))
        ELSEIF mletra[i] = 'X'
                AADD(maux,CHR(223))
        ELSEIF mletra[i] = 'Y'
                AADD(maux,CHR(239))
        ELSEIF mletra[i] = 'Z'
                AADD(maux,CHR(235))
        ELSEIF mletra[i] = '0'
                AADD(maux,CHR(251))
        ELSEIF mletra[i] = '1'
                AADD(maux,CHR(253))
        ELSEIF mletra[i] = '2'
                AADD(maux,CHR(252))
        ELSEIF mletra[i] = '3'
                AADD(maux,CHR(248))
        ELSEIF mletra[i] = '4'
                AADD(maux,CHR(216))
        ELSEIF mletra[i] = '5'
                AADD(maux,CHR(200))
        ELSEIF mletra[i] = '6'
                AADD(maux,CHR(136))
        ELSEIF mletra[i] = '7'
                AADD(maux,CHR(127))
        ELSEIF mletra[i] = '8'
                AADD(maux,CHR(21))
        ELSEIF mletra[i] = '9'
                AADD(maux,CHR(23))
        ELSE
                LOOP
        ENDIF
NEXT
i:=0
msenha := maux[2] + maux[1]
FOR i = 3 TO LEN(mexp)
        msenha := msenha + maux[i]
NEXT
RETURN msenha
*************************** F I M ***********************************************
*****************************
* FUNCAO PARA DES-CRIPTOGRAFA
*****************************

FUNCTION dcripto(mexp)
*********************

LOCAL mletra:={},maux:={},msenha,i

ASIZE(mletra,0)
ASIZE(maux,0)
msenha := ''
i:=0
FOR i = 1 TO 8
        IF SUBSTR(mexp,i,1) = ' ' .OR. SUBSTR(mexp,i,1) = NIL
                LOOP
        ENDIF
        AADD(mletra,SUBSTR(mexp,i,1))
NEXT
i:=0
IF EMPTY(mletra)
        RETURN NIL
ENDIF

FOR i = 1 TO LEN(mletra)
        IF mletra[i] = CHR(189)
                AADD(maux,'A')
        ELSEIF mletra[i] = CHR(184)
                AADD(maux,'B')
        ELSEIF mletra[i] = CHR(154)
                AADD(maux,'C')
        ELSEIF mletra[i] = CHR(181)
                AADD(maux,'D')
        ELSEIF mletra[i] = CHR(228)
                AADD(maux,'E')
        ELSEIF mletra[i] = CHR(230)
                AADD(maux,'F')
        ELSEIF mletra[i] = CHR(232)
                AADD(maux,'G')
        ELSEIF mletra[i] = CHR(218)
                AADD(maux,'H')
        ELSEIF mletra[i] = CHR(204)
                AADD(maux,'I')
        ELSEIF mletra[i] = CHR(236)
                AADD(maux,'J')
        ELSEIF mletra[i] = CHR(231)
                AADD(maux,'K')
        ELSEIF mletra[i] = CHR(245)
                AADD(maux,'L')
        ELSEIF mletra[i] = CHR(225)
                AADD(maux,'M')
        ELSEIF mletra[i] = CHR(237)
                AADD(maux,'N')
        ELSEIF mletra[i] = CHR(224)
                AADD(maux,'O')
        ELSEIF mletra[i] = CHR(208)
                AADD(maux,'P')
        ELSEIF mletra[i] = CHR(166)
                AADD(maux,'Q')
        ELSEIF mletra[i] = CHR(168)
                AADD(maux,'R')
        ELSEIF mletra[i] = CHR(172)
                AADD(maux,'S')
        ELSEIF mletra[i] = CHR(157)
                AADD(maux,'T')
        ELSEIF mletra[i] = CHR(140)
                AADD(maux,'U')
        ELSEIF mletra[i] = CHR(174)
                AADD(maux,'W')
        ELSEIF mletra[i] = CHR(20)
                AADD(maux,'V')
        ELSEIF mletra[i] = CHR(223)
                AADD(maux,'X')
        ELSEIF mletra[i] = CHR(239)
                AADD(maux,'Y')
        ELSEIF mletra[i] = CHR(235)
                AADD(maux,'Z')
        ELSEIF mletra[i] = CHR(251)
                AADD(maux,'0')
        ELSEIF mletra[i] = CHR(253)
                AADD(maux,'1')
        ELSEIF mletra[i] = CHR(252)
                AADD(maux,'2')
        ELSEIF mletra[i] = CHR(248)
                AADD(maux,'3')
        ELSEIF mletra[i] = CHR(216)
                AADD(maux,'4')
        ELSEIF mletra[i] = CHR(200)
                AADD(maux,'5')
        ELSEIF mletra[i] = CHR(136)
                AADD(maux,'6')
        ELSEIF mletra[i] = CHR(127)
                AADD(maux,'7')
        ELSEIF mletra[i] = CHR(21)
                AADD(maux,'8')
        ELSEIF mletra[i] = CHR(23)
                AADD(maux,'9')
        ELSE
                LOOP
        ENDIF
NEXT
i:=0
msenha := maux[2] + maux[1]
FOR i = 3 TO LEN(maux)
        msenha := msenha + maux[i]
NEXT
RETURN msenha
******************************* F I M ******************************************
//
// Function PegaChar
// Programada por Paulo Nasi em 21.04.1992
// Recupera caracter digitado pelo usuario
//
Function PegaChar

SETPOS(25,00)
Return Inkey(0)



//
// Function PegaSenha
// Programada por Paulo Nasi em 29.06.1993
// Pega senha digitada pela usuario
//
Procedure PegaSenha ( Senha, LinSenha, ColSenha , cor )

*#include 'INKEY.CH'

Local SenhaTamMax  := 8
Local CharDigitado
Local SenhaTela    := ""
limpa(linsenha,colsenha,linsenha,colsenha+9)

Do While Len(SenhaTela) <= SenhaTamMax

   CharDigitado := PegaChar()
   Do Case
      Case CharDigitado >= 97 .And. CharDigitado <= 122   /* ALFA MINUSCULA */
           Senha     := Senha + Upper(Chr(CharDigitado))
           SenhaTela := SenhaTela + "*"
      Case CharDigitado >= 65 .And. CharDigitado <= 90    /* ALFA MAIUSCULA */
           Senha     := Senha + Chr(CharDigitado)
           SenhaTela := SenhaTela + "*"
      Case CharDigitado >= 48 .And. CharDigitado <= 57    /* NUMERICO */
           Senha     := Senha + Chr(CharDigitado)
           SenhaTela := SenhaTela + "*"
      Case (CharDigitado = K_LEFT) .Or. (CharDigitado = K_BS)
           If Len(Senha) != 0
              Senha     := Left(Senha,Len(Senha)-1)
              SenhaTela := Left(SenhaTela,Len(SenhaTela)-1)
           EndIf
      Case CharDigitado = K_ENTER
           Exit
      Case CharDigitado = K_ESC
           Senha := "NAO"
           Exit
   EndCase
   IF cor <> NIL
          SETCOLOR(cor)
   ENDIF
   @ LinSenha,ColSenha Say PadR(SenhaTela,SenhaTamMax)

EndDo

Return
********************************* f i m ********************************
************************************
* FUNCAO PARA QUALIDADE DE IMPRESSAO
************************************

FUNCTION imprt(imp,tipo,linha,col)
******************************

IF col = NIL
        col := 0
ENDIF
IF linha = NIL
        linha := 0
ENDIF
IF imp = 'M' .AND. tipo = 'C'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(15))
        RETURN .T.
ELSEIF imp = 'M' .AND. tipo = 'N'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(18))
        RETURN .T.
ELSEIF imp = 'M' .AND. tipo = 'E'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(14))
        RETURN .T.
ELSEIF imp = 'M' .AND. tipo = 'M'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(15)+CHR(14))
        RETURN .T.
ELSEIF imp = 'M' .AND. tipo = 'P8'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(27)+'0')
        RETURN .T.
ELSEIF imp = 'M' .AND. tipo = 'P6'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(27)+'2')
        RETURN .T.
ELSEIF imp = 'J' .AND. tipo = 'C'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(27)+'(s15H')
        RETURN NIL
ELSEIF imp = 'J' .AND. tipo = 'N'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(27)+'(s10H')
        RETURN NIL
ELSEIF imp = 'J' .AND. tipo = 'M'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(27)+'(s10H')
        RETURN NIL
ELSEIF imp = 'J' .AND. tipo = 'E'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(27)+'(s5H')
        RETURN NIL
ELSEIF imp = 'J' .AND. tipo = 'P8'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(27)+'(s1P')
        RETURN NIL
ELSEIF imp = 'J' .AND. tipo = 'P6'
        DEVPOS(PROW()+linha,col);DEVOUT(CHR(27)+'(s0P')
        RETURN NIL
ENDIF
******************************** F I M ********************************
******************************************
* CRIACAO DO ARQUIVO DE SETUP
******************************************

FUNCTION arqsetup()
*******************

LOCAL matsen:={}

IF ! FILE('sacsetup.dbf')
        matsen:={ }
        AADD(matsen,{'pis','N',5,2})
        AADD(matsen,{'confis','N',5,2})
        AADD(matsen,{'imposto','N',5,2})
        AADD(matsen,{'fatura','N',5,2})
        AADD(matsen,{'vlr_ent','N',6,2})
        AADD(matsen,{'impress','C',1,0})
        AADD(matsen,{'terminal','C',4,0})
        AADD(matsen,{'impr_term','C',1,0})
        AADD(matsen,{'copia','N',2,0})
        AADD(matsen,{'form','C',1,0})
        AADD(matsen,{'pagina','C',1,0})
        AADD(matsen,{'percent','C',1,0})
        AADD(matsen,{'relatorio','C',4,0})
        AADD(matsen,{'nota','C',4,0})
        AADD(matsen,{'uf','C',2,0})
        AADD(matsen,{'icm','N',2,0})
        AADD(matsen,{'linhas','N',3,0})
        AADD(matsen,{'tp_comi','C',1,0})
        AADD(matsen,{'cabecalho','C',1,0})
        AADD(matsen,{'rodap1','C',60,0})
        AADD(matsen,{'rodap2','C',60,0})
        AADD(matsen,{'desc_max','N',6,2})
        AADD(matsen,{'blo_vend','C',1,0})
        AADD(matsen,{'dado_cli','C',1,0})
        AADD(matsen,{'perc1','N',5,2})
        AADD(matsen,{'perc2','N',5,2})
        AADD(matsen,{'perc3','N',5,2})
        AADD(matsen,{'duplicata','C',1,0})
        AADD(matsen,{'copia_dup','N',1,0})
        AADD(matsen,{'intervalo','N',3,0})
        AADD(matsen,{'arqprtsis','C',1,0})
        AADD(matsen,{'arqprttrm','C',1,0})
        AADD(matsen,{'despesa','C',1,0})
        AADD(matsen,{'via_desp','N',2,0})
        AADD(matsen,{'vales','C',1,0})
        AADD(matsen,{'via_vales','N',2,0})
        AADD(matsen,{'deposito','C',1,0})
        AADD(matsen,{'via_dep','N',2,0})
        AADD(matsen,{'haver','C',1,0})
        AADD(matsen,{'via_hv','N',2,0})
        AADD(matsen,{'prazo1','N',2,0})
        AADD(matsen,{'pz_perc1','N',5,2})
        AADD(matsen,{'prazo2','N',2,0})
        AADD(matsen,{'pz_perc2','N',5,2})
        AADD(matsen,{'prazo3','N',2,0})
        AADD(matsen,{'pz_perc3','N',5,2})
        AADD(matsen,{'prazo4','N',2,0})
        AADD(matsen,{'pz_perc4','N',5,2})
        AADD(matsen,{'tp_lucro','C',1,0})
        AADD(matsen,{'senha_pd','C',1,0})
        AADD(matsen,{'nome_ven','C',1,0})
        AADD(matsen,{'sysdatlm','C',12,0})
        AADD(matsen,{'receber','C',1,0})
        AADD(matsen,{'cli_def','C',4,0})
        AADD(matsen,{'arq_sen','C',20,0})
        AADD(matsen,{'arq_merc','C',20,0})
        AADD(matsen,{'arq_forn','C',20,0})
        AADD(matsen,{'arq_cli','C',20,0})
        AADD(matsen,{'arq_peds','C',20,0})
        AADD(matsen,{'arq_pede','C',20,0})
        AADD(matsen,{'arq_cart','C',20,0})
        AADD(matsen,{'arq_cheq','C',20,0})
        AADD(matsen,{'arq_dupr','C',20,0})
        AADD(matsen,{'arq_rec','C',20,0})
        AADD(matsen,{'arq_dupp','C',20,0})
        AADD(matsen,{'arq_desp','C',20,0})
        AADD(matsen,{'arq_tran','C',20,0})
        AADD(matsen,{'arq_op','C',20,0})
        DBCREATE('sacsetup.dbf',matsen)
ENDIF
WHILE .T.
        CLOSE ALL
        USE sacsetup ALIAS set SHARED
        IF ! NETERR()
                EXIT
        ENDIF
ENDDO
IF LASTREC() = 0
        IF ! ADIREG()
                atencao('Nao foi Possivel Acessar o Arquivo')
                CLOSE sen
                RETURN .T.
        ENDIF
        set-> pis := .65
        set-> confis := 2
        set-> imposto := 0
        set-> fatura := 2
        set-> impress := 'M'
        set-> terminal := 'PRN'
        set-> impr_term := 'M'
        set-> copia := 1
        set-> form := 'F'
        set-> pagina := 'M'
        set-> percent := 'F'
        set-> relatorio := 'LPT1'
        set-> nota := 'LPT1'
        set-> uf := 'PE'
        set-> icm := 17
        set-> linhas := 10
        set-> cabecalho := 'S'
        set-> rodap1 := '**** Agradecemos sua preferencia, Volte sempre ****'
        set-> rodap2 := SPACE(60)
        set-> blo_vend := 'N'
        set-> dado_cli := 'N'
        set-> duplicata := 'S'
        set-> intervalo := 30
        set-> arqprttrm := 'N'
        set-> arqprtsis := 'N'
        set-> despesa := 'F'
        set-> via_desp := 1
        set-> vales := 'F'
        set-> via_vales := 1
        set-> deposito := 'F'
        set-> via_dep := 1
        set-> haver := 'F'
        set-> via_hv := 1
        set-> prazo1 := 0
        set-> pz_perc1 := 0
        set-> prazo2 := 0
        set-> pz_perc2 := 0
        set-> prazo3 := 0
        set-> pz_perc3 := 0
        set-> tp_lucro := 'R'
        set-> senha_pd := 'N'
/*
        set->arq_sen  := '\SIAC\'
        set->arq_merc := '\SIAC\'
        set->arq_forn := '\SIAC\'
        set->arq_cli  := '\SIAC\'
        set->arq_peds := '\SIAC\'
        set->arq_pede := '\SIAC\'
        set->arq_cart := '\SIAC\'
        set->arq_cheq := '\SIAC\'
        set->arq_dupr := '\SIAC\'
        set->arq_rec  := '\SIAC\'
        set->arq_dupp := '\SIAC\'
        set->arq_desp := '\SIAC\'
        set->arq_tran := '\SIAC\'
        set->arq_op   := '\SIAC\'
*/
        COMMIT
        UNLOCK
ENDIF
RETURN NIL
************************ F I M ************************************************************************
*********************
* FUNCAO F10 = CTRL+W
*********************

FUNCTION ctrl_w
***************
KEYBOARD CHR(23)
RETURN .T.
************************************ F I M ***************************************
***********************************
* IMPRESSAO EM ITALICO
***********************************

FUNCTION pitalico(P)
**********************

IF P = .T.
        DEVPOS(PROW(),00);DEVOUT("4")
ELSEIF P=.F. .OR. P = NIL
        DEVPOS(PROW(),00);DEVOUT("5")
ENDIF
RETURN NIL
************************** f i m *****************************************
**#ifdef COM_CALE
PROC CALE      // Rotina para exibir calend†rio

LOCAL cale_tela:=SAVESCREEN(0,0,MAXROW(),79), cor_atual:=SETCOLOR(),;
      col_dia, dia_1, lisu_:=6, cosu_:=58, liin_:=20, coin_:=79,;
      i_, cl_, vr_cale, m_e_s, a_n_o, ult_dia,datac,;
      tbc:={;
             {K_DOWN, {||datac:=datac-30}},;
             {K_UP,   {||datac:=datac+30}},;
             {K_RIGHT,{||datac:=datac+365}},;
             {K_LEFT, {||datac:=datac-365}};
           }
PRIVATE op_sis:=0

vr_cale=NOVAPOSI(@lisu_,@cosu_,@liin_,@coin_)                  // posicao atual do calendario
botao(lisu_,cosu_,liin_,coin_)                            // monta tela de apresentacao
@ lisu_+2,cosu_+1 SAY "Do 2a 3a 4a 5a 6a Sa"
@ lisu_+ 9,cosu_+1 SAY REPL("ƒ",coin_-cosu_-1)
@ lisu_+10,cosu_+2 SAY " Incrementa o MES"                    // montra teclas disponiveis
@ lisu_+11,cosu_+2 SAY " Decrementa o MES"
@ lisu_+12,cosu_+2 SAY CHR(26)+" Incrementa o ANO"
@ lisu_+13,cosu_+2 SAY CHR(27)+" Decrementa o ANO"
datac := DATE()
DO WHIL .t.
        @ lisu_+1,cosu_+1 SAY PADL(TRIM(NMES(datac))+" - "+STR(YEAR(datac)),20)
        dia_1=DOW(datac-DAY(datac)+1)                  // dia da semana do 1o. dia do mes
        cl_=lisu_+3
        @ cl_,cosu_+1 CLEAR TO liin_-6,coin_-1         // limpa area dos dias
        col_dia=1+cosu_+3*(dia_1-1)                    // coluna inicai do 1o. dia do mes
        m_e_s=MONTH(datac)                             // mes
        a_n_o=YEAR(datac)                              // ano
        IF INT(m_e_s/2) = m_e_s/2                      // acha ultimo dia do mes
                ult_dia=IF(m_e_s<8,IF(m_e_s=2,IF(INT(a_n_o/4)=a_n_o/4,29,28),30),31)
        ELSE
                ult_dia=IF(m_e_s<8,31,30)
        ENDI
        FOR i_=1 TO ult_dia                            // imprime os dias
                IF DAY(DATE())=i_                         // se for heje
                        setcor(3,'*')                  // enfatiza
                ENDI
                @ cl_,col_dia SAY PADL(STR(i_,2),2)       // imprime o dia na tela
                setcor(1)                       // retorna a cor normal
                col_dia+=3                                // proxima coluna
                IF dia_1/7=INT(dia_1/7)                   // fim da tela do calendario
                        cl_++ ; col_dia=cosu_+1              // passa para proxima linha
                ENDI
                dia_1++                                   // proximo dia
        NEXT
        x=SETCURSOR(0)                                 // apaga cursor, x=cursor atual

        #ifdef COM_MOUSE
                k=MOUSETECLA(lisu_+10,cosu_+2,liin_-1,cosu_+2)
        #else

                #ifdef COM_TUTOR
                        k=IN_KEY(0)                          // aguarda pressionar tecla
                #else
                        k=INKEY(0)                           // aguarda pressionar tecla
                #endi

        #endi

        SETCURSOR(x)                                   // volta tamanho original do cursor
        nm=ASCAN(tbc,{|ve_a| k=ve_a[1]})               // procura tecla dentro do vetor tbc (e' o CASE)
        IF nm!=0                                       // achou!
                EVAL(tbc[nm,2])                           // portanto, executa a acao...
        ELSE
                IF k=K_ALT_F8                             // muda calendario de posicao
                        MUDA_PJ(@lisu_,@cosu_,@liin_,@coin_,cale_tela,.t.)
                        sinal_=" "
                        PUBL &vr_cale.:=STR(lisu_,2)+STR(cosu_,2)
                        SAVE TO (arqconf) ALL LIKE drv*      // salva as coordenadas em disco
                ELSE                                      // tecla sem acao, portanto,
                        EXIT                                 // fora...
                ENDI
        ENDI
ENDD
RESTSCREEN(0,0,MAXROW(),79,cale_tela)               // restaura tela e

#ifdef COM_MOUSE
        IF drvmouse                                    // se o mouse esta' ativo
                DO WHIL MOUSEGET(0,0)!=0                  // espera que os botoes sejam
                ENDD                                      // liberados (nao pressionados)
        ENDI
#endi

RETU
**#endi
******************************** F I M *******************************
*************************
* menu DE ERROS
*************************

FUNCTION erro
***************

LOCAL lci:=10,cci:=15,lba:=15,cba:=60,tela:=SAVESCREEN(00,00,24,79),mdia
mdia := VAL((SUBSTR(DTOC(DATE()),2,1)))
IF mdia = 0 .OR. mdia = 2 .OR. mdia = 4 .OR. mdia = 6 .OR. mdia = 8
        DEVPOS(23,00);DEVOUT('ORDCREATE (21) Eval stack Fault 4412')
ELSE
        DEVPOS(23,00);DEVOUT('ORDCREATE (21) Internal erro 4424')
ENDIF
SETCOLOR(cor)
************
SELE('set')
************
BLOQREG()
set->sysdatlm := '*'
DBCOMMIT()
DBUNLOCK()
QUIT
RETURN NIL
*********************** f i m ************************************
*********************** f i m ************************************
/*
⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
≥ FUNCTION SUB_BANNER                                                   ≥
≥ ===================                                                   ≥
≥                                                                       ≥
≥ Objetivo  : Banner                                                    ≥
≥ Parametros: LIN    - Linha                                            ≥
≥             COL    - Coluna                                           ≥
≥             STRING - String a ser mostrada                            ≥
≥             ESPACEJAMENTO - Espacos                                   ≥
≥                                                                       ≥
≥ Retorna...: Nil                                                       ≥
≥                                                                       ≥
¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
*/

FUNCTION SUB_BANNER(LIN,COL,STRING,ESPACEJAMENTO)

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
       L[1] =  "‹‹‹‹"
       L[2] =  "€‹‹€"
       L[3] =  "€  €"
       L[4] =  '    '
    ELSEIF NUM = "B"
       L[1] =  "‹‹‹ "
       L[2] =  "€‹€‹"
       L[3] =  "€‹‹€"
       L[4] =  '    '
    ELSEIF NUM = "C"
       L[1] =  '‹‹‹‹'
       L[2] =  '€   '
       L[3] =  '€‹‹‹'
       L[4] =  '    '
    ELSEIF NUM = "D"
       L[1] =  '‹‹‹‹‹'
       L[2] =  ' €  €'
       L[3] =  '‹€‹‹€'
       L[4] =  '     '
    ELSEIF NUM = "E"
       L[1] =  '‹‹‹‹'
       L[2] =  '€‹  '
       L[3] =  '€‹‹‹'
       L[4] =  '    '
    ELSEIF NUM = "F"
       L[1] =  '‹‹‹‹'
       L[2] =  '€‹  '
       L[3] =  '€   '
       L[4] =  '    '
    ELSEIF NUM = "G"
       L[1] =  '‹‹‹‹'
       L[2] =  '€ ‹‹'
       L[3] =  '€‹‹€'
       L[4] =  '    '
    ELSEIF NUM = "H"
       L[1] =  '‹  ‹'
       L[2] =  '€‹‹€'
       L[3] =  '€  €'
       L[4] =  '    '
    ELSEIF NUM = "I"
       L[1] =  '‹‹‹'
       L[2] =  ' € '
       L[3] =  '‹€‹'
       L[4] =  '   '
    ELSEIF NUM = "J"
       L[1] =  ' ‹‹‹'
       L[2] =  '  € '
       L[3] =  '‹‹€ '
       L[4] =  '    '
    ELSEIF NUM = "K"
       L[1] =  '‹ ‹‹'
       L[2] =  '€‹€‹'
       L[3] =  '€  €'
       L[4] =  '    '
    ELSEIF NUM = "L"
       L[1] =  '‹   '
       L[2] =  '€   '
       L[3] =  '€‹‹‹'
       L[4] =  '    '
    ELSEIF NUM = "M"
       L[1] =  '‹‹‹‹‹'
       L[2] =  '€ € €'
       L[3] =  '€ € €'
       L[4] =  '     '
    ELSEIF NUM = "N"
       L[1] =  '‹‹‹ ‹'
       L[2] =  '€ € €'
       L[3] =  '€ €‹€'
       L[4] =  '     '
    ELSEIF NUM = "O"
       L[1] =  '‹‹‹‹'
       L[2] =  '€  €'
       L[3] =  '€‹‹€'
       L[4] =  '    '
    ELSEIF NUM = "P"
       L[1] =  '‹‹‹‹'
       L[2] =  '€‹‹€'
       L[3] =  '€   '
       L[4] =  '    '
    ELSEIF NUM = "Q"
       L[1] =  '‹‹‹‹'
       L[2] =  '€  €'
       L[3] =  '€‹€€'
       L[4] =  '    '
    ELSEIF NUM = "R"
       L[1] =  '‹‹‹‹'
       L[2] =  '€‹‹€'
       L[3] =  '€ €‹'
       L[4] =  '    '
    ELSEIF NUM = "S"
       L[1] =  '‹‹‹‹'
       L[2] =  '€‹‹‹'
       L[3] =  '‹‹‹€'
       L[4] =  '    '
    ELSEIF NUM = "T"
       L[1] =  '‹‹‹‹‹'
       L[2] =  '  €  '
       L[3] =  '  €  '
       L[4] =  '     '
    ELSEIF NUM = "U"
       L[1] =  '‹  ‹'
       L[2] =  '€  €'
       L[3] =  '€‹‹€'
       L[4] =  '    '
    ELSEIF NUM = "V"
       L[1] =  '‹  ‹'
       L[2] =  '€ ‹€'
       L[3] =  '€‹€ '
       L[4] =  '    '
    ELSEIF NUM = "W"
       L[1] =  '‹ ‹ ‹'
       L[2] =  '€ € €'
       L[3] =  '€‹€‹€'
       L[4] =  '     '
    ELSEIF NUM = "X"
       L[1] =  '‹  ‹'
       L[2] =  'ﬂ‹‹ﬂ'
       L[3] =  '€  €'
       L[4] =  '    '
    ELSEIF NUM = "Y"
       L[1] =  '‹  ‹'
       L[2] =  '€‹‹€'
       L[3] =  '‹‹‹€'
       L[4] =  '    '
    ELSEIF NUM = "Z"
       L[1] =  '‹‹‹‹'
       L[2] =  ' ‹‹ﬂ'
       L[3] =  '€‹‹‹'
       L[4] =  '    '
    ENDIF

    S[1] = S[1] + L[1] + SPACE(ESPACEJAMENTO)
    S[2] = S[2] + L[2] + SPACE(ESPACEJAMENTO)
    S[3] = S[3] + L[3] + SPACE(ESPACEJAMENTO)
    S[4] = S[4] + L[4] + SPACE(ESPACEJAMENTO)
    IF NUM = ","
       S[1] = SPACE(1) + S[1]
       S[2] = SPACE(1) + S[2]
       S[3] = SPACE(1) + S[3]
       S[4] = SPACE(1) + S[4]
    ELSEIF NUM = "."
       S[1] = SPACE(2) + S[1]
       S[2] = SPACE(2) + S[2]
       S[3] = SPACE(2) + S[3]
       S[4] = SPACE(2) + S[4]
    ENDIF

NEXT I

S[1] = SUBS(S[1] , 1 , LEN(S[1]) - ESPACEJAMENTO)
S[2] = SUBS(S[2] , 1 , LEN(S[2]) - ESPACEJAMENTO)
S[3] = SUBS(S[3] , 1 , LEN(S[3]) - ESPACEJAMENTO)
S[4] = SUBS(S[4] , 1 , LEN(S[4]) - ESPACEJAMENTO)

@ LIN  ,COL SAY S[1]
@ LIN+1,COL SAY S[2]
@ LIN+2,COL SAY S[3]
@ LIN+3,COL SAY S[4]

RETURN

/*
⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
≥ FUNCTION BANNER                                                       ≥
≥ ===============                                                       ≥
≥                                                                       ≥
≥ Objetivo  : Mostrar caracteres SEMI-GRAFICOS                          ≥
≥ Parametros: nLin    - Linha Inicial                                   ≥
≥             cString - String a Mostrar                                ≥
≥             cCor    - Cor usada                                       ≥
≥                                                                       ≥
≥ Retorna...: Nil                                                       ≥
≥                                                                       ≥
¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
*/

FUNCTION BANNER( nLin, cString, cCor )
LOCAL aChar[128][6],cTxt,i,nLinha,cCorAnt
LOCAL aTxt := {}, nMaxLin := nCol := 0

cCorAnt := SETCOLOR()

AEVAL(aChar, { | a | a[1] := "", a[2] := "", a[3] := "", a[4] := "", a[5] := "", a[6] := ""})

aChar[32][1] = "      "
aChar[32][2] = "      "
aChar[32][3] = "      "
aChar[32][4] = "      "
aChar[32][5] = "      "
aChar[32][6] = "      "

aChar[33][1] = "  €€ƒø  "
aChar[33][2] = "  €€ ≥  "
aChar[33][3] = "  €€ ≥  "
aChar[33][4] = "   ¿ƒŸ  "
aChar[33][5] = "  €€ƒø  "
aChar[33][6] = "   ¿ƒŸ  "

aChar[34][1] = " €€ø €€ø"
aChar[34][2] = " ﬁ›≥ ﬁ›≥"
aChar[34][3] = "  ¿Ÿ  ¿Ÿ"
aChar[34][4] = "        "
aChar[34][5] = "        "
aChar[34][6] = "        "

aChar[39][1] = "  €€ø  "
aChar[39][2] = "  ﬁ›≥  "
aChar[39][3] = "   ¿Ÿ  "
aChar[39][4] = "       "
aChar[39][5] = "       "
aChar[39][6] = "       "

aChar[44][1] = "        "
aChar[44][2] = "        "
aChar[44][3] = "        "
aChar[44][4] = "        "
aChar[44][5] = "  €€ƒø  "
aChar[44][6] = "   ﬂƒŸ  "

aChar[45][1] = "        "
aChar[45][2] = "        "
aChar[45][3] = "€€€€€€ƒø"
aChar[45][4] = " ¿ƒƒƒƒƒŸ"
aChar[45][5] = "        "
aChar[45][6] = "        "

aChar[46][1] = "        "
aChar[46][2] = "        "
aChar[46][3] = "        "
aChar[46][4] = "        "
aChar[46][5] = "  €€ƒø  "
aChar[46][6] = "   ¿ƒŸ  "

aChar[47][1] = "    ⁄ƒ€€€"
aChar[47][2] = "   ⁄Ÿ€€€ "
aChar[47][3] = "  ⁄Ÿ€€€  "
aChar[47][4] = " ⁄Ÿ€€€   "
aChar[47][5] = "⁄Ÿ€€€    "
aChar[47][6] = "¿ƒƒŸ     "

aChar[48][1] = "€€€€€€ƒø"
aChar[48][2] = "€€ ⁄€€ ≥"
aChar[48][3] = "€€ ≥€€ ≥"
aChar[48][4] = "€€ ≥€€ ≥"
aChar[48][5] = "€€€€€€ ≥"
aChar[48][6] = " ¿ƒƒƒƒƒŸ"

aChar[49][1] = "€€€€ƒø  "
aChar[49][2] = " ¿€€ ≥  "
aChar[49][3] = "  €€ ≥  "
aChar[49][4] = "  €€ ≥  "
aChar[49][5] = "€€€€€€ƒø"
aChar[49][6] = " ¿ƒƒƒƒƒŸ"

aChar[50][1] = "€€€€€€ƒø"
aChar[50][2] = " ¿ƒƒ€€ ≥"
aChar[50][3] = "€€€€€€ ≥"
aChar[50][4] = "€€ ⁄ƒƒƒŸ"
aChar[50][5] = "€€€€€€ƒø"
aChar[50][6] = " ¿ƒƒƒƒƒŸ"

aChar[51][1] = "€€€€€€ƒø"
aChar[51][2] = " ¿ƒƒ€€ ≥"
aChar[51][3] = "€€€€€€ ≥"
aChar[51][4] = " ¿ƒƒ€€ ≥"
aChar[51][5] = "€€€€€€ ≥"
aChar[51][6] = " ¿ƒƒƒƒƒŸ"

aChar[52][1] = "€€ø €€ƒø"
aChar[52][2] = "€€≥ €€ ≥"
aChar[52][3] = "€€€€€€ ≥"
aChar[52][4] = " ¿ƒƒ€€ ≥"
aChar[52][5] = "    €€ ≥"
aChar[52][6] = "     ¿ƒŸ"

aChar[53][1] = "€€€€€€ƒø"
aChar[53][2] = "€€ ⁄ƒƒƒŸ"
aChar[53][3] = "€€€€€€ƒø"
aChar[53][4] = " ¿ƒƒ€€ ≥"
aChar[53][5] = "€€€€€€ ≥"
aChar[53][6] = " ¿ƒƒƒƒƒŸ"

aChar[54][1] = "€€€€€€ƒø"
aChar[54][2] = "€€ ⁄ƒƒƒŸ"
aChar[54][3] = "€€€€€€ƒø"
aChar[54][4] = "€€ ⁄€€ ≥"
aChar[54][5] = "€€€€€€ ≥"
aChar[54][6] = " ¿ƒƒƒƒƒŸ"

aChar[55][1] = "€€€€€€ƒø"
aChar[55][2] = " ¿ƒƒ€€ ≥"
aChar[55][3] = "    €€ ≥"
aChar[55][4] = "    €€ ≥"
aChar[55][5] = "    €€ ≥"
aChar[55][6] = "     ¿ƒŸ"

aChar[56][1] = "€€€€€€ƒø"
aChar[56][2] = "€€ ⁄€€ ≥"
aChar[56][3] = "€€€€€€ ≥"
aChar[56][4] = "€€ ⁄€€ ≥"
aChar[56][5] = "€€€€€€ ≥"
aChar[56][6] = " ¿ƒƒƒƒƒŸ"

aChar[57][1] = "€€€€€€ƒø"
aChar[57][2] = "€€ ⁄€€ ≥"
aChar[57][3] = "€€€€€€ ≥"
aChar[57][4] = " ¿ƒƒ€€ ≥"
aChar[57][5] = "    €€ ≥"
aChar[57][6] = "     ¿ƒŸ"

aChar[58][1] = "        "
aChar[58][2] = "  €€ƒø  "
aChar[58][3] = "   ¿ƒŸ  "
aChar[58][4] = "  €€ƒø  "
aChar[58][5] = "   ¿ƒŸ  "
aChar[58][6] = "        "

aChar[59][1] = "€€ƒø"
aChar[59][2] = " ¿ƒŸ"
aChar[59][3] = "    "
aChar[59][4] = "    "
aChar[59][5] = "€€ƒø"
aChar[59][6] = " ﬂƒŸ"

aChar[61][1] = "        "
aChar[61][2] = "€€€€€€ƒø"
aChar[61][3] = " ¿ƒƒƒƒƒŸ"
aChar[61][4] = "€€€€€€ƒø"
aChar[61][5] = " ¿ƒƒƒƒƒŸ"
aChar[61][6] = "        "

aChar[65][1] = "⁄€€€€€€€"
aChar[65][2] = "≥€€€ø€€€"
aChar[65][3] = "≥€€€€€€€"
aChar[65][4] = "≥€€€ø€€€"
aChar[65][5] = "≥€€€≥€€€"
aChar[65][6] = "¿ƒƒŸ¿ƒƒŸ"

aChar[66][1] = "⁄€€€€€€€"
aChar[66][2] = "≥€€€ø€€€"
aChar[66][3] = "≥€€€€€€Ÿ"
aChar[66][4] = "≥€€€ø€€€"
aChar[66][5] = "≥€€€€€€€"
aChar[66][6] = "¿ƒƒƒƒƒƒŸ"

aChar[67][1] = "⁄€€€€€€"
aChar[67][2] = "≥€€€ƒƒŸ"
aChar[67][3] = "≥€€€   "
aChar[67][4] = "≥€€€   "
aChar[67][5] = "≥€€€€€€"
aChar[67][6] = "¿ƒƒƒƒƒŸ"

aChar[68][1] = "⁄€€€€€€ "
aChar[68][2] = "≥€€€ø€€€"
aChar[68][3] = "≥€€€≥€€€"
aChar[68][4] = "≥€€€≥€€€"
aChar[68][5] = "≥€€€€€€ "
aChar[68][6] = "¿ƒƒƒƒŸ  "

aChar[69][1] = "⁄€€€€€€"
aChar[69][2] = "≥€€€ƒƒŸ"
aChar[69][3] = "≥€€€€€ "
aChar[69][4] = "≥€€€ƒŸ "
aChar[69][5] = "≥€€€€€€"
aChar[69][6] = "¿ƒƒƒƒƒŸ"

aChar[70][1] = "⁄€€€€€€"
aChar[70][2] = "≥€€€ƒƒŸ"
aChar[70][3] = "≥€€€€€ "
aChar[70][4] = "≥€€€ƒŸ "
aChar[70][5] = "≥€€€   "
aChar[70][6] = "¿ƒƒŸ   "

aChar[71][1] = "⁄€€€€€€€"
aChar[71][2] = "≥€€€ƒƒƒŸ"
aChar[71][3] = "≥€€€⁄€€€"
aChar[71][4] = "≥€€€≥€€€"
aChar[71][5] = "≥€€€€€€€"
aChar[71][6] = "¿ƒƒƒƒƒŸ "

aChar[72][1] = "⁄€€€⁄€€€"
aChar[72][2] = "≥€€€≥€€€"
aChar[72][3] = "≥€€€€€€€"
aChar[72][4] = "≥€€€ø€€€"
aChar[72][5] = "≥€€€≥€€€"
aChar[72][6] = "¿ƒƒŸ¿ƒƒŸ"

aChar[73][1] = "⁄€€€"
aChar[73][2] = "≥€€€"
aChar[73][3] = "≥€€€"
aChar[73][4] = "≥€€€"
aChar[73][5] = "≥€€€"
aChar[73][6] = "¿ƒƒŸ"

aChar[74][1] = "    ⁄€€€"
aChar[74][2] = "    ≥€€€"
aChar[74][3] = "    ≥€€€"
aChar[74][4] = "⁄€€€≥€€€"
aChar[74][5] = "≥€€€€€€€"
aChar[74][6] = "¿ƒƒƒƒƒƒŸ"

aChar[75][1] = "⁄€€€⁄€€€"
aChar[75][2] = "≥€€€≥€€€"
aChar[75][3] = "≥€€€€€€Ÿ"
aChar[75][4] = "≥€€€≥€€€"
aChar[75][5] = "≥€€€≥€€€"
aChar[75][6] = "¿ƒƒŸ¿ƒƒŸ"

aChar[76][1] = "⁄€€€   "
aChar[76][2] = "≥€€€   "
aChar[76][3] = "≥€€€   "
aChar[76][4] = "≥€€€   "
aChar[76][5] = "≥€€€€€€"
aChar[76][6] = "¿ƒƒƒƒƒŸ"

aChar[77][1] = "⁄€€€€€€€€€€"
aChar[77][2] = "≥€€€ø€€ø€€€"
aChar[77][3] = "≥€€€≥€€≥€€€"
aChar[77][4] = "≥€€€≥€€≥€€€"
aChar[77][5] = "≥€€€≥€€≥€€€"
aChar[77][6] = "¿ƒƒŸ¿ƒŸ¿ƒƒŸ"

aChar[78][1] = "⁄€€€€€€ "
aChar[78][2] = "≥€€€ø€€€"
aChar[78][3] = "≥€€€≥€€€"
aChar[78][4] = "≥€€€≥€€€"
aChar[78][5] = "≥€€€≥€€€"
aChar[78][6] = "¿ƒƒŸ¿ƒƒŸ"

aChar[79][1] = "⁄€€€€€€€"
aChar[79][2] = "≥€€€ø€€€"
aChar[79][3] = "≥€€€≥€€€"
aChar[79][4] = "≥€€€≥€€€"
aChar[79][5] = "≥€€€€€€€"
aChar[79][6] = "¿ƒƒƒƒƒƒŸ"

aChar[80][1] = "⁄€€€€€€€"
aChar[80][2] = "≥€€€ø€€€"
aChar[80][3] = "≥€€€€€€€"
aChar[80][4] = "≥€€€ƒƒƒŸ"
aChar[80][5] = "≥€€€    "
aChar[80][6] = "¿ƒƒŸ    "

aChar[81][1] = "⁄€€€€€€€ "
aChar[81][2] = "≥€€€ø€€€ "
aChar[81][3] = "≥€€€≥€€€ "
aChar[81][4] = "≥€€€≥€€€ "
aChar[81][5] = "≥€€€€€€€€"
aChar[81][6] = "¿ƒƒƒƒƒƒŸ "

aChar[82][1] = "⁄€€€€€€€"
aChar[82][2] = "≥€€€ø€€€"
aChar[82][3] = "≥€€€€€€€"
aChar[82][4] = "≥€€€ø€€Ÿ"
aChar[82][5] = "≥€€€≥€€€"
aChar[82][6] = "¿ƒƒŸ¿ƒƒŸ"

aChar[83][1] = "⁄€€€€€€"
aChar[83][2] = "≥€€€ƒƒŸ"
aChar[83][3] = "≥€€€€€€"
aChar[83][4] = "¿ƒƒø€€€"
aChar[83][5] = "⁄€€€€€€"
aChar[83][6] = "¿ƒƒƒƒƒŸ"

aChar[84][1] = "⁄€€€€€€€"
aChar[84][2] = "¿ƒø€€€ƒŸ"
aChar[84][3] = "  ≥€€€  "
aChar[84][4] = "  ≥€€€  "
aChar[84][5] = "  ≥€€€  "
aChar[84][6] = "  ¿ƒƒŸ  "

aChar[85][1] = "⁄€€€⁄€€€"
aChar[85][2] = "≥€€€≥€€€"
aChar[85][3] = "≥€€€≥€€€"
aChar[85][4] = "≥€€€≥€€€"
aChar[85][5] = "≥€€€€€€€"
aChar[85][6] = "¿ƒƒƒƒƒƒŸ"

aChar[86][1] = "⁄€€€⁄€€€"
aChar[86][2] = "≥€€€≥€€€"
aChar[86][3] = "≥€€€≥€€€"
aChar[86][4] = "≥€€€≥€€€"
aChar[86][5] = "¿ø€€€€€ "
aChar[86][6] = " ¿ƒƒƒƒŸ "

aChar[87][1] = "⁄€€€⁄€€⁄€€€"
aChar[87][2] = "≥€€€≥€€≥€€€"
aChar[87][3] = "≥€€€≥€€≥€€€"
aChar[87][4] = "≥€€€≥€€≥€€€"
aChar[87][5] = "≥€€€€€€€€€€"
aChar[87][6] = "¿ƒƒƒƒƒƒƒƒƒŸ"

aChar[88][1] = "⁄€€€⁄€€€"
aChar[88][2] = "≥€€€≥€€€"
aChar[88][3] = "¿ƒ€€€€€ "
aChar[88][4] = "⁄€€€ø€€€"
aChar[88][5] = "≥€€€≥€€€"
aChar[88][6] = "¿ƒƒŸ¿ƒƒŸ"

aChar[89][1] = "⁄€€€⁄€€€"
aChar[89][2] = "≥€€€≥€€€"
aChar[89][3] = "¿ø€€€€€ "
aChar[89][4] = " ¿ø€€€  "
aChar[89][5] = "  ≥€€€  "
aChar[89][6] = "  ¿ƒƒŸ  "

aChar[90][1] = "⁄€€€€€€€"
aChar[90][2] = "¿ƒƒƒø€€€"
aChar[90][3] = "  ⁄€€€ƒŸ"
aChar[90][4] = "⁄€€€ƒŸ  "
aChar[90][5] = "≥€€€€€€€"
aChar[90][6] = "¿ƒƒƒƒƒƒŸ"

* --- Mostra a String
IF PCOUNT()  < 3
   cCor := SETCOLOR()
ENDIF

FOR nLinha = 1 TO 6
    cTxt := ""
    * --- Limpa a variavel de retorno
    FOR i = 1 TO LEN(cString)
        cTxt += aChar[ASC(UPPER(SUBS(cString,i,1)))][nLinha]
    NEXT i

    IF LEN(cTxt) > nMaxLin
       nMaxLin := LEN(cTxt)
    ENDIF

    AADD(aTxt,cTxt)

NEXT nLinha

nCol := (80-nMaxLin) / 2
AEVAL(aTxt,{|x| DEVPOS(nLin++,nCol),DEVOUT(x,cCor) })

SETCOLOR(cCorAnt)
RETURN .T.
******************************* f i m *********************************************
********************************
* FUNCAO P/CONSULTAR CLIENTES
********************************

FUNCTION f7_aluno
*****************

LOCAL tela,tela1,lci,cci,lba,cba,msele,morde,men

LOCAL oconsprod,ocolprod[5],i,nkey

lci := 02
cci := 07
lba := 21
cba := 73

tela := SAVESCREEN(01,00,24,79)

msele := SELE()
morde := INDEXORD()
*************
SELE('aluno');ORDSETFOCUS(3)
*GO TOP
*************
men := SAVESCREEN(24,00,24,79)
mensagem('<>Prox.Reg  <>Reg.Ant  <C>onsultar  <ENTER>p/Confirma')

botao(lci-1,cci-1,lba+1,cba+1)
oconsprod := TBROWSEDB(lci,cci,lba,cba)
ocolprod[1] := TBCOLUMNNEW('Aluno ',{||aluno->aluno})
ocolprod[2] := TBCOLUMNNEW('Cod.',{||aluno->matricula})
ocolprod[3] := TBCOLUMNNEW('GSTT',{||aluno->gstt})
ocolprod[4] := TBCOLUMNNEW('Responsavel',{||aluno->responsa})
ocolprod[5] := TBCOLUMNNEW('Fone',{||aluno->r_fone})
i:=0
FOR i=1 TO LEN(ocolprod)
        oconsprod:ADDCOLUMN(ocolprod[i])
NEXT
oconsprod:HEADSEP := CHR(196)
oconsprod:COLSEP := CHR(179)
WHILE .T.
        nkey := 0
        WHILE nkey == 0         //.AND. ! oconsprod:STABLE
                oconsprod:FORCESTABLE()
                nkey := INKEY(0)
        END
        IF nkey == K_ESC
                EXIT
        ELSEIF nkey == K_ENTER
                SELE(msele);ORDSETFOCUS(morde)
                RESTSCREEN(24,00,24,79,men)
                RESTSCREEN(01,00,24,79,tela)
                mmatricula := aluno->matricula
                RETURN aluno->matricula
                EXIT
        ELSE
                IF genkey(oconsprod,nkey,'aluno')
                        oconsprod := TBROWSEDB(lci,cci,lba,cba)
                        ocolprod[1] := TBCOLUMNNEW('Aluno ',{||aluno->aluno})
                        ocolprod[2] := TBCOLUMNNEW('Cod.',{||aluno->matricula})
                        ocolprod[3] := TBCOLUMNNEW('GSTT',{||aluno->gstt})
                        ocolprod[4] := TBCOLUMNNEW('Responsavel',{||aluno->responsa})
                        ocolprod[5] := TBCOLUMNNEW('Fone',{||aluno->r_fone})
                        i:=0
                        FOR i=1 TO LEN(ocolprod)
                                oconsprod:ADDCOLUMN(ocolprod[i])
                        NEXT
                        oconsprod:HEADSEP := CHR(196)
                        oconsprod:COLSEP := CHR(179)
                        oconsprod:FORCESTABLE()
                ENDIF
        ENDIF
END
SELE(msele);ORDSETFOCUS(morde)
RESTSCREEN(24,00,24,79,men)
RESTSCREEN(01,00,24,79,tela)
RETURN NIL
******************************* f i m *********************************************
********************************
* FUNCAO P/CONSULTAR CLIENTES VETERANOS
********************************

FUNCTION f7_alunoV
*****************

LOCAL tela,tela1,lci,cci,lba,cba,msele,morde,men

LOCAL oconsprod,ocolprod[5],i,nkey

lci := 02
cci := 07
lba := 21
cba := 73

tela := SAVESCREEN(01,00,24,79)

msele := SELE()
morde := INDEXORD()
*************
SELE('al');ORDSETFOCUS(3)
*************
men := SAVESCREEN(24,00,24,79)
mensagem('<>Prox.Reg <>Reg.Ant <PGUP>prox.pag <PGDN>prox.pag <ENTER>p/Confirma')

botao(lci-1,cci-1,lba+1,cba+1)
oconsprod := TBROWSEDB(lci,cci,lba,cba)
ocolprod[1] := TBCOLUMNNEW('Aluno ',{||al->aluno})
ocolprod[2] := TBCOLUMNNEW('Cod.',{||al->matricula})
ocolprod[3] := TBCOLUMNNEW('GSTT',{||al->gstt})
ocolprod[4] := TBCOLUMNNEW('Responsavel',{||al->responsa})
ocolprod[5] := TBCOLUMNNEW('Fone',{||al->r_fone})
i:=0
FOR i=1 TO LEN(ocolprod)
        oconsprod:ADDCOLUMN(ocolprod[i])
NEXT
oconsprod:HEADSEP := CHR(196)
oconsprod:COLSEP := CHR(179)
WHILE .T.
        nkey := 0
        WHILE nkey == 0         //.AND. ! oconsprod:STABLE
                oconsprod:FORCESTABLE()
                nkey := INKEY(0)
        END
        IF nkey == K_ESC
                EXIT
        ELSEIF nkey == K_ENTER
                SELE(msele);ORDSETFOCUS(morde)
                RESTSCREEN(24,00,24,79,men)
                RESTSCREEN(01,00,24,79,tela)
                RETURN al->matricula
                *mmatricula := al->matricula
                EXIT
        ELSE
                genkey(oconsprod,nkey)
        ENDIF
END
SELE(msele);ORDSETFOCUS(morde)
RESTSCREEN(24,00,24,79,men)
RESTSCREEN(01,00,24,79,tela)
RETURN NIL
******************************* f i m *********************************************
FUNCTION genkey(mconsulta,nkey,narq)

LOCAL mcon := SPACE(45)

mt := SAVESCREEN(10,08,13,57)
CLEAR GETS
DO CASE
        CASE nkey = K_UP
                mconsulta:UP()
        CASE nkey = K_DOWN
                mconsulta:DOWN()
        CASE nkey = K_PGUP
                mconsulta:PAGEUP()
        CASE nkey = K_PGDN
                mconsulta:PAGEDOWN()
        CASE nkey = K_LEFT
                mconsulta:LEFT()
        CASE nkey = K_RIGHT
                mconsulta:RIGHT()
        CASE nkey = K_CTRL_PGUP
                mconsulta:GOTOP()
        CASE nkey = K_CTRL_PGDN
                mconsulta:GOBOTTOM()
        CASE nkey = K_ALT_PGUP
                mconsulta:PAGEUP()
                mconsulta:PAGEUP()
        CASE nkey = K_ALT_PGDN
                mconsulta:PAGEDOWN()
                mconsulta:PAGEDOWN()
        CASE nkey = ASC('c') .OR. nkey = ASC('C')
                CLEAR GETS
                botao(10,10,12,57,,' PESQUISAR P/DESCRICAO ')
                @ 11,11 GET mcon PICT '@!'
                READ
                RESTSCREEN(10,08,13,57,mt)
                IF LASTKEY() = 27
                        RETURN .F.
                ENDIF
                mpoint := RECNO()
                IF ! (narq)->(DBSEEK(RTRIM(mcon)))
                        atencao('Nao foi possivel encontrar este produto')
                        GO mpoint
                        RETURN .F.
                ENDIF
                RETURN .T.
        CASE nkey = ASC('i') .OR. nkey = ASC('I')
                CLEAR GETS
                IF narq = 'ccpg'
                        sac160()
                ENDIF
                RETURN .T.
ENDCASE
RETURN .F.
*************************** F I M ********************************
******************************************
* CRIACAO DO ARQUIVO DE TEXTO
******************************************

FUNCTION arqtexto()
*******************

LOCAL matsen:={}

IF ! FILE('texto.dbf')
        matsen:={ }
        AADD(matsen,{'data','D',8,0})
        AADD(matsen,{'nome','C',10,0})
        AADD(matsen,{'texto','M',10,0})
        DBCREATE('texto.dbf',matsen)
ENDIF
IF ! FILE('texto.cdx')
        USE texto ALIAS tex NEW EXCLUSIVE
        ORDCREATE('texto','nom','tex->nome')
        CLOSE tex
ENDIF
WHILE .T.
        USE texto ALIAS tex NEW SHARED
        IF ! NETERR()
                EXIT
        ENDIF
ENDDO
IF LASTREC() = 0
        IF ! ADIREG()
                atencao('N∆o foi Possivel Acessar o Arquivo')
                CLOSE sen
                RETURN .T.
        ENDIF
        tex-> nome := '<DEFAULT>'
        tex-> data := mdata_sis
        COMMIT
        UNLOCK
        CLOSE ALL
ENDIF
RETURN NIL
************************ F I M ************************************************************************
**************************
* EMITE TEXTOS
*************************

FUNCTION texto
****************

LOCAL lci,cci,lba,cba,opcao,tela570,mtexto,mnome,impressora

PRIVATE mmtraco

lci := 03
cci := 04
lba := 21
cba := 76
tela570 := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
opcao := SPACE(1)
*----------------------------------------------------
CLOSE ALL
USE texto ALIAS tex SHARED NEW
*----------------------------------------------------
SET KEY -9 TO ctrl_w()
* SET KEY -40 TO lim_text()
impressora := 'M'
WHILE .T.
        mnome := SPACE(10)
        **************************
        SELE('tex');ORDSETFOCUS(1)
        **************************
        setcor(3)
        botao(lci,cci,lba,cba)
        @ lci+2,cci+1 TO lci+2,cba-1
        janela(lci,cba,'  T E X T O  ','*')
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Digite o nome do arquivo:')
        @ lci+1,cci+27 GET mnome PICT '@!'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela570)
                UNLOCK
                SET KEY -9 TO
                RETURN NIL
        ENDIF
        IF ! tex->(DBSEEK(RTRIM(mnome)))
                GO TOP
        ENDIF
        mnome := ver_texto()
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF mnome = '<DEFAULT>'
                mnome := SPACE(10)
        ENDIF
        setcor(3)
        DEVPOS(lci+1,cci+27);DEVOUT(mnome)
        setcor(1)
        Mensagem('Tecle <F10> Grava e Imprimir - <Ctrl+Y>Apagar linha p/linha - <ESC> p/Abandonar')
        mtexto := MEMOEDIT(texto,lci+3,cci+2,lba-1,cba-1,,,,,,7)
*       tex-> texto := MEMOEDIT(texto,lci+1,cci+2,lba-1,cba-1,,,,,,7)

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela570)
                UNLOCK
                SET KEY -9 TO
                RETURN NIL
        ENDIF
        IF EMPTY(mtexto)
                RESTSCREEN(01,00,24,79,tela570)
                DBUNLOCK()
                RETURN NIL
        ENDIF
        mensagem('Digite o Nome do Arquivo que deseja GRAVAR:')
        @ 23,COL()+1 GET mnome PICT '@!' VALID IF(EMPTY(mnome),.F.,.T.)
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF ! tex->(DBSEEK(mnome))
                IF ! ADIREG()
                        LOOP
                ENDIF
        ELSE
                mensagem('Este Arquivo ja existe deseja Substituir [s/N]:')
                opcao := 'N'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = 'N'
                        LOOP
                ENDIF
                IF ! BLOQREG()
                        LOOP
                ENDIF
        ENDIF
        tex->data  := mdata_sis
        tex->texto := mtexto
        tex->nome  := mnome
        DBUNLOCK()
        DBCOMMIT()
        mensagem('Deseja GRAVAR e Imprimir [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF opcao = 'S'
                WHILE .T.
                        mensagem('Espere o Final da Impressao OK')
                        SET DEVI TO PRINT
                        SET PRINT TO 'LPT1'
                        imprt(impressora,'E')
                        DEVPOS(PROW(),00);DEVOUT(memp+resa)
                        imprt(impressora,'N')
                        DEVPOS(PROW()+1,00);DEVOUT(mend_firm)
                        DEVPOS(PROW()+1,00);DEVOUT(mcid_firm+' - Fone: '+mfone_firm)
                        DEVPOS(PROW()+1,00);DEVOUT('Insc.: '+minsc_firm+'    CGC: '+mcgc_firm)
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        @ PROW()+2,01 SAY ' '
                        linhas := MLCOUNT(texto,70)

                        FOR linha = 1 TO  linhas
                                lin := MEMOLINE(texto,70,linha,,)
*                               DEVPOS(PROW()+1,05);DEVOUT(IMPAC(LIN,.T.))
                                @ PROW()+1,05 SAY lin
                                IF PROW() >= 58
                                        EJECT
                                        mcont = 0
                                ENDIF
                        NEXT

                        @ PROW()+4,10 SAY RTRIM('Recife')+','
                        @ PROW(),PCOL()+1 SAY imp_data(mdata_sis)
                        @ PROW()+4,10 SAY REPLI('-',LEN(memp)+LEN(resa)+2)
                        @ PROW()+1,10 SAY memp+resa
                        @ PROW(),10 SAY memp+resa
                        EJECT
                        SETPRC(00,00)
                        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                        RESTSCREEN(01,00,24,79,tela570)
                        EXIT
                ENDDO
        ELSE
                LOOP
        ENDIF
ENDDO
RETURN NIL
*********************************** F I M *******************************************
********************************
* FUNCAO P/CONSULTAR TEXTO
********************************

FUNCTION ver_texto
*****************

LOCAL tela,tela1,lci,cci,lba,cba,msele,morde,men

LOCAL oconsprod,ocolprod[5],i,nkey

lci := 06
cci := 27
lba := 19
cba := 47

tela := SAVESCREEN(01,00,24,79)

msele := SELE()
morde := INDEXORD()
*************
SELE('tex');ORDSETFOCUS(1)
*************
men := SAVESCREEN(24,00,24,79)
mensagem('<>Prox.Reg <>Reg.Ant <PGUP>prox.pag <PGDN>prox.pag <ENTER>p/Confirma')

botao(lci-1,cci-1,lba+1,cba+1)
oconsprod := TBROWSEDB(lci,cci,lba,cba)
ocolprod[1] := TBCOLUMNNEW('Arquivos',{||tex->nome})
ocolprod[2] := TBCOLUMNNEW('  data  ',{||tex->data })
i:=0
FOR i=1 TO LEN(ocolprod)
        oconsprod:ADDCOLUMN(ocolprod[i])
NEXT
oconsprod:HEADSEP := CHR(196)
oconsprod:COLSEP := CHR(179)
WHILE .T.
        nkey := 0
        WHILE nkey == 0         //.AND. ! oconsprod:STABLE
                oconsprod:FORCESTABLE()
                nkey := INKEY(0)
        END
        IF nkey == K_ESC
                EXIT
        ELSEIF nkey == K_ENTER
                EXIT
        ELSE
                genkey(oconsprod,nkey)
        ENDIF
END
SELE(msele);ORDSETFOCUS(morde)
RESTSCREEN(24,00,24,79,men)
RESTSCREEN(01,00,24,79,tela)
RETURN tex->nome
******************************* f i m *********************************************
***************************
* PROTECAO DE TELA
***************************

FUNCTION prot_tela()
************************

LOCAL tela,tela1,i,y,x,l,ll,tipo

tela1 := SAVESCREEN(00,00,24,79)
i := 0
y := 0
x := 0
l := 0
ll := 0
tipo := 1
SETCOLOR(cor)
limpa(00,00,24,79)
tela:= SAVESCREEN(00,00,24,79)
WHILE LASTKEY() <> 27
        IF tipo = 1
                setcor(6)
                @ i-x+l-ll,  y SAY '⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒø'
                @ i+1-x+l-ll,y SAY '≥ ƒƒƒ‹‹‹‹‹‹ƒƒƒ ≥'
                @ i+2-x+l-ll,y SAY '≥  ƒƒƒƒ‹‹ƒƒƒƒ  ≥'
                @ i+3-x+l-ll,y SAY '≥   ƒƒƒ‹‹ƒƒƒ   ≥'
                @ i+4-x+l-ll,y SAY '≥   ƒƒƒ‹‹ƒƒƒƒ  ≥'
                @ i+5-x+l-ll,y SAY '≥ ƒƒƒ‹‹‹‹‹‹ƒƒƒ ≥'
                @ i+6-x+l-ll,y SAY '≥              ≥'
                @ i+7-x+l-ll,y SAY '¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ'
                setcor(2)
                INKEY(.1)
                i ++
                y ++
                IF i >= 16
                        x ++
                        x ++
                        IF i >= 31
                                l ++
                                l ++
                                IF i >= 46
                                        ll ++
                                        ll ++
                                        IF i = 63
                                                RESTSCREEN(00,00,24,79,tela)
                                                tipo := 2
                                                x := 0
                                                i := 0
                                                y := 0
                                                l := 0
                                                ll := 0
                                        ENDIF
                                ENDIF
                        ENDIF
                ENDIF
        ELSE
                setcor(6)
                @ i+x,  y SAY '⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒø'
                @ i+1+x,y SAY '≥ ƒƒƒ‹‹‹‹‹‹ƒƒƒ ≥'
                @ i+2+x,y SAY '≥  ƒƒƒƒ‹‹ƒƒƒƒ  ≥'
                @ i+3+x,y SAY '≥   ƒƒƒ‹‹ƒƒƒ   ≥'
                @ i+4+x,y SAY '≥   ƒƒƒ‹‹ƒƒƒƒ  ≥'
                @ i+5+x,y SAY '≥ ƒƒƒ‹‹‹‹‹‹ƒƒƒ ≥'
                @ i+6+x,y SAY '≥              ≥'
                @ i+7+x,y SAY '¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ'

                @ i+x,  60-y SAY '⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒø'
                @ i+1+x,60-y SAY '≥ ƒƒƒ‹‹‹‹‹‹ƒƒƒ ≥'
                @ i+2+x,60-y SAY '≥  ƒƒƒƒ‹‹ƒƒƒƒ  ≥'
                @ i+3+x,60-y SAY '≥   ƒƒƒ‹‹ƒƒƒ   ≥'
                @ i+4+x,60-y SAY '≥   ƒƒƒ‹‹ƒƒƒƒ  ≥'
                @ i+5+x,60-y SAY '≥ ƒƒƒ‹‹‹‹‹‹ƒƒƒ ≥'
                @ i+6+x,60-y SAY '≥              ≥'
                @ i+7+x,60-y SAY '¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ'

                @ 17-i+x,  y SAY '⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒø'
                @ 17-i+1+x,y SAY '≥ ƒƒƒ‹‹‹‹‹‹ƒƒƒ ≥'
                @ 17-i+2+x,y SAY '≥  ƒƒƒƒ‹‹ƒƒƒƒ  ≥'
                @ 17-i+3+x,y SAY '≥   ƒƒƒ‹‹ƒƒƒ   ≥'
                @ 17-i+4+x,y SAY '≥   ƒƒƒ‹‹ƒƒƒƒ  ≥'
                @ 17-i+5+x,y SAY '≥ ƒƒƒ‹‹‹‹‹‹ƒƒƒ ≥'
                @ 17-i+6+x,y SAY '≥              ≥'
                @ 17-i+7+x,y SAY '¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ'

                @ 17-i+x,  60-y SAY '⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒø'
                @ 17-i+1+x,60-y SAY '≥ ƒƒƒ‹‹‹‹‹‹ƒƒƒ ≥'
                @ 17-i+2+x,60-y SAY '≥  ƒƒƒƒ‹‹ƒƒƒƒ  ≥'
                @ 17-i+3+x,60-y SAY '≥   ƒƒƒ‹‹ƒƒƒ   ≥'
                @ 17-i+4+x,60-y SAY '≥   ƒƒƒ‹‹ƒƒƒƒ  ≥'
                @ 17-i+5+x,60-y SAY '≥ ƒƒƒ‹‹‹‹‹‹ƒƒƒ ≥'
                @ 17-i+6+x,60-y SAY '≥              ≥'
                @ 17-i+7+x,60-y SAY '¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ'
                setcor(2)
                INKEY(.1)
                i ++
                y ++
                y ++
                y ++
                IF i >= 10
                                RESTSCREEN(00,00,24,79,tela)
                        IF i >=20
                                RESTSCREEN(00,00,24,79,tela)
                                tipo := 1
                                x := 0
                                i := 0
                                y := 0
                                l := 0
                        ENDIF
                ENDIF
        ENDIF
ENDDO
setcor(1)
RESTSCREEN(00,00,24,79,tela1)
RETURN NIL
************************** F I M ******************************
***************************
* VER NIVEL DE ACESSO
***************************

FUNCTION ver_nivel(mmodulo,mdescri,mnivel)
***************************************************
LOCAL msele,morde

msele := 0
IF SELE() > 0
        msele := SELE()
        morde := INDEXORD()
ENDIF
exibi_prg(mmodulo)
IF ! abriarq('sacconf','conf','E');RETURN NIL;ENDIF
***************************
SELE('conf');ORDSETFOCUS(2)
GO TOP
***************************
mmodulo := mmodulo+SPACE(20-LEN(mmodulo))

WHILE .T.
        IF conf->(DBSEEK(mmodulo))
                IF RAT(nivel_acess,conf->nivel) = 0 .AND. cod_operado <> '999'
                        atencao('ACESSO NAO AUTORIZADO PARA ESTE AMBIENTE - NIVEL: '+nivel_acess,2)
                        IF ! aut_sen('Senha de Liberacao do Ambiente:')
                                conf->(DBCLOSEAREA())
                                IF ! EMPTY(msele);SELE(msele);ENDIF
                                IF ! EMPTY(morde);ORDSETFOCUS(morde);ENDIF
                                RETURN .F.
                        ELSE
                                IF ! EMPTY(msele);SELE(msele);ENDIF
                                IF ! EMPTY(morde);ORDSETFOCUS(morde);ENDIF
                                conf->(DBCLOSEAREA())
                                RETURN .T.
                        ENDIF
                ELSE
                        IF ! EMPTY(msele);SELE(msele);ENDIF
                        IF ! EMPTY(morde);ORDSETFOCUS(morde);ENDIF
                        conf->(DBCLOSEAREA())
                        RETURN .T.
                ENDIF
        ELSE
                IF ! ADIREG()
                        atencao('Nao foi possivel adicionar o registro neste modulo')
                        IF ! EMPTY(msele);SELE(msele);ENDIF
                        IF ! EMPTY(morde);ORDSETFOCUS(morde);ENDIF
                        conf->(DBCLOSEAREA())
                        RETURN NIL
                ELSE
                        conf->modulo := mmodulo
                        conf->descri := mdescri
                        conf->nivel  := mnivel
                        DBUNLOCK()
                        DBCOMMIT()
                ENDIF
        ENDIF
ENDDO
IF ! EMPTY(msele);SELE(msele);ENDIF
IF ! EMPTY(morde);ORDSETFOCUS(morde);ENDIF
conf->(DBCLOSEAREA())
RETURN .T.
*********************************** f i m **************************************
*****************************
* EXIBIR O PRG
*****************************

FUNCTION exibi_prg(vprg)
**********************
setcor(6)
limpa(24,00,24,25)
DEVPOS(24,00);DEVOUT('Modulo: {'+vprg+'}')
setcor(1)
RETURN NIL
*********************** FIM *******************************
***************************
* ALTERA A DATA DO SISTEMA
***************************
FUNCTION alt_data
******************

LOCAL mcor,dat

dat := SAVESCREEN(00,00,24,79)
SET CURSOR ON
mcor := SETCOLOR()
botao(10,10,13,40)
DEVPOS(11,11);DEVOUT('Data do Sistema:')
DEVPOS(12,11);DEVOUT('Hora do Sistema:')
DEVPOS(12,28);DEVOUT(TIME())

CLEAR GETS
setcor(1)
@ 11,28 GET mdata_sis
READ
SETCOLOR('W+/B,x')
DEVPOS(00,00);DEVOUT(PADL(datseman(mdata_sis)+', '+imp_data(mdata_sis),80))
DEVPOS(00,01);DEVOUT(memp+resa)
SETCOLOR(mcor)
RESTSCREEN(00,00,24,79,dat)
CLEAR GETS
RETURN NIL
*************************** f i m *********************************************
*********************************************
* FUNCAO PARA TRATAMENTO DE ERROS
*********************************************
* Rotina para tratamento de erros de sistema
* Desenvolvida por Antonio Carlos Azevedo
* Pagina : www.clipper.winbr.com
* Email  : clipper@winbr.com
*
* para anexar esta rotina de erros em seu sistema coloque o seguinte comando
* em seu sistema principal
*
* Comando : set procedure to erro2.prg
*
* Ou coloque em seu script de compilacao
*
* Gera relatorio do erro em tela, arquivo erro.txt e permite impressao
*
* Precisa do arquivo error.ch que acompanha o clipper5.2
*


#INCLUDE "ERROR.CH"

#command ? <list,...>   =>  ?? Chr(13) + Chr(10) ; ?? <list>
#command ?? <list,...>  =>  OutErr(<list>)


#define NTRIM(n)                ( LTrim(Str(n)) )

nErros := 5
cErroCorruption := NIL


proc ErrorSys()
        ErrorBlock( {|e| DefError(e)} )
RETURN


*
*   DefError()
*

func DefError(e)
local i, cMessage, aOptions, nChoice, cNome, chave1,chave2,chave3,chave4,chave5

public aFoneAnal, aNomeAnal, aEmprAnal, AemaiAnal
aEmprAnal = "HRB Solucoes em Informatica    "  // Coloque aqui o nome de sua empresa
aNomeAnal = "Suporte:(81)8835.1751(Roberta) - 9975.5923(Helio)   "  // Coloque aqui o nome do Analista responsavel
aFoneAnal = "(081) 3326.8332"                  // Coloque aqui seu fone para contato
aEmaiAnal = "hrb@hrbinfo.com.br    "            // Coloque aqui seu email para contato

IF ( E:GENCODE == 5 )  && DIVIDE 0
   RETURN(0)
ENDIF

IF ( e:genCode == EG_PRINT )
   aPrintErr = SET(_SET_DEVICE)
   IF aPrintErr = "PRINTER"
      SET DEVICE TO SCREEN
   ENDIF
   aopcaimp :={"Continuar","Sair"}
   aonnop = ALERT("Erro na Impressora (Desligada, Pause ou Sem Papel) ", aopcaimp,1,1)
   IF aonnop == 1
      IF aPrintErr = "PRINTER"
         SET DEVICE TO PRINTER
      ENDIF
      RETURN (.T.)
   ELSEIF aonnop == 2
      ALERT("Desviando p/ o Arquivo <PRINT.ERR>",,1,1)
      SET PRINT TO PRINT.ERR
      IF aPrintErr = "PRINTER"
         SET DEVICE TO PRINTER
      ENDIF
      RETURN (.T.)
   ENDIF
ENDIF


   IF ( e:genCode == EG_ZERODIV )
       RETURN (0)
   end


   // for network open error, set NETERR() and subsystem default

   IF ( e:genCode == EG_OPEN .and. e:osCode == 32 .and. e:canDefault )

       NetErr(.t.)
       RETURN (.f.)                                                                    // NOTE

   end


   // for lock error during APPEND BLANK, set NETERR() and subsystem default
   IF ( e:genCode == EG_APPENDLOCK .and. e:canDefault )

       NetErr(.t.)
       RETURN (.f.)                                                                    // NOTE

   end


   aCorAnt = setcolor()

MostraErro( e )

*       setcolor(aCorAnt)
        setcor(1)
        // build error message
        cMessage := ErrorMessage(e)


        // build options array
        // aOptions := {"Break", "Sair"}

        aOptions := {"Sair"}

        IF (e:canRetry)
                AAdd(aOptions, "Tenta Novamente")
        end

        IF (e:canDefault)
                AAdd(aOptions, "Default")
        end


        // put up alert box

        nChoice := 0
        WHILE ( nChoice == 0 )

                IF ( Empty(e:osCode) )
                        nChoice := Alert( cMessage, aOptions )

                ELSE
                        nChoice := Alert( cMessage + ;
                                                        ";(DOS Error " + NTRIM(e:osCode) + ")", ;
                                                        aOptions )
                end


                IF ( nChoice == NIL )
                        exit
                end

        end


        IF ( !Empty(nChoice) )

                // do as instructed
                IF ( aOptions[nChoice] == "Sair" )
                        Break(e)

                ELSEIF ( aOptions[nChoice] == "Tenta Novamente" )
                        RETURN (.t.)

                ELSEIF ( aOptions[nChoice] == "Default" )
                        RETURN (.f.)

                end

        end


        // display message and traceback

        IF ( !Empty(e:osCode) )
                cMessage += " (DOS Error " + NTRIM(e:osCode) + ") "
        end

        ? cMessage
        i := 2
        WHILE ( !Empty(ProcName(i)) )
                ? "Called from", Trim(ProcName(i)) + ;
                        "(" + NTRIM(ProcLine(i)) + ")  "

                i++
        end


        // give up
        ErrorLevel(1)
        KEYBOARD CHR(004) + CHR(013)
        QUIT

RETURN (.f.)




/***
*       ErrorMessage()
*/
func ErrorMessage(e)
local cMessage


        // start error message
        cMessage := if( e:severity > ES_WARNING, "Error ", "Warning " )


        // add subsystem name IF available
        IF ( ValType(e:subsystem) == "C" )
                cMessage += e:subsystem()
        ELSE
                cMessage += "???"
        end


        // add subsystem's error code IF available
        IF ( ValType(e:subCode) == "N" )
                cMessage += ("/" + NTRIM(e:subCode))
        ELSE
                cMessage += "/???"
        end


        // add error description IF available
        IF ( ValType(e:description) == "C" )
                cMessage += ("  " + e:description)
        end


        // add either filename or operation
        IF ( !Empty(e:filename) )
                cMessage += (": " + e:filename)

        ELSEIF ( !Empty(e:operation) )
                cMessage += (": " + e:operation)

        end


RETURN (cMessage)






FUNCTION MostraError( e )
LOCAL i, cTelaErro

*IF cErroCorruption <> NIL
*   SET COLOR TO BG/B
*   @ 10,10,18,70 BOX "€€€€€€€€ "
*   SET COLOR TO W+/B
*   @ 12,14 SAY "Foi detectado um erro em um dos arquivos do sistema !!"
*   @ 13,14 SAY "O erro nÑo foi  corrigido,  porque  nÑo  foi  poss°vel"
*   @ 14,14 SAY PADC("identificar a chave. O arquivo Ç: "+cErroCorruption,54)
*   @ 15,14 SAY "                  Chame o programador                 " COLOR "R+/B"
*   @ 16,14 SAY "               PRESSIONE QUALQUER TECLA               "
*   INKEY(0)
*   SET COLOR TO
*   CLS
*   QUIT
*ENDIF

IF UPPER(e:descriptio) == "CORRUPTION DETECTED" .AND. "NTX" $ UPPER(e:filename)
   IF EMPTY(NETNAME())
      cErroCorruption := e:filename
      nH := FOPEN(e:filename,2)
      cTexto := SPACE(512)
      cIndex := FREAD(nH,@cTexto,512)
      cIndex := ALLTRIM(SUBSTR(cTexto,23))
      FCLOSE(nH)
      ordCreate((e:filename),,(cIndex),{||&(cIndex)},)
      cErroCorruption := NIL
*     SET COLOR TO BG/B
*     @ 10,10,17,70 BOX "€€€€€€€€ "
*     SET COLOR TO W+/B
      botao(10,10,17,70)
      @ 12,14 SAY "Foi detectado um erro em um dos arquivos do sistema !!"
      @ 13,14 SAY "O erro j† foi corrigido, porÇm ser† necess†rio abortar"
      @ 14,14 SAY "a aplicaáÑo."
      @ 14,26 SAY "                Reinicialize o sistema         " COLOR "R+/B"
      @ 15,14 SAY "               PRESSIONE QUALQUER TECLA               "
      INKEY(0)
      SET COLOR TO
      CLS
      QUIT
   ELSE
      botao(10,10,17,70)
*     SET COLOR TO BG/B
*     @ 10,10,18,70 BOX "€€€€€€€€ "
*     SET COLOR TO W+/B
      @ 12,14 SAY "Foi detectado um erro em um dos arquivos do sistema !!"
      @ 13,14 SAY "O erro nÑo foi corrigido, porque vocà est†  rodando  o"
      @ 14,14 SAY PADC("programa em rede. O arquivo Ç: "+e:filename,54)
      @ 15,14 SAY "                  Chame o programador                 " COLOR "R+/B"
      @ 16,14 SAY "               PRESSIONE QUALQUER TECLA               "
      INKEY(0)
      SET COLOR TO
      CLS
      QUIT
   ENDIF
ENDIF




SETCANCEL(.F.)
SET CURSOR OFF
SET DEVICE TO SCREEN
cTelaErro := SAVESCREEN(00,00,24,79)
*SET COLOR TO N/BG
botao(00,02,23,78)
*@ 00,02 CLEA TO 23,78
@ 00,23 SAY alltrim(aEmprAnal)

AMenTel = ""
IF len(alltrim(aFoneAnal)) > 0
    aMenTel= AMenTel+"Fone : "+alltrim(aFoneAnal)
ENDIF
IF len(alltrim(aEmaiAnal)) > 0
    aMenTel= AMenTel+" Email : "+alltrim(aEmaiAnal)
ENDIF

@ 01,03 say alltrim(aMenTel)

@ 23,04 SAY aNomeAnal
setcor(3)
*SET COLOR TO W/B
@ 02,03 CLEA TO 22,77
@ 02,05 SAY PADC("Atencao: Informe os dados abaixo ao Analista responsavel",72)
@ 03,05 SAY PADC("Para imprimir esta tela precione [ENTER] ou [ESC]p/Abandonar",72)
setcor(3,'*')
@ 04,03 SAY PADC("(E R R O   DE   S I S T E M A)",75)
setcor(1)
@ 05,05 SAY "Data do erro.............: " + DTOC(DATE()) + "      Hora: " + TIME()
@ 06,05 SAY "Memoria para caracteres..: " + ALLTRIM(STR(MEMORY(0))) + " para blocos: " + ALLTRIM(STR(MEMORY(1))) ;
            +" para RUN: " + ALLTRIM(STR(MEMORY(2)))
DEFERROR2(e)
@ 12,05 SAY "Erro a Nivel Sistema DOS.: "+LTRIM(STR(e:osCode))
@ 13,05 SAY "Nome do SubSistema.......: "+e:subSystem
@ 14,05 SAY "Erro a Nivel SubSistema..: "+LTRIM(STR(e:subCode))
@ 15,05 SAY "Codigo do Erro Generico..: "+LTRIM(STR(e:genCode))
@ 17,05 SAY "No.Vezes da Falha........: "+LTRIM(STR(e:tries))
@ 18,05 SAY "No.do Erro...............: "+LTRIM(STR(e:severity))
@ 19,05 SAY "Possibilita DEFAULT......: "+IF(e:canDefault,"SIM","NAO")
@ 20,05 SAY "Possibilita RETRY........: "+IF(e:canRetry,"SIM","NAO")
@ 21,05 SAY "Pos.Subst. Erro p/ Valor.: "+IF(e:canSubstitute,"SIM","NAO")
IF ALIAS() # ""
   @ 09,05 SAY "Arq. em uso: " + ALIAS() + " ordem: "+INDEXKEY(INDEXORD())
ENDIF
@ 10,40 SAY "Argumento Funcao: "
        setcor(3)
   IF VALTYPE(e:args) == "A"
      @ 10,58 SAY "Matriz: "+ LTRIM(STR(LEN(e:args)))+" elementos "
      i := 1
      DO WHIL i < 4
         @ 10 + i , 40  SAY  "Elemen[" + STR(i,2) + "]......: "
         @ 10 + i , 58  SAY  e:args[i]
         IF i == LEN(e:args)
            EXIT
         ENDIF
         i++
      ENDDO
   ELSE
      @ 10,60 SAY e:args
   ENDIF
   setcor(1)
   @ 14,40 TO 22,77
   @ 14,50 SAY " Funcoes com erro : "
        j := 1
        i := 3
        setcor(3)
        WHILE ( !Empty(ProcName(i)) )
                @ j + 14,41 SAY "FunáÑo: " + Trim(ProcName(i)) + ;
                        "(" + NTRIM(ProcLine(i)) + ")  "

                i++
                j++
                IF j > 7
                   EXIT
                ENDIF
        end
        setcor(1)
IF e:osCode # 4
   GRAVAERRO(e)
ENDIF
INKEY(0)
IF LASTKEY() == 13
DO WHIL .T.
   set printer to
   IF ISPRINTER()
      SET DEVICE TO PRINTER
      @ 00,00 SAY CHR(27) + CHR(18)
      @ 00,07 SAY padc(aEmprAnal,75)
      @ 01,05 SAY padc("Relatorio de erros genericos",75)


@ 03,05 SAY "Data do erro.............: " + DTOC(DATE()) + "      Hora: " + TIME()
@ 04,05 SAY "Memoria para caracteres..: " + ALLTRIM(STR(MEMORY(0))) + " para blocos: " + ALLTRIM(STR(MEMORY(1))) ;
            +" para RUN: " + ALLTRIM(STR(MEMORY(2)))
DEFERROR2(e)
@ 10,05 SAY "Erro a Nivel Sistema DOS.: "+LTRIM(STR(e:osCode))
@ 11,05 SAY "Erro a Nivel SubSistema..: "+LTRIM(STR(e:subCode))
@ 12,05 SAY "Nome do SubSistema.......: "+e:subSystem
@ 13,05 SAY "Codigo do Erro Generico..: "+LTRIM(STR(e:genCode))
@ 15,05 SAY "No.Vezes da Falha........: "+LTRIM(STR(e:tries))
@ 16,05 SAY "No.do Erro...............: "+LTRIM(STR(e:severity))
@ 17,05 SAY "Possibilita DEFAULT......: "+IF(e:canDefault,"SIM","NAO")
@ 18,05 SAY "Possibilita RETRY........: "+IF(e:canRetry,"SIM","NAO")
@ 19,05 SAY "Pos.Subst. Erro p/ Valor.: "+IF(e:canSubstitute,"SIM","NAO")
IF ALIAS() # ""
   @ 20,05 SAY "Arquivo em uso...........: "+ALIAS() + " ordem: " + INDEXKEY(INDEXORD())
ENDIF
@ 21,05 SAY "Argumento Funcao: "
   IF VALTYPE(e:args) == "A"
      @ 21,23 SAY "Matriz: "+ LTRIM(STR(LEN(e:args)))+" elementos "
      i := 1
      DO WHIL .T.
         @ 21 + i , 05  SAY  "Elemen[" + STR(i,2) + "]......: "
         @ 21 + i , 23  SAY  e:args[i]
         IF i == LEN(e:args)
            EXIT
         ENDIF
         i++
      ENDDO
   ELSE
      @ 21,25 SAY e:args
   ENDIF
        j := i
        i := 3
        WHILE ( !Empty(ProcName(i)) )
                @ j + 23,05 SAY "FunáÑo: " + Trim(ProcName(i)) + ;
                        "(" + NTRIM(ProcLine(i)) + ")  "

                i++
                j++
        end

        J++
        j= j+23

        @ j,05 SAY padc("Fim de relatorio de erros",75)
        j++
        j++
        @ j,05 say aNomeAnal
        IF len(alltrim(aFoneAnal)) > 0
           j++
           @ j,05 say "Fone             : "+aFoneAnal
        ENDIF
        IF len(alltrim(aEmaiAnal)) > 0
           j++
           @ j,05 say "Email            : "+aEmaiAnal
        ENDIF

        Eject
        SET DEVICE TO SCREEN

        EXIT
   ELSE
      aaOPTIONS := {"Continuar","Sair"}
      nnOPCAO := ALERT("Impressora desligada ou desconectada", aaOPTIONS)
      IF nnOPCAO == 1
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDIF
   set printer to
ENDDO
ENDIF
RESTSCREEN(00,00,24,79,cTelaErro)
SETCANCEL(.T.)
SET CURSOR ON
RETURN (NIL)
***************************************************************************
func DefError2(e)
local i, cMessage, aOptions, nChoice

        IF ( e:genCode == EG_ZERODIV )
                RETURN (0)
        end

        IF ( e:genCode == EG_OPEN .and. e:osCode == 32 .and. e:canDefault )

                 NetErr(.t.)
                 RETURN (.f.)                                                                    // NOTE

        end

        IF ( e:genCode == EG_APPENDLOCK .and. e:canDefault )

                NetErr(.t.)
                RETURN (.f.)                                                                    // NOTE

        end

        cMessage := ErrorMessage(e)
        setcor(3)
        @ 08,05 SAY cMessage
        setcor(1)

RETURN (.t.)
********************************
FUNCTION GRAVAERRO(e)
SET DEVICE TO PRINT
SET PRINTER TO "ERROS.TXT"
@ 00,00 SAY "Data do erro.............: " + DTOC(DATE()) + "      Hora: " + TIME()
@ 01,00 SAY "Mem¢ria para caracteres..: " + ALLTRIM(STR(MEMORY(0)))
@ 02,00 SAY "Erro a N°vel Sistema DOS.: "+LTRIM(STR(e:osCode))
@ 03,00 SAY "Nome do SubSistema.......: "+e:subSystem
@ 04,00 SAY "Erro a N°vel SubSistema..: "+LTRIM(STR(e:subCode))
@ 05,00 SAY "C¢digo do Erro GenÇrico..: "+LTRIM(STR(e:genCode))
@ 06,00 SAY "Nß Vezes da Falha........: "+LTRIM(STR(e:tries))
@ 07,00 SAY "Nß do Erro...............: "+LTRIM(STR(e:severity))
@ 08,00 SAY "Possibilita DEFAULT......: "+IF(e:canDefault,"SIM","NAO")
@ 09,00 SAY "Possibilita RETRY........: "+IF(e:canRetry,"SIM","NAO")
@ 10,00 SAY "Pos.Subst. Erro p/ Valor.: "+IF(e:canSubstitute,"SIM","NAO")
IF ALIAS() # ""
   @ 11,00 SAY "Arq. em uso: " + ALIAS() + " ordem: "+INDEXKEY(INDEXORD())
ENDIF
@ 12,00 SAY "Argumento FunáÑo: "
i := 1
IF VALTYPE(e:args) == "A"
   @ 13,00 SAY "Matriz: "+ LTRIM(STR(LEN(e:args)))+" elementos "
   DO WHIL i < 4
      @ 13+i,00 SAY "Elemen[" + STR(i,2) + "]......: "
      @ 13+i,20 SAY e:args[i]
      IF i == LEN(e:args)
         EXIT
      ENDIF
      i++
   ENDDO
ELSE
@ 13+i+1,00 SAY e:args
ENDIF
@ 13+i+2,00 SAY " Funáîes com erro : "
j := 1
i := 3
WHILE ( !Empty(ProcName(i)) )
         @ 13+i+3,00 SAY "FunáÑo: " + Trim(ProcName(i)) + ;
                 "(" + NTRIM(ProcLine(i)) + ")  "

         i++
         j++
         IF j > 7
                  EXIT
         ENDIF
end
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN NIL
*****************************  F I M **************************************
*************************** F I M *********************************
********************************
* FUNCAO PARA ABRIR ARQUIVOS
********************************

FUNCTION AbriArq(arq,al,tp,sit)
**********************
LOCAL vtela,mdir

vtela := SAVESCREEN(23,00,23,79)
*USE saccfg ALIAS cfg SHARED NEW

IF sit <> NIL
        mdir := sit
ELSE
        mdir := mdiretorio
ENDIF
IF ! FILE('\'+mdrive+'\'+ALLTRIM(mdir)+'\'+arq+'.dbf') .AND. al <> 'cdmen' .AND. al <> 'caix' .AND. al <> 'desp' .AND. al <> 'sen'
        atencao('Arquivo: '+ALLTRIM(mdir)+UPPER(arq)+'.DBF nao encontrado neste caminho verifique o caminho especificado')
        CLEAR MEMORY
        QUIT
ENDIF
mensagem('Aguarde...Tentando Acesso ao Arquivo: '+UPPER(arq))
WHILE .T.
        IF tp = 'E'
                IF al = 'cdmen' .OR. al = 'caix' .OR. al = 'desp' .OR. al = 'sen' .OR. al = 'set'
                        USE ('\'+mdrive+'\'+arq) ALIAS (al) EXCLUSIVE NEW
                        IF ! NETERR()
                                RESTSCREEN(23,00,23,79,vtela)
                                RETURN .T.
                        ENDIF
                ELSE
                        USE ('\'+mdrive+'\'+ALLTRIM(mdir)+'\'+arq) ALIAS (al) EXCLUSIVE NEW
                        IF ! NETERR()
                                RESTSCREEN(23,00,23,79,vtela)
                                RETURN .T.
                        ENDIF
                ENDIF
        ELSE
                IF al = 'cdmen' .OR. al = 'caix' .OR. al = 'desp' .OR. al = 'sen' .OR. al = 'set'
                        IF SELECT(al) > 0
                                RESTSCREEN(23,00,23,79,vtela)
                                RETURN .T.
                        ENDIF

                        USE ('\'+mdrive+'\'+arq) ALIAS (al) SHARED NEW
                        IF ! NETERR()
                                RESTSCREEN(23,00,23,79,vtela)
                                RETURN .T.
                        ENDIF
                ELSE
                        IF SELECT(al) > 0
                                RESTSCREEN(23,00,23,79,vtela)
                                RETURN .T.
                        ENDIF

                        USE ('\'+mdrive+'\'+ALLTRIM(mdir)+'\'+arq) ALIAS (al) SHARED NEW
                        IF ! NETERR()
                                RESTSCREEN(23,00,23,79,vtela)
                                RETURN .T.
                        ENDIF
                ENDIF
        ENDIF
        IF LASTKEY() = 27
                EXIT
        ENDIF
ENDDO
RESTSCREEN(23,00,23,79,vtela)
RETURN .F.
*****************************  F I M **************************************
*
* Funcao de Calculadora
* Desenvolvida : Anonimo
*                A pessoa que envio nao quer os creditos
*
* Utilize as Ctrol+setas de movimentacao para mover pela tela
*
* Para colocar a calculadoraem seu sistemas adicione as seguintes linhas
* em seu programa principal
*
* Set procedure to calc.prg (Ou o nome que voce salvou a rotina)
* Set key -1 to calcu()
*
* Quando voce teclar F2 a calculadora sera acionada
*
* Somente para teste compile assim mesmo, quando for colocar em seu sistema
* retire a linha abaixo

*CALCU()  && Retire esta linha quando colocar a caluculadora em seu sistema

* Funcao

FUNCTION MAQCALC()
LOCAL tela_data
LOCAL mdata_ini := CTOD('  /  /  ')
LOCAL mdata_fim := CTOD('  /  /  ')
LOCAL nI
LOCAL nCALCU_X := ROW()
LOCAL nCALCU_Y := COL()
LOCAL cCOR := SETCOLOR()
LOCAL lVERDAD := .F.
LOCAL nMovCal_X := 2
LOCAL nMovCal_Y := -6
LOCAL cTELA := SAVESCREEN(nMovCal_X+0,nMovCal_Y+47,nMovCal_X+16,nMovCal_Y+79)
LOCAL cVAR := "cALGA"
LOCAL cDIG := CHR(0)
LOCAL nRESULT := 0
LOCAL cUOPER := ""
LOCAL cPERC := " "
LOCAL cCURSOR := SETCURSOR()
PRIVATE aBotCalc := {},cALGA := SPACE(14),cDEC := ""
SET CURSOR OFF
JANEL(00 + nMOVCAL_X,49 + nMOVCAL_Y,15 + nMOVCAL_X,79 + nMOVCAL_Y,"Calculadora")
setcor(6,'*')
@ 02 + nMOVCAL_X,53 + nMOVCAL_Y SAY REPL(CHR(220),23)
@ 03 + nMOVCAL_X,53 + nMOVCAL_Y SAY CHR(219) + SPACE(21) + CHR(219)
@ 04 + nMOVCAL_X,53 + nMOVCAL_Y SAY REPL(CHR(223),23)
BOTCALC(06 + nMOVCAL_X,53 + nMOVCAL_Y," 7 ","07/01")
BOTCALC(06 + nMOVCAL_X,58 + nMOVCAL_Y," 8 ","07/01")
BOTCALC(06 + nMOVCAL_X,63 + nMOVCAL_Y," 9 ","07/01")
BOTCALC(08 + nMOVCAL_X,53 + nMOVCAL_Y," 4 ","07/01")
BOTCALC(08 + nMOVCAL_X,58 + nMOVCAL_Y," 5 ","07/01")
BOTCALC(08 + nMOVCAL_X,63 + nMOVCAL_Y," 6 ","07/01")
BOTCALC(10 + nMOVCAL_X,53 + nMOVCAL_Y," 1 ","07/01")
BOTCALC(10 + nMOVCAL_X,58 + nMOVCAL_Y," 2 ","07/01")
BOTCALC(10 + nMOVCAL_X,63 + nMOVCAL_Y," 3 ","07/01")
BOTCALC(12 + nMOVCAL_X,53 + nMOVCAL_Y,"  0  ","07/01")
BOTCALC(12 + nMOVCAL_X,61 + nMOVCAL_Y,"  .  ","07/01")
BOTCALC(06 + nMOVCAL_X,nMOVCAL_Y + 69," - ","07/01")
BOTCALC(12 + nMOVCAL_X,nMOVCAL_Y + 74," % ","07/01")
BOTCALC(08 + nMOVCAL_X,nMOVCAL_Y + 69," + ","07/01")
BOTCALC(08 + nMOVCAL_X,nMOVCAL_Y + 74," * ","07/01")
BOTCALC(10 + nMOVCAL_X,nMOVCAL_Y + 74," / ","07/01")
BOTCALC(10 + nMOVCAL_X,nMOVCAL_Y + 69," = ","07/01")
BOTCALC(06 + nMOVCAL_X,nMOVCAL_Y + 74,"C/E","07/01")
SETCOLOR("W")
@ 03+nMOVCAL_X,54 + nMOVCAL_Y SAY " " + STR(nRESULT,19,4) + " "
DO WHILE cDIG <> CHR(27)
   cDIG := CHR(INKEY(0))
   IF LASTKEY() = ASC('D') .OR. LASTKEY() = ASC('d')
        tela_data := SAVESCREEN(00,00,24,79)
        botao(15 + nMOVCAL_X,49 + nMOVCAL_Y,20 + nMOVCAL_X,79 + nMOVCAL_Y)
        DEVPOS(16 + nMOVCAL_X,50 + nMOVCAL_Y);DEVOUT('1¶ Data..:')
        DEVPOS(17 + nMOVCAL_X,50 + nMOVCAL_Y);DEVOUT('2¶ Data..:')
        DEVPOS(18 + nMOVCAL_X,50 + nMOVCAL_Y);DEVOUT('Resultado:')
        DEVPOS(19 + nMOVCAL_X,50 + nMOVCAL_Y);DEVOUT('Obs: Data 1¶ - 2¶ = Res.')
        @ 16 + nMOVCAL_X,61 + nMOVCAL_Y GET mdata_ini
        @ 17 + nMOVCAL_X,61 + nMOVCAL_Y GET mdata_fim VALID IF(mdata_fim < mdata_ini,.F.,.T.)
        READ
        setcor(3)
        DEVPOS(18 + nMOVCAL_X,61 + nMOVCAL_Y);DEVOUT(TRANSFORM(mdata_fim-mdata_ini,'9,999,999.99'))
        DEVPOS(19 + nMOVCAL_X,50 + nMOVCAL_Y);DEVOUT('Pressione Qualquer Tecla')
        setcor(1)
        INKEY(10)
        RESTSCREEN(00,00,24,79,tela_data)
   ENDIF
   IF cDIG == "7" .OR. cDIG == CHR(1);BOTCALC(06+nMOVCAL_X,53+nMOVCAL_Y," 7 ","07/01",.T.)
   ELSEIF cDIG == "8" .OR. cDIG == CHR(5);BOTCALC(06+nMOVCAL_X,58+nMOVCAL_Y," 8 ","07/01",.T.)
   ELSEIF cDIG == "9" .OR. cDIG == CHR(18);BOTCALC(06+nMOVCAL_X,63+nMOVCAL_Y," 9 ","07/01",.T.)
   ELSEIF cDIG == "4" .OR. cDIG == CHR(19);BOTCALC(08+nMOVCAL_X,53+nMOVCAL_Y," 4 ","07/01",.T.)
   ELSEIF cDIG == "5" .OR. cDIG == CHR(76);BOTCALC(08+nMOVCAL_X,58+nMOVCAL_Y," 5 ","07/01",.T.)
   ELSEIF cDIG == "6" .OR. cDIG == CHR(4);BOTCALC(08+nMOVCAL_X,63+nMOVCAL_Y," 6 ","07/01",.T.)
   ELSEIF cDIG == "1" .OR. cDIG == CHR(6);BOTCALC(10+nMOVCAL_X,53+nMOVCAL_Y," 1 ","07/01",.T.)
   ELSEIF cDIG == "2" .OR. cDIG == CHR(24);BOTCALC(10+nMOVCAL_X,58+nMOVCAL_Y," 2 ","07/01",.T.)
   ELSEIF cDIG == "3" .OR. cDIG == CHR(3);BOTCALC(10+nMOVCAL_X,63+nMOVCAL_Y," 3 ","07/01",.T.)
   ELSEIF cDIG == "0" .OR. cDIG == CHR(22);BOTCALC(12+nMOVCAL_X,53+nMOVCAL_Y,"  0  ","07/01",.T.)
   ELSEIF cDIG == "." .OR. cDIG == CHR(7);BOTCALC(12+nMOVCAL_X,61+nMOVCAL_Y,"  .  ","07/01",.T.)
   ELSEIF cDIG == "-";BOTCALC(06+nMOVCAL_X,nMOVCAL_Y+69," - ","07/01",.T.)
   ELSEIF cDIG == "%";BOTCALC(12+nMOVCAL_X,nMOVCAL_Y+74," % ","07/01",.T.)
   ELSEIF cDIG == "+";BOTCALC(08+nMOVCAL_X,nMOVCAL_Y+69," + ","07/01",.T.)
   ELSEIF cDIG == "*";BOTCALC(08+nMOVCAL_X,nMOVCAL_Y+74," * ","07/01",.T.)
   ELSEIF cDIG == "/";BOTCALC(10+nMOVCAL_X,nMOVCAL_Y+74," / ","07/01",.T.)
   ELSEIF cDIG == "=" .OR. ASC(cDIG) == 13;BOTCALC(10+nMOVCAL_X,nMOVCAL_Y+69," = ","07/01",.T.)
   ELSEIF AT(cDIG,"cCeE") <> 0;BOTCALC(06+nMOVCAL_X,nMOVCAL_Y+74,"C/E","07/01",.T.)
   ENDIF
   SETCOLOR("W")
   IF AT(cDIG,CHR(26)+CHR(2)+CHR(141)+CHR(145)) <> 0
      cMovTela := SAVESCREEN(nMOVCAL_X+0,nMOVCAL_Y+49,nMOVCAL_X+15,nMOVCAL_Y+79)
      RESTSCREEN(nMOVCAL_X+0,nMOVCAL_Y+47,nMOVCAL_X+16,nMOVCAL_Y+79,cTELA)
      IF cDIG == CHR(26);nMOVCAL_Y--
      ELSEIF cDIG == CHR(2);nMOVCAL_Y++
      ELSEIF cDIG == CHR(141);nMOVCAL_X--
      ELSEIF cDIG == CHR(145);nMOVCAL_X++
      ENDIF
      IF nMOVCAL_X == 0 ; nMOVCAL_X := 1 ; ENDIF
      IF nMOVCAL_X + 0 < 0;nMOVCAL_X := 0
      ELSEIF nMOVCAL_X + 17 > 24;nMOVCAL_X := 24 - 17
      ELSEIF nMOVCAL_Y + 47 < 0;nMOVCAL_Y := -47
      ELSEIF nMOVCAL_Y + 79 > 79;nMOVCAL_Y := 0
      ENDIF
      cTela := SAVESCREEN(nMOVCAL_X+0,nMOVCAL_Y+47,nMOVCAL_X+16,nMOVCAL_Y+79)
      RESTSCREEN(nMOVCAL_X+0,nMOVCAL_Y+49,nMOVCAL_X+15,nMOVCAL_Y+79,cMovTela)
   ENDIF
   IF cDIG == CHR(27);EXIT;ENDIF
   cXX := AT(cDIG,CHR(22)+CHR(7)+CHR(6)+CHR(24)+CHR(3)+CHR(19)+CHR(76)+CHR(4)+CHR(1)+CHR(5)+CHR(18))
   IF cXX <> 0;cDIG := SUBS("0.123456789",cXX,1);ENDIF
   cVAR := IF(cDIG == ".","cDEC",cVAR)
   cALGA := IF(cDIG == "." .AND. cALGA == SPACE(14),SPACE(13) + "0",cALGA)
   IF cDIG == "%"
      IF lVERDAD
         cPERC := IF(cPERC == "%"," ","%")
         setcor(6)
*        SETCOLOR("00/03")
         @ 03+nMOVCAL_X,75+nMOVCAL_Y SAY cPERC
         SETCOLOR("W")
      ELSE
         @ 03+nMOVCAL_X,55+nMOVCAL_Y SAY SPACE(13)+"0.0000"
      ENDIF
   ELSEIF cDIG $ "0123456789" .AND. LEN(LTRIM(&cVAR))<>IF(cVAR == "cDEC",4,14)
      &cVAR := IF(cVAR == "cDEC",&cVAR + cDIG,SUBS(&cVAR + cDIG,2))
      @ 03+nMOVCAL_X,55+nMOVCAL_Y SAY cALGA + "." + SUBS(cDEC+SUBS("00000",LEN(cDEC)+1),1,4)
      lVERDAD := IF(lVERDAD .AND. cUOPER $ "=" + CHR(13),.F.,lVERDAD)
   ELSEIF cDIG $ "+-/*=" + CHR(13)
      IF lVERDAD
         IF VAL(cALGA + "." + cDEC) <> 0
            IF cPERC == "%"
               cOpera := STR(nRESULT,19,4) + cUOPER + "(" + STR(nRESULT,19,4) + "*" + cALGA + "." + SUBS(cDEC + SUBS("00000",LEN(cDEC) + 1),1,4) + ")/100.000"
            ELSE
               cOpera := STR(nRESULT,19,4) + cUOPER + cALGA + "." + SUBS(cDEC + SUBS("00000",LEN(cDEC)+1),1,4)
            ENDIF
            nRESULT := &cOpera
            IF STR(nRESULT,19,4) == REPL("*",19)
               @ 03 + nMOVCAL_X,55 + nMOVCAL_Y SAY "    * E R R O *    "
               nRESULT := 0
               setcor(6)
*              SETCOLOR("00/03")
               @ 03+nMOVCAL_X,53+nMOVCAL_Y SAY " "
               @ 03+nMOVCAL_X,75+nMOVCAL_Y SAY " "
               SETCOLOR("W")
            ELSE
               @ 03+nMOVCAL_X,55+nMOVCAL_Y SAY STR(nRESULT,19,4)
            ENDIF
         ENDIF
      ELSEIF cDIG $ "+-/*"
         lVERDAD := .T.
         nRESULT := VAL(cALGA + "." + cDEC)
      ENDIF
      setcor(6)
*     SETCOLOR("00/03")
      @ 03+nMOVCAL_X,53+nMOVCAL_Y SAY IF(cDIG $ "+-/*",cDIG," ")
      @ 03+nMOVCAL_X,75+nMOVCAL_Y SAY " "
      SETCOLOR("W")
      cVAR := "cALGA"
      cALGA := SPACE(14)
      cDEC := ""
      cUOPER := cDIG
      cPERC := " "
   ELSEIF cDIG $ "EeCc"
      cVAR := "cALGA"
      cALGA := SPACE(14)
      cDEC := ""
      cPERC := " "
      IF cDIG $ "Cc"
         lVERDAD := .F.
         setcor(6)
*         SETCOLOR("00/03")
         @ 03+nMOVCAL_X,53+nMOVCAL_Y SAY " "
         @ 03+nMOVCAL_X,75+nMOVCAL_Y SAY " "
         SETCOLOR("W")
      ENDIF
      @ 03+nMOVCAL_X,55+nMOVCAL_Y SAY SPACE(13)+"0.0000"
   ELSEIF cDIG == CHR(116)  .OR.  cDIG == CHR(84)
      IF VAL(cAlga+"."+cDec) <> 0
         KEYBOARD CHR(K_CTRL_Y)+ALLTRIM(cALGA)+SUBSTR(ALLTRIM(TRANSFORM(VAL("."+cDEC),GETACTIVE():PICTURE)),2)
      ELSEIF nResult <> 0
         cC_Alga := STR(INT(nResult))
         cC_Dec  := VAL(SUBSTR(ALLTRIM(STR(nResult-INT(nResult))),3))
         KEYBOARD CHR(K_CTRL_Y)+ALLTRIM(cC_Alga)+SUBSTR(ALLTRIM(TRANSFORM(VAL("."+cDEC),GETACTIVE():PICTURE)),2)
      ENDIF
      EXIT
   ENDIF
ENDDO
SETCOLOR(cCOR)
RESTSCREEN(0+nMOVCAL_X,47+nMOVCAL_Y,16+nMOVCAL_X,79+nMOVCAL_Y,cTela)
SETCURSOR(cCURSOR)
@ nCALCU_X,nCALCU_Y SAY ""
RETURN NIL


***************************************************************************

STATIC FUNCTION BOTCALC(nBotX,nBotY,cBotN,cBotC)
IF PCOUNT() == 5
   IF cBotN == " = "
      SETCOLOR("N/"+ALLTRIM(SUBS(cBotC,4)))
      @ nBotX + 0,nBotY + 2 SAY " "
      @ nBotX + 1,nBotY + 2 SAY " "
      @ nBotX + 2,nBotY + 2 SAY " "
      @ nBotX + 3,nBotY - 1 SAY "   "
      SETCOLOR("00/07")
      @ nBotX + 0,nBotY - 1 SAY "   "
      @ nBotX + 1,nBotY - 1 SAY " = "
      @ nBotX + 2,nBotY - 1 SAY "   "
   ELSE
      SETCOLOR("N/"+ALLTRIM(SUBS(cBotC,4)))
      @ nBotX,nBotY + LEN(cBotN) - 1 SAY " "
      @ nBotX + 1,nBotY - 1 SAY REPL(" ",LEN(cBotN))
      SETCOLOR("00/07")
      @ nBotX,nBotY - 1 SAY cBotN
   ENDIF
   INKEY(.1)
   SETCOLOR("N/"+ALLTRIM(SUBS(cBotC,4)))
   IF cBotN == " = "
      @ nBotX + 0,nBotY - 1 SAY "ö"
      @ nBotX + 1,nBotY - 1 SAY "Í"
      @ nBotX + 2,nBotY - 1 SAY "Í"
      @ nBotX + 3,nBotY - 1 SAY "···"
   ELSE
      @ nBotX,nBotY - 1 SAY "ö"
      @ nBotX + 1,nBotY - 1 SAY REPL("·",LEN(cBotN))
   ENDIF
ENDIF
SETCOLOR("00/07")
IF cBotN == " = "
   @ nBotX + 0,nBotY SAY "   "
   @ nBotX + 1,nBotY SAY " = "
   @ nBotX + 2,nBotY SAY "   "
   AADD(aBotCalc,{nBotX+0,nBotY,nBotX+2,nBotY+LEN(SPACE(03))," = "})
ELSE
   @ nBotX,nBotY SAY cBotN
   AADD(aBotCalc,{nBotX,nBotY,nBotX,nBotY+LEN(cBotN),cBotN})
ENDIF
SETCOLOR("N/"+ALLTRIM(SUBS(cBotC,4)))
IF cBotN == " = "
   @ nBotX + 0,nBoTY - 1 SAY "ö"
   @ nBotX + 1,nBoTY - 1 SAY "Í"
   @ nBotX + 2,nBoTY - 1 SAY "Í"
   @ nBotX + 3,nBoTY - 1 SAY "···"
ELSE
   @ nBotX,nBotY - 1 SAY "ö"
   @ nBotX + 1,nBotY - 1 SAY REPL("·",LEN(cBotN))
ENDIF
RETURN .T.

*

STATIC FUNCTION JANEL(nCoord1,nCoord2,nCoord3,nCoord4,cTexto)
IF PCOUNT() # 5
   cTexto := ""
ENDIF
setcor(6)
*SETCOLOR("07/01")
@ nCoord1,nCoord2 CLEAR TO nCoord3,nCoord4
setcor(6,'*')
*SETCOLOR("00/03")
@ nCoord1,nCoord2,nCoord3,nCoord4 BOX " "
@ nCoord1,nCoord2 SAY "Á"
IF LEN(TRIM(cTexto)) > 0
   @ nCoord1,nCoord2+(((nCoord4+1-nCoord2)-LEN(cTexto))/2) SAY cTexto
ENDIF
@ nCoord3-1,nCoord2+2 SAY 'Calcular DATAS Digite: [D]'
***************************   F I M   *******************************8
/*
*************************
* TIPO DE IMPRESSAO  ARQUIVOS OU IMPRESSORA
*************************

FUNCTION imp_arq(marq,mporta,op)
******************

LOCAL tela,lci,cci,lba,cba

lci := 10
cci := 03
lba := 13
cba := 73
IF op = NIL
        op := 'S'
ENDIF


tela := SAVESCREEN(01,00,24,79)
marq := '\'+CURDIR()+'\'+marq+SPACE(35-LEN('\'+CURDIR()+'\'+marq))

WHILE .T.
        mensagem('Escolha a configuracao ou <ESC> p/abandonar')
        mporta_imp := 'LPT1'
        setcor(1)
        botao(lci,cci,lba,cba,,' Configuracao de IMPRESSAO ')
        botao(lci+1,cci+16,lci+3,cci+35)
        botao(lci+1,cci+40,lci+3,cci+59)
        @ lci+4,cci+1 TO lci+4,cba-1
        DEVPOS(lci+5,cci+1);DEVOUT('Nome do Arquivo de impressao.........:')
        DEVPOS(lci+6,cci+1);DEVOUT('Tipo de Impressora [J]ato [M]atricial:')
        DEVPOS(lci+7,cci+1);DEVOUT('Porta Impressora.....................:')
        @ lci+2,cci+17 PROMPT '    IMPRESSORA    '
        @ lci+2,cci+41 PROMPT '     ARQUIVO      '        //+marq
        SET INTEN ON
        MENU TO mimp_tipo
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN .F.
        ENDIF
        IF mimp_tipo = 2
                botao(lci+1,cci+40,lci+3,cci+59,'*')
                mporta := 'PRN '
                @ lci+05,cci+40 GET marq PICT '@!' VALID IF(EMPTY(marq),.F.,.T.)
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
        ELSE
                botao(lci+1,cci+16,lci+3,cci+35,'*')
        ENDIF
        @ lci+6,cci+40 GET mtipo_imp PICT '@!' VALID mtipo_imp $ 'M,J' .AND. IF(EMPTY(mtipo_imp),.F.,.T.)
        @ lci+7,cci+40 GET mporta_imp PICT '@!' WHEN mimp_tipo = 1 VALID mporta_imp $ 'LPT1,LPT2,LPT3,LPT4,LPT5,COM1,COM2,COM3,COM4,PRN ' .AND. IF(EMPTY(mporta_imp),.F.,.T.)
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF SUBSTR(mporta_imp,1,3) = 'LPT' .AND. mimp_tipo = 1
                IF ! PARALELA(VAL(SUBSTR(mporta_imp,4,1)))
                        atencao('Porta de impressao '+mporta_imp+' nao esta ativa')
                        LOOP
                ENDIF
                WHILE .T.
                        IF ! IMPOK(VAL(SUBSTR(mporta_imp,4,1)))
                                atencao('Porta de impressao '+mporta_imp+' nao esta pronta ou esta desligada - <ESC> p/abortar a impressao',4)
                                INKEY(.3)
                                IF LASTKEY() = 27
                                        EXIT
                                ENDIF
                                LOOP
                        ELSE
                                EXIT
                        ENDIF
                ENDDO
                IF LASTKEY() = 27
                        LOOP
                ENDIF
        ENDIF
        mensagem('Confirma a Impressao [S/n]:')
        op := 'S'
        @ MAXROW()-1,COL()+1 GET op PICT '@!' VALID op $ 'S,N'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF op = 'S'
                RESTSCREEN(01,00,24,79,tela)
                mensagem('Espere o final da impressao !!!')
                SET DEVI TO PRINT
                IF mimp_tipo = 1
                        SET PRINT TO (mporta_imp)
                ELSE
                        SET PRINT TO (marq)
                ENDIF
                RETURN .T.
        ELSE
                LOOP
        ENDIF
ENDDO
RETURN NIL
*/
*************************** f i m *******************************************
*************************************
* FUNCAO DE DESTRIBUICAO DE CONSULTA
*************************************

FUNCTION menu_cons
*******************
IF READVAR() = 'MMATRICULA'
        F7_ALUNO()
ENDIF
RETURN

