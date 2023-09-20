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
setcor(6)
DEVPOS(la+2,ca+2);DEVOUT('ÚÄÄÄÄÄÄÄÄÄÄÛÛ  ÛÛ')
DEVPOS(la+3,ca+2);DEVOUT('³  ÜÜÜÜÜÜÜÜ  ÛÛ')
DEVPOS(la+4,ca+2);DEVOUT('³     ÛÛ       ÛÛ')
DEVPOS(la+5,ca+2);DEVOUT('³     ÛÛ ÛÛ  ÛÛ')
DEVPOS(la+6,ca+2);DEVOUT('³     ÛÛ   ÛÛ  ')
DEVPOS(la+7,ca+2);DEVOUT('³  ÜÜÜÛÛÜÜÜ  ³')
DEVPOS(la+8,ca+2);DEVOUT('ÀÄÄÄÄÄÄÄÄÄÄÄÄÙ')
SETCOLOR('N/W,X')
DEVPOS(la+2,ca+21);DEVOUT(PADC('>>> I N S E R C O M   Informatica Ltda. <<<',57),setcor(5))
DEVPOS(la+3,ca+21);DEVOUT(PADC('Av.Conselheiro Aguiar, 3426 - Sl.07 - Boa Viagem',57),setcor(2))
DEVPOS(la+4,ca+21);DEVOUT(PADC('Recife - PE. Fone:(81)3327.7530 - (Tacaimbo)(81)3755.1489',57))
DEVPOS(la+5,ca+21);DEVOUT(PADC('Suporte: H‚lio: (81) 9975.5923 - Tarcisio: (81) 9964.8264',57))
DEVPOS(la+6,ca+21);DEVOUT(PADC('         Edson: (81) 8835.1752 - Luiz....: (81) 8835.1753',57))
DEVPOS(la+7,ca+21);DEVOUT(PADC('www.insercom.com.br  -  e-mail: insercom@insercom.com.br',57),setcor(5))
DEVPOS(la+8,ca+21);DEVOUT(PADC('Direito Copia (C) 1994-2004 da INSERCOM',57),setcor(5))
IF tip <> NIL
        DEVPOS(la+9,ca+21);DEVOUT(PADC('CNPJ: 02.702.271/0001-14  - Insc.Mun.: 291.454-9',57))
ENDIF
RETURN .T.


