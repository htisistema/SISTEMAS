/****************************************************************************
 *                                                                          *
 * Funcao     =>                                                            *
 * Exemplo    =>                                                            *
 ****************************************************************************/

 Clear

Esc    = Chr(27)
Null   = ""
Print  = "E"
Altura = 2

@ 01,01 SAY "Gerador de Codigo de Barras Code39"
@ 03,01 SAY "Impressora (E)pson - (L)aserJet?:" GET Print  PICT "@!" Valid(Print $ "EL")
@ 04,01 SAY "Altura     (1-4)]               :" GET Altura PICT "9" Range 1,4
READ

IF Print = "L"
        LaserJet()
ENDIF

IF Print = "E"
        Epson()
ENDIF

Def_Code39()

Stay = .T.
WHILE Stay
        Ejetar  = Space(01)
        Message = Space(25)

        @ 07,01 SAY "Digite o Codigo a Ser Impresso  :" GET Message PICT "@K!"
        READ

        IF EMPTY(Message)
                Stay = .F.
                LOOP
        ENDIF

        Message = "*" + TRIM(Message) + "*"

        SET DEVICE TO Print

        @ PROW() + Altura,00 SAY Barcode(Message)

        @ PROW() + IF(Print="L",Altura,0),Int(Len(Message)/4) SAY Message
        SET DEVICE TO Screen

        Eject = .F.
        @ 08,01 SAY "Ejetar?                         :" GET Ejetar PICT "@!" Valid(Ejetar $ "SN")
        READ

        IF Ejetar = "S"
                Eject
        ENDIF

ENDDO
RETURN


FUNCTION BarCode()
Parameters Message
Code = ""
DO CASE
        CASE Print = "L"
                FOR i = 1 TO Len(Message)
                        Letter = SubStr(Message,i,1)
                        Code = Code + IF(At(Letter,Chars)=0,Letter,Char[At(Letter,Chars)]) + NS
                NEXT
                Code = Start + Code + End

        CASE Print = "E"
                FOR h = 1 TO Altura
                        FOR i = 1 TO Len(Message)
                                Letter = SubStr(Message,i,1)
                                Code = IF(At(Letter,Chars)=0,Letter,Char[At(Letter,Chars)]) + NS
                                PrintCode(Esc + Chr(76) + Chr(N1) + Chr(N2) + Code)
                        NEXT
                        PrintCode(Esc + Chr(74) + Chr(23) + Chr(13))
                NEXT
                PrintCode(Esc + Chr(74) + Chr(5) + Chr(13))
                PrintCode(Esc + "@")

ENDCASE
RETURN Code


PROCEDURE Def_Code39()
PUBLIC Char[44], Chars

Chars = "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ-. *$/+%"
Char[01] = WB+NS+NB+WS+NB+NS+NB+NS+WB       && "1"
Char[02] = NB+NS+WB+WS+NB+NS+NB+NS+WB       && "2"
Char[03] = WB+NS+WB+WS+NB+NS+NB+NS+NB       && "3"
Char[04] = NB+NS+NB+WS+WB+NS+NB+NS+WB       && "4"
Char[05] = WB+NS+NB+WS+WB+NS+NB+NS+NB       && "5"
Char[06] = NB+NS+WB+WS+WB+NS+NB+NS+NB       && "6"
Char[07] = NB+NS+NB+WS+NB+NS+WB+NS+WB       && "7"
Char[08] = WB+NS+NB+WS+NB+NS+WB+NS+NB       && "8"
Char[09] = NB+NS+WB+WS+NB+NS+WB+NS+NB       && "9"
Char[10] = NB+NS+NB+WS+WB+NS+WB+NS+NB       && "0"
Char[11] = WB+NS+NB+NS+NB+WS+NB+NS+WB       && "A"
Char[12] = NB+NS+WB+NS+NB+WS+NB+NS+WB       && "B"
Char[13] = WB+NS+WB+NS+NB+WS+NB+NS+NB       && "C"
Char[14] = NB+NS+NB+NS+WB+WS+NB+NS+WB       && "D"
Char[15] = WB+NS+NB+NS+WB+WS+NB+NS+NB       && "E"
Char[16] = NB+NS+WB+NS+WB+WS+NB+NS+NB       && "F"
Char[17] = NB+NS+NB+NS+NB+WS+WB+NS+WB       && "G"
Char[18] = WB+NS+NB+NS+NB+WS+WB+NS+NB       && "H"
Char[19] = NB+NS+WB+NS+NB+WS+WB+NS+NB       && "I"
Char[20] = NB+NS+NB+NS+WB+WS+WB+NS+NB       && "J"
Char[21] = WB+NS+NB+NS+NB+NS+NB+WS+WB       && "K"
Char[22] = NB+NS+WB+NS+NB+NS+NB+WS+WB       && "L"
Char[23] = WB+NS+WB+NS+NB+NS+NB+WS+NB       && "M"
Char[24] = NB+NS+NB+NS+WB+NS+NB+WS+WB       && "N"
Char[25] = WB+NS+NB+NS+WB+NS+NB+WS+NB       && "O"
Char[26] = NB+NS+WB+NS+WB+NS+NB+WS+NB       && "P"
Char[27] = NB+NS+NB+NS+NB+NS+WB+WS+WB       && "Q"
Char[28] = WB+NS+NB+NS+NB+NS+WB+WS+NB       && "R"
Char[29] = NB+NS+WB+NS+NB+NS+WB+WS+NB       && "S"
Char[30] = NB+NS+NB+NS+WB+NS+WB+WS+NB       && "T"
Char[31] = WB+WS+NB+NS+NB+NS+NB+NS+WB       && "U"
Char[32] = NB+WS+WB+NS+NB+NS+NB+NS+WB       && "V"
Char[33] = WB+WS+WB+NS+NB+NS+NB+NS+NB       && "W"
Char[34] = NB+WS+NB+NS+WB+NS+NB+NS+WB       && "X"
Char[35] = WB+WS+NB+NS+WB+NS+NB+NS+NB       && "Y"
Char[36] = NB+WS+WB+NS+WB+NS+NB+NS+NB       && "Z"
Char[37] = NB+WS+NB+NS+NB+NS+WB+NS+WB       && "-"
Char[38] = WB+WS+NB+NS+NB+NS+WB+NS+NB       && "."
Char[39] = NB+WS+WB+NS+NB+NS+WB+NS+NB       && " "
Char[40] = NB+WS+NB+NS+WB+NS+WB+NS+NB       && "*"
Char[41] = NB+WS+NB+WS+NB+WS+NB+NS+NB       && "$"
Char[42] = NB+WS+NB+WS+NB+NS+NB+WS+NB       && "/"
Char[43] = NB+WS+NB+NS+NB+WS+NB+WS+NB       && "+"
Char[44] = NB+NS+NB+WS+NB+WS+NB+WS+NB       && "%"
RETURN

FUNCTION Printcode()
Parameters Code
SET CONSOLE OFF
SET PRINT ON
?? Code
SET PRINT OFF
SET CONSOLE ON
RETURN null

PROCEDURE LaserJet()
PUBLIC nb,wb,ns,ws,start,end
small_bar = 3
wide_bar = round(small_bar * 2.25,0)
dpl = 50

nb = esc+"*c"+transFORm(small_bar,'99')+"a"+alltrim(str(Altura*dpl))+"b0P"+esc+"*p+"+transFORm(small_bar,'99')+"X"
wb = esc+"*c"+transFORm(wide_bar,'99')+"a"+alltrim(str(Altura*dpl))+"b0P"+esc+"*p+"+transFORm(wide_bar,'99')+"X"
ns = esc+"*p+"+transFORm(small_bar,'99')+"X"
ws = esc+"*p+"+transFORm(wide_bar,'99')+"X"

start = esc+"*p-50Y"
end = esc+"*p+50Y"
RETURN

PROCEDURE Epson()
PUBLIC nb,wb,ns,ws,n1,n2
ns = chr(0) + chr(0)
ws = chr(0) + chr(0) + chr(0) + chr(0)
nb = chr(255)
wb = chr(255) + chr(255) + chr(255)

printcode(esc+chr(51)+chr(2))

cols = 21
N1 = cols % 256
N2 = INT(cols/256)
RETURN
