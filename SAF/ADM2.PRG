*** ADM2.PRG: Menu de Lan‡amentos
**********************************

FUNCTION adm2

SET COLOR TO +W/R
BOTAO(06,17,10,32)
@ 07,19 SAY "Programa‡„o"
@ 08,22 SAY "Visita"
@ 09,21 SAY "Proposta"
* @ 10,19 SAY "Concorrˆncia"
save scree to adm2
do while .t.
        rest scree from adm2
        @ 07,19 PROMPT "Programa‡„o"
        @ 08,22 PROMPT "Visita"
        @ 09,21 PROMPT "Proposta"
* @ 10,19 PROMPT "Concorrˆncia"
        menu to op2
        if op2=0
                return
        endif
        do case
                case op2=1
                        adm21()
                case op2=2
                        adm22()
                case op2=3
                        adm23()
*   case op2=4
*   do adm24
*   case op2=5
*   do adm25
        endcase
        set color to +w/r
enddo

