*** ADM3.PRG: Menu de Relatorios
***
FUNCTION adm3


SET COLOR TO +W/R
BOTAO(6,31,12,52)
save scree to adm3
do while .t.
        rest scree from adm3
        @ 07, 34 PROMPT "Ficha de Visita"
        @ 08, 34 PROMPT "Hist. de visitas"
        @ 09, 33 PROMPT "Visitas em aberto"
        @ 10, 33 PROMPT "Propostas Emitidas"
        @ 11, 33 PROMPT "Lista de Clientes"
        menu to op
        if op=0
                return
        endif
        do case
                case op=1
                        adm31()
                case op=2
                        adm32()
                case op=3
                        adm33()
                case op=4
                        adm34()
                case op=5
                        adm35()
        endcase
        set color to +W/r
enddo

