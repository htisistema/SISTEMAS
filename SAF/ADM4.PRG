*** ADM4.PRG: Menu de Utilitarios
***
FUNCTION adm4

SET COLOR TO +W/R
BOTAO(6,53,10,65)
save scree to adm4
do while .t.
        rest scree from adm4
        set color to +w/r
        @ 7, 55 PROMPT "Impressao"
        @ 8, 55 PROMPT "Reindexar"
        @ 9, 55 PROMPT "Bkp/Rest."
        menu to op
        if op=0
                return
        endif
        do case
                case op=1
                        adm41()
                case op=2
                        adm42()
                case op=3
                        bkp()
        endcase
enddo




function bkp

if file("bkp.exe")
        quadro("Aguarde, executando modulo de BACKUP/RESTORE")
        ret=clipxswap("/command.com","/c BKP","c:",.t.,0,1)
else
        quadro("Modulo de BACKUP/RESTORE inexistente!")
        ?? chr(7)
        s=inkey(5)
endif
return .t.

