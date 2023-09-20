*** ADM1.PRG: Menu de Cadastro
********************************

FUNCTION adm1

SET COLOR TO +W/R
BOTAO(6, 1, 11, 14)
save scree to adm1
do while .t.
        rest scree from adm1
        @ 07,4 prompt "Clientes"
        @ 08,3 prompt "Vendedores"
        @ 09,3 prompt " Vend/Reg "
        @ 10,3 prompt "Atividades"
        menu to op1
        if op1=0
                return
        endif
        do case
                case op1=1
                        adm11()
                case op1=2
                        adm12()
                case op1=3
                        adm13()
                case op1=4
                        adm14()
        endcase
        set color to +W/r
enddo

