**** ADM5.PRG: Saida do sistema

FUNCTION adm5

SET COLOR TO +GR/R
BOTAO(6, 71, 9, 77)
@ 6, 72 SAY "Á"+replicate('Ä',3)+"Á"
do while .t.
        @ 7, 73 prompt "Sim"
        @ 8, 73 prompt "N„o"
        menu to adm5
        do case
                case adm5=0
                        return
                case adm5=1
                        set color to +w/n
                        set cursor on
                        *clock(0,2,69,31)
                        BOTAO(0,0,24,79)
                        set color to +w/n
                        clear
                        ? "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ "
                        ? "³ ADMplus v2.0 (Adm. de Vendas)  Serie: 001-1³Û"
                        ? "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´Û"
                        ? "³                                            ³Û"
                        ? "³ Insercom Servicos e Com.Informatica Ltda.  ³Û"
                        ? "³                                            ³Û"
                        ? "³             (081) 361-4400                 ³Û"
                        ? "³                                            ³Û"
                        ? "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´Û"
                        ? "³ C¢pia *exclusiva* da JERMED Com.Rep.Ltda.  ³Û"
                        ? "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ"
                        ? " ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß"
                        ?
                        ? "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
                        ? "³ A Tentativa de C¢pia deste Sistema poder   ³Û"
                        ? "³ acarretar problemas irreversiveis.         ³Û"
                        ? "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÛ"
                        ? " ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß"
                        ?
                        ?
                        quit
                case adm5=2
                        return
        endcase
enddo



