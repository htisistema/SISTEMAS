**** ADM5.PRG: Saida do sistema

FUNCTION adm5

SET COLOR TO +GR/R
BOTAO(6, 71, 9, 77)
@ 6, 72 SAY "�"+replicate('�',3)+"�"
do while .t.
        @ 7, 73 prompt "Sim"
        @ 8, 73 prompt "N�o"
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
                        ? "��������������������������������������������Ŀ "
                        ? "� ADMplus v2.0 (Adm. de Vendas)  Serie: 001-1��"
                        ? "��������������������������������������������Ĵ�"
                        ? "�                                            ��"
                        ? "� Insercom Servicos e Com.Informatica Ltda.  ��"
                        ? "�                                            ��"
                        ? "�             (081) 361-4400                 ��"
                        ? "�                                            ��"
                        ? "��������������������������������������������Ĵ�"
                        ? "� C�pia *exclusiva* da JERMED Com.Rep.Ltda.  ��"
                        ? "�����������������������������������������������"
                        ? " ����������������������������������������������"
                        ?
                        ? "��������������������������������������������Ŀ"
                        ? "� A Tentativa de C�pia deste Sistema poder�  ��"
                        ? "� acarretar problemas irreversiveis.         ��"
                        ? "�����������������������������������������������"
                        ? " ����������������������������������������������"
                        ?
                        ?
                        quit
                case adm5=2
                        return
        endcase
enddo



