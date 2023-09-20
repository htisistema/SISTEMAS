*** ADM41.PRG: Spool
***

FUNCTION adm41


m="0"
call queue with m
if asc(m)<>255
        quadro("Spool nao instalado!")
        ?? chr(7)
        s=inkey(4)
        return
endif
SET COLOR TO +GR/R
BOTAO(10, 16, 16, 61)
@ 10, 31 say "Â"
@ 15, 31 say "Á"
*@ 07, 18 say "GERAL.REL    ³     Geral dos Produtos"
*@ 08, 18 say "MINIMO.REL   ³"+space(6)+"Abaixo do Minimo"
*@ 09, 18 say "MAXIMO.REL   ³"+space(7)+"Estoque Maximo"
*@ 10, 18 say "MOVIME.REL   ³"+space(7)+"Movimenta‡„o"
@ 11, 18 say "VISITAS.REL  ³ Ficha de Visita ao Cliente"
@ 12, 18 say "HISTORI.REL  ³   Historico das Visitas  "
@ 13, 18 say "VIS_ABER.REL ³     Visitas em Aberto"
@ 14, 18 say "PROP_EMI.REL ³    Propostas Emitidas"
@ 15, 18 say "CLIENTE.REL  ³   Relatorio de Clientes"
*@ 16, 18 say "SEM_MOV.REL  ³  Mercadorias sem Movimento"
*@ 17, 18 say "MOV_MES.REL  ³   Movimentacao Mercadoria"
*@ 18, 18 say "SAL_CONS.REL ³    Saldo de Consignacoes"
SET COLOR TO +W/B
BOTAO(17, 16, 19, 61)
@ 18, 18 say "["+chr(17)+"½] Imprime Arquivo"+space(8)+"[ESC] Abandona"
do while .t.
        *@ 07, 17 prompt " GERAL.REL    "
        *@ 08, 17 prompt " MINIMO.REL   "
        *@ 09, 17 prompt " MAXIMO.REL   "
        *@ 10, 17 prompt " MOVIME.REL   "
        @ 11, 17 prompt " VISITAS.REL  "
        @ 12, 17 prompt " HISTORI.REL  "
        @ 13, 17 prompt " VIS_ABER.REL "
        @ 14, 17 prompt " PROP_EMI.REL "
        @ 15, 17 prompt " CLIENTE.REL "
        *@ 16, 17 prompt " SEM_MOV.REL  "
        *@ 17, 17 prompt " MOV_MES.REL  "
        *@ 18, 17 prompt " SAL_CONS.REL "
        menu to op
        if op=0
                return
        endif
        if op=1
                if file("visitas.rel")
                        m="1 visitas.rel"
                        call queue with m
                        ?? chr(7)
                endif
        endif
        if op=2
                if file("histori.rel")
                        m="1 histori.rel"
                        call queue with m
                        ?? chr(7)
                endif
        endif
        if op=3
                if file("vis_aber.rel")
                        m="1 vis_aber.rel"
                        call queue with m
                        ?? chr(7)
                endif
        endif
        if op=4
                if file("prop_emi.rel")
                        m="1 prop_emi.rel"
                        call queue with m
                        ?? chr(7)
                endif
        endif
        if op=5
                if file("CLIENTE.REL")
                        m="1 CLIENTE.REL"
                        call queue with m
                        ?? chr(7)
                endif
        endif
        *if op=6
*   if file("consigna.rel")
*   m="1 consigna.rel"
*   call queue with m
        *?? chr(7)
*   endif
        *endif
*
        *if op=7
*   if file("precos.rel")
*   m="1 precos.rel"
*   call queue with m
        *?? chr(7)
*   endif
        *endif
        *if op=8
*   if file("comissao.rel")
*   m="1 comissao.rel"
*   call queue with m
        *?? chr(7)
*   endif
        *endif
        *if op=9
*   if file("media_12.rel")
*   m="1 media_12.rel"
*   call queue with m
*   ?? chr(7)
*   endif
        *endif
        *if op=10
*   if file("sem_mov.rel")
*   m="1 sem_mov.rel"
*   call queue with m
*   ?? chr(7)
*   endif
        *endif
        *if op=11
*   if file("mov_mes.rel")
*   m="1 mov_mes.rel"
*   call queue with m
*   ?? chr(7)
*   endif
        *endif
        *if op=12
*   if file("sal_cons.rel")
*   m="1 sal_cons.rel"
*   call queue with m
*   ?? chr(7)
*   endif
        *endif
enddo
