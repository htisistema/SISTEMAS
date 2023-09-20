*** ADM23.PRG: Propostas
***

FUNCTION adm23

LOCAL tela:=SAVESCREEN(00,00,24,79)
PRIVATE lci,cci,lba,cba,s

lci := 10
cci := 04
lba := 17
cba := 74
BOTAO(lci,cci,lba,cba,,' Lancamento de Propostas ')
@ lci+1,cci+1 SAY "Cliente..........:"
@ lci+2,cci+1 SAY "Num. Proposta....:"
@ lci+3,cci+1 SAY "Vlr. Proposta....:"
@ lci+4,cci+1 SAY "Emissao..........:"
@ lci+5,cci+1 SAY "Validade Proposta:"
@ lci+6,cci+1 SAY "Fechado [S/N]....:"
WHILE .t.
        mensagem("[I]nclusao  [A]lteracao  [E]xclusao  [C]onsulta  [ESC] Sai")
        s=INKEY(0)
        IF s=27
                RESTSCREEN(00,00,24,79,tela)
                RETURN
        ENDIF
        DO CASE
                CASE s=ASC("i") .OR. s=ASC("I")
                        adm231()
                CASE s=ASC("a") .OR. s=ASC("A")
                        adm232()
                CASE s=ASC("e") .OR. s=ASC("E")
                        adm233('E')
                CASE s=ASC("c") .OR. s=ASC("C")
                        adm233('C')
        endCASE
enddo

