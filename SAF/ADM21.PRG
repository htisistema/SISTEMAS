*** ADM21.PRG: Menu de Programa‡„o de Visitas
***********************************************

FUNCTION adm21

LOCAL tela:= SAVESCREEN(00,00,24,79)

PRIVATE lci,cci,lba,cba

lci := 09
cci := 03
lba := 16
cba := 77
WHILE .t.
        BOTAO(lci,cci,lba,cba,,' Programacao de Visitas ')
        @ lci+1,cci+1 TO lci+1,cba-1
        @ lci+2,cci+1 SAY 'Cliente....:'
        @ lci+3,cci+1 SAY 'Regiao.....:'
        @ lci+4,cci+1 SAY 'Vendedor...:'
        @ lci+5,cci+1 SAY 'Data visita:'
        @ lci+6,cci+1 SAY 'OBS........:'
        mensagem("[I]nclus„o   [A]lteracao  [E]xclus„o   [ESC] Retorna")
        s=INKEY(0)
        do CASE
                CASE s=asc("I") .OR. s=asc("i")
                        adm211()
                CASE s=asc("A") .OR. s=asc("a")
                        adm212()
                CASE s=asc("E") .OR. s=asc("e")
                        adm213()
                CASE s=27
                        RESTSCREEN(00,00,24,79,tela)
                        RETURN NIL
        endCASE
ENDDO

