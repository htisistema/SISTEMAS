**** ADM35.PRG: Relatorio de Clientes
***
**
*

FUNCTION adm35
**************
LOCAL sss,box_reg

PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)  //variavel de impresao

IF ! AbriArq('saccfg','cfg',);RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
save scree TO adm35
quadro("Modo: [1] Ordem de Codigo - [2] Ordem Alfabetica")
reg="  "
********* VARIAVEIS DE IMPRESSAO *******************
mtipo_imp := cfg->imp_rel
****************************************************
WHILE .T.
        sss=inkey(0)
        IF sss=27
                RETURN
        ENDIF
        IF sss=asc("1")
                SELE('cli');ORDSETFOCUS(1)
                exit
        ENDIF
        IF sss=asc("2")
                SELE('cli');ORDSETFOCUS(2)
                exit
        ENDIF
ENDDO
rest scree from adm35
box_reg="  "
BOTAO(13,33,15,46)
@ 14, 35 SAY "Regiao:"
@ 14, 43 GET box_reg
READ
IF lastkey()=27
        close all
        RETURN
ENDIF
IF box_reg="  "
        reg="TODAS"
else
        reg=box_reg+SPACE(3)
ENDIF
rest scree from adm35
pag=0
qt=0
IF ! imp_arq('CLIENTE.REL','R')
        RETURN NIL
ENDIF
WHILE ! EOF()
        IF pag=0 .OR. PROW()>58
                pag=pag+1
                IF pag>1
                        EJECT
                ENDIF
                DEVPOS(PROW(),PCOL());DEVOUT( "ADM - Administracao de Vendas"+SPACE(21)+"  RELATORIO GERAL DE CLIENTES  "+SPACE(43)+"PAG: "+strzero(pag,3))
                DEVPOS(PROW()+1,00);DEVOUT( substr(memp_resa,1,40))
                DEVPOS(PROW(),PCOL());DEVOUT( SPACE(20)+"REGIAO: "+reg+SPACE(51)+dtoc(date()))
                DEVPOS(PROW()+1,00);DEVOUT( REPLI("=",132))
                DEVPOS(PROW()+1,00);DEVOUT( "Cod.   ------------ Razao Social --------------   --------------- Endereco ---------------   ------ Cidade -----   UF   C.E.P")
                DEVPOS(PROW()+1,00);DEVOUT( "--------- Telefones ---------   ---- FAX ----   --     -   ----- C.G.C ------   ---- Inscricao ---   -- Pessoa Para Contato -")
                DEVPOS(PROW()+1,00);DEVOUT( REPLI("=",132))
        ENDIF

        IF box_reg<>"  "
                IF box_reg<>regiao
                        SKIP
                        LOOP
                ENDIF
        ENDIF
        DEVPOS(PROW()+1,00);DEVOUT( cod_cli+SPACE(3)+razao+SPACE(3)+endereco+SPACE(3)+cidade+"   "+uf+"   "+cep)
        DEVPOS(PROW()+1,00);DEVOUT( tel1+"   "+tel2+"   "+fax+"               "+cgc+"   "+insc+"   "+contato)
        DEVPOS(PROW()+1,00);DEVOUT( REPLI("-",132))
        qt=qt+1
        SKIP
ENDDO
tt=PROW()
FOR f=1 TO 61-tt
        DEVPOS(PROW()+1,00);DEVOUT(' ')
next f
DEVPOS(PROW(),PCOL());DEVOUT( "Total de Clientes: "+ltrim(transform(qt,"999999999")))
SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
rest scree from adm35
*RESTSCREEN(00,00,24,79,tela)
IF mimp_tipo = 2
        lertexto('CLIENTE.REL')
ENDIF
rest scree from adm35
RETURN

