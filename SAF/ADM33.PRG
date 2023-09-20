*** adm33.PRG: Visitas em Aberto
***

FUNCTION adm33

LOCAL codcli,dat_i,dat_f,regi,tela:=SAVESCREEN(00,00,24,79)

PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)  //variavel de impresao

IF ! AbriArq('saccfg','cfg',);RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('visita','vis');RETURN NIL;ENDIF
IF ! AbriArq('regiao','reg');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
WHILE .t.
********* VARIAVEIS DE IMPRESSAO *******************
        mtipo_imp := cfg->imp_rel
****************************************************
        **************************
        SELE('cli');ORDSETFOCUS(1)
        **************************
        codcli=0
        dat_i=CTOD("  /  /  ")
        dat_f=CTOD("  /  /  ")
        regi="  "
        BOTAO(10,9,14,75,,' Historico de Visitas ')
        @ 11, 11 SAY "Cliente.: "
        @ 12, 11 SAY "Regiao  : "
        @ 13, 11 SAY "Periodo : "+SPACE(12)+"a"

        @ 11, 21 GET codcli PICT '99999' VALID ver_cli(codcli,11,27)
        @ 12, 21 GET regi PICT "99" VALID ver_regi(regi,12,24,,0)
        @ 13, 21 GET dat_i VALID dat_i<>ctod("  /  /  ")
        @ 13, 36 GET dat_f VALID dat_f<>ctod("  /  /  ") .AND. dat_f>dat_i
        READ
        IF LASTKEY()=27
                RESTSCREEN(00,00,24,79,tela)
                RETURN
        ENDIF
        IF ! imp_arq('VIS_ABER.REL','R')
                LOOP
        ENDIF
        pag=0
        **************************
        SELE('vis');ORDSETFOCUS(2)
        GO TOP
        **************************
        vis->(DBSEEK(DTOS(dat_i),.T.))
        IF EOF()
                atencao('Nao existe movimento neste periodo !!!')
                LOOP
        ENDIF
        WHILE vis->data<=dat_f .AND. ! EOF()
                IF pag=0 .OR. prow()>61
                        pag=pag+1
                        IF pag>1
                                EJECT
                        ENDIF
                        DEVPOS(PROW(),PCOL());DEVOUT( "Admnistracao de Vendas           RELATORIO DE VISITAS EM ABERTO         PAG: "+strzero(pag,3))
                        DEVPOS(PROW()+1,00);DEVOUT( SUBSTR(memp_resa,1,37))
                        DEVPOS(PROW(),PCOL());DEVOUT( SPACE(1)+DTOC(dat_i)+" a "+DTOC(dat_f)+SPACE(15)+DTOC(date()))
                        DEVPOS(PROW()+1,00);DEVOUT( REPLI("=",80))
                        DEVPOS(PROW()+1,00);DEVOUT( " Cod.          Razao Social do Cliente             Data          Vendedor")
                        DEVPOS(PROW()+1,00);DEVOUT( REPLI("-",80))
                ENDIF
                **************************
                SELE('vis');ORDSETFOCUS(2)
                **************************
                IF vis->ok
                        SKIP
                        LOOP
                ENDIF
                IF ! EMPTY(codcli)
                        IF vis->codigo<>STRZERO(codcli,5)
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                IF ! EMPTY(regi)
                        **************************
                        SELE('cli');ORDSETFOCUS(1)
                        **************************
                        SEEK vis->codigo
                        **************************
                        SELE('vis');ORDSETFOCUS(2)
                        **************************
                        IF cli->regiao<>regi
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT( " "+codigo+"  ")
                **************************
                SELE('cli');ORDSETFOCUS(1)
                **************************
                seek vis->codigo
                DEVPOS(PROW(),PCOL());DEVOUT(razao)
                **************************
                SELE('vis');ORDSETFOCUS(2)
                **************************
                DEVPOS(PROW(),PCOL());DEVOUT("  "+DTOC(data))
                **************************
                SELE('reg');ORDSETFOCUS(1)
                **************************
                seek cli->regiao
                **************************
                SELE('sen');ORDSETFOCUS(1)
                **************************
                seek reg->codvend
                DEVPOS(PROW(),PCOL()+2);DEVOUT("("+reg->codvend+") "+sen->snome)
                **************************
                SELE('vis');ORDSETFOCUS(2)
                **************************
                SKIP
        ENDDO
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(00,00,24,79,tela)
        IF mimp_tipo = 2
                lertexto('VIS_ABER.REL')
        ENDIF
ENDDO

