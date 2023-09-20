*** adm32.PRG: Ficha de visita
***
FUNCTION adm32

LOCAL codcli,dat_i,dat_f,regi,tela:=SAVESCREEN(00,00,24,79)

PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)  //variavel de impresao

IF ! AbriArq('saccfg','cfg',);RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('visita','vis');RETURN NIL;ENDIF
IF ! AbriArq('regiao','reg');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('visita','vis');RETURN NIL;ENDIF
WHILE .T.
********* VARIAVEIS DE IMPRESSAO *******************
        mtipo_imp := cfg->imp_rel
****************************************************
        **************************
        SELE('cli');ORDSETFOCUS(1)
        **************************
        codcli=0
        dat_i=ctod("  /  /  ")
        dat_f=ctod("  /  /  ")
        regi="  "
        BOTAO(10,9,14,75,,' Historico de Visitas ')
        @ 11, 11 SAY "Cliente.: "
        @ 12, 11 SAY "Regiao  : "
        @ 13, 11 SAY "Periodo : "+SPACE(12)+"a"

        @ 11, 21 GET codcli PICT '99999' VALID ver_cli(codcli,11,27)
        @ 12, 21 GET regi PICT "99" VALID ver_regi(regi,12,24,,0)
        @ 13, 21 GET dat_i VALID dat_i<>ctod("  /  /  ")
        @ 13, 36 GET dat_f VALID dat_f<>ctod("  /  /  ") .and. dat_f>dat_i
        READ
        IF LASTKEY()=27
                RESTSCREEN(00,00,24,79,tela)
                RETURN
        ENDIF
        IF ! imp_arq('HIST_VIS.REL','R')
                LOOP
        ENDIF
        pag=0
        **************************
        SELE('vis');ORDSETFOCUS(2)
        **************************
        WHILE data<=dat_f .and. eof()=.f.
                IF data<dat_i
                        SKIP
                        LOOP
                ENDIF
                IF pag=0 .or. prow()>59
                        pag=pag+1
                        IF pag>1
                                eject
                        ENDIF
                        DEVPOS(PROW(),00);DEVOUT("Admnistracao de Vendas             HISTORICO GERAL DE VISITAS           PAG: "+strzero(pag,3))
                        DEVPOS(PROW()+1,00);DEVOUT(substr(memp_resa,1,37))
                        DEVPOS(PROW(),00);DEVOUT(SPACE(1)+DTOC(dat_i)+" a "+DTOC(dat_f)+SPACE(15)+DTOC(date()))
                        DEVPOS(PROW()+1,00);DEVOUT(replicate("=",80))
                ENDIF
                **************************
                SELE('vis');ORDSETFOCUS(2)
                **************************
                IF ! vis->ok
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
                DEVPOS(PROW()+1,00);DEVOUT("CLIENTE: ("+codigo+") ")
                **************************
                SELE('cli');ORDSETFOCUS(1)
                **************************
                SEEK vis->codigo
                DEVPOS(PROW(),PCOL());DEVOUT(cli->razao)
                **************************
                SELE('vis');ORDSETFOCUS(2)
                **************************
                DEVPOS(PROW(),PCOL()+2);DEVOUT("     DATA: "+DTOC(data))
                DEVPOS(PROW()+2,00);DEVOUT("  ("+substr(flag,1,1)+") Proposta      ("+substr(flag,2,1)+") Informacoes      ("+substr(flag,3,1)+") Vis.Gerencia     ("+substr(flag,4,1)+") Nova Visita   ")
                DEVPOS(PROW()+1,00);DEVOUT("  ("+substr(flag,5,1)+") C.Telefonico  ("+substr(flag,6,1)+") ___________      ("+substr(flag,7,1)+") _____________    ("+substr(flag,8,1)+") ____________  ")
                DEVPOS(PROW()+2,00);DEVOUT(SPACE(10)+l1)
                DEVPOS(PROW()+1,00);DEVOUT(SPACE(10)+l2)
                DEVPOS(PROW()+1,00);DEVOUT(SPACE(10)+l3)
                DEVPOS(PROW()+1,00);DEVOUT(SPACE(10)+l4)
                DEVPOS(PROW()+1,00);DEVOUT(SPACE(10)+l5)
                DEVPOS(PROW()+1,00);DEVOUT(replicate(".",80))
                DEVPOS(PROW()+1,00);DEVOUT(' ')
                SKIP
        ENDDO
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(00,00,24,79,tela)
        IF mimp_tipo = 2
                lertexto('HIST_VIS.REL')
        ENDIF
ENDDO

