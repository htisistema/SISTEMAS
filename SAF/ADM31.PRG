*** ADM31.PRG: Ficha de visita
***
FUNCTION adm31

LOCAL tela:=SAVESCREEN(00,00,24,79)
PRIVATE codcli,codreg,datv,mcliente,mcpf,mcgc

PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)  //variavel de impresao

IF ! AbriArq('saccfg','cfg',);RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('regiao','reg');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('visita','vis');RETURN NIL;ENDIF
WHILE .T.
********* VARIAVEIS DE IMPRESSAO *******************
        mtipo_imp := cfg->imp_rel
****************************************************
        BOTAO(11,16,15,77,,' Ficha de Visita ')
        @ 12, 18 SAY "Cliente.:"
        @ 13, 18 SAY "Regiao..:"
        @ 14, 18 SAY "Data....:"
        **************************
        SELE('cli');ORDSETFOCUS(1)
        **************************
        codcli=0
        codreg=space(2)
        datv=ctod("  /  /  ")
        @ 12,27 GET codcli PICT '99999' VALID ver_cli(codcli,12,33)
        READ
        IF LASTKEY()=27
                RESTSCREEN(00,00,24,79,tela)
                RETURN
        ENDIF
        codreg := cli->regiao
        @ 13,27 GET codreg VALID ver_regi(codreg,13,33)
        @ 14,27 GET datv VALID datv<>ctod("  /  /  ")
        READ
        IF LASTKEY()=27
                LOOP
        ENDIF
        mensagem("Checando arquivo de visitas...")
        **************************
        SELE('vis');ORDSETFOCUS(2)
        **************************
        SEEK dtos(datv)
        IF ! FOUND()
                atencao("Nao ha visitas nesta data!")
                LOOP
        ENDIF
        IF ! imp_arq('FICHA_VIS.REL','R')
                LOOP
        ENDIF
        qt=0
        WHILE data=datv
                **************************
                SELE('cli');ORDSETFOCUS(1)
                **************************
                SEEK vis->codigo
                **************************
                SELE('reg');ORDSETFOCUS(1)
                **************************
                SEEK cli->regiao
                **************************
                SELE('sen');ORDSETFOCUS(1)
                **************************
                SEEK reg->codvend
                **************************
                SELE('vis');ORDSETFOCUS(2)
                **************************
                IF ! EMPTY(codcli)
                        IF STRZERO(codcli,5) <> vis->codigo
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                IF ! EMPTY(codreg)
                        IF cli->regiao<>codreg
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
/*
                IF ! EMPTY(codreg) .AND. ! EMPTY(codcli)
                        IF STRZERO(codcli,5) <> vis->codigo
                                SKIP
                        ENDIF
                ENDIF
                IF EMPTY(codreg) .AND. ! EMPTY(codcli)
                        IF STRZERO(codcli,5) <> vis->codigo
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                IF ! EMPTY(codreg) .AND. EMPTY(codcli)
                        IF cli->regiao<>codreg
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
*/
                IF ok
                        SKIP
                        LOOP
                ENDIF
                DEVPOS(PROW(),00);DEVOUT("--------------------------------------------------------------------------------")
                DEVPOS(PROW()+1,00);DEVOUT( " "+memp_resa+"             FICHA DE VISITA: "+dtoc(data))
                DEVPOS(PROW()+1,00);DEVOUT( "--------------------------------------------------------------------------------")
                DEVPOS(PROW()+1,00);DEVOUT( " Codigo Cliente: ("+cli->cod_cli+")                          Vendedor: ("+reg->codvend+") "+sen->snome)
                DEVPOS(PROW()+1,00);DEVOUT( "................................................................................")
                DEVPOS(PROW()+1,00);DEVOUT( "CLIENTE.: "+cli->razao+"    CIDADE: "+cli->cidade)
                DEVPOS(PROW()+1,00);DEVOUT( "ENDERECO: "+cli->endereco+"     ESTADO: "+cli->uf)
                DEVPOS(PROW()+1,00);DEVOUT( "TELEFONE: "+cli->tel1+" / "+cli->tel2+"    FAX: "+cli->fax)
                DEVPOS(PROW()+1,00);DEVOUT( "CONTATO.:                            OBSERVACAO:                                ")
                DEVPOS(PROW()+1,00);DEVOUT( "................................................................................")
                DEVPOS(PROW()+1,00);DEVOUT( "  ( ) Proposta      ( ) Informacoes      ( ) Vis.Gerencia     ( ) Nova Visita   ")
                DEVPOS(PROW()+1,00);DEVOUT( "                                                                  ___/___/___   ")
                DEVPOS(PROW()+1,00);DEVOUT( "  ( ) C.Telefonico  ( ) ___________      ( ) _____________    ( ) ____________  ")
                DEVPOS(PROW()+1,00);DEVOUT( "................................................................................")
                DEVPOS(PROW()+1,00);DEVOUT( " ** OBS: "+obs)
                DEVPOS(PROW()+1,00);DEVOUT( "................................................................................")
                DEVPOS(PROW()+2,00);DEVOUT( "                              *** COMENTARIOS ***                               ")
                DEVPOS(PROW()+2,00);DEVOUT( "          ------------------------------------------------------------          ")
                DEVPOS(PROW()+2,00);DEVOUT( "          ------------------------------------------------------------          ")
                DEVPOS(PROW()+2,00);DEVOUT( "          ------------------------------------------------------------          ")
                DEVPOS(PROW()+2,00);DEVOUT( "          ------------------------------------------------------------          ")
                DEVPOS(PROW()+2,00);DEVOUT( "          ------------------------------------------------------------          ")
                DEVPOS(PROW()+2,00);DEVOUT( "          ------------------------------------------------------------          ")
                SKIP
                qt=qt+1
                IF qt=2
                        EJECT
                        qt=0
                ELSE
                        DEVPOS(PROW()+1,00);DEVOUT( REPLI("=",80))
                        DEVPOS(PROW()+2,00);DEVOUT(' ')
                ENDIF
        ENDDO
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(00,00,24,79,tela)
        IF mimp_tipo = 2
                lertexto('FICHA_VIS.REL')
        ENDIF
        LOOP
ENDDO




function cli

IF codcli=0
        RETURN .T.
ENDIF
**************************
SELE('cli');ORDSETFOCUS(1)
**************************
SEEK STRZERO(codcli,5)
IF ! FOUND()
        atencao(" 様 Cliente nao cadastrado!")
        RETURN .f.
ENDIF
mensagem(" 様 "+cli->nome)
keyboard cli->regiao
RETURN .T.



function reg
IF codreg="  "
        RETURN .T.
ENDIF
**************************
SELE('reg');ORDSETFOCUS(1)
**************************
SEEK codreg
IF ! FOUND()
        atencao(" 様 Regiao nao cadastrada!")
        ?? chr(7)
        sele a
        RETURN .f.
ENDIF
mensagem(" 様 "+regiao)
**************************
SELE('cli');ORDSETFOCUS(1)
**************************
RETURN .T.

