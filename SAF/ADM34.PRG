*** adm34.PRG: Propostas Emitidas
***

FUNCTION adm34
LOCAL mprg:='ADM34'
LOCAL codcli,dat_i,dat_f,regi,tela:=SAVESCREEN(00,00,24,79)
PRIVATE mtipo_imp,mimp_tipo:=0,marq:=SPACE(35)  //variavel de impresao

IF ! AbriArq('saccfg','cfg',);RETURN NIL;ENDIF
IF ! AbriArq('sacsetup','set');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('proposta','prop');RETURN NIL;ENDIF
IF ! AbriArq('regiao','reg');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
WHILE .T.
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
        BOTAO(10,9,14,75,,' Proposta Emitidas ')
        @ 11, 11 SAY "Cliente.: "
        @ 12, 11 SAY "Regiao  : "
        @ 13, 11 SAY "Periodo : "+SPACE(12)+"a"

        @ 11, 21 GET codcli PICT '99999' VALID ver_cli(codcli,11,27)
        @ 12, 21 GET regi PICT "99" VALID ver_regi(regi,12,24,,0)
        @ 13, 21 GET dat_i VALID dat_i<>CTOD("  /  /  ")
        @ 13, 36 GET dat_f VALID dat_f<>CTOD("  /  /  ") .AND. dat_f>dat_i
        READ
        IF LASTKEY()=27
                RESTSCREEN(00,00,24,79,tela)
                RETURN
        ENDIF
        mensagem('Propostas ออ [T]odas, [F]echadas, [P]erdidas ou em [A]berto:')
        tudo=.F.
        cond=""
        WHILE .T.
                s=INKEY(0)
                IF s=ASC("T") .OR. s=ASC("t")
                        tudo=.T.
                        EXIT
                ENDIF
                IF s=ASC("F") .OR. s=ASC("f")
                        cond="S"
                        EXIT
                ENDIF

                IF s=ASC("P") .OR. s=ASC("p")
                        cond="N"
                        EXIT
                ENDIF
                IF s=ASC("A") .OR. s=ASC("a")
                        cond=" "
                        EXIT
                ENDIF
                IF s=27
                        EXIT
                ENDIF
        ENDDO
        IF s = 27
                LOOP
        ENDIF
        IF ! imp_arq('PROP_EMI.REL','R')
                LOOP
        ENDIF
        pag=0
        **************************
        SELE('prop');ORDSETFOCUS(2)
        **************************
        tot=0
        totq=0
        mtit := 'Relatorio de Propostas Emitidas'
        mtipo := 'Periodo: '+DTOC(dat_i)+" a "+DTOC(dat_f)
        WHILE data<=dat_f .AND. EOF()=.F.
                IF data<dat_i
                        SKIP
                        LOOP
                ENDIF
                IF pag=0 .OR. prow()>59
                        pag=pag+1
                        IF pag>1
                                eject
                        ENDIF
                        cabecalho(pag,mtit,mtipo,mprg,1)
*                       DEVPOS(PROW(),00);DEVOUT(memp_resa)
*                       DEVPOS(PROW()+1,00);DEVOUT( "Admnistracao de Vendas           RELATORIO DE PROPOSTAS EMITIDAS        PAG: "+strzero(pag,3))
*                       DEVPOS(PROW()+1,00);DEVOUT('Periodo: '+DTOC(dat_i)+" a "+DTOC(dat_f)+SPACE(15)+'Emissao: '+DTOC(date()))
*                       DEVPOS(PROW()+1,00);DEVOUT( replicate("=",80))
                        DEVPOS(PROW()+1,00);DEVOUT( "   Razao Social do Cliente      No.Prop.  Emissao  Validade     V A L O R")
                        *DEVPOS(PROW()+1,00);DEVOUT( "------------------------------ ---------- -------- -------- -------------------")
                        DEVPOS(PROW()+1,00);DEVOUT( replicate("-",80))
                ENDIF
                **************************
                SELE('prop');ORDSETFOCUS(2)
                **************************
                IF ! EMPTY(codcli)
                        IF codigo<>STRZERO(codcli,5)
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                IF regi<>"  "
                        **************************
                        SELE('cli');ORDSETFOCUS(1)
                        **************************
                        SEEK prop->codigo
                        **************************
                        SELE('prop');ORDSETFOCUS(2)
                        **************************
                        IF cli->regiao<>regi
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                IF tudo=.F.
                        IF fecha<>cond
                                SKIP
                                LOOP
                        ENDIF
                ENDIF
                **************************
                SELE('cli');ORDSETFOCUS(1)
                **************************
                SEEK prop->codigo
                DEVPOS(PROW()+1,00);DEVOUT( SUBSTR(razao,1,30))
                **************************
                SELE('prop');ORDSETFOCUS(2)
                **************************
                DEVPOS(PROW(),PCOL());DEVOUT( " "+numprop)
                DEVPOS(PROW(),PCOL());DEVOUT( " "+DTOC(data))
                DEVPOS(PROW(),PCOL());DEVOUT( " "+DTOC(validade))
                DEVPOS(PROW(),PCOL());DEVOUT( " "+transform(valor,"@E 999,999,999.99"))
                tot=tot+valor
                totq=totq+1
                SKIP
        ENDDO
        DEVPOS(PROW()+2,00);DEVOUT( "Total de Propostas: ("+ltrim(str(totq))+") Cr$ "+ltrim(transform(tot,"@E 999,999,999.99")))
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(00,00,24,79,tela)
        IF mimp_tipo = 2
                lertexto('PROP_EMI.REL')
        ENDIF
ENDDO

