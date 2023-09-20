*** ADM233.PRG: EXCLUSAO/CONSULTA de Propostas
***

FUNCTION adm233(mtip)

LOCAL mcodigo,mnumprop,mvalor,mdata,mvalidade,mfecha
LOCAL tela := SAVESCREEN(00,00,24,79)

IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('proposta','prop');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
WHILE .T.
        RESTSCREEN(00,00,24,79,tela)
        **************************
        SELE('cli');ORDSETFOCUS(1)
        GO TOP
        **************************
        mcodigo=0
        mnumprop=space(10)
        mvalor=0.00
        mdata=date()
        mvalidade=date()+30
        mfecha=" "
        IF mtip = 'E'
                mensagem("Exclusao de Propostas 様 Digite o Codigo do Cliente.    [ESC] Abandona.")
        ELSE
                mensagem("Consulta de Propostas 様 Digite o Codigo do Cliente.    [ESC] Abandona.")
        ENDIF
        @ lci+1,cci+21 GET mcodigo PICTURE "99999"
        READ
        IF LASTKEY()=27 .OR. mcodigo=0
                RETURN
        ENDIF
        SEEK STRZERO(mcodigo,5)
        IF ! FOUND()
                atencao(" 様 Cliente nao cadastrado!        [ENTER] Continua.")
                LOOP
        ENDIF
        **************************
        SELE('prop');ORDSETFOCUS(1)
        GO TOP
        **************************
        setcor(3)
        @ lci+1,cci+27 SAY cli->razao
        setcor(1)
        @ lci+2,cci+21 GET mnumprop
        READ
        IF LASTKEY()=27 .OR. mnumprop=space(10)
                LOOP
        ENDIF
        SEEK STRZERO(mcodigo,5)+mnumprop
        IF ! FOUND()
                atencao(" 様 Esta proposta NAO foi cadastrada para este cliente! [ENTER]")
                LOOP
        ENDIF
        setcor(3)
        @ lci+3,cci+21 SAY prop->valor pict "9,999,999.99"
        @ lci+4,cci+21 SAY prop->data
        @ lci+5,cci+21 SAY prop->validade
        @ lci+6,cci+21 SAY prop->fecha
        setcor(1)
        IF mtip = 'E'
                mensagem(" 様 Confirma a EXCLUSAO da proposta ? [S/n]:")
                s=INKEY(0)
                IF s=asc("n") .OR. s=asc("N")
                        LOOP
                ENDIF
                BLOQREG()
                DELE
                DBCOMMIT()
                DBUNLOCK()
        ELSE
                mensagem(" 様 Pressione qualquer tecla p/continuar ...")
                INKEY(0)
        ENDIF
ENDDO

