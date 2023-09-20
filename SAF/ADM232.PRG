*** ADM232.PRG: Alteracao de Propostas
***

FUNCTION adm232

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
        mensagem("Alteracao de Propostas 様 Digite o Codigo do Cliente.    [ESC] Abandona.")
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
        mensagem(" 様 Preencha os dados corretamente.  [ESC] Abandona.")
        mdata     := prop-> data
        mvalor    := prop-> valor
        mvalidade := prop-> validade
        mfecha    := prop-> fecha
        @ lci+3,cci+21 GET mvalor pict "9,999,999.99"
        @ lci+4,cci+21 GET mdata valid mdata<>ctod("  /  /  ")
        @ lci+5,cci+21 GET mvalidade valid mvalidade<>ctod("  /  /  ")
        @ lci+6,cci+21 GET mfecha PICTURE "!" VALID (mfecha $ "SN ")
        READ
        IF LASTKEY()=27
                LOOP
        ENDIF
        mensagem(" 様 Confirma os dados acima digitados ? [S/n]:")
        s=INKEY(0)
        IF s=asc("n") .OR. s=asc("N")
                LOOP
        ENDIF
        mensagem(" 様 Salvando Informacoes, aguarde ....")
        BLOQREG()
        prop-> data     :=  mdata
        prop-> numprop  :=  mnumprop
        prop-> valor    :=  mvalor
        prop-> validade :=  mvalidade
        prop-> fecha    :=  mfecha
        DBCOMMIT()
        DBUNLOCK()
ENDDO

