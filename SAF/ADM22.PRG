*** ADM22.PRG: Digitacao de Visitas
***************************************

FUNCTION adm22

LOCAL lci,cci,lba,cba,tela:=SAVESCREEN(00,00,24,79)

LOCAL mobs,mdat,mtel

LOCAL mcodigo,mdata,mf1,mf2,mf3,mf4,mf5,ml1,ml2,ml3,ml4,ml5


lci := 05
cci := 05
lba := 18
cba := 78


IF ! AbriArq('saccli','cli');RETURN NIL;ENDIF
IF ! AbriArq('insopera','sen');RETURN NIL;ENDIF
IF ! AbriArq('visita','vis');RETURN NIL;ENDIF
IF ! AbriArq('regiao','reg');RETURN NIL;ENDIF
/*
sele a
use cliente index cliente1
sele b
use vendedor index vendedor
sele c
use visita index visita1,visita2
sele d
use regiao index regiao
*/
WHILE .t.
        BOTAO(lci,cci,lba,cba,,' Digitacao de Visitas ')
        DEVPOS(lci+1,cci+1);DEVOUT('Cliente.......:')
        DEVPOS(lci+2,cci+1);DEVOUT('Regiao........:')
        DEVPOS(lci+3,cci+1);DEVOUT('Vendedor......:')
        DEVPOS(lci+4,cci+1);DEVOUT('Data da Visita:')
        DEVPOS(lci+5,cci+1);DEVOUT("Propo: [ ] - Infor: [ ] - Geren: [ ] - Visita: [ ] - Telef.: [ ]")
        BOTAO(lci+6,cci+4,lba-1,cba-6,,' *** Comentarios *** ')
        **************************
        SELE('cli');ORDSETFOCUS(1)
        **************************
        mcodigo=0
        mdata=CTOD("  /  /  ")
        mf1=" "
        mf2=" "
        mf3=" "
        mf4=" "
        mf5=" "
        ml1=space(60)
        ml2=space(60)
        ml3=space(60)
        ml4=space(60)
        ml5=space(60)
        mensagem(" 様 Digite o Codigo do Cliente.    [ESC] Abandona.")
        @ lci+1,cci+17 GET mcodigo PICTURE "99999"
        READ
        IF LASTKEY()=27 .OR. mcodigo=0
                RESTSCREEN(00,00,24,79,tela)
                CLOSE ALL
                RETURN
        ENDIF
        uv="  "
        SEEK STRZERO(mcodigo,5)
        IF ! FOUND()
                atencao(" Mensagem: Cliente nao cadastrado!  [ 潰 ] Continua:")
                LOOP
        ENDIF
        setcor(3)
        @ lci+1,cci+23 SAY cli->razao
        @ lci+2,cci+17 SAY cli->regiao
        ver_regi(cli->regiao,lci+2,cci+20)
        ver_ven(reg->codvend)
        @ lci+3,cci+17 SAY sen->scod_op+' - '+sen->snome
/*
        IF found()
                sele b
                uv=d->codvend
                SEEK d->codvend
                IF found()
                        vend=vendedor
                ENDIF
        ENDIF
*/
        **************************
        SELE('vis');ORDSETFOCUS(1)
        **************************
        WHILE .t.
                mensagem(" 様 Digite a data da visita.  [ESC] Abandona")
                @ lci+4,cci+17 GET mdata
                READ
                IF LASTKEY()=27
                        EXIT
                ENDIF
                SEEK STRZERO(mcodigo,5)+DTOS(mdata)
                IF ! FOUND()
                        atencao(" 様 Nao existe visita nesta data para este cliente!  [ENTER] Continua:")
                        LOOP
                ENDIF
                IF ok
                        atencao(" 様 Esta visita ja' foi digitada!!  [ENTER] Continua:")
                        LOOP
                ENDIF
                EXIT
        ENDDO
        IF LASTKEY()=27
                LOOP
        ENDIF
        @ lci+5,cci+9 GET mf1 PICTURE "!" VALID (mf1 $ "X ")
        @ lci+5,cci+22 GET mf2 PICTURE "!" VALID (mf2 $ "X ")
        @ lci+5,cci+35 GET mf3 PICTURE "!" VALID (mf3 $ "X ")
        @ lci+5,cci+49 GET mf4 PICTURE "!" VALID (mf4 $ "X ")
        @ lci+5,cci+63 GET mf5 PICTURE "!" VALID (mf5 $ "X ")
        @ lci+7,cci+6 GET ml1
        @ lci+8,cci+6 GET ml2
        @ lci+9,cci+6 GET ml3
        @ lci+10,cci+6 GET ml4
        @ lci+11,cci+6 GET ml5
        READ
        IF LASTKEY()=27
                LOOP
        ENDIF
        mensagem(" 様 Confirma os dados acima digitados ? [S/n]:")
        s=INKEY(0)
        IF s=asc("n") .OR. s=asc("N")
                LOOP
        ENDIF
        mensagem(" 様 Aguarde, processando informacoes....")
        BLOQREG()
        vis->l1 := ml1
        vis->l2 := ml2
        vis->l3 := ml3
        vis->l4 := ml4
        vis->l5 := ml5
        vis->ok := .t.
        vis->flag := mf1+mf2+mf3+mf4+mf5
        vis->uvend := uv
        **************************
        SELE('cli');ORDSETFOCUS(1)
        **************************
        BLOQREG()
        cli->qtdvis := qtdvis+1
        cli->uvisita := mdata
        DBCOMMITALL()
        DBUNLOCKALL()
        IF mf4="X"
                mensagem(" 様 Deseja marcar data da proxima visita ? [S/n]:")
                ss=INKEY(0)
                IF ss=asc("n") .or. ss=asc("N")
*
                ELSE
                        **************************
                        SELE('vis');ORDSETFOCUS(1)
                        **************************
                        mtel := SAVESCREEN(00,00,24,79)
                        mobs=vis->obs
                        mdat=mdata+7
                        BOTAO(14,2,19,76,,' Proxima Visita ')
                        @ 15, 6 SAY 'Data: '
                        @ 16,06 SAY 'Obs.:'
                        WHILE .t.
                                mensagem(" 様 Modifique os dados necessarios. [ESC] Abandona.")
                                @ 15,11 GET mdat VALID mdat<>CTOD("  /  /  ")
                                @ 16,11 GET mobs
                                READ
                                IF LASTKEY()=27
                                        EXIT
                                ENDIF
                                mensagem(" 様 Confirma os dados digitados ? [S/n]:")
                                sx=INKEY(0)
                                IF sx=asc("n") .or. sx=asc("N")
                                        LOOP
                                ENDIF
                                IF sx=27
                                        RESTSCREEN(00,00,24,79,mtel)
                                        EXIT
                                ENDIF
                                ADIREG()
                                vis->codigo := STRZERO(mcodigo,5)
                                vis->data := mdat
                                vis->obs := mobs
                                vis->ok := .f.
                                DBCOMMIT()
                                DBUNLOCK()
                                RESTSCREEN(00,00,24,79,mtel)
                                EXIT
                        ENDDO
                ENDIF
        ENDIF
ENDDO

