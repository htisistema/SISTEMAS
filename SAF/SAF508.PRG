************************************************
* FOLHA DE AVALIACAO
************************************************

FUNCTION saf508
***************
LOCAL mprg:='SAF508'
LOCAL li,ci,cb,lb,tela1,lci,cci,lba,cba,tela,opcao,mquantd,mcod_disc,mgstt,munid,;
      mdisc:={},i,mcont,mun

IF ! ver_nivel(mprg,'RELATORIO FOLHA DE AVALIACAO','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)
lci := 10
cci := 01
lba := 14
cba := 53

li := 02
ci := 54
lb := 22
cb := 79

tela := SAVESCREEN(00,00,24,79)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        setcor(3)
        botao(lci,cci,lba,cba)
        janela(lci,cba,' Folha de Avaliacao ','*')
        setcor(1)
        mun := SPACE(2)
        munid := SPACE(12)
        mgstt := SPACE(4)
        DEVPOS(lci+1,cci+1);DEVOUT('Digite o GSTT que deseja:')
        DEVPOS(lci+2,cci+1);DEVOUT('Digite a Unidade........:')
        DEVPOS(lci+3,cci+1);DEVOUT('Codigo da Disciplina....:')
        @ lci+1,cci+27 GET mgstt PICT '@!'
        @ lci+2,cci+27 GET mun PICT '@!' VALID IF(EMPTY(mun),.F.,.T.)
        READ
        munid := ALLTRIM(mun)+' UNIDADE'
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        ***************
        SELE('aluno');ORDSETFOCUS(6)
        DBSETFILTER({|| aluno->gstt = mgstt})
        GO TOP
        ***************
        IF EOF()
                atencao('Nao existe aluno neste GSTT')
                LOOP
        ENDIF
        tela1 := SAVESCREEN(00,00,24,79)
        setcor(3)
        botao(li,ci,lb,cb)
        janela(li,cb,' Disciplinas ','*')
        setcor(1)
        ASIZE(mdisc,0)
        WHILE .T.
                mcod_disc := 0
                @ lci+3,cci+27 GET mcod_disc PICT '99'
                READ
                IF LASTKEY() = 27
                        EXIT
                ENDIF
                IF EMPTY(mcod_disc)
                        EXIT
                ENDIF
                mdescr := ver_disc(STRZERO(mcod_disc,2))
                setcor(3)
                DEVPOS(lci+3,cci+30);DEVOUT(mdescr)
                setcor(1)
                IF EMPTY(mdescr)
                        atencao('Disciplina nao foi encontrado')
                        LOOP
                ENDIF
                mensagem('Confirma a Disciplina [S/n]:')
                opcao := 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27 .OR. opcao = 'N'
                        LOOP
                ENDIF
                AADD(mdisc,{disc->cod_disc,disc->descr})
                i := 1
                FOR i = 1 TO LEN(mdisc)
                        DEVPOS(li+i,ci+1);DEVOUT(mdisc[i,1])
                        DEVPOS(li+i,COL()+1);DEVOUT(mdisc[i,2])
                NEXT
        ENDDO
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF LEN(mdisc) = 0
                atencao('Nao foi solicitado nenhuma disciplina')
                LOOP
        ENDIF
        mensagem('Confirma a Impressao [S/n]:')
        opcao = 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF opcao = 'N' .OR. LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        SET DEVI TO PRINT
*       SET PRINT TO (set->porta)
        i := 0
        FOR i = 1 TO LEN(mdisc)
                mcont := 0
                imprt('M','C',0)
                DEVPOS(PROW(),110);DEVOUT(munid)
                DEVPOS(PROW(),195);DEVOUT(munid)
                DEVPOS(PROW()+1,110);DEVOUT(mdisc[i,1])
                DEVPOS(PROW(),113);DEVOUT(mdisc[i,2])
                DEVPOS(PROW(),195);DEVOUT(mdisc[i,1])
                DEVPOS(PROW(),198);DEVOUT(mdisc[i,2])
                DEVPOS(PROW()+1,60);DEVOUT(mdisc[i,1])
                DEVPOS(PROW(),63);DEVOUT(mdisc[i,2])
                DEVPOS(PROW()+1,30);DEVOUT(munid)
                DEVPOS(PROW(),55);DEVOUT(SUBSTR(mgstt,4,1))
                DEVPOS(PROW(),75);DEVOUT(SUBSTR(mgstt,2,1))
                DEVPOS(PROW(),90);DEVOUT(SUBSTR(mgstt,3,1))
                DEVPOS(PROW(),106);DEVOUT(SUBSTR(mgstt,2,1))
                DEVPOS(PROW(),192);DEVOUT(SUBSTR(mgstt,2,1))
                DEVPOS(PROW()+1,106);DEVOUT(SUBSTR(mgstt,3,1))
                DEVPOS(PROW(),192);DEVOUT(SUBSTR(mgstt,3,1))

                GO TOP
                imprt('M','C',2)
                WHILE ! EOF()
                        DEVPOS(PROW()+1,07);DEVOUT(aluno->aluno)
                        IF ! EMPTY(dt_tranf)
                                DEVPOS(PROW(),PCOL());DEVOUT('**Transf.**')
                        ELSE
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(aluno->area+'  '+aluno->disciplina)
                        ENDIF
                        DEVPOS(PROW(),103);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),129);DEVOUT(aluno->aluno)
                        IF ! EMPTY(dt_tranf)
                                DEVPOS(PROW(),PCOL());DEVOUT('**Transf.**')
                        ELSE
                                DEVPOS(PROW(),PCOL()+1);DEVOUT(aluno->area+'  '+aluno->disciplina)
                        ENDIF
                        DEVPOS(PROW(),190);DEVOUT(aluno->matricula)
                        mcont ++
                        SKIP
                        IF mcont = 55
                                mcont := 0
                                EJECT
                                imprt('M','C',0)
                                DEVPOS(PROW(),110);DEVOUT(munid)
                                DEVPOS(PROW(),195);DEVOUT(munid)
                                DEVPOS(PROW()+1,110);DEVOUT(mdisc[i,1])
                                DEVPOS(PROW(),113);DEVOUT(mdisc[i,2])
                                DEVPOS(PROW(),195);DEVOUT(mdisc[i,1])
                                DEVPOS(PROW(),198);DEVOUT(mdisc[i,2])
                                DEVPOS(PROW()+1,60);DEVOUT(mdisc[i,1])
                                DEVPOS(PROW(),63);DEVOUT(mdisc[i,2])
                                DEVPOS(PROW()+1,30);DEVOUT(munid)
                                DEVPOS(PROW(),55);DEVOUT(SUBSTR(mgstt,4,1))
                                DEVPOS(PROW(),75);DEVOUT(SUBSTR(mgstt,2,1))
                                DEVPOS(PROW(),90);DEVOUT(SUBSTR(mgstt,3,1))
                                DEVPOS(PROW(),106);DEVOUT(SUBSTR(mgstt,2,1))
                                DEVPOS(PROW(),192);DEVOUT(SUBSTR(mgstt,2,1))
                                DEVPOS(PROW()+1,106);DEVOUT(SUBSTR(mgstt,3,1))
                                DEVPOS(PROW(),192);DEVOUT(SUBSTR(mgstt,3,1))
                                imprt('M','C',2)
                        ENDIF
                ENDDO
                EJECT
                SETPRC(00,00)
        NEXT
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDDO

