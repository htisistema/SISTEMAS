*****************************************
* TRANSFERENCIA DE ALUNO DE UMA TURMA P/OUTRA
*****************************************
FUNCTION saf22
**************
LOCAL mprg := 'SAF22'
LOCAL lci,cci,lba,cba,tela,opcao,mmatricula,mgstt,mnumero,mnum1,mgstt1
LOCAL point
IF ! ver_nivel(mprg,'TRANSFERENCIA DE ALUNO DE UMA TURMA P/OUTRA','125')
        RETURN NIL
ENDIF
lci := 10
cci := 10
lba := 13
cba := 70
tela := SAVESCREEN(00,00,24,79)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------

WHILE .T.
        *****************
        SELE('aluno');ORDSETFOCUS(4)
        GO TOP
        *****************
        botao(lci,cci,lba,cba,,' Transferencia de Aluno de uma Turma p/outra ')
        mmatricula := SPACE(6)
        mgstt := SPACE(4)
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula:')
        DEVPOS(lci+2,cci+1);DEVOUT('Novo GSTT:')
        @ lci+1,cci+12 GET mmatricula PICT '999999' VALID ver_aluno(mmatricula,,lci+1,cci+19)
        @ lci+2,cci+12 GET mgstt PICT '@!' VALID ver_gstt(mgstt)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL

        ENDIF
        opcao := 'N'
        mensagem('Confirma a Transferencia [s/N]:')
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        point  := RECNO()
        mnum1  := aluno->numero
        mgstt1 := aluno->gstt
        ADIREG()
        aluno->numero    := mnum1
        aluno->gstt      := mgstt1
        aluno->aluno     := 'TRANSF. P/ GSTT:'+mgstt
        aluno->matricula := '000001'
        aluno->faixa_1   := 'F'
        aluno->faixa_2   := 'F'
        aluno->faixa_3   := 'F'
        aluno->faixa_4   := 'F'
        aluno->faixa_5   := 'F'
        aluno->faixa_6   := 'F'
        aluno->faixa_7   := 'F'
        aluno->faixa_8   := 'F'
        aluno->faixa_9   := 'F'
        aluno->faixa_10  := 'F'
        aluno->faixa_11  := 'F'
        aluno->faixa_12  := 'F'
        aluno->dt_tranf  := CTOD('01/01/'+SUBSTR(mdiretorio,3,2))
        DBCOMMIT()
        DBUNLOCK()
        *****************
        SELE('aluno');ORDSETFOCUS(6)
        GO TOP
        *****************
        IF ! aluno->(DBSEEK(mgstt))
                atencao('Matricula nao encontrada no arquivo')
                LOOP
        ENDIF
        WHILE mgstt = aluno->gstt
                mnumero := STRZERO(VAL(aluno->numero)+1,3)
                SKIP
        ENDDO
        GO point
        BLOQREG()
        aluno->numero := mnumero
        aluno->gstt   := mgstt
        DBCOMMIT()
        DBUNLOCK()
        *****************
        SELE('nota');ORDSETFOCUS(1)
        GO TOP
        *****************
        IF ! nota->(DBSEEK(mmatricula))
                atencao('Matricula nao encontrada no arquivo')
                LOOP
        ENDIF
        WHILE mmatricula = nota->matricula
                BLOQREG()
                nota->numero := mnumero
                nota->gstt   := mgstt
                DBCOMMIT()
                DBUNLOCK()
                SKIP
        ENDDO
ENDDO
RETURN NIL
