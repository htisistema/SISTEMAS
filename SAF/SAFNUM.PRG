**********************************
* FUNCAO P/GERAR NUMERO DE CHAMADA
**********************************

FUNCTION safnum()
*****************
LOCAL mprg:='SAFNUM'
LOCAL opcao,mgstt,mnumero

IF ! ver_nivel(mprg,'GERAR NUMERO DE CHAMADA P/ALUNOS','125')
        RETURN NIL
ENDIF
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
*****************************
SELE('aluno');ORDSETFOCUS(2)
GO TOP
*****************************
opcao := ' '
mnumero := 0
IF ! EMPTY(aluno->numero)
        opcao := mensagem1('Esta rotina ja foi efetuada deseja realiza novamente','S','S,N')
        IF LASTKEY() = 27 .OR. opcao = 'N'
                RETURN NIL
        ENDIF
ELSE
        opcao := mensagem1('Confirma esta Rotina','S','S,N')
        IF LASTKEY() = 27 .OR. opcao = 'N'
                RETURN NIL
        ENDIF
ENDIF
mnumero := 1
mgstt := aluno->gstt
IF ! BLOQARQ()
        atencao('Nao foi possivel bloquear o arquivo')
        RETURN NIL
ENDIF
mensagem('Aguarde um momento gerando numero de CHAMADA')
WHILE ! EOF()
        WHILE mgstt = aluno->gstt .AND. ! EOF()
                aluno->numero := STRZERO(mnumero,3)
                SKIP
                mnumero ++
        ENDDO
        mgstt := aluno->gstt
        mnumero := 1
ENDDO
RETURN NIL
