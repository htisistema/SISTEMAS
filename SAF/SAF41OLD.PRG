************************************************
* CONSULTA DE BOLETIM
************************************************

FUNCTION saf41
***************
LOCAL mprg:='SAF41'
LOCAL lci,cci,lba,cba,opcao,tela,i,mmatricula,mtransf,mconsulta:={}
LOCAL md_1,md_2,md_3,md_4,media_u,recup_a,media_a,recup_f,media_f,situacao
PRIVATE m_nota := {},munidade

IF ! ver_nivel(mprg,'CONSULTA DE BOLETIM ALUNOS','125')
        RETURN NIL
ENDIF

*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
lci := 03
cci := 00
lba := 21
cba := 79
tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        ASIZE(mconsulta,0)
        mmatricula := SPACE(6)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        CAIXA(mold,lci,cci,lba,cba,,,.T.)
        *****************
        SELE('aluno');ORDSETFOCUS(4)
        GO TOP
        *****************
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula:')
        DEVPOS(lci+2,cci+1);DEVOUT('GRAU:')
        DEVPOS(lci+2,cci+9);DEVOUT('SERIE:')
        DEVPOS(lci+2,cci+18);DEVOUT('TURNO:')
        DEVPOS(lci+2,cci+27);DEVOUT('TURMA:')
        @ lci+3,cci+1 TO lci+3,cba-1
        DEVPOS(lci+4,cci+01);DEVOUT('Disciplina')
        DEVPOS(lci+4,cci+27);DEVOUT('1a.U')
        DEVPOS(lci+4,COL()+1);DEVOUT('2a.U')
        DEVPOS(lci+4,COL()+1);DEVOUT('3a.U')
        DEVPOS(lci+4,COL()+1);DEVOUT('4a.U')
        DEVPOS(lci+4,COL()+1);DEVOUT('MD.U')
        DEVPOS(lci+4,COL()+1);DEVOUT('RC.A')
        DEVPOS(lci+4,COL()+1);DEVOUT('MD.A')
        DEVPOS(lci+4,COL()+1);DEVOUT('RC.F')
        DEVPOS(lci+4,COL()+1);DEVOUT('MD.F')
        DEVPOS(lci+4,COL()+1);DEVOUT('SIT.')
        @ lci+5,cci+1 TO lci+5,cba-1
        setcor(1)
        @ lci+1,cci+12 GET mmatricula PICT '999999'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF ! aluno->(DBSEEK(mmatricula))
                atencao('Nao foi possivel encontrar esta matricula')
                LOOP
        ENDIF
        setcor(3)
        DEVPOS(lci+1,cci+22);DEVOUT(aluno->aluno)
        DEVPOS(lci+2,cci+7);DEVOUT(SUBSTR(aluno->gstt,1,1))
        DEVPOS(lci+2,cci+16);DEVOUT(SUBSTR(aluno->gstt,2,1))
        DEVPOS(lci+2,cci+25);DEVOUT(SUBSTR(aluno->gstt,4,1))
        DEVPOS(lci+2,cci+34);DEVOUT(SUBSTR(aluno->gstt,3,1))
        setcor(1)
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        ASIZE(m_nota,0)
        mtransf := SPACE(1)
        IF ! EMPTY(aluno->dt_tranf)
                mtransf := 'S'
        ENDIF
        boletim(aluno->matricula)
        i := 0
        FOR i = 1 TO LEN(m_nota)
                
                IF m_nota[i,1] = '14' .AND. aluno->bloqueio = '*'
                        md_1 := '  D '
                ELSEIF EMPTY(m_nota[i,3]) .AND. mtransf = 'S'
                        md_1 := 'tran'
                ELSE
                        md_1 := TRANSFORM(m_nota[i,3],'99.9')  //media1
                ENDIF
                IF m_nota[i,1] = '14' .AND. aluno->bloqueio2 = '*'
                        md_2 := '  D '
                ELSEIF EMPTY(m_nota[i,5]) .AND. mtransf = 'S'
                        md_2 := 'tran'
                ELSE
                        md_2 := TRANSFORM(m_nota[i,5],'99.9')   //media2
                ENDIF
                IF m_nota[i,1] = '14' .AND. aluno->bloqueio3 = '*'
                        md_3 := '  D '
                ELSEIF EMPTY(m_nota[i,7]) .AND. mtransf = 'S'
                        md_3 := 'tran'
                ELSE
                        md_3 := TRANSFORM(m_nota[i,7],'99.9')   //media3
                ENDIF
                IF m_nota[i,1] = '14' .AND. aluno->bloqueio4 = '*'
                        md_4 := '  D '
                ELSEIF EMPTY(m_nota[i,9]) .AND. mtransf = 'S'
                        md_4 := 'tran'
                ELSE
                        md_4 := TRANSFORM(m_nota[i,9],'99.9')   //media4
                ENDIF
                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                        media_u := '  D '
                ELSE
                        media_u := TRANSFORM(m_nota[i,11],'99.9')  //media unidade
                ENDIF
                // recuperacao anual
                recup_a := IF(EMPTY(m_nota[i,12]),SPACE(4),TRANSFORM(m_nota[i,12],'99.9'))
                // media anual
                media_a := IF(EMPTY(m_nota[i,13]),SPACE(4),TRANSFORM(m_nota[i,13],'99.9'))
                // recuperacao final
                recup_f := IF(EMPTY(m_nota[i,14]),SPACE(4),TRANSFORM(m_nota[i,14],'99.9'))
                // media final
                media_f := IF(EMPTY(m_nota[i,15]),SPACE(4),TRANSFORM(m_nota[i,15],'99.9'))
                // total das faltas
                // situacao
                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                        situacao := '  D '
                ELSEIF mtransf = 'S'
                        situacao := 'TRAN'
                ELSE
                        situacao := m_nota[i,16]
                ENDIF
                AADD(mconsulta,m_nota[i,1]+' '+m_nota[i,2]+'   '+md_1+' '+md_2+' '+md_3+' '+md_4+' '+media_u+' '+recup_a;
                     +' '+media_a+' '+recup_f+' '+media_f+' '+situacao)
        NEXT
        CLEAR TYPEAHEAD
        mensagem("< ou > Consulta  -  <ESC> Retorna  -  <ENTER> p/Resumo" )
        point := ACHOICE(lci+6,cci+1,lba-1,cba-1,mconsulta)
        DO CASE
                CASE LASTKEY()=27
                        LOOP
                ENDCASE
        LOOP
ENDDO
**************************** F I M **************************

