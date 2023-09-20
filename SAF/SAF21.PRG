*************************
* MANUTENCAO DE NOTAS
*************************
FUNCTION saf21
***************
LOCAL mprg:='SAF21'
LOCAL tela1,tela,li,ci,lb,cb,lci,cci,lba,cba,point,pode,men,f,mnota,mfaltas,msit,;
      m_matriz:={},m_nota:={}
LOCAL mgstt,munidade,mdisciplina,msele,morde

IF ! ver_nivel(mprg,'MANUTENCAO DE NOTAS ALUNOS','125')
        RETURN NIL
ENDIF

lci := 01
cci := 00
lba := 18
cba := 79

li := 19
ci := 00
lb := 22
cb := 79
tela := SAVESCREEN(01,00,24,79)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
pode=.t.
WHILE .T.
        setcor(3)
        CAIXA(mold,lci,cci,lba,cba,,,.T.)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('GSTT:')
        DEVPOS(lci+1,cci+13);DEVOUT('Unidade:')
        DEVPOS(lci+1,cci+25);DEVOUT('Disciplina:')
        setcor(3)
        DEVPOS(lci+3,cci+1);DEVOUT('Aluno')
        DEVPOS(lci+3,cci+42);DEVOUT('   U1')
        DEVPOS(lci+3,cci+48);DEVOUT('   U2')
        DEVPOS(lci+3,cci+54);DEVOUT('   U3')
        DEVPOS(lci+3,cci+60);DEVOUT('   U4')
        DEVPOS(lci+3,cci+66);DEVOUT('Recup')
        DEVPOS(lci+3,cci+72);DEVOUT('  P.F')
        CAIXA(mold,li,ci,lb,cb,,,.T.)
        setcor(1)
        janela(li,ci,' Manutencao de Notas ','*','*')
        DEVPOS(li+1,ci+1);DEVOUT('Aluno:')
        DEVPOS(li+2,ci+1);DEVOUT('Nota:')
        DEVPOS(li+2,ci+13);DEVOUT('Faltas:')
        DEVPOS(li+2,ci+26);DEVOUT('Situacao:')
        mgstt     := '    '
        munidade  := '  '
        mdisciplina := '  '
        malteracao := ' '
        **************
        SELE('aluno');ORDSETFOCUS(6)
        GO TOP
        ************
        @ lci+1,cci+07 GET mgstt PICT '@!' VALID ver_gstt(mgstt)
        @ lci+1,cci+22 GET munidade PICT '@!'  WHEN men_get(lci+2,cci+22,'Informe o Tipo de Unidade que deseja [U1]1a.unidade [U2]2a.unidade [U3]3a.unidade [U4]4a.unidade [RC]recup.anual [PF]Prova final') VALID lim_get() .AND. munidade $ 'U1,U2,U3,U4,RC,PF'
        @ lci+1,cci+37 GET mdisciplina PICT '@!' VALID ver_gstt(mgstt,mdisciplina)
        READ
        IF LASTKEY()=27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        setcor(3)
        DEVPOS(lci+1,cci+40);DEVOUT(tbgstt->disci1)
        setcor(1)
        ASIZE(m_matriz,0)
        ASIZE(m_nota,0)
        IF ! aluno->(DBSEEK(mgstt))
                atencao('Nao existe nenhum aluno com este GSTT')
                LOOP
        ENDIF
        WHILE mgstt = aluno->gstt
                msele := SELE()
                morde := INDEXORD()
                IF ! EMPTY(aluno->dt_tranf)
                        SKIP
                        LOOP
                ENDIF
                point := RECNO()
                SELE('nota');ORDSETFOCUS(2)
                GO TOP
                IF nota->(DBSEEK(mgstt+mdisciplina+aluno->matricula))
                        AADD(m_matriz,' '+aluno->matricula+' '+LEFT(aluno->aluno,30)+' ³ '+sit_nota('U1')+' '+sit_nota('U2')+' '+sit_nota('U3')+' '+sit_nota('U4')+' '+sit_nota('RC')+' '+sit_nota('PF'))
*                       AADD(m_matriz,' '+aluno->matricula+' '+LEFT(aluno->aluno,30)+' '+TRANSFORM(nota->media_1,'99.99')+' '+TRANSFORM(nota->media_2,'99.99')+' '+TRANSFORM(nota->media_3,'99.99')+' '+TRANSFORM(nota->media_4,'99.99')+' '+TRANSFORM(nota->nt_recan,'99.99')+' '+TRANSFORM(nota->nt_prfi,'99.99'))
                        AADD(m_nota,{aluno->matricula,nota->media_1,nota->media_2,nota->media_3,nota->media_4,nota->nt_recan,nota->nt_prfi,nota->falta_1,nota->falta_2,nota->falta_3,nota->falta_4,nota->ftpr_1,nota->ftpr_2,nota->ftpr_3,nota->ftpr_4,nota->ft_recan,nota->ft_prfi})
                ELSE
                        AADD(m_matriz,' '+aluno->matricula+' '+LEFT(aluno->aluno,30)+' ³ '+'                                   ')
                        AADD(m_nota,{aluno->matricula,0,0,0,0,0,0,0,0,0,0,' ',' ',' ',' ',' ',' '})
                ENDIF
                SELE(msele);ORDSETFOCUS(morde)
                GO point
                SKIP
        ENDDO
        point := 0
        WHILE .T.
                mensagem("< ou > Consulta  -  <ESC> Retorna  -  <ENTER> p/Resumo" )
                point := ACHOICE(lci+5,cci+1,lba-1,cba-1,m_matriz,,,point+1)

                DO CASE
                        CASE LASTKEY()=27
                                EXIT
                        CASE LASTKEY() = 13
                                **************
                                SELE('aluno');ORDSETFOCUS(6)
                                GO TOP
                                ************
                                mnota := 0
                                mfaltas := 0
                                msit := SPACE(1)
                                ver_aluno(m_nota[point,1])
                                tela1 := SAVESCREEN(01,00,24,79)
                                setcor(3)
                                DEVPOS(li+1,ci+08);DEVOUT(aluno->matricula+'-'+aluno->aluno)
                                setcor(1)
                                IF munidade = 'U1'
                                        mnota := m_nota[point,2]
                                        mfaltas := m_nota[point,8]
                                        msit := m_nota[point,12]
                                ELSEIF munidade = 'U2'
                                        mnota := m_nota[point,3]
                                        mfaltas := m_nota[point,9]
                                        msit := m_nota[point,13]
                                ELSEIF munidade = 'U3'
                                        mnota := m_nota[point,4]
                                        mfaltas := m_nota[point,10]
                                        msit := m_nota[point,14]
                                ELSEIF munidade = 'U4'
                                        mnota := m_nota[point,5]
                                        mfaltas := m_nota[point,11]
                                        msit := m_nota[point,15]
                                ELSEIF munidade = 'RC'
                                        mnota := m_nota[point,6]
                                        mfaltas := 0
                                        msit := m_nota[point,16]
                                ELSEIF munidade = 'PF'
                                        mnota := m_nota[point,7]
                                        mfaltas := 0
                                        msit := m_nota[point,17]
                                ENDIF
                                @ li+2,ci+07 GET mnota PICT '99.99'
                                @ li+2,ci+21 GET mfaltas PICT '9999' WHEN ! EMPTY(mnota)
                                @ li+2,ci+36 GET msit PICT '@!' WHEN EMPTY(mnota)
                                READ
                                IF LASTKEY() = 27
                                        RESTSCREEN(01,00,24,79,tela1)
                                        LOOP
                                ENDIF
                                IF ! EMPTY(mnota)
                                        msit := SPACE(1)
                                        @ li+2,ci+36 GET msit PICT '@!' WHEN EMPTY(mnota)
                                ENDIF
                                mensagem('Confirma a NOTA [S/n]:')
                                opcao := 'S'
                                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                                READ
                                IF LASTKEY() = 27 .OR. opcao = 'N'
                                        RESTSCREEN(01,00,24,79,tela1)
                                        LOOP
                                ENDIF
                                ***************************
                                SELE('nota');ORDSETFOCUS(2)
                                GO TOP
                                ***************************
                                IF nota->(DBSEEK(mgstt+mdisciplina+aluno->matricula))
                                        BLOQREG()
                                ELSE
                                        ADIREG()
                                        nota->gstt := aluno->gstt
                                        nota->cod_disc := mdisciplina
                                        nota->numero := aluno->numero
                                        nota->matricula := aluno->matricula
                                ENDIF
                                IF munidade = 'U1'
                                        m_nota[point,2] := mnota
                                        nota->media_1 := mnota
                                        nota->opera_1 := cod_operado
                                        nota->falta_1 := mfaltas
                                        nota->ftpr_1 := msit
                                        m_matriz[point] := ' '+aluno->matricula+' '+LEFT(aluno->aluno,30)+' ³ '+sit_nota('U1')+' '+sit_nota('U2')+' '+sit_nota('U3')+' '+sit_nota('U4')+' '+sit_nota('RC')+' '+sit_nota('PF')
                                        m_nota[point,8] := mfaltas
                                ELSEIF munidade = 'U2'
                                        m_nota[point,3] := mnota
                                        nota->media_2 := mnota
                                        nota->opera_2 := cod_operado
                                        nota->falta_2 := mfaltas
                                        nota->ftpr_2 := msit
                                        m_matriz[point] := ' '+aluno->matricula+' '+LEFT(aluno->aluno,30)+' ³ '+sit_nota('U1')+' '+sit_nota('U2')+' '+sit_nota('U3')+' '+sit_nota('U4')+' '+sit_nota('RC')+' '+sit_nota('PF')
                                        m_nota[point,9] := mfaltas
                                ELSEIF munidade = 'U3'
                                        m_nota[point,4] := mnota
                                        nota->media_3 := mnota
                                        nota->opera_3 := cod_operado
                                        nota->falta_3 := mfaltas
                                        nota->ftpr_3 := msit
                                        m_matriz[point] := ' '+aluno->matricula+' '+LEFT(aluno->aluno,30)+' ³ '+sit_nota('U1')+' '+sit_nota('U2')+' '+sit_nota('U3')+' '+sit_nota('U4')+' '+sit_nota('RC')+' '+sit_nota('PF')
                                        m_nota[point,10] := mfaltas
                                ELSEIF munidade = 'U4'
                                        m_nota[point,5] := mnota
                                        nota->media_4 := mnota
                                        nota->opera_4 := cod_operado
                                        nota->falta_4 := mfaltas
                                        nota->ftpr_4 := msit
                                        m_matriz[point] := ' '+aluno->matricula+' '+LEFT(aluno->aluno,30)+' ³ '+sit_nota('U1')+' '+sit_nota('U2')+' '+sit_nota('U3')+' '+sit_nota('U4')+' '+sit_nota('RC')+' '+sit_nota('PF')
                                        m_nota[point,11] := mfaltas
                                ELSEIF munidade = 'RC'
                                        m_nota[point,6] := mnota
                                        nota->nt_recan := mnota
                                        nota->opera_ra := cod_operado
                                        nota->ft_recan := msit
                                        m_matriz[point] := ' '+aluno->matricula+' '+LEFT(aluno->aluno,30)+' ³ '+sit_nota('U1')+' '+sit_nota('U2')+' '+sit_nota('U3')+' '+sit_nota('U4')+' '+sit_nota('RC')+' '+sit_nota('PF')
                                ELSEIF munidade = 'PF'
                                        m_nota[point,7] := mnota
                                        nota->nt_prfi := mnota
                                        nota->opera_pf := cod_operado
                                        nota->ft_prfi := msit
                                        m_matriz[point] := ' '+aluno->matricula+' '+LEFT(aluno->aluno,30)+' ³ '+sit_nota('U1')+' '+sit_nota('U2')+' '+sit_nota('U3')+' '+sit_nota('U4')+' '+sit_nota('RC')+' '+sit_nota('PF')
                                ENDIF
                                DBCOMMITALL()
                                DBUNLOCKALL()
                                SELE(msele);ORDSETFOCUS(morde)
                                RESTSCREEN(01,00,24,79,tela1)
                ENDCASE
        ENDDO
ENDDO
RETURN NIL
*************************** f i m *******************************************
FUNCTION sit_nota(munid)
************************

IF munid = 'U1'
        IF ! EMPTY(nota->media_1)
                RETURN TRANSFORM(nota->media_1,'99.99')
        ELSEIF ! EMPTY(nota->ftpr_1)
                RETURN SPACE(2)+nota->ftpr_1+SPACE(2)
        ELSE
                RETURN TRANSFORM(nota->media_1,'99.99')
        ENDIF
ELSEIF munid = 'U2'
        IF ! EMPTY(nota->media_2)
                RETURN TRANSFORM(nota->media_2,'99.99')
        ELSEIF ! EMPTY(nota->ftpr_2)
                RETURN SPACE(2)+nota->ftpr_2+SPACE(2)
        ELSE
                RETURN TRANSFORM(nota->media_2,'99.99')
        ENDIF
ELSEIF munid = 'U3'
        IF ! EMPTY(nota->media_3)
                RETURN TRANSFORM(nota->media_3,'99.99')
        ELSEIF ! EMPTY(nota->ftpr_3)
                RETURN SPACE(2)+nota->ftpr_3+SPACE(2)
        ELSE
                RETURN TRANSFORM(nota->media_3,'99.99')
        ENDIF
ELSEIF munid = 'U4'
        IF ! EMPTY(nota->media_4)
                RETURN TRANSFORM(nota->media_4,'99.99')
        ELSEIF ! EMPTY(nota->ftpr_4)
                RETURN SPACE(2)+nota->ftpr_4+SPACE(2)
        ELSE
                RETURN TRANSFORM(nota->media_4,'99.99')
        ENDIF
ELSEIF munid = 'RC'
        IF ! EMPTY(nota->nt_recan)
                RETURN TRANSFORM(nota->nt_recan,'99.99')
        ELSEIF ! EMPTY(nota->ft_recan)
                RETURN SPACE(2)+nota->ft_recan+SPACE(2)
        ELSE
                RETURN TRANSFORM(nota->nt_recan,'99.99')
        ENDIF
ELSEIF munid = 'PF'
        IF ! EMPTY(nota->nt_prfi)
                RETURN TRANSFORM(nota->nt_prfi,'99.99')
        ELSEIF ! EMPTY(nota->ft_prfi)
                RETURN SPACE(2)+nota->ft_prfi+SPACE(2)
        ELSE
                RETURN TRANSFORM(nota->nt_prfi,'99.99')
        ENDIF
ENDIF
RETURN NIL




