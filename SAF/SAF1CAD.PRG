************************************************
* RELATORIO CADASTRO DE ALUNOS
************************************************

FUNCTION saf1cad
***************
LOCAL mprg:='SAF1CAD'
LOCAL lci:=05,cci:=03,lba:=09,cba:=77
LOCAL li:=11,ci:=03,la:=22,ca:=77,y
LOCAL opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      tela,mordem,mop,mmatricula,mgstt,mbolsa,maluno,m_matriz:={}

PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIOS CADASTRO DE ALUNO','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(00,00,24,79)
mtraco := REPLI('-',130)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        ***************
        SELE('aluno');ORDSETFOCUS(4)
        GO TOP
        ***************
        ASIZE(m_matriz,0)
        mquantd = 1
        mordem := 1
        mmatricula := SPACE(6)
        mgstt := SPACE(4)
        mbolsa := SPACE(1)
        mtipo := ' '
        mtit := 'Relatorio de Cadastro dos Alunos'
        mtit := mtit+SPACE((50-LEN(mtit)))
        botao(lci,cci,lba,cba)
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula do Aluno:')
        DEVPOS(lci+2,cci+1);DEVOUT('GSTT..............:')
        DEVPOS(lci+3,cci+1);DEVOUT('Titulo Relatorio..:')
        @ lci+1,cci+21 GET mmatricula PICT '999999'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF ! EMPTY(mmatricula)
                botao(li,ci,la,ca)
                @ lci+3,cci+21 GET mtit
                WHILE .T.
                        maluno := SPACE(40)
                        @ lci+1,cci+21 GET mmatricula PICT '999999'
                        @ lci+1,cci+28 GET maluno PICT '@!' VALID IF(EMPTY(maluno),.F.,.T.) WHEN EMPTY(mmatricula)
                        READ
                        IF LASTKEY() = 27
                                EXIT
                        ENDIF
                        IF ! EMPTY(mmatricula)
                                ORDSETFOCUS(4)
                                IF ! aluno->(DBSEEK(mmatricula))
                                        atencao('Matricula nao encontrada no arquivo')
                                        LOOP
                                ENDIF
                        ELSEIF ! EMPTY(maluno)
                                ORDSETFOCUS(3)
                                IF ! aluno->(DBSEEK(RTRIM(maluno)))
                                        atencao('Aluno nao encontrada no arquivo')
                                        LOOP
                                ENDIF
                                mmatricula := f7_aluno()
                                IF LASTKEY() = 27
                                        LOOP
                                ENDIF
                        ELSE
                                LOOP
                        ENDIF
                        setcor(3)
                        DEVPOS(lci+1,cci+21);DEVOUT(mmatricula)
                        DEVPOS(lci+1,cci+28);DEVOUT(aluno->aluno)
                        setcor(1)
                        IF 0 < ASCAN(m_matriz,mmatricula)
                                atencao('Este Aluno ja foi Solicitado !!!')
                                LOOP
                        ENDIF
                        mensagem('Confirma este ALUNO [S/n]:')
                        opcao := 'S'
                        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                        READ
                        IF LASTKEY() = 27 .OR. opcao = 'N'
                                LOOP
                        ENDIF
                        AADD(m_matriz,mmatricula)
                        y := 0
                        FOR y = 1 TO LEN(m_matriz)
                                IF y <= 10
                                        DEVPOS(li+y,ci+1);DEVOUT(m_matriz[y])
                                ELSEIF Y <= 20
                                        DEVPOS(li+(y-10),ci+9);DEVOUT(m_matriz[y])
                                ELSEIF Y <= 30
                                        DEVPOS(li+(y-20),ci+17);DEVOUT(m_matriz[y])
                                ELSEIF Y <= 40
                                        DEVPOS(li+(y-30),ci+17);DEVOUT(m_matriz[y])
                                ELSEIF Y <= 50
                                        DEVPOS(li+(y-40),ci+17);DEVOUT(m_matriz[y])
                                ELSEIF Y <= 60
                                        DEVPOS(li+(y-50),ci+17);DEVOUT(m_matriz[y])
                                ENDIF
                        NEXT
                ENDDO
                IF LEN(m_matriz) > 0
                        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                        IF ! imp_arq('CADASTRO.REL')
                                LOOP
                        ENDIF
                        mpag := 0
                        FOR i = 1 TO LEN(m_matriz)
                                IF PROW() > 57 .OR. mpag = 0
                                        IF mpag > 0
                                                EJECT
                                        ENDIF
                                        mpag := mpag + 1
                                        cabecalho(mpag,mtit)
                                ENDIF
                                ***************
                                SELE('aluno');ORDSETFOCUS(4)
                                GO TOP
                                ***************
                                aluno->(DBSEEK(m_matriz[i]))
                                imprt('M','C')
                                DEVPOS(PROW()+1,00);DEVOUT('Matricula: '+aluno->matricula+'  Aluno: '+aluno->aluno+'  GSTT: '+aluno->gstt)
                                DEVPOS(PROW()+1,00);DEVOUT('Data de Nascimento: '+DTOC(aluno->a_datan)+'  Sexo: '+aluno->sexo+'  Natural: '+aluno->a_natur+' Nacionalidade: '+aluno->a_nacion)
                                DEVPOS(PROW()+1,00);DEVOUT('Pai: '+aluno->pai+' Fone:'+aluno->p_fone+' - Mae: '+aluno->mae+' Fone:'+m_fone)
                                DEVPOS(PROW()+1,00);DEVOUT('Responsavel: '+aluno->responsa+' - CPF:'+aluno->r_cpf+' - Fone:'+aluno->r_fone1+'/'+aluno->r_fone)
                                DEVPOS(PROW()+1,00);DEVOUT('Endereco: '+RTRIM(aluno->r_rua)+', '+aluno->r_num+'-'+RTRIM(aluno->r_apto)+' - '+RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - UF:'+aluno->r_estado+' - CEP:'+aluno->r_cep)
                                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        NEXT
                ENDIF
        ELSE
                @ lci+2,cci+21 GET mgstt PICT '999999'
                @ lci+3,cci+21 GET mtit
                READ
                ***************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                ***************
                IF ! aluno->(DBSEEK(mgstt)) .AND. ! EMPTY(mgstt)
                        atencao('Nao foi encontrado esta GSTT')
                        LOOP
                ENDIF
                mensagem('Ordem:[1]-Resp. [2]-GSTT [3]-Aluno [4]-Matricula [10]-Bairro:')
                opcao = 'S'
                @ 23,COL()+1 GET mordem PICT '99' RANGE 1,10
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                ***************
                SELE('aluno');ORDSETFOCUS(mordem)
                GO TOP
                ***************
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                IF ! imp_arq('CADASTRO.REL')
                        LOOP
                ENDIF
                mpag := 0
                WHILE ! EOF()
                        IF PROW() > 57 .OR. mpag = 0
                                IF mpag > 0
                                        EJECT
                                ENDIF
                                mpag := mpag + 1
                                cabecalho(mpag,mtit)
                        ENDIF
                        IF ! EMPTY(mgstt) .AND. mgstt <> aluno->gstt
                                SKIP
                                LOOP
                        ENDIF
                        imprt('M','C')
                        DEVPOS(PROW()+1,00);DEVOUT('Matricula: '+aluno->matricula+'  Aluno: '+aluno->aluno+'  GSTT: '+aluno->gstt)
                        DEVPOS(PROW()+1,00);DEVOUT('Data de Nascimento: '+DTOC(aluno->a_datan)+'  Sexo: '+aluno->sexo+'  Natural: '+aluno->a_natur+' Nacionalidade: '+aluno->a_nacion)
                        DEVPOS(PROW()+1,00);DEVOUT('Pai: '+aluno->pai+' Fone:'+aluno->p_fone+'  - Mae: '+aluno->mae+' Fone:'+m_fone)
                        DEVPOS(PROW()+1,00);DEVOUT('Responsavel: '+aluno->responsa+' CPF:'+aluno->r_cpf+' Fone:'+aluno->r_fone1+'/'+aluno->r_fone)
                        DEVPOS(PROW()+1,00);DEVOUT('Endereco: '+RTRIM(aluno->r_rua)+', '+aluno->r_num+'-'+RTRIM(aluno->r_apto)+' - '+RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - UF:'+aluno->r_estado+' - CEP:'+aluno->r_cep)
                        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                        SKIP
                ENDDO
        ENDIF
        imprt('M','N')
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('CADASTRO.REL')
        ENDIF
        RETURN NIL
ENDDO
RETURN NIL
**************************** F I M **************************

