***************************************
* GERAR DISCO P/IMPRESSAO DE CONTRATO
***************************************

FUNCTION safcontr
*******************
LOCAL mprg:='SAFCONTR'
LOCAL lci,cci,lba,cba,tela,mmatricula,mtipo_con,opcao,mindex,mcod_gstt,msele,morde

PRIVATE mgstt,munidade:='MF',msituacao,mtipo_sit,m_nota:={}

IF ! ver_nivel(mprg,'GERAR DISCO P/IMPRESSAO DE CONTRATO','125')
        RETURN NIL
ENDIF

*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('safcontr','contr','E');RETURN NIL;ENDIF
ZAP
*--------------------------------------------------------------------------
lci := 10
cci := 08
lba := 14
cba := 70
tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        mmatricula := SPACE(6)
        mtipo_con := 0
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        CAIXA(mold,lci,cci,lba,cba,,,.T.)
        @ lci+2,cci+1 TO lci+2,cba-1
        setcor(1)
        @ lci+1,cci+4 PROMPT ' Individual ' MESSAGE 'Emissao de Boletim INDIVIDUAL'
        @ lci+1,COL()+4 PROMPT '    GSTT    ' MESSAGE 'Emissao de Boletim p/GSTT'
        @ lci+1,COL()+4 PROMPT '    Geral   ' MESSAGE 'Emissao de Boletim em GERAL'
        SET INTEN ON
        MENU TO mtipo_con

        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        IF mtipo_con = 1
                *****************
                SELE('aluno');ORDSETFOCUS(4)
                GO TOP
                *****************
                DEVPOS(lci+3,cci+1);DEVOUT('Matricula:')
                @ lci+3,cci+12 GET mmatricula PICT '999999'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF ! aluno->(DBSEEK(mmatricula))
                        atencao('Nao foi possivel encontrar esta matricula')
                        LOOP
                ENDIF
                setcor(3)
                DEVPOS(lci+3,cci+22);DEVOUT(aluno->aluno)
                setcor(1)
                IF ! EMPTY(aluno->dt_tranf)
                        atencao('Este Aluno foi Transferido')
                        LOOP
                ENDIF
                mensagem('Confirma o processo do CONTRATO [S/N]:')
                opcao := 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF opcao = 'N'
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                mgstt := SPACE(4)
                IF SUBSTR(aluno->gstt,1,2) = '01'
                        mgstt := '02'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '02'
                        mgstt := '03'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '03'
                        mgstt := '04'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '04'
                        mgstt := '05'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '05'
                        mgstt := '11'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '11'
                        mgstt := '12'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '12'
                        mgstt := '13'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '13'
                        mgstt := '14'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '14'
                        mgstt := '15'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '15'
                        mgstt := '16'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '16'
                        mgstt := '17'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '17'
                        mgstt := '18'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '18'
                        mgstt := '21'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '21'
                        mgstt := '22'+SUBSTR(aluno->gstt,3,2)
                ELSEIF SUBSTR(aluno->gstt,1,2) = '22'
                        mgstt := '23'+SUBSTR(aluno->gstt,3,2)
                ENDIF
                msele := SELE()
                morde := INDEXORD()
                ****************
                SELE('contr')
                ****************
                ADIREG()
                contr->nome      := aluno->aluno
                contr->matricula := aluno->matricula
                contr->grau      := SUBSTR(mgstt,1,1)
                contr->serie     := SUBSTR(mgstt,2,1)
                contr->turma     := SUBSTR(mgstt,3,1)
                contr->turno     := SUBSTR(mgstt,4,1)
                contr->responsa  := aluno->responsa
                contr->profissao := aluno->r_prof
                contr->cpf       := aluno->r_cpf
                contr->rua       := aluno->r_rua
                contr->no        := aluno->r_num
                contr->compl     := aluno->r_apto
                contr->bairro    := aluno->r_bairro
                contr->cidade    := aluno->r_cidade
                contr->estado    := aluno->r_estado
                contr->cep       := aluno->r_cep
                DBCOMMIT()
                DBUNLOCK()
                *****************
                SELE(msele);ORDSETFOCUS(morde)
                *****************
                RETURN NIL
        ELSEIF mtipo_con = 2
                *****************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                *****************
                mcod_gstt := SPACE(4)
                DEVPOS(lci+3,cci+1);DEVOUT('GSTT.....:')
                @ lci+3,cci+12 GET mcod_gstt PICT '@!'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF ! aluno->(DBSEEK(mcod_gstt))
                        atencao('Nao foi possivel encontrar este GSTT')
                        LOOP
                ENDIF
                mensagem('Confirma o processo do CONTRATO [S/N]:')
                opcao := 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF opcao = 'N'
                        LOOP
                ENDIF
                mensagem('Espere o Final do Processo OK - [ESC]Abandonar')
                WHILE mcod_gstt = aluno->gstt .AND. ! EOF()
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mgstt := aluno->gstt
                        IF SUBSTR(aluno->gstt,1,2) = '01'
                                mgstt := '02'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '02'
                                mgstt := '03'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '03'
                                mgstt := '04'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '04'
                                mgstt := '05'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '05'
                                mgstt := '11'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '11'
                                mgstt := '12'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '12'
                                mgstt := '13'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '13'
                                mgstt := '14'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '14'
                                mgstt := '15'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '15'
                                mgstt := '16'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '16'
                                mgstt := '17'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '17'
                                mgstt := '18'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '18'
                                mgstt := '21'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '21'
                                mgstt := '22'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '22'
                                mgstt := '23'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '23'
                                SKIP
                                LOOP
                        ENDIF
                        msele := SELE()
                        morde := INDEXORD()
                        ****************
                        SELE('contr')
                        ****************
                        ADIREG()
                        contr->nome      := aluno->aluno
                        contr->matricula := aluno->matricula
                        contr->grau      := SUBSTR(mgstt,1,1)
                        contr->serie     := SUBSTR(mgstt,2,1)
                        contr->turma     := SUBSTR(mgstt,3,1)
                        contr->turno     := SUBSTR(mgstt,4,1)
                        contr->responsa  := aluno->responsa
                        contr->profissao := aluno->r_prof
                        contr->cpf       := aluno->r_cpf
                        contr->rua       := aluno->r_rua
                        contr->no        := aluno->r_num
                        contr->compl     := aluno->r_apto
                        contr->bairro    := aluno->r_bairro
                        contr->cidade    := aluno->r_cidade
                        contr->estado    := aluno->r_estado
                        contr->cep       := aluno->r_cep
                        DBCOMMIT()
                        DBUNLOCK()
                        *****************
                        SELE(msele);ORDSETFOCUS(morde)
                        *****************
                        SKIP
                ENDDO
                RETURN NIL
        ELSEIF mtipo_con = 3
                mindex := 0
                mensagem('Por ordem de:[1] p/Resp.[2]GSTT [3]Aluno [4] Matricula: ')
                @ 23,COL()+1 GET mindex PICT '9' VALID IF(mindex<1 .OR. mindex > 4,.F.,.T.)
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF mindex = 1
                        ***************
                        SELE('aluno');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                ELSEIF mindex = 2
                        ***************
                        SELE('aluno');ORDSETFOCUS(2)
                        GO TOP
                        ***************
                ELSEIF mindex = 3
                        ***************
                        SELE('aluno');ORDSETFOCUS(3)
                        GO TOP
                        ***************
                ELSE
                        ***************
                        SELE('aluno');ORDSETFOCUS(4)
                        GO TOP
                        ***************
                ENDIF
                mensagem('Confirma a Impressao do CONTRATO [S/N]:')
                opcao := 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF opcao = 'N'
                        LOOP
                ENDIF
                mensagem('Espere o Final do processo OK - [ESC]Abandonar')
                WHILE ! EOF()
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mgstt := aluno->gstt
                        IF SUBSTR(aluno->gstt,1,2) = '01'
                                mgstt := '02'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '02'
                                mgstt := '03'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '03'
                                mgstt := '04'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '04'
                                mgstt := '05'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '05'
                                mgstt := '11'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '11'
                                mgstt := '12'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '12'
                                mgstt := '13'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '13'
                                mgstt := '14'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '14'
                                mgstt := '15'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '15'
                                mgstt := '16'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '16'
                                mgstt := '17'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '17'
                                mgstt := '18'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '18'
                                mgstt := '21'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '21'
                                mgstt := '22'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '22'
                                mgstt := '23'+SUBSTR(aluno->gstt,3,2)
                        ELSEIF SUBSTR(aluno->gstt,1,2) = '23'
                                SKIP
                                LOOP
                        ENDIF
                        msele := SELE()
                        morde := INDEXORD()
                        ****************
                        SELE('contr')
                        ****************
                        ADIREG()
                        contr->nome      := aluno->aluno
                        contr->matricula := aluno->matricula
                        contr->grau      := SUBSTR(mgstt,1,1)
                        contr->serie     := SUBSTR(mgstt,2,1)
                        contr->turma     := SUBSTR(mgstt,3,1)
                        contr->turno     := SUBSTR(mgstt,4,1)
                        contr->responsa  := aluno->responsa
                        contr->profissao := aluno->r_prof
                        contr->cpf       := aluno->r_cpf
                        contr->rua       := aluno->r_rua
                        contr->no        := aluno->r_num
                        contr->compl     := aluno->r_apto
                        contr->bairro    := aluno->r_bairro
                        contr->cidade    := aluno->r_cidade
                        contr->estado    := aluno->r_estado
                        contr->cep       := aluno->r_cep
                        DBCOMMIT()
                        DBUNLOCK()
                        *****************
                        SELE(msele);ORDSETFOCUS(morde)
                        *****************
                        SKIP
                ENDDO
                RETURN NIL
        ENDIF
ENDDO

******************************* F I M ****************************************

