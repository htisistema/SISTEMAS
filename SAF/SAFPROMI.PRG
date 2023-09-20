*************************************************
* GERAR DISCO DE CARNE DE MATRICULA E PROMISSORIA
*************************************************

FUNCTION safpromi()
*******************
LOCAL mprg:='SAFPROMI'
LOCAL lci,cci,lba,cba,tela,mmatricula,mtipo_con,opcao,mindex,mpromi,mparc,mlin,lin1,;
      linha,linhas,msele,morde

PRIVATE mgstt,munidade:='MF',msituacao,mtipo_sit,m_nota:={}

IF ! ver_nivel(mprg,'GERAR DISCO CARNE P/MATRICULA E PROMISSORIA','15')
        RETURN NIL
ENDIF
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('safpromi','promi','E');RETURN NIL;ENDIF
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
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                SET DEVI TO PRINT
                * SET PRINT TO LPT1
                mpromi := 0
                mparc := 0
                mgstt := SPACE(4)
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
                ENDIF
                IF SUBSTR(mgstt,1,2) = '01' .OR. SUBSTR(mgstt,1,2) = '02'
                        mpromi := 2463.55
                        mparc := 205.30
                ELSEIF SUBSTR(mgstt,1,2) = '03' .OR. SUBSTR(mgstt,1,2) = '04';
                   .OR. SUBSTR(mgstt,1,2) = '05'
                        mpromi := 2510.48
                        mparc := 209.21
                ELSEIF SUBSTR(mgstt,1,2) = '11' .OR. SUBSTR(mgstt,1,2) = '12';
                   .OR. SUBSTR(mgstt,1,2) = '13' .OR. SUBSTR(mgstt,1,2) = '14'
                        mpromi := 2603.95
                        mparc := 217.00
                ELSEIF SUBSTR(mgstt,1,2) = '15' .OR. SUBSTR(mgstt,1,2) = '16';
                        .OR. SUBSTR(mgstt,1,2) = '17' .OR. SUBSTR(mgstt,1,2) = '18'
                        mpromi := 2684.07
                        mparc := 223.67
                ELSEIF SUBSTR(mgstt,1,2) = '21' .OR. SUBSTR(mgstt,1,2) = '22';
                        .OR. SUBSTR(mgstt,1,2) = '23'
                        mpromi := 2777.55
                        mparc := 231.46
                ENDIF
                msele := SELE()
                morde := INDEXORD()
                ****************
                SELE('promi')
                ****************
                ADIREG()
                promi->nome := aluno->aluno
                promi->matricula := aluno->matricula
                promi->grau := SUBSTR(mgstt,1,1)
                promi->serie:= SUBSTR(mgstt,2,1)
                promi->turma:= SUBSTR(mgstt,3,1)
                promi->turno:= SUBSTR(mgstt,4,1)
                promi->responsa := aluno->responsa
                promi->cpf := aluno->r_cpf
                promi->rua := aluno->r_rua
                promi->no := aluno->r_num
                promi->compl := aluno->r_apto
                promi->bairro := aluno->r_bairro
                promi->cidade := aluno->r_cidade
                promi->estado := aluno->r_estado
                promi->cep := aluno->cep
                promi->vlr_matri := mparc
                promi->vlr_promi := mpromi
                IF promi->vlr_promi = 2463.55
                        promi->extenso := 'dois mil quatrocentos e sessenta e tres reais e cinquenta e cinco centavos'
                ELSEIF promi->vlr_promi = 2510.48
                        promi->extenso := 'dois mil quinhentos e dez reais e quarenta e oito centavos'
                ELSEIF promi->vlr_promi = 2603.95
                        promi->extenso := 'dois mil seissentos e tres reais e noventa e cinco centavos'
                ELSEIF promi->vlr_promi = 2684.07
                        promi->extenso := 'dois mil seissentos e oitenta e quatro reais e sete centavos'
                ELSEIF promi->vlr_promi = 2777.55
                        promi->extenso := 'dois mil setecentos e setenta e sete reais e cinquenta e cinco centavos'
                ENDIF
                DBCOMMIT()
                DBUNLOCK()
                *****************
                SELE(msele);ORDSETFOCUS(morde)
                *****************
                RETURN NIL
        ELSEIF mtipo_con = 2
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
                mensagem('Confirma a geracao do DISCO [S/N]:')
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
                WHILE ! EOF()
                        IF ! EMPTY(aluno->dt_tranf) .OR. aluno->faixa_12 = 'Z'
                                SKIP
                                LOOP
                        ENDIF
                        mpromi := 0
                        mparc := 0
                        mgstt := SPACE(4)
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
                        IF SUBSTR(mgstt,1,2) = '01' .OR. SUBSTR(mgstt,1,2) = '02'
                                mpromi := 2463.55
                                mparc := 205.30
                        ELSEIF SUBSTR(mgstt,1,2) = '03' .OR. SUBSTR(mgstt,1,2) = '04';
                           .OR. SUBSTR(mgstt,1,2) = '05'
                                mpromi := 2510.48
                                mparc := 209.21
                        ELSEIF SUBSTR(mgstt,1,2) = '11' .OR. SUBSTR(mgstt,1,2) = '12';
                           .OR. SUBSTR(mgstt,1,2) = '13' .OR. SUBSTR(mgstt,1,2) = '14'
                                mpromi := 2603.95
                                mparc := 217.00
                        ELSEIF SUBSTR(mgstt,1,2) = '15' .OR. SUBSTR(mgstt,1,2) = '16';
                                .OR. SUBSTR(mgstt,1,2) = '17' .OR. SUBSTR(mgstt,1,2) = '18'
                                mpromi := 2684.07
                                mparc := 223.67
                        ELSEIF SUBSTR(mgstt,1,2) = '21' .OR. SUBSTR(mgstt,1,2) = '22';
                                .OR. SUBSTR(mgstt,1,2) = '23'
                                mpromi := 2777.55
                                mparc := 231.46
                        ENDIF
                        msele := SELE()
                        morde := INDEXORD()
                        ****************
                        SELE('promi')
                        ****************
                        ADIREG()
                        promi->nome := aluno->aluno
                        promi->matricula := aluno->matricula
                        promi->grau := SUBSTR(mgstt,1,1)
                        promi->serie:= SUBSTR(mgstt,2,1)
                        promi->turma:= SUBSTR(mgstt,3,1)
                        promi->turno:= SUBSTR(mgstt,4,1)
                        promi->responsa := aluno->responsa
                        promi->cpf := aluno->r_cpf
                        promi->rua := aluno->r_rua
                        promi->no := aluno->r_num
                        promi->compl := aluno->r_apto
                        promi->bairro := aluno->r_bairro
                        promi->cidade := aluno->r_cidade
                        promi->estado := aluno->r_estado
                        promi->cep := aluno->r_cep
                        promi->vlr_matri := mparc
                        promi->vlr_promi := mpromi
                        IF promi->vlr_promi = 2463.55
                                promi->extenso := 'dois mil quatrocentos e sessenta e tres reais e cinquenta e cinco centavos'
                        ELSEIF promi->vlr_promi = 2510.48
                                promi->extenso := 'dois mil quinhentos e dez reais e quarenta e oito centavos'
                        ELSEIF promi->vlr_promi = 2603.95
                                promi->extenso := 'dois mil seissentos e tres reais e noventa e cinco centavos'
                        ELSEIF promi->vlr_promi = 2684.07
                                promi->extenso := 'dois mil seissentos e oitenta e quatro reais e sete centavos'
                        ELSEIF promi->vlr_promi = 2777.55
                                promi->extenso := 'dois mil setecentos e setenta e sete reais e cinquenta e cinco centavos'
                        ENDIF
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
