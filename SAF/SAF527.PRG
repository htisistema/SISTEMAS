***************************************
* EMISSAO DE CARNE DE MATRICULA
***************************************

FUNCTION saf527()
*******************
LOCAL mprg:='SAF527'
LOCAL lci,cci,lba,cba,tela,mmatricula,mtipo_con,opcao,mindex,mpromi,mparc,mlin,lin1,;
      linha,linhas

PRIVATE mgstt,munidade:='MF',msituacao,mtipo_sit,m_nota:={},mcont_rec:=0

IF ! ver_nivel(mprg,'EMISSAO DE CARNE DE MATRICULA','125')
        RETURN NIL
ENDIF

*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
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
        @ lci+1,cci+4 PROMPT ' Individual ' MESSAGE 'Emissao de carne de MATRICULA INDIVIDUAL'
        @ lci+1,COL()+4 PROMPT '    Geral   ' MESSAGE 'Emissao de carne de MATRIUCLA em GERAL'
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
                * * SET PRINT TO LPT1
                mpromi := 0
                mparc := 0
                mgstt := SPACE(4)
                ASIZE(m_nota,0)
                boletim(aluno->matricula)
                msituacao := ' '
                i := 0
                FOR i = 1 TO LEN(m_nota)
                        IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                LOOP
                        ELSEIF munidade = 'MF'
                                // media final
                                IF m_nota[i,15] < 6 .AND. m_nota[i,11] < 6
                                        msituacao := '*'
                                ENDIF
                                LOOP
                        ENDIF
                NEXT
                IF EMPTY(m_nota) .AND. SUBSTR(aluno->gstt,1,1) <> '0'
                        msituacao := '*'
                ENDIF
                mgstt := aluno->gstt
                IF msituacao = ' '
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
                DEVPOS(PROW()+3,00);DEVOUT(PADC('CARNE DE MATRICULA',75))
                DEVPOS(PROW(),00);DEVOUT(PADC('CARNE DE MATRICULA',75))

                DEVPOS(PROW()+4,00);DEVOUT(CHR(14))
                DEVPOS(PROW(),00);DEVOUT(memp+resa)
                DEVPOS(PROW()+1,00);DEVOUT('Avenida Oliveira Lima, No. 824')
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT(memp+resa)
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW(),65);DEVOUT(memp+resa)
                DEVPOS(PROW()+1,00);DEVOUT('Boa Vista, Recife - PE')
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW()+1,00);DEVOUT('CNPJ/MF No. 10.977.585/0001-58')
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW()+1,45);DEVOUT('|')
                DEVPOS(PROW(),64);DEVOUT('|')

                DEVPOS(PROW()+1,00);DEVOUT('Matricula: '+aluno->matricula)
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT('Matr.: '+aluno->matricula)
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW(),65);DEVOUT('Matr.: '+aluno->matricula)
                DEVPOS(PROW(),00);DEVOUT('           '+aluno->matricula)
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT('       '+aluno->matricula)
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW(),65);DEVOUT('       '+aluno->matricula)

                DEVPOS(PROW()+1,00);DEVOUT('Nome:'+aluno->aluno)
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT('GSTT : '+mgstt)
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW(),65);DEVOUT('GSTT : '+mgstt)
                DEVPOS(PROW(),00);DEVOUT('     '+aluno->aluno)
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT('       '+mgstt)
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW(),65);DEVOUT('       '+mgstt)

                DEVPOS(PROW()+1,00);DEVOUT('GSTT: '+mgstt)
                DEVPOS(PROW(),12);DEVOUT('Parcela: MATRICULA')
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT('Parcela: 01')
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW(),65);DEVOUT('Parcela: 01')
                DEVPOS(PROW(),00);DEVOUT('      '+mgstt)
                DEVPOS(PROW(),12);DEVOUT('         MATRICULA')
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT('         01')
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW(),65);DEVOUT('         01')

                DEVPOS(PROW()+1,00);DEVOUT('Valor R$:'+TRANSFORM(mparc,'999.99'))
                DEVPOS(PROW(),18);DEVOUT('Data: ___/___/___')
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT('Data: __/__/__')
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW(),65);DEVOUT('Data: __/__/__')
                DEVPOS(PROW(),00);DEVOUT('         '+TRANSFORM(mparc,'999.99'))

                DEVPOS(PROW()+1,00);DEVOUT('Resp.: '+aluno->responsa)
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT('Valor R$:'+TRANSFORM(mparc,'999.99'))
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW(),65);DEVOUT('Valor R$:'+TRANSFORM(mparc,'999.99'))
                DEVPOS(PROW(),00);DEVOUT('       '+aluno->responsa)
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT('         '+TRANSFORM(mparc,'999.99'))
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW(),65);DEVOUT('         '+TRANSFORM(mparc,'999.99'))

                DEVPOS(PROW()+1,00);DEVOUT('Pai..: '+aluno->pai)
                DEVPOS(PROW(),00);DEVOUT('       '+aluno->pai)
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),64);DEVOUT('|')
                DEVPOS(PROW()+1,00);DEVOUT('Mae..: '+aluno->mae)
                DEVPOS(PROW(),45);DEVOUT('|')
                DEVPOS(PROW(),46);DEVOUT(LEFT(aluno->aluno,33))
                DEVPOS(PROW(),00);DEVOUT('       '+aluno->mae)

                * CORTE
                DEVPOS(PROW()+3,00);DEVOUT(REPLI('-',80))
                DEVPOS(PROW(),00);DEVOUT(REPLI('-',80))
                DEVPOS(PROW()+1,00);DEVOUT(PADC('Corte Aqui',80))
                DEVPOS(PROW(),00);DEVOUT(PADC('Corte Aqui',80))

                DEVPOS(PROW()+5,00);DEVOUT(PADC('NOTA PROMISSORIA',75))
                DEVPOS(PROW(),00);DEVOUT(PADC('NOTA PROMISSORIA',75))

                DEVPOS(PROW()+5,00);DEVOUT('Vencimento: ____ de ___________ de 19___')
                DEVPOS(PROW()+5,60);DEVOUT('R$: ('+TRANSFORM(mpromi,'9,999.99')+')')
                DEVPOS(PROW(),60);DEVOUT('R$: ('+TRANSFORM(mpromi,'9,999.99')+')')
                DEVPOS(PROW()+4,00);DEVOUT('Ao(s) _____________________________________________________________________')
                lin1 := 'pagarei por esta unica via de NOTA PROMISSORIA ao '+memp+resa+', CNPJ/MF No. '+mcgc_firm+' ou a sua ordem, a quantia de R$ '+TRANSFORM(mpromi,'9,999.99')+' ('+extenso(mpromi,.T.)+') em moeda corrente deste pais, pagavel em '+sis_cidade+'.'
                linhas := MLCOUNT(lin1,75)

                FOR linha = 1 TO  linhas
                        LIN := MEMOLINE(lin1,75,linha,,10)
                        DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(LIN),75))
                NEXT
                DEVPOS(PROW()+1,00);DEVOUT('EMITENTE: '+aluno->responsa)
                DEVPOS(PROW(),00);DEVOUT('          '+aluno->responsa)
                DEVPOS(PROW()+1,00);DEVOUT('CPF/CNPJ: '+aluno->r_cpf)
                DEVPOS(PROW(),00);DEVOUT('          '+aluno->r_cpf)
                DEVPOS(PROW()+1,00);DEVOUT('ENDERECO: '+aluno->r_rua+' No.'+r_num+' Compl.: '+r_apto)
                DEVPOS(PROW(),00);DEVOUT('          '+aluno->r_rua+' No.'+r_num+' Compl.: '+r_apto)
                DEVPOS(PROW()+1,00);DEVOUT('BAIRRO  : '+aluno->r_bairro+' CIDADE: '+r_cidade+' ESTADO: '+r_estado)
                DEVPOS(PROW(),00);DEVOUT('          '+aluno->r_bairro+'         '+r_cidade+'         '+r_estado)
                DEVPOS(PROW()+1,00);DEVOUT('ALUNO   : '+aluno->matricula+' - '+aluno->aluno)
                DEVPOS(PROW(),00);DEVOUT('          '+aluno->matricula+' - '+aluno->aluno)
                DEVPOS(PROW()+6,00);DEVOUT(PADC('____________________________________',75))
                DEVPOS(PROW()+1,00);DEVOUT(PADC('             ASSINATURA             ',75))
                DEVPOS(PROW(),00);DEVOUT(PADC('             ASSINATURA             ',75))
                EJECT
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
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
                mensagem('Confirma a Impressao dos CONTRATOS [S/N]:')
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
                * * SET PRINT TO LPT1
                WHILE ! EOF()
                        mcont_rec := 0
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mpromi := 0
                        mparc := 0
                        mgstt := SPACE(4)
                        msituacao := ' '
                        ASIZE(m_nota,0)
                        boletim(aluno->matricula)
                        i := 0
                        FOR i = 1 TO LEN(m_nota)
                                IF aluno->bloqueio = '*' .AND. aluno->bloqueio2 = '*' .AND. aluno->bloqueio3 = '*' .AND. aluno->bloqueio4 = '*' .AND. m_nota[i,1] = '14'
                                        LOOP
                                ELSEIF munidade = 'MF'
                                        // media final
                                        IF m_nota[i,15] < 6 .AND. m_nota[i,11] < 6
                                                msituacao := '*'
                                        ENDIF
                                        LOOP
                                ENDIF
                        NEXT
                        IF EMPTY(m_nota) .AND. SUBSTR(aluno->gstt,1,1) <> '0'
                                msituacao := '*'
                        ENDIF
                        mgstt := aluno->gstt
                        IF msituacao = ' '
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
                        ELSE
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
                        *PAG 1

                        DEVPOS(PROW()+3,00);DEVOUT(PADC('CARNE DE MATRICULA',75))
                        DEVPOS(PROW(),00);DEVOUT(PADC('CARNE DE MATRICULA',75))

                        DEVPOS(PROW()+4,00);DEVOUT(CHR(14))
                        DEVPOS(PROW(),00);DEVOUT(memp+resa)
                        DEVPOS(PROW()+1,00);DEVOUT('Avenida Oliveira Lima, No. 824')
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT(memp+resa)
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW(),65);DEVOUT(memp+resa)
                        DEVPOS(PROW()+1,00);DEVOUT('Boa Vista, Recife - PE')
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW()+1,00);DEVOUT('CNPJ/MF No. 10.977.585/0001-58')
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW()+1,45);DEVOUT('|')
                        DEVPOS(PROW(),64);DEVOUT('|')

                        DEVPOS(PROW()+1,00);DEVOUT('Matricula: '+aluno->matricula)
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT('Matr.: '+aluno->matricula)
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW(),65);DEVOUT('Matr.: '+aluno->matricula)
                        DEVPOS(PROW(),00);DEVOUT('           '+aluno->matricula)
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT('       '+aluno->matricula)
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW(),65);DEVOUT('       '+aluno->matricula)

                        DEVPOS(PROW()+1,00);DEVOUT('Nome:'+aluno->aluno)
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT('GSTT : '+mgstt)
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW(),65);DEVOUT('GSTT : '+mgstt)
                        DEVPOS(PROW(),00);DEVOUT('     '+aluno->aluno)
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT('       '+mgstt)
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW(),65);DEVOUT('       '+mgstt)

                        DEVPOS(PROW()+1,00);DEVOUT('GSTT: '+mgstt)
                        DEVPOS(PROW(),12);DEVOUT('Parcela: MATRICULA')
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT('Parcela: 01')
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW(),65);DEVOUT('Parcela: 01')
                        DEVPOS(PROW(),00);DEVOUT('      '+mgstt)
                        DEVPOS(PROW(),12);DEVOUT('         MATRICULA')
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT('         01')
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW(),65);DEVOUT('         01')

                        DEVPOS(PROW()+1,00);DEVOUT('Valor R$:'+TRANSFORM(mparc,'999.99'))
                        DEVPOS(PROW(),18);DEVOUT('Data: ___/___/___')
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT('Data: __/__/__')
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW(),65);DEVOUT('Data: __/__/__')
                        DEVPOS(PROW(),00);DEVOUT('         '+TRANSFORM(mparc,'999.99'))

                        DEVPOS(PROW()+1,00);DEVOUT('Resp.: '+aluno->responsa)
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT('Valor R$:'+TRANSFORM(mparc,'999.99'))
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW(),65);DEVOUT('Valor R$:'+TRANSFORM(mparc,'999.99'))
                        DEVPOS(PROW(),00);DEVOUT('       '+aluno->responsa)
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT('         '+TRANSFORM(mparc,'999.99'))
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW(),65);DEVOUT('         '+TRANSFORM(mparc,'999.99'))

                        DEVPOS(PROW()+1,00);DEVOUT('Pai..: '+aluno->pai)
                        DEVPOS(PROW(),00);DEVOUT('       '+aluno->pai)
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),64);DEVOUT('|')
                        DEVPOS(PROW()+1,00);DEVOUT('Mae..: '+aluno->mae)
                        DEVPOS(PROW(),45);DEVOUT('|')
                        DEVPOS(PROW(),46);DEVOUT(LEFT(aluno->aluno,33))
                        DEVPOS(PROW(),00);DEVOUT('       '+aluno->mae)

                        * CORTE
                        DEVPOS(PROW()+3,00);DEVOUT(REPLI('-',80))
                        DEVPOS(PROW(),00);DEVOUT(REPLI('-',80))
                        DEVPOS(PROW()+1,00);DEVOUT(PADC('Corte Aqui',80))
                        DEVPOS(PROW(),00);DEVOUT(PADC('Corte Aqui',80))

                        DEVPOS(PROW()+5,00);DEVOUT(PADC('NOTA PROMISSORIA',75))
                        DEVPOS(PROW(),00);DEVOUT(PADC('NOTA PROMISSORIA',75))

                        DEVPOS(PROW()+5,00);DEVOUT('Vencimento: ____ de ___________ de 19___')
                        DEVPOS(PROW()+5,60);DEVOUT('R$: ('+TRANSFORM(mpromi,'9,999.99')+')')
                        DEVPOS(PROW(),60);DEVOUT('R$: ('+TRANSFORM(mpromi,'9,999.99')+')')
                        DEVPOS(PROW()+4,00);DEVOUT('Ao(s) _____________________________________________________________________')
                        lin1 := 'pagarei por esta unica via de NOTA PROMISSORIA ao '+memp+resa+', CNPJ/MF No. '+mcgc_firm+' ou a sua ordem, a quantia de R$ '+TRANSFORM(mpromi,'9,999.99')+' ('+extenso(mpromi,.T.)+') em moeda corrente deste pais, pagavel em '+sis_cidade+'.'
                        linhas := MLCOUNT(lin1,75)

                        FOR linha = 1 TO  linhas
                                LIN := MEMOLINE(lin1,75,linha,,10)
                                DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(LIN),75))
                        NEXT
                        DEVPOS(PROW()+1,00);DEVOUT('EMITENTE: '+aluno->responsa)
                        DEVPOS(PROW(),00);DEVOUT('          '+aluno->responsa)
                        DEVPOS(PROW()+1,00);DEVOUT('CPF/CNPJ: '+aluno->r_cpf)
                        DEVPOS(PROW(),00);DEVOUT('          '+aluno->r_cpf)
                        DEVPOS(PROW()+1,00);DEVOUT('ENDERECO: '+aluno->r_rua+' No.'+r_num+' Compl.: '+r_apto)
                        DEVPOS(PROW(),00);DEVOUT('          '+aluno->r_rua+' No.'+r_num+' Compl.: '+r_apto)
                        DEVPOS(PROW()+1,00);DEVOUT('BAIRRO  : '+aluno->r_bairro+' CIDADE: '+r_cidade+' ESTADO: '+r_estado)
                        DEVPOS(PROW(),00);DEVOUT('          '+aluno->r_bairro+'         '+r_cidade+'         '+r_estado)
                        DEVPOS(PROW()+1,00);DEVOUT('ALUNO   : '+aluno->matricula+' - '+aluno->aluno)
                        DEVPOS(PROW(),00);DEVOUT('          '+aluno->matricula+' - '+aluno->aluno)
                        DEVPOS(PROW()+6,00);DEVOUT(PADC('____________________________________',75))
                        DEVPOS(PROW()+1,00);DEVOUT(PADC('             ASSINATURA             ',75))
                        DEVPOS(PROW(),00);DEVOUT(PADC('             ASSINATURA             ',75))
                        EJECT
                        SKIP
                ENDDO
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                RETURN NIL
        ENDIF
ENDDO
