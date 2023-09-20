***************************************
* EMISSAO DE CONTRATOS
***************************************

FUNCTION saf526()
*******************
LOCAL mprg:='SAF526'
LOCAL lci,cci,lba,cba,tela,mmatricula,mtipo_con,opcao,mindex,mcod_gstt
PRIVATE mgstt,munidade:='MF',msituacao,mtipo_sit,m_nota:={},mcont_rec:=0

IF ! ver_nivel(mprg,'RELATORIO EMISSAO DE CONTRATOS','125')
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
                contrato()
                EJECT
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
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
*               * * SET PRINT TO LPT1
                SET PRINT TO CONTRATO.TXT
                WHILE mcod_gstt = aluno->gstt .AND. ! EOF()
                        mcont_rec := 0
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
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
                        contrato()
                        EJECT
                        SKIP
                ENDDO
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
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
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                SET DEVI TO PRINT
*               * * SET PRINT TO LPT1
                SET PRINT TO CONTRATO.TXT
                WHILE ! EOF()
                        mcont_rec := 0
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
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
                        *PAG 1
                        DEVPOS(PROW()+1,00);DEVOUT(CHR(14))
                        DEVPOS(PROW(),00);DEVOUT('COLEGIO NOBREGA')
                        DEVPOS(PROW()+1,00);DEVOUT('Avenida Oliveira Lima, No 824,')
                        DEVPOS(PROW()+1,00);DEVOUT('Boa Vista, Recife - PE.')
                        DEVPOS(PROW()+1,00);DEVOUT('CGC/MF No 10.977.585/0001-58')

                        DEVPOS(PROW()+4,00);DEVOUT(PADC('CONTRATO DE PRESTACAO DE SERVICOS',75))
                        DEVPOS(PROW(),00);DEVOUT(PADC('CONTRATO DE PRESTACAO DE SERVICOS',75))
                        DEVPOS(PROW()+1,00);DEVOUT(PADC('EDUCACIONAIS',75))
                        DEVPOS(PROW(),00);DEVOUT(PADC('EDUCACIONAIS',75))
                        DEVPOS(PROW()+1,00);DEVOUT(PADC('ANO LETIVO DE 1999',75))
                        DEVPOS(PROW(),00);DEVOUT(PADC('ANO LETIVO DE 1999',75))

                        DEVPOS(PROW()+4,00);DEVOUT('INSTRUMENTO PARTICULAR DE CONTRATO DE PRESTACAO  DE  SERVICOS  EDUCACIONAIS')
                        DEVPOS(PROW()+1,00);DEVOUT('QUE ENTRE SI FAZEM, DE UM LADO, O COLEGIO NOBREGA, DEVIDAMENTE INSCRITO  NO')
                        DEVPOS(PROW()+1,00);DEVOUT('CGC/MF No 10.977.585/0001-58, COM SEDE A AVENIDA OLIVEIRA LIMA, No 824, BOA')
                        DEVPOS(PROW()+1,00);DEVOUT('VISTA, RECIFE-PE., NESTE ATO REPRESENTADO PELO  SEU  DIRETOR, O Pe. EUGENIO')
                        DEVPOS(PROW()+1,00);DEVOUT('PACELLI  CORREIA  AGUIAR S.J., BRASILEIRO, SOLTEIRO, SACERDOTE, RESIDENTE E')
                        DEVPOS(PROW()+1,00);DEVOUT('DOMICILIADO  NESTA  CIDADE  DE   RECIFE-PE., DORAVANTE  DENOMINADO SIMPLES-')
                        DEVPOS(PROW()+1,00);DEVOUT('MENTE DE COLEGIO, E  DE  OUTRO  LADO, O(A)   RESPONSAVEL  PELO(A) ALUNO(A):')
                        DEVPOS(PROW(),00);DEVOUT('         COLEGIO                                                           ')

                        DEVPOS(PROW()+1,00);DEVOUT(aluno->aluno+' MATRICULA: '+aluno->matricula+' DA MANHA DA '+SUBSTR(mgstt,2,2))
                        DEVPOS(PROW(),00);DEVOUT(aluno->aluno+'            '+aluno->matricula+'    MANHA    '+SUBSTR(mgstt,2,2))
                        DEVPOS(PROW()+1,00);DEVOUT('DO '+SUBSTR(mgstt,1,1)+' GRAU, SR.(A) '+aluno->responsa)
                        DEVPOS(PROW(),00);DEVOUT('   '+SUBSTR(mgstt,1,1)+'              '+aluno->responsa)
                        DEVPOS(PROW()+1,00);DEVOUT('ESTADO CIVIL:_______ NACIONALIDADE _________,PROFISSAO '+aluno->r_prof)
                        DEVPOS(PROW(),00);DEVOUT('                                                       '+aluno->r_prof)
                        DEVPOS(PROW()+1,00);DEVOUT('IDENTIDADE No ___________, ORGAO EXPEDIDOR ______ CPF/MF No '+aluno->r_cpf)
                        DEVPOS(PROW(),00);DEVOUT('                                                            '+aluno->r_cpf)
                        DEVPOS(PROW()+1,00);DEVOUT(', RESIDENTE E DOMICILIADO(A) A '+aluno->r_rua+',No '+aluno->r_num)
                        DEVPOS(PROW(),00);DEVOUT('                               '+aluno->r_rua+',No '+aluno->r_num)
                        DEVPOS(PROW()+1,00);DEVOUT('COMPLEMENTO '+aluno->r_apto+',  BAIRRO/DISTRITO '+aluno->r_bairro)
                        DEVPOS(PROW(),00);DEVOUT('            '+aluno->r_apto+'                   '+aluno->r_bairro)
                        DEVPOS(PROW()+1,00);DEVOUT(', CIDADE '+aluno->r_cidade+', ESTADO '+aluno->r_estado+', CEP '+aluno->r_cep)
                        DEVPOS(PROW(),00);DEVOUT('         '+aluno->r_cidade+',        '+aluno->r_estado+'      '+aluno->r_cep)
                        DEVPOS(PROW()+1,00);DEVOUT('DORAVANTE DENOMINADO(A) SIMPLESMENTE DE CONTRATANTE, TEM ENTRE SI, JUSTO  E')
                        DEVPOS(PROW()+1,00);DEVOUT(IMPAC("PACTUADO AS CLAUSULAS E CONDICOES A SEGUIR:",.T.))

                        DEVPOS(PROW()+2,00);DEVOUT(IMPAC("CLAUSULA PRIMEIRA - FUNDAMENTO LEGAL DO CONTRATO:                          "))
                        DEVPOS(PROW(),00);DEVOUT(IMPAC("CLAUSULA PRIMEIRA - FUNDAMENTO LEGAL DO CONTRATO:                          "))

                        DEVPOS(PROW()+2,00);DEVOUT('- O presente contrato firmado pelas partes contratantes tem por  fundamento')
                        DEVPOS(PROW()+1,00);DEVOUT('  legal:                                                                   ')
                        DEVPOS(PROW()+2,00);DEVOUT('(a) CONSTITUICAO FEDERAL DE 1988: art. 1o,IV;art.206,II e III, e, art.209; ')
                        DEVPOS(PROW()+2,00);DEVOUT('(b) CODIGO CIVIL BRASILEIRO:arts.81,82 e 1.092;    ')
                        DEVPOS(PROW()+2,00);DEVOUT('(c) LEI No 8.069/90;                                                       ')
                        DEVPOS(PROW()+2,00);DEVOUT('(d) LEI No 8.078/90, com a redacao dada ao art.39, pela MP-1.477-53 de 24  ')
                        DEVPOS(PROW()+2,00);DEVOUT('de setembro de 1998;                                                       ')
                        DEVPOS(PROW()+2,00);DEVOUT('(e) LEI No 9.131/95;                                                       ')
                        DEVPOS(PROW()+2,00);DEVOUT('(f) LEI No 9.394/96                                                        ')
                        DEVPOS(PROW()+2,00);DEVOUT('(g) MEDIDADE PROVISORIA No 1.477-53, DE 24 DE SETEMBRO DE 1998.            ')


                        EJECT
                        *PAG 2

                        DEVPOS(PROW()+1,00);DEVOUT(IMPAC('CLAUSULA SEGUNDA-DA FORMACAO DO VALOR DAS PRESTACOES ESCOLARES:            '))
                        DEVPOS(PROW(),00);DEVOUT(IMPAC('CLAUSULA SEGUNDA-DA FORMACAO DO VALOR DAS PRESTACOES ESCOLARES:            '))

                        DEVPOS(PROW()+2,00);DEVOUT('O valor total anual das mensalidades escolares ora contratadas, e o  resul-')
                        DEVPOS(PROW()+1,00);DEVOUT('do da multiplacacao do quanto da parcela cobrada no mes de dezembro de 1997')
                        DEVPOS(PROW()+1,00);DEVOUT(', multiplicado por 12(doze) meses,acrescido do montante apurado na planilha')
                        DEVPOS(PROW()+1,00);DEVOUT('de custos para o ano letivo de 1999, sendo o total encontrado, dividido em ')
                        DEVPOS(PROW()+1,00);DEVOUT('12(doze) parcelas, que corresponderao  as  12(doze)  prestacoes  que  serao')
                        DEVPOS(PROW()+1,00);DEVOUT('cobradas no ano letivo de 1999, ficando de  logo  convencionado  que  as 03')
                        DEVPOS(PROW()+1,00);DEVOUT('(tres) primeiras parcelas, as dos meses de janeiro,fevereiro e marco, serao')
                        DEVPOS(PROW()+1,00);DEVOUT('cobradas no mesmo valor da ultima parcela do mes de dezembro de 1998, e  as')
                        DEVPOS(PROW()+1,00);DEVOUT('demais, a partir do mes de abril de 1999, com o reajuste resultante da ope-')
                        DEVPOS(PROW()+1,00);DEVOUT('racao definida nesta clausula.                                             ')

                        DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA TERCEIRA-DA PUBLICACAO DA PLANILHA DE CUSTOS PARA O ANO LETIVO  DE')
                        DEVPOS(PROW(),00);DEVOUT('CLAUSULA TERCEIRA-DA PUBLICACAO DA PLANILHA DE CUSTOS PARA O ANO LETIVO  DE')
                        DEVPOS(PROW()+1,00);DEVOUT('1999, DOS TERMOS DESTE CONTRATO E DO NUMERO DE VAGAS:                      ')
                        DEVPOS(PROW(),00);DEVOUT('1999, DOS TERMOS DESTE CONTRATO E DO NUMERO DE VAGAS:                      ')

                        DEVPOS(PROW()+2,00);DEVOUT('O(A) CONTRATANTE declara expressamente que teve conhecimento previo, a mais')
                        DEVPOS(PROW(),00);DEVOUT('     CONTRATANTE                                                           ')
                        DEVPOS(PROW()+1,00);DEVOUT('de 45(quarenta e cinco) dias antes da sua decisao de firmar o presente con-')
                        DEVPOS(PROW()+1,00);DEVOUT('trato, atraves de publicacao feita  pelo  COLEGIO, em quadros de avisos, da')
                        DEVPOS(PROW(),00);DEVOUT('                                          COLEGIO, em quadros de avisos    ')
                        DEVPOS(PROW()+1,00);DEVOUT('planilha de custos para o ano letivo de 1999, do texto  da  proposta  deste')
                        DEVPOS(PROW()+1,00);DEVOUT('contrato e do numero de vagas oferecidas pelas salas de cada turma,  e  que')
                        DEVPOS(PROW()+1,00);DEVOUT('por isso esta decidindo com plena liberdade em anuir com o mesmo.          ')

                        DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA QUARTA-DO OBJETIVO DO CONTRATO:                                   ')
                        DEVPOS(PROW(),00);DEVOUT('CLAUSULA QUARTA-DO OBJETIVO DO CONTRATO:                                   ')

                        DEVPOS(PROW()+2,00);DEVOUT('O objeto do presente contrato e a prestacao de servicos educacionais conti-')
                        DEVPOS(PROW()+1,00);DEVOUT('nuos, no ano letivo de 1999, correspondente a  '+SUBSTR(mgstt,2,1)+'  serie, do '+SUBSTR(mgstt,1,1)+'  grau, em')
                        DEVPOS(PROW()+1,00);DEVOUT('estrito cumprimento ao que disciplina a legislacao do ensino medio em vigor')
                        DEVPOS(PROW()+1,00);DEVOUT(', e ao Regimento Interno do COLEGIO, a(ao) dependente do(a) CONTRATANTE,que')
                        DEVPOS(PROW(),00);DEVOUT('                            COLEGIO                         CONTRATANTE    ')
                        DEVPOS(PROW()+1,00);DEVOUT('por si, e por ele(a) se compromete a observar e cumprir.                   ')

                        DEVPOS(PROW()+2,00);DEVOUT('& PRIMEIRO-DA FORMA DE CUMPRIMENTO DO OBJETIVO DO CONTRATO:                ')
                        DEVPOS(PROW(),00);DEVOUT('& PRIMEIRO-DA FORMA DE CUMPRIMENTO DO OBJETIVO DO CONTRATO:                ')

                        DEVPOS(PROW()+2,00);DEVOUT('O COLEGIO se compromete a ministrar a instrucao atraves de aulas  e  demais')
                        DEVPOS(PROW(),00);DEVOUT('  COLEGIO                                                                  ')
                        DEVPOS(PROW()+1,00);DEVOUT('atividades correlatas, devendo o plano  de aulas, programas  e  curriculos,')
                        DEVPOS(PROW()+1,00);DEVOUT('escolares, estar conforme a legislacao pertinente;                         ')

                        DEVPOS(PROW()+2,00);DEVOUT('& SEGUNDO-LOCAL DE CUMPRIMENTO DO OBJETIVO DO CONTRATO:                    ')
                        DEVPOS(PROW(),00);DEVOUT('& SEGUNDO-LOCAL DE CUMPRIMENTO DO OBJETIVO DO CONTRATO:                    ')

                        DEVPOS(PROW()+2,00);DEVOUT('A instrucao sera ministrada nas salas de aula do estabelecimento do COLEGIO,')
                        DEVPOS(PROW(),00);DEVOUT('                                                                    COLEGIO,')
                        DEVPOS(PROW()+1,00);DEVOUT('a Avenida Oliveira Lima, No 824, Boa Vista, Recife-PE., ou em outros locais')
                        DEVPOS(PROW()+1,00);DEVOUT('que o mesmo venha a orientar, tendo em vista a natureza do  conteudo  e  da')
                        DEVPOS(PROW()+1,00);DEVOUT('tecnica pedagogica exigidas no momento oportuno.                           ')

                        DEVPOS(PROW()+2,00);DEVOUT('& TERCEIRO-DA SUBORDINACAO AO REGIMENTO INTERNO:                           ')
                        DEVPOS(PROW(),00);DEVOUT('& TERCEIRO-DA SUBORDINACAO AO REGIMENTO INTERNO:                           ')

                        DEVPOS(PROW()+2,00);DEVOUT('O(A) Dependente beneficiario(a) estara sujeito(a) as  Normas  do  Regimento')
                        DEVPOS(PROW()+1,00);DEVOUT('Interno do COLEGIO, a disposicao dos interessados nas respectivas coordena-')
                        DEVPOS(PROW(),00);DEVOUT('           COLEGIO')
                        DEVPOS(PROW()+1,00);DEVOUT('coes pedagogicas, e cujas normatizacoes fazem parte integrante do  presente')
                        DEVPOS(PROW()+1,00);DEVOUT('contrato, para todos os efeitos legais.                                    ')

                        DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA QUINTA-DOS SERVICOS NAO COBERTOS PELO CONTRATO:                   ')
                        DEVPOS(PROW(),00);DEVOUT('CLAUSULA QUINTA-DOS SERVICOS NAO COBERTOS PELO CONTRATO:                   ')

                        DEVPOS(PROW()+2,00);DEVOUT('Nao estao incluidos neste contrato, os servicos especiais  de  recuperacao,')
                        DEVPOS(PROW()+1,00);DEVOUT('reforco, dependencia, adaptacao, reciclagem, transporte escolar, as escoli-')
                        DEVPOS(PROW()+1,00);DEVOUT('nhas de esportes e dancas, os opcionais e  de  uso  facultativo  para  o(a)')
                        DEVPOS(PROW()+1,00);DEVOUT('dependente beneficiario(a), bem como uniformes, merenda e material didatico')
                        DEVPOS(PROW()+1,00);DEVOUT('de uso individual e obrigatorio, inclusive, uma resma de papel  que  devera')
                        DEVPOS(PROW()+1,00);DEVOUT('ser entregue no ato da matricula ao COLEGIO, e a agenda escolar, que devera')
                        DEVPOS(PROW(),00);DEVOUT('                                    COLEGIO')
                        DEVPOS(PROW()+1,00);DEVOUT('ser adquirida na mesma ocasiao, junto ao referido contratado, e os que tam-')
                        DEVPOS(PROW()+1,00);DEVOUT('bem poderao ser objeto de ajuste a parte, e ainda  a  segunda  e  seguintes')
                        DEVPOS(PROW()+1,00);DEVOUT('vias de documentos escolares e carnes de pagamento.                        ')

                        EJECT
                        *PAG 3

                        DEVPOS(PROW()+1,00);DEVOUT('CLAUSULA SEXTA-DA REMUNERACAO DOS SERVICOS:                                ')
                        DEVPOS(PROW(),00);DEVOUT('CLAUSULA SEXTA-DA REMUNERACAO DOS SERVICOS:                                ')
                        DEVPOS(PROW()+2,00);DEVOUT('Pelos servicos sumariados  na  CLAUSULA  quarta  e  seus  paragrafos, o (a)')
                        DEVPOS(PROW()+1,00);DEVOUT('CONTRATANTE, pagara ao COLEGIO, a anuidade de,                             ')
                        DEVPOS(PROW(),00);DEVOUT('CONTRATANTE            COLEGIO')

                        DEVPOS(PROW()+2,00);DEVOUT('PARA A EDUCACAO INFANTIL. R$ 2.323,68(dois mil trezentos  e  vinte  e  tres')
                        DEVPOS(PROW(),00);DEVOUT('PARA A EDUCACAO INFANTIL.')
                        DEVPOS(PROW()+1,00);DEVOUT('reais e sessenta e oito centavos) dividida  em  12 (doze) parcelas  mensais')
                        DEVPOS(PROW()+1,00);DEVOUT('sendo as 03(tres) parcelas iniciais de R$ 188,00 (cento e  oitenta  e  oito')
                        DEVPOS(PROW()+1,00);DEVOUT('reais) e as 09(nove) parcelas restantes, de  R$ 195,52 (cento  e  noventa e')
                        DEVPOS(PROW()+1,00);DEVOUT('cinco reais e cinquenta e dois centavos);                                  ')

                        DEVPOS(PROW()+2,00);DEVOUT('PARA O ENSINO FUNDAMENTAL (1a a 4a SERIE),R$ 2.410,20(dois mil quatrocentos')
                        DEVPOS(PROW(),00);DEVOUT('PARA O ENSINO FUNDAMENTAL (1a a 4a SERIE)')
                        DEVPOS(PROW()+1,00);DEVOUT('e dez reais e vinta centavos) dividida em 12(doze) parcelas  mensais, sendo')
                        DEVPOS(PROW()+1,00);DEVOUT('as 03(tres) parcelas iniciais de R$ 195,00 (cento e noventa e cinco reais),')
                        DEVPOS(PROW()+1,00);DEVOUT('e as 09 (nove) parcelas restantes  de  R$ 202,80 (duzentos  e  dois reais e')
                        DEVPOS(PROW()+1,00);DEVOUT('oitenta centavos);                                  ')

                        DEVPOS(PROW()+2,00);DEVOUT('PARA O ENSINO FUNDAMENTAL  (5a a 8a SERIE), R$ 2.484,36(dois mil quatrocen-')
                        DEVPOS(PROW(),00);DEVOUT('PARA O ENSINO FUNDAMENTAL  (5a a 8a SERIE)')
                        DEVPOS(PROW()+1,00);DEVOUT('tos e oitenta e quatro reais e trinta e seis centavos) dividida em 12(doze)')
                        DEVPOS(PROW()+1,00);DEVOUT('parcelas mensais,sendo as 03(tres) parcelas iniciais de R$ 201,00 (duzentos')
                        DEVPOS(PROW()+1,00);DEVOUT('e um reais), e as 09(nove) parcelas restantes de R$ 209,04 (duzentos e nove')
                        DEVPOS(PROW()+1,00);DEVOUT('reais e quatro centavos);                               ')

                        DEVPOS(PROW()+2,00);DEVOUT('PARA O ENSINO MEDIO(1o. AO 3o.), R$ 2.570,88(dois mil quinhentos e  setenta')
                        DEVPOS(PROW(),00);DEVOUT('PARA O ENSINO MEDIO(1o. AO 3o.)')
                        DEVPOS(PROW()+1,00);DEVOUT('reais e oitenta e oito centavos) dividida  em 12 (doze)  parcelas  mensais,')
                        DEVPOS(PROW()+1,00);DEVOUT('sendo as 03(tres)parcelas iniciais de R$ 208,00(duzentos e oito reais),e as')
                        DEVPOS(PROW()+1,00);DEVOUT('09(nove) parcelas restantes, de R$ 216,32(duzentos e dezeseis reais e trin-')
                        DEVPOS(PROW()+1,00);DEVOUT('ta e dois centavos).                                          ')

                        DEVPOS(PROW()+2,00);DEVOUT('& PRIMEIRO-DO PAGAMENTO DA PRIMEIRA PARCELA E DAS DEMAIS PARCELAS:         ')
                        DEVPOS(PROW(),00);DEVOUT('& PRIMEIRO-DO PAGAMENTO DA PRIMEIRA PARCELA E DAS DEMAIS PARCELAS:         ')

                        DEVPOS(PROW()+2,00);DEVOUT('A primeira prestacao, representada por carne de pagamento, sera paga no ato')
                        DEVPOS(PROW()+1,00);DEVOUT('da matricula, e as demais, em numero de onze(11), tambem representadas  por')
                        DEVPOS(PROW()+1,00);DEVOUT('carnes de pagamento, serao pagas sucessivamente, a segunda, no dia 01 feve-')
                        DEVPOS(PROW()+1,00);DEVOUT('reiro de 1999, e as demais, a cada dia primeiro de cada mes subsequente,ate')
                        DEVPOS(PROW()+1,00);DEVOUT('a ultima prestacao, que tera vencimento  no  dia  1o  de  dezembro de 1999,')
                        DEVPOS(PROW()+1,00);DEVOUT('ficando de logo pactuado, entre as parte contratantes, uma tolerancia ate o')
                        DEVPOS(PROW()+1,00);DEVOUT('5o(quinto) dia util do mes do vencimento, para que seja feito  o  pagamento')
                        DEVPOS(PROW()+1,00);DEVOUT('sem acrescimo.                                                             ')

                        DEVPOS(PROW()+2,00);DEVOUT('& SEGUNDO-DOS ENCARGOS EM RAZAO DE INADIMPLENCIA:                          ')
                        DEVPOS(PROW(),00);DEVOUT('& SEGUNDO-DOS ENCARGOS EM RAZAO DE INADIMPLENCIA:                          ')

                        DEVPOS(PROW()+2,00);DEVOUT('O atraso no pagamento de qualquer das parcelas,em razao dos fixados no &1o.')
                        DEVPOS(PROW()+1,00);DEVOUT('desta clausula, sujeitara o(a) CONTRATANTE ao pagamento adicional ao  valor')
                        DEVPOS(PROW(),00);DEVOUT('                               CONTRATANTE ao pagamento adicional ao  valor')
                        DEVPOS(PROW()+1,00);DEVOUT('principal, dos seguintes encargos:')
                        DEVPOS(PROW(),00);DEVOUT('principal')
                        DEVPOS(PROW()+1,00);DEVOUT('(a) Multa contratual de 2%(dois por cento) sobre a mensalidade en atraso;')
                        DEVPOS(PROW()+1,00);DEVOUT('(b) Atualizacao do valor de debito, pelo indice oficial mais  favoravel  ao')
                        DEVPOS(PROW()+1,00);DEVOUT('COLEGIO;')
                        DEVPOS(PROW(),00);DEVOUT('COLEGIO;')
                        DEVPOS(PROW()+1,00);DEVOUT('(c) Juros de mora')

                        DEVPOS(PROW()+2,00);DEVOUT('& TERCEIRO-DA COBRANCA DE MENSALIDADES EM ATRASO:                         ')
                        DEVPOS(PROW(),00);DEVOUT('& TERCEIRO-DA COBRANCA DE MENSALIDADES EM ATRASO:                         ')

                        DEVPOS(PROW()+2,00);DEVOUT('Em havendo atraso  no  pagamento  de  qualquer  das  parcelas  por  periodo')
                        DEVPOS(PROW()+1,00);DEVOUT('superior a sessenta(60) dias, o COLEGIO, a seu criterio, podera  encaminhar')
                        DEVPOS(PROW(),00);DEVOUT('                                COLEGIO, a seu criterio')
                        DEVPOS(PROW()+1,00);DEVOUT('os titulos para protesto e depois executa-los judicialmente,ou encaminha-los')
                        DEVPOS(PROW()+1,00);DEVOUT('para cobranca por advogado e/ou pelo Servico de Protecao ao Credito  (SPC),')
                        DEVPOS(PROW()+1,00);DEVOUT('casos em que, alem dos encargos referidos no paragrafo anterior, o(a)  CON-')
                        DEVPOS(PROW(),00);DEVOUT('                                                                       CON-')
                        DEVPOS(PROW()+1,00);DEVOUT('TRATANTE ainda tera que arcar  com  o  pagamento  das  despesas  judiciais,')
                        DEVPOS(PROW(),00);DEVOUT('TRATANTE')
                        DEVPOS(PROW()+1,00);DEVOUT('custas, diligencias, emolumentos, e com o pagamento dos honorarios  advoca-')
                        DEVPOS(PROW()+1,00);DEVOUT('ticios, FICANDO DE LOGO CONVENCIONADO, para o caso, que nao sera renovado o')
                        DEVPOS(PROW()+1,00);DEVOUT('contrato para o ano letivo de 2000, renunciando de logo o(a) CONTRATANTE ao')
                        DEVPOS(PROW(),00);DEVOUT('                                                             CONTRATANTE   ')
                        DEVPOS(PROW()+1,00);DEVOUT('direito de preferencia que lhe confere o art.5o, da Medida Provisoria de No')
                        DEVPOS(PROW()+1,00);DEVOUT('1.477-53, de 24 de setembro 1998.                                        ')

                        EJECT
                        *PAG 4

                        DEVPOS(PROW()+2,00);DEVOUT('& QUARTO-DA ENTREGA DOS CARNES DE PAGAMENTO:                               ')
                        DEVPOS(PROW(),00);DEVOUT('& QUARTO-DA ENTREGA DOS CARNES DE PAGAMENTO:                               ')

                        DEVPOS(PROW()+2,00);DEVOUT('O(a) CONTRATANTE recebera mensalmente o carne de pagamento, ate o dia 30 do')
                        DEVPOS(PROW(),00);DEVOUT('     CONTRATANTE')
                        DEVPOS(PROW()+1,00);DEVOUT('mes anterior ao do vencimento, no COLEGIO, no valor  da  parcela, para  ser')
                        DEVPOS(PROW(),00);DEVOUT('                                  COLEGIO')
                        DEVPOS(PROW()+1,00);DEVOUT('pago na rede bancaria, e o ato da entrega ficara configurado pela  aposicao')
                        DEVPOS(PROW()+1,00);DEVOUT('da assinatura do(a) mesmo(a), ou de pessoa  por  ele(a)  credenciado(a), na')
                        DEVPOS(PROW()+1,00);DEVOUT('lista de entrega de carnes, existente no COLEGIO.                          ')
                        DEVPOS(PROW(),00);DEVOUT('                                         COLEGIO.                          ')

                        DEVPOS(PROW()+2,00);DEVOUT('& QUINTO-DA EMISSAO E DO RESGATE DE NOTA PROMISSORIA "PRO SOLVENDO":       ')
                        DEVPOS(PROW(),00);DEVOUT('& QUINTO-DA EMISSAO E DO RESGATE DE NOTA PROMISSORIA "PRO SOLVENDO":       ')

                        DEVPOS(PROW()+2,00);DEVOUT('O(a) CONTRATANTE em garantia do pagamento da divida, emite no ato da matri-')
                        DEVPOS(PROW(),00);DEVOUT('     CONTRATANTE')
                        DEVPOS(PROW()+1,00);DEVOUT('cula uma unica Nota Promissoria, "PRO SOLVENDO", que apos  o  pagamento  da')
                        DEVPOS(PROW(),00);DEVOUT('                                 "PRO SOLVENDO", que')
                        DEVPOS(PROW()+1,00);DEVOUT('anuidade , na rede bancaria, mediante carne, devera resgatar junto a tesou-')
                        DEVPOS(PROW()+1,00);DEVOUT('raria do COLEGIO, fazendo a exibicao dos carnes dos 12(doze) meses pagos.  ')
                        DEVPOS(PROW(),00);DEVOUT('         COLEGIO')

                        DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA SETIMA-DA CONFIGURACAO DO ATO DE MATRICULA:                       ')
                        DEVPOS(PROW(),00);DEVOUT('CLAUSULA SETIMA-DA CONFIGURACAO DO ATO DE MATRICULA:                       ')

                        DEVPOS(PROW()+2,00);DEVOUT('A configuracao formal do ato de matricula dar-se-a com a entrega a Secreta-')
                        DEVPOS(PROW()+1,00);DEVOUT('ria do COLEGIO, dos formularios de matricula fornecidos pelo COLEGIO, cons-')
                        DEVPOS(PROW(),00);DEVOUT('       COLEGIO                                               COLEGIO')
                        DEVPOS(PROW()+1,00);DEVOUT('tante de Termo de Responsabilidade, Requerimento de Matricula  e  Fichas de')
                        DEVPOS(PROW()+1,00);DEVOUT('Retratos, corretamente preenchidos.                                        ')

                        DEVPOS(PROW()+2,00);DEVOUT('& PRIMEIRO-DA MATRICULA DOS ALUNOS NOVOS:                                  ')
                        DEVPOS(PROW(),00);DEVOUT('& PRIMEIRO-DA MATRICULA DOS ALUNOS NOVOS:                                  ')

                        DEVPOS(PROW()+2,00);DEVOUT('O(a) dependente beneficiario(a) do(a) CONTRATANTE que  estiver  ingressando')
                        DEVPOS(PROW(),00);DEVOUT('                                      CONTRATANTE')
                        DEVPOS(PROW()+1,00);DEVOUT('no COLEGIO no ano letivo de 1999, alem dos  documentos  exigidos  no  caput')
                        DEVPOS(PROW(),00);DEVOUT('   COLEGIO')
                        DEVPOS(PROW()+1,00);DEVOUT('desta CLAUSULA tera ainda de preencher a ficha  de  matricula  para  alunos')
                        DEVPOS(PROW()+1,00);DEVOUT('novos, e  entregar na Secretaria, o  historico escolar, a partir da segunda')
                        DEVPOS(PROW()+1,00);DEVOUT('(2a) serie do 1o grau, e mais o certificado de conclusao, para  o  caso  de')
                        DEVPOS(PROW()+1,00);DEVOUT('dependente do 2o(segundo) grau.                                            ')

                        DEVPOS(PROW()+2,00);DEVOUT('& SEGUNDO-DA ENTREGA DOS DOCUMENTOS:                                       ')
                        DEVPOS(PROW(),00);DEVOUT('& SEGUNDO-DA ENTREGA DOS DOCUMENTOS:                                       ')

                        DEVPOS(PROW()+2,00);DEVOUT('Os formularios e a documentacao exigidos  no  caput  desta  CLAUSULA  e  no')
                        DEVPOS(PROW()+1,00);DEVOUT('& primeiro serao entregues impreterivelmente na oportunidade  da matricula,')
                        DEVPOS(PROW()+1,00);DEVOUT('a Comissao de Matricula, e se a matricula for realizada fora do periodo  do')
                        DEVPOS(PROW()+1,00);DEVOUT('calendario, no prazo que for inscrito pelo COLEGIO, na Secretaria,  pelo(a)')
                        DEVPOS(PROW(),00);DEVOUT('                                           COLEGIO')
                        DEVPOS(PROW()+1,00);DEVOUT('CONTRATANTE.                                                               ')
                        DEVPOS(PROW(),00);DEVOUT('CONTRATANTE.                                                               ')


                        DEVPOS(PROW()+1,00);DEVOUT('& TERCEIRO-DA RESPONSABILIDADE PELAS DECLARACOES CONSTANTES DOS DOCUMENTOS ')
                        DEVPOS(PROW(),00);DEVOUT('& TERCEIRO-DA RESPONSABILIDADE PELAS DECLARACOES CONSTANTES DOS DOCUMENTOS ')
                        DEVPOS(PROW()+1,00);DEVOUT('DE MATRICULAS:                                                             ')
                        DEVPOS(PROW(),00);DEVOUT('DE MATRICULAS:                                                             ')

                        DEVPOS(PROW()+2,00);DEVOUT('O(A) CONTRATANTE tem exclusiva responsabilidade civil e penal  pelas decla-')
                        DEVPOS(PROW(),00);DEVOUT('     CONTRATANTE')
                        DEVPOS(PROW()+1,00);DEVOUT('racoes que fizer, neste contrato e no ato de matricula,  quanto  a  aptidao')
                        DEVPOS(PROW()+1,00);DEVOUT('legal do(a) dependente beneficiario(a) de se matricular na  serie  do  grau')
                        DEVPOS(PROW()+1,00);DEVOUT('que indicar, e o nao cumprimento do prazo para a entrega da documentacao,ou')
                        DEVPOS(PROW()+1,00);DEVOUT('a constatacao futura de falta de habilitacao do(a) dependente, para a serie')
                        DEVPOS(PROW()+1,00);DEVOUT('e grau pretendidos, importara no cancelamento de  qualquer  ato  preliminar')
                        DEVPOS(PROW()+1,00);DEVOUT('que tenha sido praticado, no sentido de obtencao da matricula,e na rescisao')
                        DEVPOS(PROW()+1,00);DEVOUT('antecipada do presente contrato, sem prejuizo do cumprimento das obrigacoes')
                        DEVPOS(PROW()+1,00);DEVOUT('assumidas pelo(a) CONTRATANTE, ate a data da efetiva  rescisao  contratual,')
                        DEVPOS(PROW(),00);DEVOUT('                  CONTRATANTE')
                        DEVPOS(PROW()+1,00);DEVOUT('nao podendo, sob qualquer hipotese, o(a) CONTRATANTE alegar dano  de  qual-')
                        DEVPOS(PROW(),00);DEVOUT('                                         CONTRATANTE')
                        DEVPOS(PROW()+1,00);DEVOUT('quer natureza, de responsabilidade do COLEGIO, se configurar-se tais situa-')
                        DEVPOS(PROW(),00);DEVOUT('                                      COLEGIO')
                        DEVPOS(PROW()+1,00);DEVOUT('coes.                                                                      ')

                        EJECT
                        *PAG 5

                        DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA OITAVA-DO SEGURO EDUCACIONAL:')
                        DEVPOS(PROW(),00);DEVOUT('CLAUSULA OITAVA-DO SEGURO EDUCACIONAL:')

                        DEVPOS(PROW()+2,00);DEVOUT('O COLEGIO se compromete a fazer um seguro educacional,em favor do(a) depen-')
                        DEVPOS(PROW()+1,00);DEVOUT('dente do(a) CONTRATANTE, para fins de cobertura dos pagamentos das mensali-')
                        DEVPOS(PROW()+1,00);DEVOUT('dades escolares, que garante a continuidade dos estudos do(a) dependente,em')
                        DEVPOS(PROW()+1,00);DEVOUT('caso de  falecimento ou invalidez  permanente  total  por  acidente,  do(a)')
                        DEVPOS(PROW()+1,00);DEVOUT('CONTRATANTE, afora de indenizacao de  despesas  medicas  e  hospitalares ao')
                        DEVPOS(PROW()+1,00);DEVOUT('ao CONTRATANTE, em caso de acidente no ambito  da responsabilidade do COLE-')
                        DEVPOS(PROW()+1,00);DEVOUT('GIO, e cobertura de 03(tres) meses de mensalidades escolares  em  caso  de ')
                        DEVPOS(PROW()+1,00);DEVOUT('perda de renda do CONTRATANTE, em razao de demissao do  emprego  sem  justa')
                        DEVPOS(PROW()+1,00);DEVOUT('causa, que o(a)  CONTRATANTE  e  o(a)  seu(sua)  DEPENDENTE  somente  terao')
                        DEVPOS(PROW()+1,00);DEVOUT('cobertura, se estiver com os pagamentos das prestacoes  escolares  em dias,')
                        DEVPOS(PROW()+1,00);DEVOUT('pagos nos vencimentos.')

                        DEVPOS(PROW()+2,00);DEVOUT('PARAGRAFO UNICO-DAS INDENIZACOES POR DANOS PESSOAIS:                       ')
                        DEVPOS(PROW(),00);DEVOUT('PARAGRAFO UNICO-DAS INDENIZACOES POR DANOS PESSOAIS:                       ')

                        DEVPOS(PROW()+2,00);DEVOUT('O(A) CONTRATANTE declara expressamente que quaisquer indenizacoes por danos')
                        DEVPOS(PROW(),00);DEVOUT('     CONTRATANTE')
                        DEVPOS(PROW()+1,00);DEVOUT('de qualquer especie de que for vitima o(a) seu(sua) dependente, no  recinto')
                        DEVPOS(PROW()+1,00);DEVOUT('do COLEGIO, a caminho dele, ou voltando  para  casa, ou  realizando  tarefa')
                        DEVPOS(PROW(),00);DEVOUT('   COLEGIO')
                        DEVPOS(PROW()+1,00);DEVOUT('escolar fora, por orientacao do COLEGIO, ficarao limitadas ao valor pago de')
                        DEVPOS(PROW(),00);DEVOUT('                                COLEGIO')
                        DEVPOS(PROW()+1,00);DEVOUT('sinistro, pelo Seguro, renunciando de logo e expressamente a qualquer outro')
                        DEVPOS(PROW()+1,00);DEVOUT('tipo de indenizacao, mesmo de carater complementar.                        ')

                        DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA NONA-DO PRAZO CONTRATUAL:                                         ')
                        DEVPOS(PROW(),00);DEVOUT('CLAUSULA NONA-DO PRAZO CONTRATUAL:                                         ')

                        DEVPOS(PROW()+2,00);DEVOUT('O presente contrato esta sendo pactuado pelo prazo do ano letivo  de  1999,')
                        DEVPOS(PROW()+1,00);DEVOUT('cujo termo final se dara no dia 30 de dezembro de 1999.                    ')

                        DEVPOS(PROW()+2,00);DEVOUT('& PRIMEIRO-DA RESCISAO ANTECIPADA:                                         ')
                        DEVPOS(PROW(),00);DEVOUT('& PRIMEIRO-DA RESCISAO ANTECIPADA:                                         ')

                        DEVPOS(PROW()+2,00);DEVOUT('O presente contrato podera ser  rescindido  antecipadamente  nos  seguintes')
                        DEVPOS(PROW()+1,00);DEVOUT('casos:                                                                     ')
                        DEVPOS(PROW()+2,00);DEVOUT('(a) Por iniciativa do COLEGIO, nos casos de desligamento do(a)  dependente,')
                        DEVPOS(PROW(),00);DEVOUT('                      COLEGIO')
                        DEVPOS(PROW()+1,00);DEVOUT('nos termos do Regimento Interno, por razoes disciplinares, e para os alunos')
                        DEVPOS(PROW()+1,00);DEVOUT('novos, pela falta de aptidao legal para cursar a serie  e o  grau  que tive')
                        DEVPOS(PROW()+1,00);DEVOUT('rem contratado, caso em que  podera  o(a)  CONTRATANTE  evitar  a  rescisao')
                        DEVPOS(PROW()+1,00);DEVOUT('retificando  a  matricula, para  a  serie  e  grau corretos, desde que seja')
                        DEVPOS(PROW()+1,00);DEVOUT('feito em tempo habil, e pela falta de entrega da documentacao exigida  para')
                        DEVPOS(PROW()+1,00);DEVOUT('transferencia;                                                             ')
                        DEVPOS(PROW()+2,00);DEVOUT('(b) Por iniciativa do(a) CONTRATANTE, em casos de tracamento de  matricula,')
                        DEVPOS(PROW(),00);DEVOUT('                         CONTRATANTE')
                        DEVPOS(PROW()+1,00);DEVOUT('por desistencia e por transferencia.                                       ')

                        DEVPOS(PROW()+2,00);DEVOUT('& SEGUNDO-QUITACAO DAS PARCELAS VENCIDAS:                                  ')
                        DEVPOS(PROW(),00);DEVOUT('& SEGUNDO-QUITACAO DAS PARCELAS VENCIDAS:                                  ')

                        DEVPOS(PROW()+2,00);DEVOUT('Em todos os casos, fica o(a) CONTRATANTE, obrigado(a) a pagar  o  valor  da')
                        DEVPOS(PROW(),00);DEVOUT('                             CONTRATANTE, obrigado(a)')
                        DEVPOS(PROW()+1,00);DEVOUT('parcela do mes em que ocorrer o evento,alem de outros debitos aventualmente')
                        DEVPOS(PROW()+1,00);DEVOUT('existentes. Se a rescisao antecipada ocorrer no mes de julho, serao devidos')
                        DEVPOS(PROW()+1,00);DEVOUT('os meses de junho e julho, e se ocorrer no mes de novembro,serao devidos os')
                        DEVPOS(PROW()+1,00);DEVOUT('meses de novembro e dezembro.                                              ')

                        EJECT
                        *PAG 6


                        DEVPOS(PROW()+1,00);DEVOUT('CLAUSULA DECIMA-DO FORO:                                                   ')
                        DEVPOS(PROW(),00);DEVOUT('CLAUSULA DECIMA-DO FORO:                                                   ')

                        DEVPOS(PROW()+3,00);DEVOUT('Para dirimir questoes porventura resultantes do cumprimento deste contrato,')
                        DEVPOS(PROW()+1,00);DEVOUT('as partes  de comum acordo elegem expressamente o foro da comarca de Recife')
                        DEVPOS(PROW()+1,00);DEVOUT('-PE., como o unico competente, renunciando de logo a qualquer um outro, por')
                        DEVPOS(PROW()+1,00);DEVOUT('mais privilegiado que possa se afigurar.                                   ')
                        DEVPOS(PROW()+3,00);DEVOUT('              E por estarem assim justos e contratados, firmaram o presente')
                        DEVPOS(PROW()+1,00);DEVOUT('contrato em duas(02) vias de igual teor e forma, e para um unico efeito, na')
                        DEVPOS(PROW()+1,00);DEVOUT('presenca de duas(02) testemunhas, para que produza os efeitos legais.      ')
                        DEVPOS(PROW()+3,00);DEVOUT('Recife,_____de_________________de 199__                                    ')
                        DEVPOS(PROW()+4,00);DEVOUT('COLEGIO    :______________________________                                 ')
                        DEVPOS(PROW(),00);DEVOUT('COLEGIO')
                        DEVPOS(PROW()+4,00);DEVOUT('CONTRATANTE:______________________________                                 ')
                        DEVPOS(PROW(),00);DEVOUT('CONTRATANTE')
                        DEVPOS(PROW()+4,00);DEVOUT('         T E S T E M U N H A S:                                            ')
                        DEVPOS(PROW(),00);DEVOUT('         T E S T E M U N H A S:                                            ')
                        DEVPOS(PROW()+3,00);DEVOUT('1.__________________________________                                       ')
                        DEVPOS(PROW()+3,00);DEVOUT('2.__________________________________                                       ')
                        EJECT
                        SKIP
                ENDDO
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                RETURN NIL
        ENDIF
ENDDO

******************************* F I M ****************************************
*************************************
* FUNCAO PARA IMPRIMIR O CONTRATO
*************************************

FUNCTION contrato
*****************

*PAG 1
DEVPOS(PROW()+1,00);DEVOUT(CHR(14))
DEVPOS(PROW(),00);DEVOUT('COLEGIO NOBREGA')
DEVPOS(PROW()+1,00);DEVOUT('Avenida Oliveira Lima, No 824,')
DEVPOS(PROW()+1,00);DEVOUT('Boa Vista, Recife - PE.')
DEVPOS(PROW()+1,00);DEVOUT('CGC/MF No 10.977.585/0001-58')

DEVPOS(PROW()+4,00);DEVOUT(PADC('CONTRATO DE PRESTACAO DE SERVICOS',75))
DEVPOS(PROW(),00);DEVOUT(PADC('CONTRATO DE PRESTACAO DE SERVICOS',75))
DEVPOS(PROW()+1,00);DEVOUT(PADC('EDUCACIONAIS',75))
DEVPOS(PROW(),00);DEVOUT(PADC('EDUCACIONAIS',75))
DEVPOS(PROW()+1,00);DEVOUT(PADC('ANO LETIVO DE 2000',75))
DEVPOS(PROW(),00);DEVOUT(PADC('ANO LETIVO DE 2000',75))

DEVPOS(PROW()+4,00);DEVOUT('INSTRUMENTO PARTICULAR DE CONTRATO DE PRESTACAO  DE  SERVICOS  EDUCACIONAIS')
DEVPOS(PROW()+1,00);DEVOUT('QUE ENTRE SI FAZEM, DE UM LADO, O COLEGIO NOBREGA, DEVIDAMENTE INSCRITO  NO')
DEVPOS(PROW()+1,00);DEVOUT('CNPJ/MF No 10.977.585/0001-58,COM SEDE A AVENIDA OLIVEIRA LIMA, No 824, BOA')
DEVPOS(PROW()+1,00);DEVOUT('VISTA, RECIFE-PE., NESTE ATO REPRESENTADO PELO  SEU  DIRETOR, O Pe. EUGENIO')
DEVPOS(PROW()+1,00);DEVOUT('PACELLI  CORREIA  AGUIAR S.J., BRASILEIRO, SOLTEIRO, SACERDOTE, RESIDENTE E')
DEVPOS(PROW()+1,00);DEVOUT('DOMICILIADO  NESTA  CIDADE  DE   RECIFE-PE., DORAVANTE  DENOMINADO SIMPLES-')
DEVPOS(PROW()+1,00);DEVOUT('MENTE DE COLEGIO, E  DE  OUTRO  LADO, O(A)   RESPONSAVEL  PELO(A) ALUNO(A):')
DEVPOS(PROW(),00);DEVOUT('         COLEGIO                                                           ')

DEVPOS(PROW()+1,00);DEVOUT(aluno->aluno+' MATRICULA: '+aluno->matricula+' DA MANHA DA '+SUBSTR(mgstt,2,2))
DEVPOS(PROW(),00);DEVOUT(aluno->aluno+'            '+aluno->matricula+'    MANHA    '+SUBSTR(mgstt,2,2))
DEVPOS(PROW()+1,00);DEVOUT('DO '+mgstt+', SR.(A) '+aluno->responsa)
DEVPOS(PROW(),00);DEVOUT('   '+mgstt+'         '+aluno->responsa)
DEVPOS(PROW()+1,00);DEVOUT('ESTADO CIVIL:_______ NACIONALIDADE _________,PROFISSAO '+aluno->r_prof)
DEVPOS(PROW(),00);DEVOUT('                                                       '+aluno->r_prof)
DEVPOS(PROW()+1,00);DEVOUT('IDENTIDADE No ___________, ORGAO EXPEDIDOR ______ CPF/MF No '+aluno->r_cpf)
DEVPOS(PROW(),00);DEVOUT('                                                            '+aluno->r_cpf)
DEVPOS(PROW()+1,00);DEVOUT(', RESIDENTE E DOMICILIADO(A) A '+aluno->r_rua+',No '+aluno->r_num)
DEVPOS(PROW(),00);DEVOUT('                               '+aluno->r_rua+',No '+aluno->r_num)
DEVPOS(PROW()+1,00);DEVOUT('COMPLEMENTO '+aluno->r_apto+',  BAIRRO/DISTRITO '+aluno->r_bairro)
DEVPOS(PROW(),00);DEVOUT('            '+aluno->r_apto+'                   '+aluno->r_bairro)
DEVPOS(PROW()+1,00);DEVOUT(', CIDADE '+aluno->r_cidade+', ESTADO '+aluno->r_estado+', CEP '+aluno->r_cep)
DEVPOS(PROW(),00);DEVOUT('         '+aluno->r_cidade+',        '+aluno->r_estado+'      '+aluno->r_cep)
DEVPOS(PROW()+1,00);DEVOUT('DORAVANTE DENOMINADO(A) SIMPLESMENTE DE CONTRATANTE, TEM ENTRE SI, JUSTO  E')
DEVPOS(PROW()+1,00);DEVOUT(IMPAC("PACTUADO AS CLAUSULAS E CONDICOES A SEGUIR:",.T.))

DEVPOS(PROW()+2,00);DEVOUT(IMPAC("CLAUSULA PRIMEIRA - FUNDAMENTO LEGAL DO CONTRATO:                          "))
DEVPOS(PROW(),00);DEVOUT(IMPAC("CLAUSULA PRIMEIRA - FUNDAMENTO LEGAL DO CONTRATO:                          "))

DEVPOS(PROW()+2,00);DEVOUT('- O presente contrato firmado pelas partes contratantes tem por  fundamento')
DEVPOS(PROW()+1,00);DEVOUT('  legal:                                                                   ')
DEVPOS(PROW()+2,00);DEVOUT('(a) CONSTITUICAO FEDERAL DE 1988: art. 1o,IV;art.206,II e III, e, art.209; ')
DEVPOS(PROW()+2,00);DEVOUT('(b) CODIGO CIVIL BRASILEIRO:arts.81,82 e 1.092;    ')
DEVPOS(PROW()+2,00);DEVOUT('(c) LEI No 8.069/90;                                                       ')
DEVPOS(PROW()+2,00);DEVOUT('(d) LEI No 8.078/90, com a redacao dada ao art.39, pela MP-1.890-67 de 22  ')
DEVPOS(PROW()+2,00);DEVOUT('de outubro de 1999;                                                        ')
DEVPOS(PROW()+2,00);DEVOUT('(e) LEI No 9.131/95;                                                       ')
DEVPOS(PROW()+2,00);DEVOUT('(f) LEI No 9.394/96                                                        ')
DEVPOS(PROW()+2,00);DEVOUT('(g) MEDIDADE PROVISORIA No 1.890/67, DE 22 DE OUTUBRO DE 1999.             ')
DEVPOS(PROW()+2,00);DEVOUT('(h) A LEGISLACAO QUE VIER A REVOGAR NO TODO OU EM PARTE A MEDIDA PROVISORIA')
DEVPOS(PROW()+1,00);DEVOUT('    No. 1.890/67, DE 22 DE OUTUBRO DE 1999.')


EJECT
*PAG 2

DEVPOS(PROW()+1,00);DEVOUT(IMPAC('CLAUSULA SEGUNDA-DA FORMACAO DO VALOR DAS PRESTACOES ESCOLARES:            '))
DEVPOS(PROW(),00);DEVOUT(IMPAC('CLAUSULA SEGUNDA-DA FORMACAO DO VALOR DAS PRESTACOES ESCOLARES:            '))

DEVPOS(PROW()+2,00);DEVOUT('O valor total anual das mensalidades escolares ora contratadas, e o  resul-')
DEVPOS(PROW()+1,00);DEVOUT('do da multiplacacao do quanto da parcela cobrada no mes de dezembro de 1998')
DEVPOS(PROW()+1,00);DEVOUT(', multiplicado por 12(doze) meses,acrescido do montante apurado na planilha')
DEVPOS(PROW()+1,00);DEVOUT('de custos para o ano letivo de 2000, sendo o total encontrado, dividido em ')
DEVPOS(PROW()+1,00);DEVOUT('12(doze) parcelas, que corresponderao  as  12(doze)  prestacoes  que  serao')
DEVPOS(PROW()+1,00);DEVOUT('cobradas no ano letivo de  2000, com  o  reajuste  resultante  da  operacao')
DEVPOS(PROW()+1,00);DEVOUT('definida nesta clausula.')

DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA TERCEIRA-DA PUBLICACAO DA PLANILHA DE CUSTOS PARA O ANO LETIVO  DE')
DEVPOS(PROW(),00);DEVOUT('CLAUSULA TERCEIRA-DA PUBLICACAO DA PLANILHA DE CUSTOS PARA O ANO LETIVO  DE')
DEVPOS(PROW()+1,00);DEVOUT('2000, DOS TERMOS DESTE CONTRATO E DO NUMERO DE VAGAS:                      ')
DEVPOS(PROW(),00);DEVOUT('2000, DOS TERMOS DESTE CONTRATO E DO NUMERO DE VAGAS:                      ')

DEVPOS(PROW()+2,00);DEVOUT('O(A) CONTRATANTE declara expressamente que teve conhecimento previo, a mais')
DEVPOS(PROW(),00);DEVOUT('     CONTRATANTE                                                           ')
DEVPOS(PROW()+1,00);DEVOUT('de 45(quarenta e cinco) dias antes da sua decisao de firmar o presente con-')
DEVPOS(PROW()+1,00);DEVOUT('trato em razao da data final para matricula, atraves  de  publicacao  feita')
DEVPOS(PROW()+1,00);DEVOUT('pelo COLEGIO, em quadros de avisos, da planilha de custos para o ano letivo')
DEVPOS(PROW(),00);DEVOUT('     COLEGIO, em quadros de avisos')
DEVPOS(PROW()+1,00);DEVOUT('de 2000,do texto da proposta deste contrato e do numero de vagas oferecidas')
DEVPOS(PROW()+1,00);DEVOUT('pelas salas de cada turma,e que por isso esta decidindo com plena liberdade')
DEVPOS(PROW()+1,00);DEVOUT('em anuir com o mesmo.          ')

DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA QUARTA-DO OBJETIVO DO CONTRATO:                                   ')
DEVPOS(PROW(),00);DEVOUT('CLAUSULA QUARTA-DO OBJETIVO DO CONTRATO:                                   ')

DEVPOS(PROW()+2,00);DEVOUT('O objeto do presente contrato e a prestacao de servicos educacionais conti-')
DEVPOS(PROW()+1,00);DEVOUT('nuos, no ano letivo de 2000, correspondente a  '+SUBSTR(mgstt,2,1)+'  serie, do ensino '+mgstt+', em')
DEVPOS(PROW()+1,00);DEVOUT('estrito cumprimento ao que disciplina a legislacao do ensino medio em vigor')
DEVPOS(PROW()+1,00);DEVOUT(', e ao Regimento Interno do COLEGIO, a(ao) dependente do(a) CONTRATANTE,que')
DEVPOS(PROW(),00);DEVOUT('                            COLEGIO                         CONTRATANTE    ')
DEVPOS(PROW()+1,00);DEVOUT('por si, e por ele(a) se compromete a observar e cumprir.                   ')

DEVPOS(PROW()+2,00);DEVOUT('& PRIMEIRO-DA FORMA DE CUMPRIMENTO DO OBJETIVO DO CONTRATO:                ')
DEVPOS(PROW(),00);DEVOUT('& PRIMEIRO-DA FORMA DE CUMPRIMENTO DO OBJETIVO DO CONTRATO:                ')

DEVPOS(PROW()+2,00);DEVOUT('O COLEGIO se compromete a ministrar a instrucao atraves de aulas  e  demais')
DEVPOS(PROW(),00);DEVOUT('  COLEGIO                                                                  ')
DEVPOS(PROW()+1,00);DEVOUT('atividades correlatas, devendo o plano  de aulas, programas  e  curriculos,')
DEVPOS(PROW()+1,00);DEVOUT('escolares, estar conforme a legislacao pertinente;                         ')

DEVPOS(PROW()+2,00);DEVOUT('& SEGUNDO-LOCAL DE CUMPRIMENTO DO OBJETIVO DO CONTRATO:                    ')
DEVPOS(PROW(),00);DEVOUT('& SEGUNDO-LOCAL DE CUMPRIMENTO DO OBJETIVO DO CONTRATO:                    ')

DEVPOS(PROW()+2,00);DEVOUT('A instrucao sera ministrada nas salas de aula do estabelecimento do COLEGIO,')
DEVPOS(PROW(),00);DEVOUT('                                                                    COLEGIO,')
DEVPOS(PROW()+1,00);DEVOUT('a Avenida Oliveira Lima, No 824, Boa Vista, Recife-PE., ou em outros locais')
DEVPOS(PROW()+1,00);DEVOUT('que o mesmo venha a orientar, tendo em vista a natureza do  conteudo  e  da')
DEVPOS(PROW()+1,00);DEVOUT('tecnica pedagogica exigidas no momento oportuno.                           ')

DEVPOS(PROW()+2,00);DEVOUT('& TERCEIRO-DA SUBORDINACAO AO REGIMENTO INTERNO:                           ')
DEVPOS(PROW(),00);DEVOUT('& TERCEIRO-DA SUBORDINACAO AO REGIMENTO INTERNO:                           ')

DEVPOS(PROW()+2,00);DEVOUT('O(A) Dependente beneficiario(a) estara sujeito(a) as  Normas  do  Regimento')
DEVPOS(PROW()+1,00);DEVOUT('Interno do COLEGIO, a disposicao dos interessados nas respectivas coordena-')
DEVPOS(PROW(),00);DEVOUT('           COLEGIO')
DEVPOS(PROW()+1,00);DEVOUT('coes pedagogicas, e cujas normatizacoes fazem parte integrante do  presente')
DEVPOS(PROW()+1,00);DEVOUT('contrato, para todos os efeitos legais.                                    ')

DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA QUINTA-DOS SERVICOS NAO COBERTOS PELO CONTRATO:                   ')
DEVPOS(PROW(),00);DEVOUT('CLAUSULA QUINTA-DOS SERVICOS NAO COBERTOS PELO CONTRATO:                   ')

DEVPOS(PROW()+2,00);DEVOUT('Nao estao incluidos neste contrato, os servicos especiais  de  recuperacao,')
DEVPOS(PROW()+1,00);DEVOUT('reforco, dependencia, adaptacao, reciclagem, transporte escolar, as escoli-')
DEVPOS(PROW()+1,00);DEVOUT('nhas de esportes e dancas, os opcionais e  de  uso  facultativo  para  o(a)')
DEVPOS(PROW()+1,00);DEVOUT('dependente beneficiario(a), bem como uniformes, merenda e material didatico')
DEVPOS(PROW()+1,00);DEVOUT('de uso individual e obrigatorio, inclusive, uma resma de papel  que  devera')
DEVPOS(PROW()+1,00);DEVOUT('ser entregue no ato da matricula ao COLEGIO, e a agenda escolar, que devera')
DEVPOS(PROW(),00);DEVOUT('                                    COLEGIO')
DEVPOS(PROW()+1,00);DEVOUT('ser adquirida na mesma ocasiao, junto ao referido contratado, e os que tam-')
DEVPOS(PROW()+1,00);DEVOUT('bem poderao ser objeto de ajuste a parte, e ainda  a  segunda  e  seguintes')
DEVPOS(PROW()+1,00);DEVOUT('vias de documentos escolares e carnes de pagamento.                        ')

EJECT
*PAG 3

DEVPOS(PROW()+1,00);DEVOUT('CLAUSULA SEXTA-DA REMUNERACAO DOS SERVICOS:                                ')
DEVPOS(PROW(),00);DEVOUT('CLAUSULA SEXTA-DA REMUNERACAO DOS SERVICOS:                                ')
DEVPOS(PROW()+2,00);DEVOUT('Pelos servicos sumariados  na  CLAUSULA  quarta  e  seus  paragrafos, o (a)')
DEVPOS(PROW()+1,00);DEVOUT('CONTRATANTE, pagara ao COLEGIO, a anuidade de,                             ')
DEVPOS(PROW(),00);DEVOUT('CONTRATANTE            COLEGIO')

DEVPOS(PROW()+2,00);DEVOUT('PARA O MATERNAL, R$ 2.463,55(dois mil quatrocentos e sessenta e  tres reais')
DEVPOS(PROW(),00);DEVOUT('PARA O MATERNAL,')
DEVPOS(PROW()+1,00);DEVOUT('cinquenta e cinco centavos)  dividida  em  12 (doze)  parcelas  mensais  de')
DEVPOS(PROW()+1,00);DEVOUT('R$ 205,30 (duzentos e cinco reais e trinta centavos);')

DEVPOS(PROW()+2,00);DEVOUT('PARA A EDUCACAO INFANTIL, R$ 2.510,48(dois mil quinhentos  e  dez  reais  e')
DEVPOS(PROW(),00);DEVOUT('PARA A EDUCACAO INFANTIL,')
DEVPOS(PROW()+1,00);DEVOUT('quarenta e oito centavos)  dividida  em  12 (doze)  parcelas   mensais,  de')
DEVPOS(PROW()+1,00);DEVOUT('R$ 209,21(duzentos e nove reais e vinte e um centavos);')

DEVPOS(PROW()+2,00);DEVOUT('PARA O ENSINO FUNDAMENTAL I, R$ 2.603,95(dois mil seiscentos e tres reais e')
DEVPOS(PROW(),00);DEVOUT('PARA O ENSINO FUNDAMENTAL I')
DEVPOS(PROW()+1,00);DEVOUT('noventa e  cinco  centavos)  dividida  em  12 (doze)  parcelas  mensais  de')
DEVPOS(PROW()+1,00);DEVOUT('R$ 217,00 (duzentos e dezessete reais);')

DEVPOS(PROW()+2,00);DEVOUT('PARA O ENSINO FUNDAMENTAL II, R$ 2.684,07(dois mil seiscentos  e  oitenta e')
DEVPOS(PROW(),00);DEVOUT('PARA O ENSINO FUNDAMENTAL II')
DEVPOS(PROW()+1,00);DEVOUT('quatro reais  e  sete  centavos) dividida  em  12 (doze)  parcelas  mensais')
DEVPOS(PROW()+1,00);DEVOUT('de R$ 223,67 (duzentos e vinte e tres reais e sessenta e sete centavos);')

DEVPOS(PROW()+2,00);DEVOUT('PARA O ENSINO MEDIO, R$ 2.777,55(dois mil setecentos e setenta e sete reais')
DEVPOS(PROW(),00);DEVOUT('PARA O ENSINO MEDIO')
DEVPOS(PROW()+1,00);DEVOUT('cinquenta e cinco  centavos) dividida  em  12 (doze)  parcelas  mensais  de')
DEVPOS(PROW()+1,00);DEVOUT('R$ 231,46 (duzentos e trinta e um reais e quarenta e seis centavos).')

DEVPOS(PROW()+2,00);DEVOUT('& PRIMEIRO-DO PAGAMENTO DA PRIMEIRA PARCELA E DAS DEMAIS PARCELAS:         ')
DEVPOS(PROW(),00);DEVOUT('& PRIMEIRO-DO PAGAMENTO DA PRIMEIRA PARCELA E DAS DEMAIS PARCELAS:         ')

DEVPOS(PROW()+2,00);DEVOUT('A primeira prestacao, representada por carne de pagamento, sera paga no ato')
DEVPOS(PROW()+1,00);DEVOUT('da matricula, e as demais, em numero de onze(11), tambem representadas  por')
DEVPOS(PROW()+1,00);DEVOUT('carnes de pagamento, serao pagas sucessivamente,a segunda, no dia 30 janei-')
DEVPOS(PROW()+1,00);DEVOUT('ro de 2000, e as demais, a cada dia trinta(30) de cada mes  subsequente,')
DEVPOS(PROW()+1,00);DEVOUT('ate a ultima prestacao, que tera vencimento no dia 30 de novembro  de 2000,')
DEVPOS(PROW()+1,00);DEVOUT('ficando de logo pactuado, entre as parte contratantes, uma tolerancia ate o')
DEVPOS(PROW()+1,00);DEVOUT('5o(quinto) dia util do mes seguinte ao do vencimento, para que  seja  feito')
DEVPOS(PROW()+1,00);DEVOUT('o pagamento sem acrescimos.                                                ')

DEVPOS(PROW()+2,00);DEVOUT('& SEGUNDO-DOS ENCARGOS EM RAZAO DE INADIMPLENCIA:                          ')
DEVPOS(PROW(),00);DEVOUT('& SEGUNDO-DOS ENCARGOS EM RAZAO DE INADIMPLENCIA:                          ')

DEVPOS(PROW()+2,00);DEVOUT('O atraso no pagamento de qualquer das parcelas,em razao dos fixados no &1o.')
DEVPOS(PROW()+1,00);DEVOUT('desta clausula, sujeitara o(a) CONTRATANTE ao pagamento adicional ao  valor')
DEVPOS(PROW(),00);DEVOUT('                               CONTRATANTE ao pagamento adicional ao  valor')
DEVPOS(PROW()+1,00);DEVOUT('principal, dos seguintes encargos:')
DEVPOS(PROW(),00);DEVOUT('principal')
DEVPOS(PROW()+1,00);DEVOUT('(a) Multa contratual de 2%(dois por cento) sobre a mensalidade em atraso;')
DEVPOS(PROW()+1,00);DEVOUT('(b) Atualizacao do valor do debito, pelo indice oficial mais  favoravel  ao')
DEVPOS(PROW()+1,00);DEVOUT('COLEGIO;')
DEVPOS(PROW(),00);DEVOUT('COLEGIO;')
DEVPOS(PROW()+1,00);DEVOUT('(c) Juros de mora')

DEVPOS(PROW()+2,00);DEVOUT('& TERCEIRO-DA COBRANCA DE MENSALIDADES EM ATRASO:                         ')
DEVPOS(PROW(),00);DEVOUT('& TERCEIRO-DA COBRANCA DE MENSALIDADES EM ATRASO:                         ')

DEVPOS(PROW()+2,00);DEVOUT('Em havendo atraso  no  pagamento  de  qualquer  das  parcelas  por  periodo')
DEVPOS(PROW()+1,00);DEVOUT('superior a sessenta(60) dias, o COLEGIO, a seu criterio, podera  encaminhar')
DEVPOS(PROW(),00);DEVOUT('                                COLEGIO, a seu criterio')
DEVPOS(PROW()+1,00);DEVOUT('os titulos para protesto e depois executa-los judicialmente,ou encaminha-los')
DEVPOS(PROW()+1,00);DEVOUT('para cobranca por advogado e/ou pelo Servico de Protecao ao Credito  (SPC),')
DEVPOS(PROW()+1,00);DEVOUT('casos em que, alem dos encargos referidos no paragrafo anterior, o(a)  CON-')
DEVPOS(PROW(),00);DEVOUT('                                                                       CON-')
DEVPOS(PROW()+1,00);DEVOUT('TRATANTE ainda tera que arcar  com  o  pagamento  das  despesas  judiciais,')
DEVPOS(PROW(),00);DEVOUT('TRATANTE')
DEVPOS(PROW()+1,00);DEVOUT('custas, diligencias, emolumentos, e com o pagamento dos honorarios  advoca-')
DEVPOS(PROW()+1,00);DEVOUT('ticios, FICANDO DE LOGO CONVENCIONADO, para o caso, que nao sera renovado o')
DEVPOS(PROW()+1,00);DEVOUT('contrato para o ano letivo de 2001, renunciando de logo o(a) CONTRATANTE ao')
DEVPOS(PROW(),00);DEVOUT('                                                             CONTRATANTE   ')
DEVPOS(PROW()+1,00);DEVOUT('direito de preferencia que lhe confere o art.6o, da Medida Provisoria de No')
DEVPOS(PROW()+1,00);DEVOUT('1.890-67, de 22 de outubro 1999.                                         ')

EJECT
*PAG 4

DEVPOS(PROW()+2,00);DEVOUT('& QUARTO-DA ENTREGA DOS CARNES DE PAGAMENTO:                               ')
DEVPOS(PROW(),00);DEVOUT('& QUARTO-DA ENTREGA DOS CARNES DE PAGAMENTO:                               ')

DEVPOS(PROW()+2,00);DEVOUT('O(a) CONTRATANTE recebera mensalmente o carne de pagamento, ate o dia 20 do')
DEVPOS(PROW(),00);DEVOUT('     CONTRATANTE')
DEVPOS(PROW()+1,00);DEVOUT('mes do vencimento, no COLEGIO, no valor  da  parcela, para ser pago na rede')
DEVPOS(PROW(),00);DEVOUT('                   no COLEGIO')
DEVPOS(PROW()+1,00);DEVOUT('bancaria, e o ato da entrega ficara configurado pela  aposicao da assinatu-')
DEVPOS(PROW()+1,00);DEVOUT('ra do(a) mesmo(a), ou de pessoa  por  ele(a)  credenciado(a), na  lista  de')
DEVPOS(PROW()+1,00);DEVOUT('entrega de carnes, existente no COLEGIO.                          ')
DEVPOS(PROW(),00);DEVOUT('                                COLEGIO.                          ')

DEVPOS(PROW()+2,00);DEVOUT('& QUINTO-DA EMISSAO E DO RESGATE DE NOTA PROMISSORIA "PRO SOLVENDO":       ')
DEVPOS(PROW(),00);DEVOUT('& QUINTO-DA EMISSAO E DO RESGATE DE NOTA PROMISSORIA "PRO SOLVENDO":       ')

DEVPOS(PROW()+2,00);DEVOUT('O(a) CONTRATANTE em garantia do pagamento da divida, emite no ato da matri-')
DEVPOS(PROW(),00);DEVOUT('     CONTRATANTE')
DEVPOS(PROW()+1,00);DEVOUT('cula uma unica Nota Promissoria, "PRO SOLVENDO", que apos  o  pagamento  da')
DEVPOS(PROW(),00);DEVOUT('                                 "PRO SOLVENDO", que')
DEVPOS(PROW()+1,00);DEVOUT('anuidade , na rede bancaria, mediante carne, devera resgatar junto a tesou-')
DEVPOS(PROW()+1,00);DEVOUT('raria do COLEGIO, fazendo a exibicao dos carnes dos 12(doze) meses pagos.  ')
DEVPOS(PROW(),00);DEVOUT('         COLEGIO')

DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA SETIMA-DA CONFIGURACAO DO ATO DE MATRICULA:                       ')
DEVPOS(PROW(),00);DEVOUT('CLAUSULA SETIMA-DA CONFIGURACAO DO ATO DE MATRICULA:                       ')

DEVPOS(PROW()+2,00);DEVOUT('A configuracao formal do ato de matricula dar-se-a com a entrega a Secreta-')
DEVPOS(PROW()+1,00);DEVOUT('ria do COLEGIO, dos formularios de matricula fornecidos pelo COLEGIO, cons-')
DEVPOS(PROW(),00);DEVOUT('       COLEGIO                                               COLEGIO')
DEVPOS(PROW()+1,00);DEVOUT('tante de Termo de Responsabilidade, Requerimento de Matricula  e  Fichas de')
DEVPOS(PROW()+1,00);DEVOUT('Retratos, corretamente preenchidos.                                        ')

DEVPOS(PROW()+2,00);DEVOUT('& PRIMEIRO-DA MATRICULA DOS ALUNOS NOVOS:                                  ')
DEVPOS(PROW(),00);DEVOUT('& PRIMEIRO-DA MATRICULA DOS ALUNOS NOVOS:                                  ')

DEVPOS(PROW()+2,00);DEVOUT('O(a) dependente beneficiario(a) do(a) CONTRATANTE que  estiver  ingressando')
DEVPOS(PROW(),00);DEVOUT('                                      CONTRATANTE')
DEVPOS(PROW()+1,00);DEVOUT('no COLEGIO no ano letivo de 2000, alem dos  documentos  exigidos  no  caput')
DEVPOS(PROW(),00);DEVOUT('   COLEGIO')
DEVPOS(PROW()+1,00);DEVOUT('desta CLAUSULA tera ainda de preencher a ficha  de  matricula  para  alunos')
DEVPOS(PROW()+1,00);DEVOUT('novos, e  entregar na Secretaria, o  historico escolar, a partir da segunda')
DEVPOS(PROW()+1,00);DEVOUT('(2a)serie do ensino fundamental I, e mais o certificado de conclusao,  para')
DEVPOS(PROW()+1,00);DEVOUT('o caso de dependente do ensino medio.                                             ')

DEVPOS(PROW()+2,00);DEVOUT('& SEGUNDO-DA ENTREGA DOS DOCUMENTOS:                                       ')
DEVPOS(PROW(),00);DEVOUT('& SEGUNDO-DA ENTREGA DOS DOCUMENTOS:                                       ')

DEVPOS(PROW()+2,00);DEVOUT('Os formularios e a documentacao exigidos  no  caput  desta  CLAUSULA  e  no')
DEVPOS(PROW()+1,00);DEVOUT('& primeiro serao entregues impreterivelmente na oportunidade  da matricula,')
DEVPOS(PROW()+1,00);DEVOUT('a Comissao de Matricula, e se a matricula for realizada fora do periodo  do')
DEVPOS(PROW()+1,00);DEVOUT('calendario, no prazo que for inscrito pelo COLEGIO, na Secretaria,  pelo(a)')
DEVPOS(PROW(),00);DEVOUT('                                           COLEGIO')
DEVPOS(PROW()+1,00);DEVOUT('CONTRATANTE.                                                               ')
DEVPOS(PROW(),00);DEVOUT('CONTRATANTE.                                                               ')


DEVPOS(PROW()+1,00);DEVOUT('& TERCEIRO-DA RESPONSABILIDADE PELAS DECLARACOES CONSTANTES DOS DOCUMENTOS ')
DEVPOS(PROW(),00);DEVOUT('& TERCEIRO-DA RESPONSABILIDADE PELAS DECLARACOES CONSTANTES DOS DOCUMENTOS ')
DEVPOS(PROW()+1,00);DEVOUT('DE MATRICULAS:                                                             ')
DEVPOS(PROW(),00);DEVOUT('DE MATRICULAS:                                                             ')

DEVPOS(PROW()+2,00);DEVOUT('O(A) CONTRATANTE tem exclusiva responsabilidade civil e penal  pelas decla-')
DEVPOS(PROW(),00);DEVOUT('     CONTRATANTE')
DEVPOS(PROW()+1,00);DEVOUT('racoes que fizer, neste contrato e no ato de matricula,  quanto  a  aptidao')
DEVPOS(PROW()+1,00);DEVOUT('legal do(a) dependente beneficiario(a) de se matricular na serie do  ensino')
DEVPOS(PROW()+1,00);DEVOUT('que indicar, e o nao cumprimento do prazo para a entrega da documentacao,ou')
DEVPOS(PROW()+1,00);DEVOUT('a constatacao futura de falta de habilitacao do(a) dependente, para a serie')
DEVPOS(PROW()+1,00);DEVOUT('e ensino pretendidos, importara no cancelamento de qualquer ato  preliminar')
DEVPOS(PROW()+1,00);DEVOUT('que tenha sido praticado, no sentido de obtencao da matricula,e na rescisao')
DEVPOS(PROW()+1,00);DEVOUT('antecipada do presente contrato, sem prejuizo do cumprimento das obrigacoes')
DEVPOS(PROW()+1,00);DEVOUT('assumidas pelo(a) CONTRATANTE, ate a data da efetiva  rescisao  contratual,')
DEVPOS(PROW(),00);DEVOUT('                  CONTRATANTE')
DEVPOS(PROW()+1,00);DEVOUT('nao podendo, sob qualquer hipotese, o(a) CONTRATANTE alegar dano  de  qual-')
DEVPOS(PROW(),00);DEVOUT('                                         CONTRATANTE')
DEVPOS(PROW()+1,00);DEVOUT('quer natureza, de responsabilidade do COLEGIO, se configurar tais situacoes')
DEVPOS(PROW(),00);DEVOUT('                                      COLEGIO')

EJECT
*PAG 5

DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA OITAVA-DO SEGURO EDUCACIONAL E DE RESPONSABILIDADE CIVIL:')
DEVPOS(PROW(),00);DEVOUT('CLAUSULA OITAVA-DO SEGURO EDUCACIONAL E DE RESPONSABILIDADE CIVIL:')

DEVPOS(PROW()+2,00);DEVOUT('O COLEGIO se compromete a fazer um seguro educacional,em favor do(a) depen-')
DEVPOS(PROW()+1,00);DEVOUT('dente do(a) CONTRATANTE, para fins de cobertura dos pagamentos das mensali-')
DEVPOS(PROW()+1,00);DEVOUT('dades escolares, em caso de perda do emprego sem justa causa  do(a) CONTRA-')
DEVPOS(PROW()+1,00);DEVOUT('TANTE que tenha CTPS anotada ha pelo menos 12(doze) meses no mesmo emprego,')
DEVPOS(PROW()+1,00);DEVOUT('para garantia da continuidade dos estudos do(a) dependente,  com  cobertura')
DEVPOS(PROW()+1,00);DEVOUT('por tres(03) meses das mensalidades escolares, e em caso de falecimento, ou')
DEVPOS(PROW()+1,00);DEVOUT('invalidez total (permanente) por acidente,do(a) CONTRATANTE,ate a conclusao')
DEVPOS(PROW()+1,00);DEVOUT('do ensino medio do(a) dependente, cobertura que  sera  dada  exclusivamente')
DEVPOS(PROW()+1,00);DEVOUT('para o(a) mesmo(a) enquanto permanecer estudando no Colegio Nobrega.       ')

DEVPOS(PROW()+2,00);DEVOUT('O COLEGIO se compromete ainda a contratar um seguro de RESPONSABILIDADE CI-')
DEVPOS(PROW()+1,00);DEVOUT('VIL, para indenizacao de despesas medicas e hospitalares ao CONTRATANTE, em')
DEVPOS(PROW()+1,00);DEVOUT('caso de acidentes no ambito da responsabilidade do COLEGIO, ate o limite de')
DEVPOS(PROW()+1,00);DEVOUT('cobertura fixado na apolice do seguro.                                     ')

DEVPOS(PROW()+2,00);DEVOUT('& 1o. - O CONTRATANTE e o(a) seu(sua) DEPENDENTE  somente  terao  cobertura')
DEVPOS(PROW()+1,00);DEVOUT('dos seguros de que trata o caput desta clausula, se mantiverem os  pagamen-')
DEVPOS(PROW()+1,00);DEVOUT('tos das prestacoes escolares em dias, pagos nos vencimentos')

DEVPOS(PROW()+2,00);DEVOUT('& 2o. - Nao poderao assinar o CONTRATO, na condicao  de  CONTRATANTE,  para')
DEVPOS(PROW()+1,00);DEVOUT('fins das coberturas dos seguros acima discriminados, pessoas fisicas que ja')
DEVPOS(PROW()+1,00);DEVOUT('tenham completado 66(sessenta e seis anos) na data de celebracao deste pac-')
DEVPOS(PROW()+1,00);DEVOUT('to.                                                                        ')

DEVPOS(PROW()+2,00);DEVOUT('& 3o. - O(A) CONTRATANTE declara expressamente  que  quaisquer indenizacoes')
DEVPOS(PROW()+1,00);DEVOUT('por danos de qualquer especie de que for vitima o(a)  seu(sua)  dependente,')
DEVPOS(PROW()+1,00);DEVOUT('no recinto do COLEGIO, a caminho dele, ou voltando para casa, ou realizando')
DEVPOS(PROW()+1,00);DEVOUT('tarefa escolar fora, por orientacao do COLEGIO, ficarao  limitas  ao  valor')
DEVPOS(PROW()+1,00);DEVOUT('de sinistro, pelo Seguro, renunciando de logo e  expressamente  a  qualquer')
DEVPOS(PROW()+1,00);DEVOUT('outro tipo de indenizacao, mesmo de caracter complementar.                 ')

DEVPOS(PROW()+2,00);DEVOUT('CLAUSULA NONA-DO PRAZO CONTRATUAL:                                         ')
DEVPOS(PROW(),00);DEVOUT('CLAUSULA NONA-DO PRAZO CONTRATUAL:                                         ')

DEVPOS(PROW()+2,00);DEVOUT('O presente contrato esta sendo pactuado pelo prazo do ano letivo  de  2000,')
DEVPOS(PROW()+1,00);DEVOUT('cujo termo final se dara no dia 30 de dezembro de 2000.                    ')

DEVPOS(PROW()+2,00);DEVOUT('& PRIMEIRO-DA RESCISAO ANTECIPADA:                                         ')
DEVPOS(PROW(),00);DEVOUT('& PRIMEIRO-DA RESCISAO ANTECIPADA:                                         ')

DEVPOS(PROW()+2,00);DEVOUT('O presente contrato podera ser  rescindido  antecipadamente  nos  seguintes')
DEVPOS(PROW()+1,00);DEVOUT('casos:                                                                     ')
DEVPOS(PROW()+2,00);DEVOUT('(a) Por iniciativa do COLEGIO, nos casos de desligamento do(a)  dependente,')
DEVPOS(PROW(),00);DEVOUT('                      COLEGIO')
DEVPOS(PROW()+1,00);DEVOUT('nos termos do Regimento Interno, por razoes disciplinares, e para os alunos')
DEVPOS(PROW()+1,00);DEVOUT('novos,pela falta de aptidao legal para cursar a serie e o grau para os quais')
DEVPOS(PROW()+1,00);DEVOUT('foram contratos, caso em que  podera  o(a)  CONTRATANTE  evitar  a rescisao')
DEVPOS(PROW()+1,00);DEVOUT('se houver tempo habil para retificar a matricula, e pela falta  de  entrega')
DEVPOS(PROW()+1,00);DEVOUT('da documentacao exigida para transferencia;                                ')
DEVPOS(PROW()+2,00);DEVOUT('(b) Por iniciativa do(a) CONTRATANTE, em casos de tracamento de  matricula,')
DEVPOS(PROW(),00);DEVOUT('                         CONTRATANTE')
DEVPOS(PROW()+1,00);DEVOUT('por desistencia e por transferencia.                                       ')

DEVPOS(PROW()+2,00);DEVOUT('& SEGUNDO-QUITACAO DAS PARCELAS VENCIDAS:                                  ')
DEVPOS(PROW(),00);DEVOUT('& SEGUNDO-QUITACAO DAS PARCELAS VENCIDAS:                                  ')

DEVPOS(PROW()+2,00);DEVOUT('Em todos os casos, fica o(a) CONTRATANTE, obrigado(a) a pagar  o  valor  da')
DEVPOS(PROW(),00);DEVOUT('                             CONTRATANTE, obrigado(a)')
DEVPOS(PROW()+1,00);DEVOUT('parcela do mes em que ocorrer o evento,alem de outros debitos eventualmente')
DEVPOS(PROW()+1,00);DEVOUT('existentes. Se a rescisao antecipada ocorrer no mes de julho, serao devidos')
DEVPOS(PROW()+1,00);DEVOUT('os meses de junho e julho, e se ocorrer no mes de novembro,serao devidos os')
DEVPOS(PROW()+1,00);DEVOUT('meses de novembro e dezembro.                                              ')

EJECT
*PAG 6


DEVPOS(PROW()+1,00);DEVOUT('CLAUSULA DECIMA-DO FORO:                                                   ')
DEVPOS(PROW(),00);DEVOUT('CLAUSULA DECIMA-DO FORO:                                                   ')

DEVPOS(PROW()+3,00);DEVOUT('Para dirimir questoes porventura resultantes do cumprimento deste contrato,')
DEVPOS(PROW()+1,00);DEVOUT('as partes  de comum acordo elegem expressamente o  foro da cidade de Recife')
DEVPOS(PROW()+1,00);DEVOUT('-PE., como o unico competente, renunciando de logo a qualquer um outro, por')
DEVPOS(PROW()+1,00);DEVOUT('mais privilegiado que possa se afigurar.                                   ')
DEVPOS(PROW()+3,00);DEVOUT('              E por estarem assim justos e contratados, firmaram o presente')
DEVPOS(PROW()+1,00);DEVOUT('contrato em duas(02) vias de igual teor e forma, e para um unico efeito, na')
DEVPOS(PROW()+1,00);DEVOUT('presenca de duas(02) testemunhas, para que produza os efeitos legais.      ')
DEVPOS(PROW()+3,00);DEVOUT('Recife,_____de_________________de _____                                    ')
DEVPOS(PROW()+4,00);DEVOUT('COLEGIO    :______________________________                                 ')
DEVPOS(PROW(),00);DEVOUT('COLEGIO')
DEVPOS(PROW()+4,00);DEVOUT('CONTRATANTE:______________________________                                 ')
DEVPOS(PROW(),00);DEVOUT('CONTRATANTE')
DEVPOS(PROW()+4,00);DEVOUT('         T E S T E M U N H A S:                                            ')
DEVPOS(PROW(),00);DEVOUT('         T E S T E M U N H A S:                                            ')
DEVPOS(PROW()+3,00);DEVOUT('1.__________________________________                                       ')
DEVPOS(PROW()+3,00);DEVOUT('2.__________________________________                                       ')
RETURN NIL


