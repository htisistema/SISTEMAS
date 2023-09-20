*******************************
* INCLUSAO DE ALUNOS VETERANOS
*******************************

FUNCTION saf102
**************
LOCAL mprg:='SAF102'
LOCAL tela,opcao,lci,cci,lba,cba,mpoint
LOCAL maluno,maluno1,msexo,ma_datan,ma_natur,ma_estado,ma_nacion,mdata_ing,;
      mdt_tranf,mpai,mp_fone,mmae,mm_fone,mresponsa,mr_rua,mr_num,mr_apto,;
      mr_bairro,mr_cidade,mr_estado,mr_cep,mr_prof,mr_cpf,mr_fone,mr_fone1,mr_fone2,mr_ramal
LOCAL mfaixa_1,mfaixa_2,mfaixa_3,mfaixa_4,mfaixa_5,mfaixa_6,mfaixa_7,mfaixa_8,;
      mfaixa_9,mfaixa_10,mfaixa_11,mfaixa_12,mr_datnasc,marea,mdisciplina,mcontrato
PRIVATE mgstt,msituacao,mtipo_sit,m_nota:={},munidade:='MF',mcont_rec,mmatricula

IF ! ver_nivel(mprg,'INCLUSAO DE ALUNOS VETERANOS','125')
        RETURN NIL
ENDIF
tela := SAVESCREEN(00,00,24,79)

lci := 02
cci := 02
lba := 20
cba := 78
*--------------------------------------------------------------------------
CLOSE ALL
USE ('\'+mdrive+'\'+ALLTRIM(STR(VAL(mdiretorio)-1))+'\CADDISC') ALIAS disc SHARED NEW
USE ('\'+mdrive+'\'+ALLTRIM(STR(VAL(mdiretorio)-1))+'\CADNOTA') ALIAS nota SHARED NEW
USE ('\'+mdrive+'\'+ALLTRIM(STR(VAL(mdiretorio)-1))+'\CADALUNO') ALIAS aluno SHARED NEW

IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('caddisc','di');RETURN NIL;ENDIF
IF ! abriarq('cadnota','not');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','al');RETURN NIL;ENDIF
*--------------------------------------------------------------------------

WHILE .T.
        mmatricula := SPACE(6)
        maluno     := SPACE(40)
        maluno1    := SPACE(55)
        mgstt      := SPACE(4)
        msexo      := SPACE(1)
        ma_datan   := CTOD('  /  /  ')
        ma_natur   := SPACE(14)
        ma_estado  := SPACE(2)
        ma_nacion  := SPACE(8)
        mdata_ing  := mdata_sis
        mfaixa_1   := SPACE(1)
        mfaixa_2   := SPACE(1)
        mfaixa_3   := SPACE(1)
        mfaixa_4   := SPACE(1)
        mfaixa_5   := SPACE(1)
        mfaixa_6   := SPACE(1)
        mfaixa_7   := SPACE(1)
        mfaixa_8   := SPACE(1)
        mfaixa_9   := SPACE(1)
        mfaixa_10  := SPACE(1)
        mfaixa_11  := SPACE(1)
        mfaixa_12  := SPACE(1)
        mdt_tranf  := CTOD('  /  /  ')
        mpai       := SPACE(40)
        mp_fone   := SPACE(8)
        mmae       := SPACE(40)
        mm_fone   := SPACE(8)
        mresponsa  := SPACE(40)
        mr_rua     := SPACE(35)
        mr_num     := SPACE(5)
        mr_apto    := SPACE(5)
        mr_bairro  := SPACE(20)
        mr_cidade  := SPACE(20)
        mr_estado  := SPACE(2)
        mr_cep     := SPACE(8)
        mr_prof    := SPACE(20)
        mr_cpf     := SPACE(12)
        mr_fone    := SPACE(8)
        mr_fone1   := SPACE(8)
        mr_fone2   := SPACE(8)
        mr_ramal   := SPACE(4)
        mr_datnasc := CTOD('  /  /  ')
        marea      := SPACE(1)
        mdisciplina:= SPACE(3)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci,cci+1);DEVOUT('Contrato.:')
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula:')
        DEVPOS(lci+1,cci+22);DEVOUT('Nome:')
        DEVPOS(lci+2,cci+1);DEVOUT('Nome p/ATA:')
        DEVPOS(lci+3,cci+1);DEVOUT('GSTT:')
        DEVPOS(lci+3,cci+13);DEVOUT('Sexo:')
        DEVPOS(lci+3,cci+21);DEVOUT('Nasc.:')
        DEVPOS(lci+3,cci+40);DEVOUT('Natural:')
        DEVPOS(lci+3,cci+65);DEVOUT('Estado:')
        DEVPOS(lci+4,cci+1);DEVOUT('Nac.:')
        DEVPOS(lci+4,cci+17);DEVOUT('Ingresso:')
        DEVPOS(lci+4,cci+39);DEVOUT('Matric.:')
        DEVPOS(lci+4,cci+60);DEVOUT('Transf.:')
        DEVPOS(lci+5,cci+1);DEVOUT('Bolsa: Mes: 01:  02:  03:  04:  05:  06:  07:  08:  09:  10:  11:  12:')
        @ lci+6,cci+1 TO lci+6,cba-1
        DEVPOS(lci+7,cci+1);DEVOUT('Nome do Pai:')
        DEVPOS(lci+7,cci+56);DEVOUT('Telefone:')
        DEVPOS(lci+8,cci+1);DEVOUT('Nome do Mae:')
        DEVPOS(lci+8,cci+56);DEVOUT('Telefone:')
        @ lci+9,cci+1 TO lci+9,cba-1
        DEVPOS(lci+10,cci+1);DEVOUT('Responsavel:')
        DEVPOS(lci+10,cci+56);DEVOUT('Data Nasc.:')
        DEVPOS(lci+11,cci+1);DEVOUT('Endereco...:')
        DEVPOS(lci+11,cci+51);DEVOUT('No.:')
        DEVPOS(lci+11,cci+63);DEVOUT('Apto.:')
        DEVPOS(lci+12,cci+1);DEVOUT('Bairro.....:')
        DEVPOS(lci+12,cci+36);DEVOUT('Cidade:')
        DEVPOS(lci+12,cci+66);DEVOUT('Estado:')
        DEVPOS(lci+13,cci+1);DEVOUT('CEP........:')
        DEVPOS(lci+13,cci+26);DEVOUT('Fone:')
        DEVPOS(lci+14,cci+1);DEVOUT('Profissao.:')
        DEVPOS(lci+14,cci+35);DEVOUT('CPF.:')
        DEVPOS(lci+15,cci+1);DEVOUT('Fone......:')
        DEVPOS(lci+15,cci+24);DEVOUT('Fone:')
        DEVPOS(lci+15,cci+40);DEVOUT('Ramal:')
        @ lci+16,cci+1 TO lci+16,cba-1
        janela(lci+16,cci+1,' Opcoes ')
        DEVPOS(lci+17,cci+1);DEVOUT('Area:')
        DEVPOS(lci+17,cci+10);DEVOUT('Tipo Disciplina <ESP/ING>:')
        *****************
        SELE('aluno');ORDSETFOCUS(4)
        GO TOP
        *****************
        @ lci+1,cci+12 GET mmatricula PICT '999999'
        @ lci+1,cci+28 GET maluno PICT '@!' VALID IF(EMPTY(maluno),.F.,.T.) WHEN EMPTY(mmatricula)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
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
        mpoint := RECNO()
        *****************
        SELE('al');ORDSETFOCUS(4)
        GO TOP
        *****************
        IF al->(DBSEEK(mmatricula))
                atencao('Aluno com matricula feita')
                LOOP
        ENDIF
        *****************
        SELE('aluno');ORDSETFOCUS(4)
        GO mpoint
        *****************
        mgstt := SPACE(4)
        msituacao := ' '
        ASIZE(m_nota,0)
        mcont_rec := 0
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
                ENDIF
        ENDIF
        mmatricula := aluno->matricula
        maluno     := aluno->aluno
        maluno1    := aluno->aluno1
        msexo      := aluno->sexo
        ma_datan   := aluno->a_datan
        ma_natur   := aluno->a_natur
        ma_estado  := aluno->a_estado
        ma_nacion  := aluno->a_nacion
        mdata_ing  := mdata_sis
        ming_esco  := aluno->data_ing
        mfaixa_1   := aluno->faixa_1
        mfaixa_2   := aluno->faixa_2
        mfaixa_3   := aluno->faixa_3
        mfaixa_4   := aluno->faixa_4
        mfaixa_5   := aluno->faixa_5
        mfaixa_6   := aluno->faixa_6
        mfaixa_7   := aluno->faixa_7
        mfaixa_8   := aluno->faixa_8
        mfaixa_9   := aluno->faixa_9
        mfaixa_10  := aluno->faixa_10
        mfaixa_11  := aluno->faixa_11
        mfaixa_12  := aluno->faixa_12
        mdt_tranf  := aluno->dt_tranf
        mpai       := aluno->pai
        mp_fone    := aluno->p_fone
        mmae       := aluno->mae
        mm_fone    := aluno->m_fone
        mresponsa  := aluno->responsa
        mr_rua     := aluno->r_rua
        mr_num     := aluno->r_num
        mr_apto    := aluno->r_apto
        mr_bairro  := aluno->r_bairro
        mr_cidade  := aluno->r_cidade
        mr_estado  := aluno->r_estado
        mr_cep     := aluno->r_cep
        mr_prof    := aluno->r_prof
        mr_cpf     := aluno->r_cpf
        mr_fone    := aluno->r_fone
        mr_fone1   := aluno->r_fone1
        mr_fone2   := aluno->r_fone2
        mr_ramal   := aluno->r_ramal
        mr_datnasc := aluno->r_datnasc
        marea      := aluno->area
        mdisciplina:= aluno->disciplina
        setcor(3)
        DEVPOS(lci+1,cci+12);DEVOUT(mmatricula)
        DEVPOS(lci+3,cci+27);DEVOUT(ming_esco)
        setcor(1)
        @ lci+1,cci+28 GET maluno PICT '@!' VALID IF(EMPTY(maluno),.F.,.T.)
        @ lci+2,cci+12 GET maluno1 PICT '@!' VALID IF(EMPTY(maluno1),.F.,.T.)
        @ lci+3,cci+7 GET mgstt PICT '@!' VALID ver_gstt(mgstt)
        @ lci+3,cci+19 GET msexo PICT '@!' VALID msexo $ 'M,F'
        @ lci+3,cci+28 GET ma_datan
        @ lci+3,cci+49 GET ma_natur PICT '@!'
        @ lci+3,cci+73 GET ma_estado PICT '@!'
        @ lci+4,cci+7 GET ma_nacion PICT '@!'
        @ lci+4,cci+27 GET ming_esco
        @ lci+4,cci+48 GET mdata_ing
        @ lci+4,cci+68 GET mdt_tranf PICT '@!'
        @ lci+5,cci+16 GET mfaixa_1 VALID ver_fx(mfaixa_1)
        @ lci+5,cci+21 GET mfaixa_2 VALID ver_fx(mfaixa_2)
        @ lci+5,cci+26 GET mfaixa_3 VALID ver_fx(mfaixa_3)
        @ lci+5,cci+31 GET mfaixa_4 VALID ver_fx(mfaixa_4)
        @ lci+5,cci+36 GET mfaixa_5 VALID ver_fx(mfaixa_5)
        @ lci+5,cci+41 GET mfaixa_6 VALID ver_fx(mfaixa_6)
        @ lci+5,cci+46 GET mfaixa_7 VALID ver_fx(mfaixa_7)
        @ lci+5,cci+51 GET mfaixa_8 VALID ver_fx(mfaixa_8)
        @ lci+5,cci+56 GET mfaixa_9 VALID ver_fx(mfaixa_9)
        @ lci+5,cci+61 GET mfaixa_10 VALID ver_fx(mfaixa_10)
        @ lci+5,cci+66 GET mfaixa_11 VALID ver_fx(mfaixa_11)
        @ lci+5,cci+71 GET mfaixa_12 VALID ver_fx(mfaixa_12)
        @ lci+7,cci+14 GET mpai PICT '@!'
        @ lci+7,cci+66 GET mp_fone
        @ lci+8,cci+14 GET mmae PICT '@!'
        @ lci+8,cci+66 GET mm_fone
        @ lci+10,cci+14 GET mresponsa
        @ lci+10,cci+68 GET mr_datnasc
        @ lci+11,cci+14 GET mr_rua PICT '@!'
        @ lci+11,cci+56 GET mr_num PICT '@!'
        @ lci+11,cci+70 GET mr_apto PICT '@!'

        @ lci+12,cci+14 GET mr_bairro PICT '@!'
        @ lci+12,cci+44 GET mr_cidade PICT '@!'
        @ lci+12,cci+74 GET mr_estado PICT '@!'
        @ lci+13,cci+14 GET mr_cep PICT '@!'
        @ lci+13,cci+32 GET mr_fone PICT '@!'
        @ lci+14,cci+14 GET mr_prof PICT '@!'
        @ lci+14,cci+41 GET mr_cpf PICT '999999999-99'
        @ lci+15,cci+14 GET mr_fone1 PICT '@!'
        @ lci+15,cci+30 GET mr_fone2 PICT '@!'
        @ lci+15,cci+47 GET mr_ramal PICT '@!'
        @ lci+17,cci+7 GET  marea PICT '@!'
        @ lci+17,cci+37 GET  mdisciplina PICT '@!'
        CLEAR GETS
        mensagem('Deseja Alterar algun CAMPO [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        IF opcao = 'S'
                @ lci+1,cci+28 GET maluno PICT '@!' VALID IF(EMPTY(maluno),.F.,.T.)
                @ lci+2,cci+12 GET maluno1 PICT '@!' VALID IF(EMPTY(maluno1),.F.,.T.)
                @ lci+3,cci+7 GET mgstt PICT '@!' VALID ver_gstt(mgstt)
                @ lci+3,cci+19 GET msexo PICT '@!' VALID msexo $ 'M,F'
                @ lci+3,cci+28 GET ma_datan
                @ lci+3,cci+49 GET ma_natur PICT '@!'
                @ lci+3,cci+73 GET ma_estado PICT '@!'
                @ lci+4,cci+7 GET ma_nacion PICT '@!'
                @ lci+4,cci+27 GET ming_esco
                @ lci+4,cci+48 GET mdata_ing
                @ lci+4,cci+68 GET mdt_tranf PICT '@!'
                @ lci+5,cci+16 GET mfaixa_1 VALID ver_fx(mfaixa_1)
                @ lci+5,cci+21 GET mfaixa_2 VALID ver_fx(mfaixa_2)
                @ lci+5,cci+26 GET mfaixa_3 VALID ver_fx(mfaixa_3)
                @ lci+5,cci+31 GET mfaixa_4 VALID ver_fx(mfaixa_4)
                @ lci+5,cci+36 GET mfaixa_5 VALID ver_fx(mfaixa_5)
                @ lci+5,cci+41 GET mfaixa_6 VALID ver_fx(mfaixa_6)
                @ lci+5,cci+46 GET mfaixa_7 VALID ver_fx(mfaixa_7)
                @ lci+5,cci+51 GET mfaixa_8 VALID ver_fx(mfaixa_8)
                @ lci+5,cci+56 GET mfaixa_9 VALID ver_fx(mfaixa_9)
                @ lci+5,cci+61 GET mfaixa_10 VALID ver_fx(mfaixa_10)
                @ lci+5,cci+66 GET mfaixa_11 VALID ver_fx(mfaixa_11)
                @ lci+5,cci+71 GET mfaixa_12 VALID ver_fx(mfaixa_12)
                @ lci+7,cci+14 GET mpai PICT '@!'
                @ lci+7,cci+66 GET mp_fone
                @ lci+8,cci+14 GET mmae PICT '@!'
                @ lci+8,cci+66 GET mm_fone
                @ lci+10,cci+14 GET mresponsa
                @ lci+10,cci+68 GET mr_datnasc
                @ lci+11,cci+14 GET mr_rua PICT '@!'
                @ lci+11,cci+56 GET mr_num PICT '@!'
                @ lci+11,cci+70 GET mr_apto PICT '@!'

                @ lci+12,cci+14 GET mr_bairro PICT '@!'
                @ lci+12,cci+44 GET mr_cidade PICT '@!'
                @ lci+12,cci+74 GET mr_estado PICT '@!'
                @ lci+13,cci+14 GET mr_cep PICT '@!'
                @ lci+13,cci+32 GET mr_fone PICT '@!'
                @ lci+14,cci+14 GET mr_prof PICT '@!'
                @ lci+14,cci+41 GET mr_cpf PICT '999999999-99'
                @ lci+15,cci+14 GET mr_fone1 PICT '@!'
                @ lci+15,cci+30 GET mr_fone2 PICT '@!'
                @ lci+15,cci+47 GET mr_ramal PICT '@!'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
        ELSE
                ver_fx(mfaixa_1)
        ENDIF
        mensagem('Confirma a Inclusao do ALUNO [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        *****************
        SELE('al');ORDSETFOCUS(9)
        GO BOTT
        *****************
        mcontrato := STRZERO(VAL(SUBSTR(al->contrato,1,5))+1,5)+'/'+SUBSTR(mdiretorio,3)
        *****************
        SELE('al');ORDSETFOCUS(4)
        GO TOP
        *****************
        IF ! ADIREG()
                atencao('Nao foi possivel bloquear o arquivo')
                LOOP
        ENDIF
        al->contrato  := mcontrato
        al->matricula := mmatricula
        al->aluno     := maluno
        al->aluno1    := maluno1
        al->gstt      := mgstt
        al->sexo      := msexo
        al->a_datan   := ma_datan
        al->a_natur   := ma_natur
        al->a_estado  := ma_estado
        al->a_nacion  := ma_nacion
        al->ing_esco  := ming_esco
        al->data_ing  := mdata_ing
        al->faixa_1   := mfaixa_1
        al->faixa_2   := mfaixa_2
        al->faixa_3   := mfaixa_3
        al->faixa_4   := mfaixa_4
        al->faixa_5   := mfaixa_5
        al->faixa_6   := mfaixa_6
        al->faixa_7   := mfaixa_7
        al->faixa_8   := mfaixa_8
        al->faixa_9   := mfaixa_9
        al->faixa_10  := mfaixa_10
        al->faixa_11  := mfaixa_11
        al->faixa_12  := mfaixa_12
        al->dt_tranf  := mdt_tranf
        al->pai       := mpai
        al->p_fone    := mp_fone
        al->mae       := mmae
        al->m_fone    := mm_fone
        al->responsa  := mresponsa
        al->r_rua     := mr_rua
        al->r_num     := mr_num
        al->r_apto    := mr_apto
        al->r_bairro  := mr_bairro
        al->r_cidade  := mr_cidade
        al->r_estado  := mr_estado
        al->r_cep     := mr_cep
        al->r_prof    := mr_prof
        al->r_cpf     := mr_cpf
        al->r_fone    := mr_fone
        al->r_fone1   := mr_fone1
        al->r_fone2   := mr_fone2
        al->r_ramal   := mr_ramal
        al->r_datnasc := mr_datnasc
        al->area      := marea
        al->disciplina:= mdisciplina
        DBUNLOCKALL()
        DBCOMMITALL()
        setcor(3)
        DEVPOS(lci,cci+12);DEVOUT(mcontrato)
        setcor(1)
        INKEY(9)
        *****************
        SELE('cdmen');ORDSETFOCUS(1)
        GO BOTT
        *****************
        i     := 0
        mparc := SPACE(2)
        FOR i := 1 TO  12
                IF ! ADIREG()
                        atencao('Nao foi possivel criar o registro - CDMENSAL')
                        LOOP
                ENDIF
                mparcela          := STRZERO(i + VAL(mparc),2)
                cdmen->ano        := mdiretorio
                cdmen->contrato   := mcontrato
                cdmen->tipo       := 'ME'
                cdmen->gstt       := mgstt
                cdmen->matricula  := mmatricula
*                mbolsa            := ver_faixa(mparcela)
                IF i = 1
                        mbolsa := mfaixa_1
                ELSEIF i = 2
                        mbolsa := mfaixa_2
                ELSEIF i = 3
                        mbolsa := mfaixa_3
                ELSEIF i = 4
                        mbolsa := mfaixa_4
                ELSEIF i = 5
                        mbolsa := mfaixa_5
                ELSEIF i = 6
                        mbolsa := mfaixa_6
                ELSEIF i = 7
                        mbolsa := mfaixa_7
                ELSEIF i = 8
                        mbolsa := mfaixa_8
                ELSEIF i = 9
                        mbolsa := mfaixa_9
                ELSEIF i = 10
                        mbolsa := mfaixa_10
                ELSEIF i = 11
                        mbolsa := mfaixa_11
                ELSEIF i = 12
                        mbolsa := mfaixa_12
                ENDIF
                cdmen->bolsa      := mbolsa
                cdmen->parcela    := mparcela
                mvalor := vlr_men(SUBSTR(mgstt,1,2)+SUBSTR(mgstt,4),mparcela)
                cdmen->data_venc  := CTOD(tabmen->data_venc+'/'+mdiretorio)
                cdmen->vlr_parc   := mvalor
                IF faixa_vlr(mbolsa) = 100
                        cdmen->desc_bolsa := mvalor
                ELSE
                        cdmen->desc_bolsa := mvalor * (faixa_vlr(mbolsa)/100)
                ENDIF
                cdmen->flag       := ' '
                DBUNLOCKALL()
                DBCOMMITALL()
        NEXT
ENDDO
RETURN NIL
