*******************************
* INCLUSAO DE ALUNOS NOVATOS
*******************************

FUNCTION saf101
**************
LOCAL mprg:='SAF101'
LOCAL tela,opcao,lci,cci,lba,cba
LOCAL mnumero,mmatricula,maluno,maluno1,mgstt,msexo,ma_datan,ma_natur,ma_estado,ma_nacion,;
      mdata_ing,mfaixa_1,mfaixa_2,mfaixa_3,mfaixa_4,mfaixa_5,mfaixa_6,mfaixa_7,;
      mfaixa_8,mfaixa_9,mfaixa_10,mfaixa_11,mfaixa_12,mdt_tranf,mpai,mp_fone,mmae,mm_fone,mresponsa,mr_rua,mr_num,;
      mr_apto,mr_bairro,mr_cidade,mr_estado,mr_cep,mr_prof,mr_cpf,mr_fone,mr_fone1,;
      mr_fone2,mr_ramal,ming_esco,mr_datanasc,marea,mdisciplina,mcontrato:=SPACE(8),mobs,;
      mmatr

IF ! ver_nivel(mprg,'INCLUSAO DE ALUNOS NOVATOS','125')
        RETURN NIL
ENDIF
tela := SAVESCREEN(00,00,24,79)

lci := 02
cci := 02
lba := 21
cba := 78
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('safnomat','nomat');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------

WHILE .T.
        mnumero    := SPACE(3)
        mmatricula := SPACE(8)
        mmatr      := SPACE(8)
        maluno     := SPACE(40)
        maluno1    := SPACE(55)
        mgstt      := SPACE(4)
        msexo      := SPACE(1)
        ma_datan   := CTOD('  /  /  ')
        ma_natur   := SPACE(14)
        ma_estado  := SPACE(2)
        ma_nacion  := SPACE(8)
        mdata_ing  := mdata_sis
        ming_esco  := mdata_sis
        mfaixa_1   := 'A'
        mfaixa_2   := 'A'
        mfaixa_3   := 'A'
        mfaixa_4   := 'A'
        mfaixa_5   := 'A'
        mfaixa_6   := 'A'
        mfaixa_7   := 'A'
        mfaixa_8   := 'A'
        mfaixa_9   := 'A'
        mfaixa_10  := 'A'
        mfaixa_11  := 'A'
        mfaixa_12  := 'A'
        mdt_tranf  := CTOD('  /  /  ')
        mpai       := SPACE(40)
        mp_fone    := SPACE(8)
        mmae       := SPACE(40)
        mm_fone    := SPACE(8)
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
        mobs       := SPACE(50)

        mparc      := SPACE(2)
        mparcela   := SPACE(2)
        mbolsa     := SPACE(1)
        mvalor     := 0

        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci,cci+1);DEVOUT('Contrato.:        ')
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
        DEVPOS(lci+18,cci+1);DEVOUT('OBS:')

        @ lci+1,cci+12 GET mmatr PICT '999999' VALID ver_aluno(mmatr,,,,'*')
        @ lci+1,cci+28 GET maluno PICT '@!' VALID IF(EMPTY(maluno),.F.,.T.)
        maluno1 := maluno
        @ lci+2,cci+13 GET maluno1 PICT '@!' VALID IF(EMPTY(maluno1),.F.,.T.)
        @ lci+3,cci+7  GET mgstt PICT '@!' VALID ver_gstt(mgstt)
        @ lci+3,cci+19 GET msexo PICT '@!' VALID msexo $ 'M,F'
        @ lci+3,cci+28 GET ma_datan
        @ lci+3,cci+49 GET ma_natur PICT '@!'
        @ lci+3,cci+73 GET ma_estado PICT '@!'
        @ lci+4,cci+7  GET ma_nacion PICT '@!'
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
        @ lci+17,cci+7  GET marea PICT '@!'
        @ lci+17,cci+37 GET mdisciplina PICT '@!'
        @ lci+18,cci+7 GET mobs
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        mensagem('Confirma a Inclusao do ALUNO [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        *****************
        SELE('nomat')
        GO TOP
        *****************
        IF ! BLOQARQ()
                atencao('Nao foi possivel bloquear o arquivo')
                LOOP
        ENDIF
*       IF VAL('19'+SUBSTR(aluno->matricula,1,2)) < VAL(mdiretorio)
        IF EMPTY(nomat->no_mat) .AND. EMPTY(mmatr)
                mmatricula := ALLTRIM(SUBSTR(mdiretorio,3,2)+'0001')
                ADIREG()
                nomat->no_mat := mmatricula
        ELSEIF EMPTY(mmatricula) .AND. EMPTY(mmatr)
*               mmatricula := ALLTRIM(STR(VAL(SUBSTR(aluno->matr,1,6))+1))
                mmatricula := STRZERO(VAL(nomat->no_mat)+1,6)
                nomat->no_mat := mmatricula
        ELSE
                mmatricula := mmatr
        ENDIF
        DBCOMMIT()
        DBUNLOCK()
        setcor(3)
        DEVPOS(lci+1,cci+12);DEVOUT(mmatricula)
        setcor(1)
        INKEY(8)
        IF LASTKEY() = 27
                DBUNLOCKALL()
                DBCOMMITALL()
                LOOP
        ENDIF
        mcontrato := SPACE(8)
        IF EMPTY(mmatr)
                *****************
                SELE('aluno');ORDSETFOCUS(9)
                GO TOP
                GO BOTT
                *****************
                mcontrato := STRZERO(VAL(SUBSTR(aluno->contrato,1,5))+1,5)+'/'+SUBSTR(mdiretorio,3)
                *****************
                SELE('aluno');ORDSETFOCUS(4)
                GO TOP
                GO BOTT
                *****************
                IF ! EMPTY(aluno->numero)
                        ***************
                        SELE('aluno');ORDSETFOCUS(6)
                        DBSETFILTER({|| aluno->gstt = mgstt})
                        GO TOP
                        GO BOTT
                        ***************
                        mnumero := STRZERO(VAL(aluno->numero)+1,3)
                ENDIF
        ELSE
                *****************
                SELE('aluno');ORDSETFOCUS(4)
                GO TOP
                *****************
        ENDIF
        IF ! ADIREG()
                atencao('Nao foi possivel criar o registro')
                LOOP
        ENDIF
        aluno->contrato  := mcontrato
        aluno->numero    := mnumero
        aluno->matricula := mmatricula
        aluno->aluno     := maluno
        aluno->aluno1    := maluno1
        aluno->gstt      := mgstt
        aluno->sexo      := msexo
        aluno->a_datan   := ma_datan
        aluno->a_natur   := ma_natur
        aluno->a_estado  := ma_estado
        aluno->a_nacion  := ma_nacion
        aluno->data_ing  := mdata_ing
        aluno->ing_esco  := ming_esco
        aluno->faixa_1   := mfaixa_1
        aluno->faixa_2   := mfaixa_2
        aluno->faixa_3   := mfaixa_3
        aluno->faixa_4   := mfaixa_4
        aluno->faixa_5   := mfaixa_5
        aluno->faixa_6   := mfaixa_6
        aluno->faixa_7   := mfaixa_7
        aluno->faixa_8   := mfaixa_8
        aluno->faixa_9   := mfaixa_9
        aluno->faixa_10  := mfaixa_10
        aluno->faixa_11  := mfaixa_11
        aluno->faixa_12  := mfaixa_12
        aluno->dt_tranf  := mdt_tranf
        aluno->pai       := mpai
        aluno->p_fone   := mp_fone
        aluno->mae       := mmae
        aluno->m_fone   := mm_fone
        aluno->responsa  := mresponsa
        aluno->r_rua     := mr_rua
        aluno->r_num     := mr_num
        aluno->r_apto    := mr_apto
        aluno->r_bairro  := mr_bairro
        aluno->r_cidade  := mr_cidade
        aluno->r_estado  := mr_estado
        aluno->r_cep     := mr_cep
        aluno->r_prof    := mr_prof
        aluno->r_cpf     := mr_cpf
        aluno->r_fone    := mr_fone
        aluno->r_fone1   := mr_fone1
        aluno->r_fone2   := mr_fone2
        aluno->r_ramal   := mr_ramal
        aluno->r_datnasc := mr_datnasc
        aluno->area      := marea
        aluno->disciplina:= mdisciplina
        aluno->obs       := mobs
        DBUNLOCKALL()
        DBCOMMITALL()
        setcor(3)
        DEVPOS(lci,cci+12);DEVOUT(mcontrato)
        setcor(1)
        INKEY(9)
        mensagem('Deseja Gerar as mensalidades deste Aluno [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        *****************
        SELE('cdmen');ORDSETFOCUS(1)
        GO BOTT
        *****************
        i     := 0
        mparc := SPACE(2)
        FOR i := 1 TO  12
                IF VAL(SUBSTR(DTOC(mdata_ing),4,2)) > i
                        LOOP
                ENDIF
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
                mbolsa            := ver_faixa(mparcela)
                cdmen->bolsa      := mbolsa
                cdmen->parcela    := mparcela
                mvalor := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),mparcela)
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
