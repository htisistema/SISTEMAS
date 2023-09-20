*******************************
* ALTERACAO DE ALUNOS
*******************************

FUNCTION saf103
**************
LOCAL mprg:='SAF103'
LOCAL tela,opcao,lci,cci,lba,cba,i:=0
LOCAL maluno,maluno1,msexo,ma_datan,ma_natur,ma_estado,;
      ma_nacion,mdata_ing,;
      mdt_tranf,mpai,mp_fone,mmae,mm_fone,mresponsa,mr_rua,mr_num,mr_apto,;
      mr_bairro,mr_cidade,mr_estado,mr_cep,mr_prof,mr_cpf,mr_fone,mr_fone1,mr_fone2,mr_ramal
LOCAL mfaixa_1,mfaixa_2,mfaixa_3,mfaixa_4,mfaixa_5,mfaixa_6,mfaixa_7,mfaixa_8,;
      mfaixa_9,mfaixa_10,mfaixa_11,mfaixa_12,mr_datnasc,marea,mdisciplina,mgstt_ant,;
      mobs

PRIVATE mparc,mvalor,mbolsa,mgstt,mmatricula

IF ! ver_nivel(mprg,'ALTERACAO DE CADASTRO ALUNOS','125')
        RETURN NIL
ENDIF
tela := SAVESCREEN(00,00,24,79)

lci := 02
cci := 02
lba := 21
cba := 78
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
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
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba,,' ALTERACAO DE ALUNO ')
        setcor(1)
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
        DEVPOS(lci+18,cci+1);DEVOUT('OBS.:')
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
        ELSE
                LOOP
        ENDIF
        mgstt_ant  := aluno->gstt
        mmatricula := aluno->matricula
        maluno     := aluno->aluno
        maluno1    := aluno->aluno1
        mgstt      := aluno->gstt
        msexo      := aluno->sexo
        ma_datan   := aluno->a_datan
        ma_natur   := aluno->a_natur
        ma_estado  := aluno->a_estado
        ma_nacion  := aluno->a_nacion
        mdata_ing  := aluno->data_ing
        ming_esco  := aluno->ing_esco
        mdata_ing  := aluno->data_ing
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
        mobs       := aluno->obs
        setcor(3)
        DEVPOS(lci+1,cci+12);DEVOUT(mmatricula)
        setcor(1)

        @ lci+1,cci+28 GET maluno PICT '@!' VALID IF(EMPTY(maluno),.F.,.T.)
        @ lci+2,cci+12 GET maluno1 PICT '@!' VALID IF(EMPTY(maluno1),.F.,.T.)
        @ lci+3,cci+7  GET mgstt PICT '@!'
        @ lci+3,cci+19 GET msexo PICT '@!' VALID msexo $ 'M,F'
        @ lci+3,cci+28 GET ma_datan
        @ lci+3,cci+49 GET ma_natur PICT '@!'
        @ lci+3,cci+73 GET ma_estado PICT '@!'
        @ lci+4,cci+7  GET ma_nacion PICT '@!'
        @ lci+4,cci+27 GET ming_esco
        @ lci+4,cci+48 GET mdata_ing
        @ lci+4,cci+68 GET mdt_tranf PICT '@!'
        @ lci+5,cci+16 GET mfaixa_1 VALID ver_fx(mfaixa_1,'01',aluno->faixa_1,mfaixa_1)
        @ lci+5,cci+21 GET mfaixa_2 VALID ver_fx(mfaixa_2,'02',aluno->faixa_2,mfaixa_2)
        @ lci+5,cci+26 GET mfaixa_3 VALID ver_fx(mfaixa_3,'03',aluno->faixa_3,mfaixa_3)
        @ lci+5,cci+31 GET mfaixa_4 VALID ver_fx(mfaixa_4,'04',aluno->faixa_4,mfaixa_4)
        @ lci+5,cci+36 GET mfaixa_5 VALID ver_fx(mfaixa_5,'05',aluno->faixa_5,mfaixa_5)
        @ lci+5,cci+41 GET mfaixa_6 VALID ver_fx(mfaixa_6,'06',aluno->faixa_6,mfaixa_6)
        @ lci+5,cci+46 GET mfaixa_7 VALID ver_fx(mfaixa_7,'07',aluno->faixa_7,mfaixa_7)
        @ lci+5,cci+51 GET mfaixa_8 VALID ver_fx(mfaixa_8,'08',aluno->faixa_8,mfaixa_8)
        @ lci+5,cci+56 GET mfaixa_9 VALID ver_fx(mfaixa_9,'09',aluno->faixa_9,mfaixa_9)
        @ lci+5,cci+61 GET mfaixa_10 VALID ver_fx(mfaixa_10,'10',aluno->faixa_10,mfaixa_10)
        @ lci+5,cci+66 GET mfaixa_11 VALID ver_fx(mfaixa_11,'11',aluno->faixa_11,mfaixa_11)
        @ lci+5,cci+71 GET mfaixa_12 VALID ver_fx(mfaixa_12,'12',aluno->faixa_12,mfaixa_12)
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
        @ lci+18,cci+7 GET  mobs
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
        mensagem('Confirma a Alteracao do ALUNO [S/n]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        IF ! BLOQREG()
                atencao('Nao foi possivel bloquear o arquivo - CADALUNO')
                LOOP
        ENDIF
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
        aluno->dt_tranf  := mdt_tranf
        aluno->pai       := mpai
        aluno->p_fone    := mp_fone
        aluno->mae       := mmae
        aluno->m_fone    := mm_fone
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
*       *****************
*       SELE('aluno');ORDSETFOCUS(4)
*       *****************
        DBUNLOCKALL()
        DBCOMMITALL()
        i := 0
        FOR i = 1 TO 12
                mparc  := STRZERO(i,2)
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
                mvalor := 0
                safcdmen()
        NEXT
ENDDO
RETURN NIL
*********************************  F I M *******************************
FUNCTION safcdmen()

*****************
SELE('cdmen');ORDSETFOCUS(1)
GO TOP
*****************

IF cdmen->(DBSEEK(aluno->matricula+mdiretorio+mparc+'ME')) .AND. EMPTY(cdmen->data_pag)
        BLOQREG()
        cdmen->bolsa    := mbolsa
        mvalor := vlr_men(SUBSTR(mgstt,1,2)+SUBSTR(mgstt,4),mparc)
        cdmen->vlr_parc := mvalor
        IF faixa_vlr(mbolsa) = 100
                cdmen->desc_bolsa := mvalor
        ELSE
                cdmen->desc_bolsa := mvalor * (faixa_vlr(mbolsa)/100)
        ENDIF
        DBUNLOCK()
        DBCOMMIT()
ENDIF
RETURN NIL
