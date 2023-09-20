*******************************
* EXCLUSAO DE ALUNOS
*******************************

FUNCTION saf104
**************
LOCAL mprg:='SAF104'
LOCAL tela,opcao,lci,cci,lba,cba
LOCAL maluno,mgstt,msexo,ma_datan,ma_natur,ma_estado,ma_nacion,mdata_ing,;
      mfaixa_1,mdt_tranf,mpai,mp_fone,mmae,mm_fone,mresponsa,mr_rua,mr_num,mr_apto,;
      mr_bairro,mr_cidade,mr_estado,mr_cep,mr_prof,mr_cpf,mr_fone,mr_fone1,mr_fone2,mr_ramal
PRIVATE mmatricula
IF ! ver_nivel(mprg,'EXCLUSAO DE ALUNOS','1')
        RETURN NIL
ENDIF
tela := SAVESCREEN(00,00,24,79)

lci := 05
cci := 02
lba := 19
cba := 78
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------

WHILE .T.
        mmatricula := SPACE(6)
        maluno     := SPACE(40)
        mgstt      := SPACE(4)
        msexo      := SPACE(1)
        ma_datan   := CTOD('  /  /  ')
        ma_natur   := SPACE(14)
        ma_estado  := SPACE(2)
        ma_nacion  := SPACE(8)
        mdata_ing  := mdata_sis
        mfaixa_1   := SPACE(1)
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
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula:')
        DEVPOS(lci+1,cci+22);DEVOUT('Nome:')
        DEVPOS(lci+2,cci+1);DEVOUT('GSTT:')
        DEVPOS(lci+2,cci+13);DEVOUT('Sexo:')
        DEVPOS(lci+2,cci+21);DEVOUT('Nasc.:')
        DEVPOS(lci+2,cci+40);DEVOUT('Natural:')
        DEVPOS(lci+2,cci+65);DEVOUT('Estado:')
        DEVPOS(lci+3,cci+1);DEVOUT('Nac.:')
        DEVPOS(lci+3,cci+17);DEVOUT('Ingresso:')
        DEVPOS(lci+3,cci+39);DEVOUT('Bolsa:')
        DEVPOS(lci+3,cci+48);DEVOUT('Transferido:')
        @ lci+4,cci+1 TO lci+4,cba-1
        DEVPOS(lci+5,cci+1);DEVOUT('Nome do Pai:')
        DEVPOS(lci+5,cci+56);DEVOUT('Telefone:')
        DEVPOS(lci+6,cci+1);DEVOUT('Nome do Mae:')
        DEVPOS(lci+6,cci+56);DEVOUT('Telefone:')
        @ lci+7,cci+1 TO lci+7,cba-1
        DEVPOS(lci+8,cci+1);DEVOUT('Responsavel:')
        DEVPOS(lci+9,cci+1);DEVOUT('Endereco...:')
        DEVPOS(lci+9,cci+51);DEVOUT('No.:')
        DEVPOS(lci+9,cci+63);DEVOUT('Apto.:')
        DEVPOS(lci+10,cci+1);DEVOUT('Bairro.....:')
        DEVPOS(lci+10,cci+36);DEVOUT('Cidade:')
        DEVPOS(lci+10,cci+66);DEVOUT('Estado:')
        DEVPOS(lci+11,cci+1);DEVOUT('CEP........:')
        DEVPOS(lci+11,cci+26);DEVOUT('Fone:')
        DEVPOS(lci+12,cci+1);DEVOUT('Profissao.:')
        DEVPOS(lci+12,cci+35);DEVOUT('CPF.:')
        DEVPOS(lci+13,cci+1);DEVOUT('Fone......:')
        DEVPOS(lci+13,cci+24);DEVOUT('Fone:')
        DEVPOS(lci+13,cci+40);DEVOUT('Ramal:')
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
        mmatricula := aluno->matricula
        maluno     := aluno->aluno
        mgstt      := aluno->gstt
        msexo      := aluno->sexo
        ma_datan   := aluno->a_datan
        ma_natur   := aluno->a_natur
        ma_estado  := aluno->a_estado
        ma_nacion  := aluno->a_nacion
        mdata_ing  := aluno->data_ing
        mfaixa_1   := aluno->faixa_1
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
        setcor(3)
        DEVPOS(lci+1,cci+12);DEVOUT(mmatricula)

        @ lci+1,cci+28 SAY maluno PICT '@!'
        @ lci+2,cci+7  SAY mgstt PICT '@!'
        @ lci+2,cci+19 SAY msexo PICT '@!'
        @ lci+2,cci+28 SAY ma_datan
        @ lci+2,cci+49 SAY ma_natur PICT '@!'
        @ lci+2,cci+73 SAY ma_estado PICT '@!'
        @ lci+3,cci+7  SAY ma_nacion PICT '@!'
        @ lci+3,cci+27 SAY mdata_ing
        @ lci+3,cci+46 SAY mfaixa_1 PICT '@!'
        @ lci+3,cci+61 SAY mdt_tranf PICT '@!'
        @ lci+5,cci+14 SAY mpai PICT '@!'
        @ lci+5,cci+66 SAY mp_fone
        @ lci+6,cci+14 SAY mmae PICT '@!'
        @ lci+6,cci+66 SAY mm_fone
        @ lci+8,cci+14 SAY mresponsa
        @ lci+9,cci+14 SAY mr_rua PICT '@!'
        @ lci+9,cci+56 SAY mr_num PICT '@!'
        @ lci+9,cci+70 SAY mr_apto PICT '@!'

        @ lci+10,cci+14 SAY mr_bairro PICT '@!'
        @ lci+10,cci+44 SAY mr_cidade PICT '@!'
        @ lci+10,cci+74 SAY mr_estado PICT '@!'
        @ lci+11,cci+14 SAY mr_cep PICT '@!'
        @ lci+11,cci+32 SAY mr_fone PICT '@!'
        @ lci+12,cci+14 SAY mr_prof PICT '@!'
        @ lci+12,cci+41 SAY mr_cpf PICT '999999999-99'
        @ lci+13,cci+14 SAY mr_fone1 PICT '@!'
        @ lci+13,cci+30 SAY mr_fone2 PICT '@!'
        @ lci+13,cci+47 SAY mr_ramal PICT '@!'
        setcor(1)
        mensagem('Confirma a EXCLUSAO do ALUNO [S/n]:')
        opcao := 'N'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        IF ! BLOQREG()
                atencao('Nao foi possivel bloquear o arquivo')
                LOOP
        ENDIF
        DELE
        DBCOMMIT()
        DBUNLOCK()

        *****************
        SELE('cdmen');ORDSETFOCUS(1)
        GO TOP
        *****************
        IF ! BLOQARQ()
                atencao('Nao foi possivel bloquear o arquivo')
                LOOP
        ENDIF
        DELE FOR mmatricula = cdmen->matricula .AND. EMPTY(cdmen->data_pag)
        DBCOMMIT()
        DBUNLOCK()

        *****************
        SELE('nota');ORDSETFOCUS(1)
        GO TOP
        *****************
        IF ! BLOQARQ()
                atencao('Nao foi possivel bloquear o arquivo')
                LOOP
        ENDIF
        DELE FOR mmatricula = cdmen->matricula
        DBCOMMIT()
        DBUNLOCK()
        LOOP
ENDDO
RETURN NIL
