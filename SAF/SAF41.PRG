*******************************
* CONSULTA DE ALUNOS
*******************************

FUNCTION saf41
**************
LOCAL mprg:='SAF41'
LOCAL tela,opcao,lci,cci,lba,cba
LOCAL maluno,mobs
PRIVATE mmatricula

IF ! ver_nivel(mprg,'CONSULTA DE CADASTRO ALUNOS','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(00,00,24,79)

lci := 05
cci := 01
lba := 21
cba := 79
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------

WHILE .T.
        mmatricula := SPACE(6)
        maluno     := SPACE(40)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        DEVPOS(lci,cci+1);DEVOUT('Contrato.:         ')
        DEVPOS(lci+1,cci+1);DEVOUT('Matricula:')
        DEVPOS(lci+1,cci+22);DEVOUT('Nome:')
        DEVPOS(lci+2,cci+1);DEVOUT('GSTT:')
        DEVPOS(lci+2,cci+13);DEVOUT('Sexo:')
        DEVPOS(lci+2,cci+21);DEVOUT('Nasc.:')
        DEVPOS(lci+2,cci+40);DEVOUT('Natural:')
        DEVPOS(lci+2,cci+65);DEVOUT('Estado:')
        DEVPOS(lci+3,cci+1);DEVOUT('Nac.:')
        DEVPOS(lci+3,cci+17);DEVOUT('Ingresso:')
        DEVPOS(lci+3,cci+39);DEVOUT('Matric.:')
        DEVPOS(lci+3,cci+60);DEVOUT('Transf.:')
        DEVPOS(lci+4,cci+1);DEVOUT('Bolsa: Mes: 01:  02:  03:  04:  05:  06:  07:  08:  09:  10:  11:  12:')
        @ lci+5,cci+1 TO lci+5,cba-1
        DEVPOS(lci+6,cci+1);DEVOUT('Nome do Pai:')
        DEVPOS(lci+6,cci+56);DEVOUT('Telefone:')
        DEVPOS(lci+7,cci+1);DEVOUT('Nome do Mae:')
        DEVPOS(lci+7,cci+56);DEVOUT('Telefone:')
        @ lci+8,cci+1 TO lci+8,cba-1
        DEVPOS(lci+9,cci+1);DEVOUT('Responsavel:')
        DEVPOS(lci+10,cci+1);DEVOUT('Endereco...:')
        DEVPOS(lci+10,cci+51);DEVOUT('No.:')
        DEVPOS(lci+10,cci+63);DEVOUT('Apto.:')
        DEVPOS(lci+11,cci+1);DEVOUT('Bairro.....:')
        DEVPOS(lci+11,cci+36);DEVOUT('Cidade:')
        DEVPOS(lci+11,cci+66);DEVOUT('Estado:')
        DEVPOS(lci+12,cci+1);DEVOUT('CEP........:')
        DEVPOS(lci+12,cci+26);DEVOUT('Fone:')
        DEVPOS(lci+13,cci+1);DEVOUT('Profissao.:')
        DEVPOS(lci+13,cci+35);DEVOUT('CPF.:')
        DEVPOS(lci+14,cci+1);DEVOUT('Fone......:')
        DEVPOS(lci+14,cci+24);DEVOUT('Fone:')
        DEVPOS(lci+14,cci+40);DEVOUT('Ramal:')
        DEVPOS(lci+15,cci+1);DEVOUT('OBS.:')
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
        WHILE .T.
                setcor(3)
                DEVPOS(lci,cci+12);DEVOUT(aluno->contrato)
                DEVPOS(lci+1,cci+12);DEVOUT(mmatricula)
                DEVPOS(lci+1,cci+28);DEVOUT(aluno->aluno)
                DEVPOS(lci+2,cci+7);DEVOUT(aluno->gstt)
                DEVPOS(lci+2,cci+19);DEVOUT(aluno->sexo)
                DEVPOS(lci+2,cci+28);DEVOUT(aluno->a_datan)
                DEVPOS(lci+2,cci+49);DEVOUT(aluno->a_natur)
                DEVPOS(lci+2,cci+73);DEVOUT(aluno->a_estado)
                DEVPOS(lci+3,cci+7);DEVOUT(aluno->a_nacion)
                DEVPOS(lci+3,cci+27);DEVOUT(aluno->ing_esco)
                DEVPOS(lci+3,cci+48);DEVOUT(aluno->data_ing)
                DEVPOS(lci+3,cci+68);DEVOUT(aluno->dt_tranf)
                DEVPOS(lci+4,cci+16);DEVOUT(aluno->faixa_1)
                DEVPOS(lci+4,cci+21);DEVOUT(aluno->faixa_2)
                DEVPOS(lci+4,cci+26);DEVOUT(aluno->faixa_3)
                DEVPOS(lci+4,cci+31);DEVOUT(aluno->faixa_4)
                DEVPOS(lci+4,cci+36);DEVOUT(aluno->faixa_5)
                DEVPOS(lci+4,cci+41);DEVOUT(aluno->faixa_6)
                DEVPOS(lci+4,cci+46);DEVOUT(aluno->faixa_7)
                DEVPOS(lci+4,cci+51);DEVOUT(aluno->faixa_8)
                DEVPOS(lci+4,cci+56);DEVOUT(aluno->faixa_9)
                DEVPOS(lci+4,cci+61);DEVOUT(aluno->faixa_10)
                DEVPOS(lci+4,cci+66);DEVOUT(aluno->faixa_11)
                DEVPOS(lci+4,cci+71);DEVOUT(aluno->faixa_12)
                DEVPOS(lci+6,cci+14);DEVOUT(aluno->pai)
                DEVPOS(lci+6,cci+66);DEVOUT(aluno->p_fone)
                DEVPOS(lci+7,cci+14);DEVOUT(aluno->mae)
                DEVPOS(lci+7,cci+66);DEVOUT(aluno->m_fone)
                DEVPOS(lci+9,cci+14);DEVOUT(aluno->responsa)
                DEVPOS(lci+10,cci+14);DEVOUT(aluno->r_rua)
                DEVPOS(lci+10,cci+56);DEVOUT(aluno->r_num)
                DEVPOS(lci+10,cci+70);DEVOUT(aluno->r_apto)

                DEVPOS(lci+11,cci+14);DEVOUT(aluno->r_bairro)
                DEVPOS(lci+11,cci+44);DEVOUT(aluno->r_cidade)
                DEVPOS(lci+11,cci+74);DEVOUT(aluno->r_estado)
                DEVPOS(lci+12,cci+14);DEVOUT(aluno->r_cep)
                DEVPOS(lci+12,cci+32);DEVOUT(aluno->r_fone)
                DEVPOS(lci+13,cci+14);DEVOUT(aluno->r_prof)
                DEVPOS(lci+13,cci+41);DEVOUT(aluno->r_cpf)
                DEVPOS(lci+14,cci+14);DEVOUT(aluno->r_fone1)
                DEVPOS(lci+14,cci+30);DEVOUT(aluno->r_fone2)
                DEVPOS(lci+14,cci+47);DEVOUT(aluno->r_ramal)
                DEVPOS(lci+15,cci+7);DEVOUT(aluno->obs)
                setcor(1)
                mensagem('[N] Notas - [P] Parcelas Pagas - [R] Parcelas a Receber - [ESC] Retornar')
                INKEY(0)
                IF LASTKEY() = 27
                        EXIT
                ELSEIF LASTKEY() = ASC('N') .OR. LASTKEY() = ASC('n')
                         saf509('*')
                ELSEIF LASTKEY() = ASC('P') .OR. LASTKEY() = ASC('p')
                        saf411()
                ELSEIF LASTKEY() = ASC('R') .OR. LASTKEY() = ASC('r')
                        saf412()
                ELSE
                        LOOP
                ENDIF
        ENDDO
ENDDO
RETURN NIL
