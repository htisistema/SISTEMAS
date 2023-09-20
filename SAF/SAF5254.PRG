************************************************
* EMISSAO DE CARNES DO MES (ESCOLINHA)
************************************************

FUNCTION saf5254
***************
LOCAL mprg:='SAF5254'
LOCAL lci,cci,lba,cba,opcao,mquantd,mtraco,mtit,mtipo,mmes,i,mcont_aluno,;
      mcont_parc,mdesconto,mvlr_men,mgstt,mtot_men,mtot_ger,mindex,minicio,;
      tela,mgst,mmatricula,mop,maluno,mano
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)
PRIVATE m_parcela:={},mquantd_parc

IF ! ver_nivel(mprg,'EMISSAO DE CARNES DO MES (ESCOLINHA)','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
lci := 06
cci := 05
lba := 14
cba := 75
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        mop := 0
        mgst := SPACE(4)
        mmatricula := SPACE(6)
        maluno     := SPACE(40)
        mindex := 0
        mmes := SPACE(2)
        mquantd = 1
        mano := '    '
        mtipo := '  '
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        @ lci+2,cci+1 TO lci+2,cba-1
        @ lci+4,cci+1 TO lci+4,cba-1
        DEVPOS(lci+1,cci+1);DEVOUT('Digite a Parcela que deseja:     Ano: 9999  Tipo:')
        @ lci+1,cci+30 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
        @ lci+1,cci+39 GET mano PICT '9999'
        @ lci+1,cci+51 GET mtipo PICT '@!'
        READ
        IF LASTKEY() = 27
                CLOSE ALL
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        @ lci+3,cci+1 PROMPT ' P/GSTT ' MESSAGE '** Relacao de debitos por GSTT - <ESC> Retornar **'
        @ lci+3,cci+11 PROMPT ' INDIVIDUAL  ' MESSAGE '** Emissao de carne INDIVIDUAL - <ESC> Retornar **'
        @ lci+3,cci+26 PROMPT ' GERAL ' MESSAGE '** Relatorio de DEBITO geral - <ESC> p/retornar **'
        SET INTEN ON
        MENU TO mop
        IF LASTKEY() = 27
                CLOSE ALL
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF mop = 1
                DEVPOS(lci+5,cci+1);DEVOUT('Digite a GSTT : ')
                @ lci+5,cci+17 GET mgst PICT '@!'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                ***************
                SELE('cdmen');ORDSETFOCUS(2)
                GO TOP
                ***************
                IF ! cdmen->(DBSEEK(mmes+mgst+' '+mano))
                        atencao('Nao foi encontrado nenhum aluno com este GSTT')
                        LOOP
                ENDIF
        ELSEIF mop = 2
                DEVPOS(lci+5,cci+1);DEVOUT('Digite a MATRICULA : ')
                @ lci+5,cci+22 GET mmatricula PICT '999999'
                @ lci+5,cci+29 GET maluno PICT '@!' WHEN EMPTY(mmatricula)
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF ! EMPTY(mmatricula)
                        ****************************
                        SELE('aluno');ORDSETFOCUS(4)
                        GO TOP
                        ****************************
                        IF ! aluno->(DBSEEK(mmatricula))
                                atencao('Matricula nao encontrada no arquivo')
                                LOOP
                        ENDIF
                ELSEIF ! EMPTY(maluno)
                        ****************************
                        SELE('aluno');ORDSETFOCUS(3)
                        GO TOP
                        ****************************
                        IF ! aluno->(DBSEEK(RTRIM(maluno)))
                                atencao('Aluno nao encontrada no arquivo')
                                LOOP
                        ENDIF
                        mmatricula := f7_aluno()
                ELSE
                        LOOP
                ENDIF
                ***************
                SELE('cdmen');ORDSETFOCUS(1)
                GO TOP
                ***************
                IF ! cdmen->(DBSEEK(aluno->matricula+mano+mmes+mtipo))
                        atencao('Nao foi encontrado nenhum aluno com este GSTT')
                        LOOP
                ENDIF

        ENDIF
        setcor(3)
        DEVPOS(lci+5,cci+22);DEVOUT(aluno->matricula)
        DEVPOS(lci+5,cci+29);DEVOUT(aluno->aluno)
        setcor(1)
        mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
        IF ! imp_arq('CARNE.REL')
                LOOP
        ENDIF
        IF mop = 3
                ***************
                SELE('cdmen');ORDSETFOCUS(2)
                GO TOP
                ***************
                cdmen->(DBSEEK(mmes))
                i := 0
                WHILE ! EOF() .AND. cdmen->parcela = mmes
                        IF ! ver_aluno(cdmen->matricula,'*');
                           .OR. mano <> cdmen->ano;
                           .OR. mmes < SUBSTR(DTOC(aluno->data_ing),4,2);
                           .OR. ! EMPTY(aluno->dt_tranf);
                           .OR. cdmen->tipo <> mtipo;
                           .OR. ! EMPTY(cdmen->data_pag)
                                SKIP
                                LOOP
                        ENDIF
                        IF maut = 'NOBREGA'
                                imprt('M','N')
                                DEVPOS(PROW()+4,00);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                                DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                                DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),86);DEVOUT(aluno->gstt+'-'+cdmen->tipo)
                                DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                                DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                                DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                                DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                                DEVPOS(PROW(),33);DEVOUT(mmes+'a.Parcela-'+cdmen->tipo)
                                DEVPOS(PROW(),51);DEVOUT(mmes)
                                DEVPOS(PROW(),57);DEVOUT(cdmen->data_venc)
                                DEVPOS(PROW(),76);DEVOUT(mmes)
                                DEVPOS(PROW(),81);DEVOUT(cdmen->data_venc)
                                DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(cdmen->vlr_parc,'9,999.99'))
                                DEVPOS(PROW(),55);DEVOUT(TRANSFORM(cdmen->vlr_parc,'9,999.99'))
                                DEVPOS(PROW(),81);DEVOUT(TRANSFORM(cdmen->vlr_parc,'9,999.99'))
                                DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                                DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                                DEVPOS(PROW()+4,00);DEVOUT(' ')
                        ELSE
                                imprt('M','N')
                                DEVPOS(PROW()+1,00);DEVOUT(memp+resa)
                                DEVPOS(PROW(),00);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                                DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                                DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                                DEVPOS(PROW(),86);DEVOUT(aluno->gstt)
                                DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                                DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                                DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                                DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                                DEVPOS(PROW(),33);DEVOUT(mmes+'a.Parcela')
                                DEVPOS(PROW(),51);DEVOUT(mmes)
                                DEVPOS(PROW(),57);DEVOUT(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
*                               DEVPOS(PROW(),57);DEVOUT('05/'+mmes+'/'+SUBSTR(mdiretorio,3,2))
                                DEVPOS(PROW(),76);DEVOUT(mmes)
                                DEVPOS(PROW(),81);DEVOUT(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3,2))
*                               DEVPOS(PROW(),81);DEVOUT('05/'+mmes+'/'+SUBSTR(mdiretorio,3,2))
                                DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW(),55);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW(),81);DEVOUT(TRANSFORM(mvlr_men,'9,999.99'))
                                DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                                DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                                DEVPOS(PROW()+4,00);DEVOUT(' ')
                        ENDIF
                        SKIP
                ENDDO
        ELSEIF mop = 1
                ***************
                SELE('cdmen');ORDSETFOCUS(2)
                GO TOP
                ***************
                cdmen->(DBSEEK(mmes+mgst+' '+mano))
                i := 0
                WHILE mmes = cdmen->parcela .AND. ! EOF()
                        IF ! ver_aluno(cdmen->matricula,'*');
                           .OR. cdmen->ano <> mano;
                           .OR. cdmen->tipo <> mtipo;
                           .OR. cdmen->gstt <> mgst;
                           .OR. ! EMPTY(cdmen->data_pag)
                                SKIP
                                LOOP
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        imprt('M','N')
                        DEVPOS(PROW()+4,00);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                        DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                        DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                        DEVPOS(PROW(),86);DEVOUT(aluno->gstt+'-'+cdmen->tipo)
                        DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                        DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                        DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                        DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                        DEVPOS(PROW(),33);DEVOUT(mmes+'a.Parcela-'+cdmen->tipo)
                        DEVPOS(PROW(),51);DEVOUT(mmes)
                        DEVPOS(PROW(),57);DEVOUT(cdmen->data_venc)
                        DEVPOS(PROW(),76);DEVOUT(mmes)
                        DEVPOS(PROW(),81);DEVOUT(cdmen->data_venc)
                        DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(cdmen->vlr_parc,'9,999.99'))
                        DEVPOS(PROW(),55);DEVOUT(TRANSFORM(cdmen->vlr_parc,'9,999.99'))
                        DEVPOS(PROW(),81);DEVOUT(TRANSFORM(cdmen->vlr_parc,'9,999.99'))
                        DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                        DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                        DEVPOS(PROW()+4,00);DEVOUT(' ')
                        SKIP
                ENDDO
        ELSEIF mop = 2
                imprt('M','N')
                DEVPOS(PROW()+4,00);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),10);DEVOUT(aluno->aluno)
                DEVPOS(PROW(),51);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),61);DEVOUT(aluno->gstt)
                DEVPOS(PROW(),76);DEVOUT(aluno->matricula)
                DEVPOS(PROW(),86);DEVOUT(aluno->gstt+'-'+cdmen->tipo)
                DEVPOS(PROW()+2,02);DEVOUT(SUBSTR(aluno->gstt,1,1))
                DEVPOS(PROW(),10);DEVOUT(SUBSTR(aluno->gstt,2,1))
                DEVPOS(PROW(),16);DEVOUT(SUBSTR(aluno->gstt,3,1))
                DEVPOS(PROW(),24);DEVOUT(SUBSTR(aluno->gstt,4,1))
                DEVPOS(PROW(),33);DEVOUT(mmes+'a.Parcela-'+cdmen->tipo)
                DEVPOS(PROW(),51);DEVOUT(mmes)
                DEVPOS(PROW(),57);DEVOUT(cdmen->data_venc)
                DEVPOS(PROW(),76);DEVOUT(mmes)
                DEVPOS(PROW(),81);DEVOUT(cdmen->data_venc)
                DEVPOS(PROW()+2,02);DEVOUT(TRANSFORM(cdmen->vlr_parc,'9,999.99'))
                DEVPOS(PROW(),55);DEVOUT(TRANSFORM(cdmen->vlr_parc,'9,999.99'))
                DEVPOS(PROW(),81);DEVOUT(TRANSFORM(cdmen->vlr_parc,'9,999.99'))
                DEVPOS(PROW()+4,00);DEVOUT('TESOURARIA DO COLEGIO NOBREGA')
                DEVPOS(PROW()+6,51);DEVOUT(aluno->aluno)
                DEVPOS(PROW()+4,00);DEVOUT(' ')
        ENDIF
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('CARNE.REL')
        ENDIF
ENDDO
RETURN NIL
**************************** F I M **************************

