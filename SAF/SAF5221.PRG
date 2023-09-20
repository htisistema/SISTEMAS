************************************************
* ETIQUETAS PARA COBRANCA DE ALUNOS EM DEBITOS
************************************************

FUNCTION saf5221
***************
LOCAL mprg:='SAF5221'
LOCAL lci,cci,lba,cba,tela,mmatricula,mgst,mtipo_con,;
      opcao,mquantd,mtraco,mmes,i,mcont_aluno,mcont_parc,mdesconto,;
      mgstt,mindex

PRIVATE m_parcela:={},mquantd_parc:=0
PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

IF ! ver_nivel(mprg,'RELATORIO ETIQUETAS P/COBRANCAS MENSALIDADES','125')
        RETURN NIL
ENDIF

mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
lci := 10
cci := 08
lba := 14
cba := 70
tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        mmatricula := SPACE(6)
        mtipo_con  := 0
        mgst       := SPACE(4)
        setcor(3)
        BOTAO(lci,cci,lba,cba)
        @ lci+2,cci+1 TO lci+2,cba-1
        setcor(1)
        @ lci+1,cci+4 PROMPT ' Individual ' MESSAGE 'Emissao de Etiqueta INDIVIDUAL'
        @ lci+1,COL()+4 PROMPT '    GSTT    ' MESSAGE 'Emissao de Etiquetas por Grau, Serie, Turma e turno'
        @ lci+1,COL()+4 PROMPT '    Geral   ' MESSAGE 'Emissao de Etiquetas em GERAL'
        SET INTEN ON
        MENU TO mtipo_con

        IF LASTKEY() = 27
                CLOSE ALL
                RESTSCREEN(01,00,24,79,tela)
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
                mtot_men   := 0
                mtot_ger   := 0
                mcont_aluno:= 0
                mcont_parc := 0
                mmes       := SPACE(2)
                mquantd    := 1
                mensagem('Digite ate que MES deseja: ')
                @ 23,COL()+1 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN NIL
                ENDIF

                i         := 0
                mdesconto := 0
                mvlr_men  := 0
                ASIZE(m_parcela,0)

                ***************
                SELE('cdmen');ORDSETFOCUS(1)
                GO TOP
                ***************
                IF cdmen->(DBSEEK(aluno->matricula))
                        WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. SUBSTR(DTOC(cdmen->data_venc),4,2) <= mmes
                                IF EMPTY(cdmen->data_pag)
                                        AADD(m_parcela,{cdmen->parcela,TRANSFORM(cdmen->vlr_parc,'9,999.99')})
                                ENDIF
                                SKIP
                        ENDDO
                ENDIF

                ***************
                SELE('aluno');ORDSETFOCUS(4)
                ***************

                IF LEN(m_parcela) == 0
                        atencao('Este aluno nao esta em DEBITO com o colegio')
                        LOOP
                ENDIF
                mdesconto := faixa_vlr(aluno->faixa_1)
                IF mdesconto == 100
                        atencao('Este aluno e BOLSISTA INTEGRAL')
                        LOOP
                ENDIF
                IF ! imp_arq('ETQ_DEB.REL')
                       LOOP
                ENDIF
                imprt('M','C')
                DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - '+RTRIM(aluno->r_apto))
                DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+RTRIM(aluno->r_estado))
                DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_cep))
                imprt('M','N',3)
        ELSEIF mtipo_con = 2
                *****************
                SELE('aluno');ORDSETFOCUS(2)
                GO TOP
                *****************
                DEVPOS(lci+3,cci+1);DEVOUT('GSTT:')
                @ lci+3,COL()+2 GET mgst PICT '@!'
                READ
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                IF ! aluno->(DBSEEK(mgst))
                        atencao('Nao foi possivel encontrar esta TURMA')
                        LOOP
                ENDIF
                mmes := SPACE(2)
                mensagem('Digite ate que MES deseja: ')
                @ 23,COL()+1 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN NIL
                ENDIF
                IF ! imp_arq('ETQ_DEB.REL')
                        LOOP
                ENDIF

                i := 0
                WHILE mgst == aluno->gstt .AND. ! EOF()
                        INKEY(.1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mdesconto := 0
                        mvlr_men := 0
                        ASIZE(m_parcela,0)

                        ***************
                        SELE('cdmen');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                        IF cdmen->(DBSEEK(aluno->matricula))
                                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. SUBSTR(DTOC(cdmen->data_venc),4,2) <= mmes
                                        IF EMPTY(cdmen->data_pag)
                                                AADD(m_parcela,{cdmen->parcela,TRANSFORM(cdmen->vlr_parc,'9,999.99')})
                                        ENDIF
                                        SKIP
                                ENDDO
                        ENDIF

                        ***************
                        SELE('aluno');ORDSETFOCUS(2)
                        ***************


                        IF LEN(m_parcela) == 0
                                SKIP
                                LOOP
                        ENDIF
                        mdesconto := faixa_vlr(aluno->faixa_1)
                        IF mdesconto == 100
                                SKIP
                                LOOP
                        ENDIF
                        imprt('M','C')
                        DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - '+RTRIM(aluno->r_apto))
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+RTRIM(aluno->r_estado))
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_cep))
                        imprt('M','C',3)
                        SKIP
                ENDDO
        ELSEIF mtipo_con = 3
                mindex := 0
                mtot_men := 0
                mtot_ger := 0
                mcont_aluno := 0
                mcont_parc := 0
                mmes := SPACE(2)
                mensagem('Digite ate que MES deseja: ')
                @ 23,COL()+1 GET mmes PICT '99' VALID mmes $ '01,02,03,04,05,06,07,08,09,10,11,12'
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN NIL
                ENDIF
                mensagem('Por ordem de:[1] p/Resp.[2]GSTT [3]Aluno [4] Matricula: ')
                @ 23,COL()+1 GET mindex PICT '9' VALID IF(mindex<1 .OR. mindex > 4,.F.,.T.)
                READ
                IF LASTKEY() = 27
                        CLOSE ALL
                        RESTSCREEN(01,00,24,79,tela)
                        RETURN NIL
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

                IF ! imp_arq('ETQ_DEB.REL')
                       LOOP
                ENDIF
                i := 0
                WHILE ! EOF()
                        INKEY(.3)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        mtot_men := 0
                        mdesconto := 0
                        mvlr_men := 0
                        ASIZE(m_parcela,0)

                        ***************
                        SELE('cdmen');ORDSETFOCUS(1)
                        GO TOP
                        ***************
                        IF cdmen->(DBSEEK(aluno->matricula))
                                WHILE cdmen->matricula = aluno->matricula .AND. ! EOF() .AND. SUBSTR(DTOC(cdmen->data_venc),4,2) <= mmes
                                        IF EMPTY(cdmen->data_pag)
                                                AADD(m_parcela,{cdmen->parcela,TRANSFORM(cdmen->vlr_parc,'9,999.99')})
                                        ENDIF
                                        SKIP
                                ENDDO
                        ENDIF

                        IF mindex = 1
                                ***************
                                SELE('aluno');ORDSETFOCUS(1)
                                ***************
                        ELSEIF mindex = 2
                                ***************
                                SELE('aluno');ORDSETFOCUS(2)
                                ***************
                        ELSEIF mindex = 3
                                ***************
                                SELE('aluno');ORDSETFOCUS(3)
                                ***************
                        ELSE
                                ***************
                                SELE('aluno');ORDSETFOCUS(4)
                                ***************
                        ENDIF


                        IF LEN(m_parcela) == 0
                                SKIP
                                LOOP
                        ENDIF
                        mdesconto := faixa_vlr(aluno->faixa_1)
                        IF mdesconto == 100
                                SKIP
                                LOOP
                        ENDIF
                        imprt('M','C')
                        DEVPOS(PROW()+1,00);DEVOUT(aluno->responsa)
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - '+RTRIM(aluno->r_apto))
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+RTRIM(aluno->r_estado))
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_cep))
                        imprt('M','C',3)
                        SKIP
                ENDDO
        ENDIF
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        IF mimp_tipo = 2
                lertexto('ETQ_DEB.REL')
        ENDIF
ENDDO
**************************** F I M **************************

