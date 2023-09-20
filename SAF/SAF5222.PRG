************************************************
* ETIQUETAS DE ALUNOS
************************************************

FUNCTION saf5222
***************
LOCAL mprg:='SAF5222'
LOCAL lci,cci,lba,cba,tela,mmatricula,mgst,mtipo_con,;
      opcao,mquantd,mtraco,mmes,i,mcont_aluno,mcont_parc,mdesconto,;
      mgstt,mindex

PRIVATE m_parcela:={}

IF ! ver_nivel(mprg,'RELATORIO DE ETIQUETAS DE ALUNOS','125')
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
        mtipo_con := 0
        mgst := SPACE(4)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        CAIXA(mold,lci,cci,lba,cba,,,.T.)
        @ lci+2,cci+1 TO lci+2,cba-1
        setcor(1)
        @ lci+1,cci+4 PROMPT ' Individual ' MESSAGE 'Emissao de Boletim INDIVIDUAL'
        @ lci+1,COL()+4 PROMPT '    GSTT    ' MESSAGE 'Emissao de Boletim por Grau, Serie, Turma e turno'
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
                mensagem('Confirma a Impressao [S/n]:')
                opcao = 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF opcao = 'N'
                        CLOSE ALL
                        RETURN NIL
                ENDIF
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                SET DEVI TO PRINT
                * * SET PRINT TO LPT1
                imprt('M','C')
                DEVPOS(PROW(),00);DEVOUT(aluno->aluno+' - '+aluno->gstt)
                DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - '+RTRIM(aluno->r_apto))
                DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+RTRIM(aluno->r_estado))
                DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_cep))
                imprt('M','N',3)
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
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
                mensagem('Confirma a Impressao [S/n]:')
                opcao = 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF opcao = 'N'
                        CLOSE ALL
                        RETURN NIL
                ENDIF
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                SET DEVI TO PRINT
                * * SET PRINT TO LPT1
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
                        imprt('M','C')
                        DEVPOS(PROW(),00);DEVOUT(aluno->aluno+' - '+aluno->gstt)
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - '+RTRIM(aluno->r_apto))
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+RTRIM(aluno->r_estado))
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_cep))
                        imprt('M','C',3)
                        SKIP
                ENDDO
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
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
                mensagem('Confirma a Impressao [S/n]:')
                opcao = 'S'
                @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
                READ
                IF opcao = 'N'
                        CLOSE ALL
                        RETURN NIL
                ENDIF
                IF LASTKEY() = 27
                        LOOP
                ENDIF
                mensagem('Espere o Final da Impressao OK - [ESC]Abandonar')
                SET DEVI TO PRINT
                * * SET PRINT TO LPT1
                i := 0
                WHILE ! EOF()
                        INKEY(.1)
                        IF LASTKEY() = 27
                                DEVPOS(PROW()+1,00);DEVOUT('Relatorio foi cancelado pelo operador')
                                EXIT
                        ENDIF
                        IF ! EMPTY(aluno->dt_tranf)
                                SKIP
                                LOOP
                        ENDIF
                        imprt('M','C')
                        DEVPOS(PROW(),00);DEVOUT(aluno->aluno+' - '+aluno->gstt)
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+' - '+RTRIM(aluno->r_apto))
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+RTRIM(aluno->r_estado))
                        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(aluno->r_cep))
                        imprt('M','C',3)
                        SKIP
                ENDDO
                SETPRC(00,00)
                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                LOOP
        ENDIF
ENDDO
**************************** F I M **************************

