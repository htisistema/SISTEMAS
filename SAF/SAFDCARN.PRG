*********************************************
* FUNCAO P/GERAR DISCO P/IMPRESSAO DE CARNES
*********************************************

FUNCTION safdcarn
*****************
LOCAL mprg:='SAFDCARN'
LOCAL tela,opcao,lci,cci,lba,cba,mparcela,msele,morde,mseq,mlin,mvalor,mfaixa,;
      mbanlinh1,mbanlinh2,mbanlinh3,mbanlinh4,mbanlinh5,mvlr_bolsa:=0

IF ! ver_nivel(mprg,'GERAR DISCO P/IMPRESSAO DE CARNES','125')
        RETURN NIL
ENDIF

lci := 10
cci := 03
lba := 17
cba := 79
tela := SAVESCREEN(00,00,24,79)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('nobrega','nobre','E');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
**************
SELE('nobre')
**************
mparcela := SPACE(2)
mbanlinh1 := nobre->banlinh1
mbanlinh2 := nobre->banlinh2
mbanlinh3 := nobre->banlinh3
mbanlinh4 := nobre->banlinh4
mbanlinh5 := nobre->banlinh5
@ 23,00 CLEAR TO 23,79
setcor(3)
botao(lci,cci,lba,cba)
setcor(1)
DEVPOS(lci+1,cci+1);DEVOUT('Digite Parcela que deseja [01...12]:')
DEVPOS(lci+2,cci+1);DEVOUT('Instrucoes:')
@ lci+1,cci+38 GET mparcela PICT '99' VALID mparcela $ '01,02,03,04,05,06,07,08,09,10,11,12'
@ lci+2,cci+13 GET mbanlinh1 PICT '@!'
@ lci+3,cci+13 GET mbanlinh2 PICT '@!'
@ lci+4,cci+13 GET mbanlinh3 PICT '@!'
@ lci+5,cci+13 GET mbanlinh4 PICT '@!'
@ lci+6,cci+13 GET mbanlinh5 PICT '@!'
READ
IF LASTKEY() = 27
        RESTSCREEN(00,00,24,79,tela)
        RETURN NIL
ENDIF
opcao := ' '
opcao := mensagem1('Confirma a Operacao:','S','S,N')
IF LASTKEY() = 27 .OR. opcao = 'N'
        RETURN NIL
ENDIF
**************
SELE('nobre')
ZAP
**************
********************************
SELE('aluno');ORDSETFOCUS(6)
GO TOP
********************************
msele := SELE()
morde := INDEXORD()
mseq := 1
mlin := SAVESCREEN(24,00,24,79)
WHILE ! EOF()
        limpa(24,00,24,79)
        DEVPOS(24,00);DEVOUT('Esta Gerando o carne em arquivo, aguarde processamento MAT: '+aluno->matricula)
        IF aluno->gstt = 'DEBI'
                SKIP
                LOOP
        ENDIF
        IF mparcela = '01'
                mfaixa := aluno->faixa_1
        ELSEIF mparcela = '02'
                mfaixa := aluno->faixa_2
        ELSEIF mparcela = '03'
                mfaixa := aluno->faixa_3
        ELSEIF mparcela = '04'
                mfaixa := aluno->faixa_4
        ELSEIF mparcela = '05'
                mfaixa := aluno->faixa_5
        ELSEIF mparcela = '06'
                mfaixa := aluno->faixa_6
        ELSEIF mparcela = '07'
                mfaixa := aluno->faixa_7
        ELSEIF mparcela = '08'
                mfaixa := aluno->faixa_8
        ELSEIF mparcela = '09'
                mfaixa := aluno->faixa_9
        ELSEIF mparcela = '10'
                mfaixa := aluno->faixa_10
        ELSEIF mparcela = '11'
                mfaixa := aluno->faixa_11
        ELSEIF mparcela = '12'
                mfaixa := aluno->faixa_12
        ENDIF
        IF ! EMPTY(aluno->dt_tranf)
                SKIP
                LOOP
        ENDIF
        IF ! ver_tab(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),mparcela)
                atencao('Este aluno nao foi incluso GSTT: '+aluno->GSTT+' - Matricula: '+aluno->matricula+'-'+aluno->aluno)
                SKIP
                LOOP
        ENDIF
        mvalor := 0
        mvlr_bolsa := 0
        IF ver_fx(mfaixa)
                mvlr_bolsa := tabmen->valor - (tabmen->valor*(fx->desconto/100))
        ENDIF
        mvalor := tabmen->valor
        IF fx->desconto = 100 .OR. mfaixa = 'S'
                SKIP
                LOOP
        ENDIF
        IF ! matparc(aluno->matricula,mparcela)
                SKIP
                LOOP
        ENDIF
        **************
        SELE('nobre')
        **************
        IF ! ADIREG()
                atencao('Nao foi possivel adicionar o registro no arquivo')
                SELE(msele);ORDSETFOCUS(morde)
                LOOP
        ENDIF
        nobre->banseque := mseq
        nobre->banumero := aluno->matricula+mparcela
        nobre->banomecl := memp+resa
        nobre->banvalor := mvalor
        nobre->bandatap := CTOD(tabmen->data_venc+'/'+SUBSTR(mdiretorio,3))
        nobre->banlinh1 := '- Ate o vencimento receber o valor R$:'+TRANSFORM(mvlr_bolsa,'999.99')
        nobre->banlinh2 := '- Ate 30 dias apos vencimento receber R$:'+TRANSFORM(mvlr_bolsa*1.02,'999.99')
        nobre->banlinh3 := '- Apos 30 dias do vencimento receber o valor  do  documento'
        nobre->banlinh4 := '  mais multa de 2% ao mes e juros de 1% ao mes   '
        nobre->banlinh5 := '- Apos 60 dias sera encaminhado para protesto  '
/*
        nobre->banlinh1 := mbanlinh1
        nobre->banlinh2 := mbanlinh2
        nobre->banlinh3 := mbanlinh3
        nobre->banlinh4 := mbanlinh4
        IF mvlr_bolsa <> mvalor .AND. SUBSTR(mbanlinh5,1,3) = 'OBS'
                nobre->banlinh5 := ALLTRIM(mbanlinh5)+ALLTRIM(TRANSFORM(mvlr_bolsa,'9,999.99'))
        ENDIF
*/
        nobre->banlinf1 := aluno->responsa+'- CPF: '+aluno->r_cpf
        nobre->banlinf2 := RTRIM(aluno->r_rua)+' '+RTRIM(aluno->r_num)+' '+RTRIM(aluno->r_apto)+'-'+RTRIM(aluno->r_bairro)+'-'+RTRIM(aluno->r_cidade)+'-'+RTRIM(aluno->r_estado)
        nobre->banlinf3 := aluno->gstt+' - '+aluno->aluno
        nobre->conta    := '1030.003.00001074-0'
        DBUNLOCK()
        DBCOMMIT()
        SELE(msele);ORDSETFOCUS(morde)
        mseq ++
        SKIP
ENDDO
RESTSCREEN(24,00,24,79,mlin)
CLOSE ALL
atencao('Processamento concluido com sucesso',3)
mensagem('Coloque o disco do drive e presione qualquer tecla para continuar')
INKEY(0)
IF LASTKEY() = 27
        RETURN NIL
ENDIF
mensagem('Copiando o arquivo NOBREGA.DBF para o diskete aguarde .....')
COPY FILE ('\NOBREGA\'+mdiretorio+'\NOBREGA.DBF') TO A:NOBREGA.DBF
atencao('Fim da Copia do arquivo NOBREGA.DBF com sucesso')
RETURN NIL

