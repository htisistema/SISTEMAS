********************************
* INCLUSAO DE PAGAMENTOS
*******************************

FUNCTION saf203
***************
LOCAL mprg:='SAF203'
LOCAL tela,opcao,lci,cci,lba,cba
LOCAL mgstt,mmatricula,mparcela,mdata_venc,mvalor,mbolsa,mtipo,mcontrato,mano

IF ! ver_nivel(mprg,'INCLUSAO DE DOCUMENTOS','125')
        RETURN NIL
ENDIF

tela := SAVESCREEN(00,00,24,79)

lci := 05
cci := 08
lba := 14
cba := 72

*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
*--------------------------------------------------------------------------

mano := SPACE(4)
WHILE .T.
        mgstt       := SPACE(4)
        mmatricula  := SPACE(6)
        mparcela    := SPACE(2)
        mvalor      := 0
        mbolsa      := SPACE(1)
        mdata_venc  := CTOD('  /  /  ')
        mtipo       := SPACE(2)
        mcontrato   := SPACE(8)
        @ 23,00 CLEAR TO 23,79
        setcor(3)
        botao(lci,cci,lba,cba,,' INCLUSAO DE DOCUMENTOS ')
        setcor(1)
        DEVPOS(lci+1,cci+1);DEVOUT('Ano de Competencia:')
        DEVPOS(lci+2,cci+1);DEVOUT('Matricula.........:')
        DEVPOS(lci+3,cci+1);DEVOUT('Nome..............:')
        DEVPOS(lci+4,cci+1);DEVOUT('GSTT..............:')
        DEVPOS(lci+5,cci+1);DEVOUT('Tipo Documento....:')
        DEVPOS(lci+6,cci+1);DEVOUT('Parcela...[01..12]:')
        DEVPOS(lci+6,cci+24);DEVOUT('Bolsa:')
        DEVPOS(lci+7,cci+1);DEVOUT('Valor ..........R$:')
        DEVPOS(lci+8,cci+1);DEVOUT('Data Vencimento...:')
        setcor(1)
        *****************
        SELE('aluno');ORDSETFOCUS(4)
        GO TOP
        *****************
        @ lci+1,cci+21 GET mano PICT '9999'
        @ lci+2,cci+21 GET mmatricula PICT '999999' VALID IF(EMPTY(mmatricula),.F.,.T.)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF ! aluno->(DBSEEK(mmatricula))
                atencao('Matricula nao encontrada no arquivo')
                LOOP
        ENDIF
        setcor(3)
        DEVPOS(lci+3,cci+21);DEVOUT(aluno->aluno)
        DEVPOS(lci+4,cci+21);DEVOUT(aluno->gstt)
        mgstt     := aluno->gstt
        mcontrato := aluno->contrato

        SETCOR(1)
        @ lci+5,cci+21 GET mtipo      PICT '@!' VALID mtipo $ 'ME,NT,FT,FS,HB,VL,PO,GO,DV,CO,DN,BQ,JD' .AND. lim_get() WHEN men_get(lci+6,cci+21,'Informe o tipo de documento: [ME] Mensalidade Escolar - [NT] Natacao - [FT] Futebol - [FS] Futsal - [HB] Handbol - [VL] Voleybol - [PO] Polo - [GO] Ginastica Olimpica - [CO] Coral - [DN] Danca - [BQ] Basquete - [JD] Judo - [DV] Diversos')
        @ lci+6,cci+21 GET mparcela   PICT '99'
        mbolsa  := IF(mtipo = 'ME',ver_faixa(mparcela),SPACE(1))
        @ lci+6,cci+31 GET mbolsa WHEN mtipo = 'ME'
        @ lci+7,cci+21 GET mvalor     PICT '9,999.99'
        @ lci+8,cci+21 GET mdata_venc
        READ
        IF LASTKEY() = 27
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF

        ***************
        SELE('cdmen');ORDSETFOCUS(1)
        GO TOP
        ***************
        IF cdmen->(DBSEEK(aluno->matricula+mparcela+mtipo))
                mensagem('Documento p/ Parcela j  existente, confirma inclusao [S/n]:')
                opcao := ' '
        ELSE
                mensagem('Confirma a inclusao do documento [S/n]:')
                opcao := 'S'
        ENDIF
        @ MAXROW()-1,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        IF ! ADIREG()
                atencao('Nao foi possivel criar o registro - CDMENSAL')
                LOOP
        ENDIF
        cdmen->ano        := mano
        cdmen->contrato   := mcontrato
        cdmen->gstt       := mgstt
        cdmen->matricula  := mmatricula
        IF ! EMPTY(mparcela)
                cdmen->tipo       := mtipo
*               mbolsa            := ver_faixa(mparcela)
                cdmen->bolsa      := mbolsa
                cdmen->parcela    := mparcela
                cdmen->data_venc  := mdata_venc
*               mvalor := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),mparcela)
                cdmen->vlr_parc   := mvalor
                IF (faixa_vlr(mbolsa) = 100 .OR. EMPTY(mbolsa)) .AND. mtipo = 'ME'
                        cdmen->desc_bolsa := mvalor
                ELSEIF mtipo = 'ME'
                        cdmen->desc_bolsa := mvalor * (faixa_vlr(mbolsa)/100)
                ENDIF
        ELSE
                cdmen->tipo       := mtipo
                cdmen->data_venc  := mdata_venc
                cdmen->vlr_parc   := mvalor
        ENDIF
        cdmen->flag       := ' '
        DBUNLOCKALL()
        DBCOMMITALL()
ENDDO
RETURN NIL
