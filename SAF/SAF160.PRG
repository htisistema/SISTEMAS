*************************
* INCLUSAO DE CONTA APAGAR
*************************

FUNCTION sac160
***************
LOCAL MPRG:='SAC160'
LOCAL mcod_forn,mfabrica,mendereco,mbairro,mcidade,muf,mcep,mcpostal,mtel1,mtel2,;
      mfax,mcgc,minsc,mprazo_pag,mct_gerente,mct_vendedo,mct_fatura,;
      mct_cobran,mlimite,mobs,mbanco,mcarteira,opcao,li,ci,lb,cb,;
      mcodigo,tela,tela1,tela2,lci,cci,lba,cba,point,pode,mtipo,memail

IF ! ver_nivel(mprg,'INCLUSAO DE CONTAS APAGAR','15')
        RETURN NIL
ENDIF

lci := 02
cci := 01
lba := 22
cba := 78

tela:=SAVESCREEN(01,00,24,79)

WHILE .T.
        mcodigo := SPACE(4)
        opcao := 'S'
        mcod_forn := SPACE(4)
        mfabrica := SPACE(40)
        mtipo := 'FO'
        mendereco := SPACE(35)
        mbairro := SPACE(20)
        mcidade := SPACE(20)
        muf := SPACE(2)
        mcep := SPACE(9)
        mcpostal := SPACE(6)
        mtel1 := SPACE(14)
        mtel2 := SPACE(14)
        mfax := SPACE(14)
        mcgc := SPACE(14)
        minsc := SPACE(14)
        mcpf := SPACE(11)
        mcarteira := SPACE(1)
        mbanco := SPACE(1)
        mprazo_pag := SPACE(8)
        mct_gerente := SPACE(15)
        mct_vendedo := SPACE(15)
        mct_fatura := SPACE(15)
        mct_cobran := SPACE(15)
        mlimite := 0
        mobs := SPACE(30)
        memail := SPACE(40)
        botao(lci,cci,lba,cba)
        janela(lci,cba,' Inclusao de Fornecedor e OUTROS ')
        mensagem('Digite o Nome do Fabricante a ser Cadastrado - <ESC> p/Retornar')
        DEVPOS(lci+1,cci+1);DEVOUT('Codigo....:')
        DEVPOS(lci+1,cci+21);DEVOUT('Data Cadastro:')
        DEVPOS(lci+2,cci+1);DEVOUT('R.Social..:')
        DEVPOS(lci+2,cci+55);DEVOUT('Tipo de Conta:')

        @ lci+3,cci+1 TO lci+3,cba-1
        janela(lci+3,cci,' Identificacao ','*','*')
        DEVPOS(lci+4,cci+1);DEVOUT('CGC:')
        DEVPOS(lci+4,cci+25);DEVOUT('Insc.:')

        @ lci+5,cci+1 TO lci+5,cba-1
        janela(lci+5,cci,' Logradouro ','*','*')
        DEVPOS(lci+6,cci+1);DEVOUT('Endereco..:')
        DEVPOS(lci+7,cci+1);DEVOUT('Bairro....:')
        DEVPOS(lci+8,cci+1);DEVOUT('Cidade....:')
        DEVPOS(lci+8,cci+35);DEVOUT('UF...:')
        DEVPOS(lci+9,cci+1);DEVOUT('CEP.......:')
        DEVPOS(lci+9,cci+35);DEVOUT('C.P..:')
        DEVPOS(lci+10,cci+1);DEVOUT('EMAIL....:')

        @ lci+11,cci+1 TO lci+11,cba-1
        janela(lci+11,cci,' Telefones ','*','*')
        DEVPOS(lci+12,cci+1);DEVOUT('Fone:')
        DEVPOS(lci+12,cci+23);DEVOUT('Fone:')
        DEVPOS(lci+12,cci+45);DEVOUT('Fax:')

        @ lci+13,cci+1 TO lci+13,cba-1
        janela(lci+13,cci,' Cobranca ','*','*')
        DEVPOS(lci+14,cci+1);DEVOUT('Banco:')
        DEVPOS(lci+14,cci+11);DEVOUT('Carteira:')
        DEVPOS(lci+14,cci+24);DEVOUT('Prazo Pag.:')
        DEVPOS(lci+14,cci+46);DEVOUT('Limite:')

        @ lci+15,cci+1 TO lci+15,cba-1
        janela(lci+15,cci,' Contatos ','*','*')
        DEVPOS(lci+16,cci+1);DEVOUT('Gerente..:')
        DEVPOS(lci+16,cci+32);DEVOUT('Faturamento:')
        DEVPOS(lci+17,cci+1);DEVOUT('Cobranca.:')
        DEVPOS(lci+17,cci+32);DEVOUT('Vendedor...:')

        @ lci+18,cci+1 TO lci+18,cba-1
        DEVPOS(lci+19,cci+1);DEVOUT('Observacao.:')
        ***********
        SELE('ccpg');ORDSETFOCUS(1)
        GO BOTT
        ***********

        mcod_forn := STRZERO(VAL(cod_forn)+1,4)

        SET INTEN ON
        setcor(3)
        mdata_cad = mdata_sis

        setcor(3)
        DEVPOS(lci+1,cci+13);DEVOUT(mcod_forn)
        DEVPOS(lci+1,cci+36);DEVOUT(mdata_cad)
        setcor(1)
        @ lci+2,cci+13 GET mfabrica PICT '@!' VALID IF(EMPTY(mfabrica),.F.,.T.)
        @ lci+2,cci+70 GET mtipo PICT '@!' VALID mtipo $ 'FO,FV,VV,AD,TR,FI,DV' .AND. lim_get() WHEN men_get(lci+3,cci+75,'Informe o Tipo de Conta: <FO>rnecedor  <FV>Fixas Venda  <VV>Variavel Venda  <AD>ministrativa  <TR>ibutaria  <FI>nanceira  <DV>Diversos')
        @ lci+4,cci+6 GET mcgc PICT '@@R 99.999.999/9999-99' VALID pesq_cgc(mcgc,'ccpg','*')
        @ lci+4,cci+32 GET minsc PICT '@!'

        @ lci+6,cci+13 GET mendereco PICT '@!'
        @ lci+7,cci+13 GET mbairro PICT '@!'
        @ lci+8,cci+13 GET mcidade PICT '@!'
        @ lci+8,cci+42 GET muf PICT '@!'       // VALID IF(EMPTY(muf),.F.,VUF(muf))
        @ lci+9,cci+13 GET mcep PICT '99999-999'
        @ lci+9,cci+42 GET mcpostal PICT '999999'
        @ lci+10,cci+13 GET memail

        @ lci+12,cci+07 GET mtel1 PICT '(999)XXXXXXXXX'
        @ lci+12,cci+29 GET mtel2 PICT '(999)XXXXXXXXX'
        @ lci+12,cci+50 GET mfax PICT '(999)XXXXXXXXX'

        @ lci+14,cci+8 GET mbanco PICT '@!'
        @ lci+14,cci+21 GET mcarteira PICT '@!'
        @ lci+14,cci+36 GET mprazo_pag PICT '@R 99-99-99'
        @ lci+14,cci+54 GET mlimite PICT '9,999,999.99'

        @ lci+16,cci+12 GET mct_gerente PICT '@!'
        @ lci+16,cci+45 GET mct_fatura PICT '@!'
        @ lci+17,cci+12 GET mct_cobranca PICT '@!'
        @ lci+17,cci+45 GET mct_vendedo PICT '@!'

        @ lci+19,cci+14 GET mobs PICT '@!'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                SELE('ccpg');ORDSETFOCUS(2)
                GO TOP
                RETURN NIL
        ENDIF
        mensagem('Confirma a Inclusao [S/n]:')
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ELSE
                IF EMPTY(muf)
                        mensagem('Nao foi preenchido a campo do Estado, Campo Obrigatorio !!!')
                        @ lci+8,cci+42 GET muf PICT '@!' VALID IF(EMPTY(muf),.F.,VUF(muf))
                        READ
                        IF LASTKEY() = 27
                                LOOP
                        ENDIF
                ENDIF
                ADIREG()
                ccpg-> cod_forn := mcod_forn
                ccpg-> razao := mfabrica
                ccpg-> tipo := mtipo
                ccpg-> data_cad := mdata_cad
                ccpg-> endereco := mendereco
                ccpg-> bairro := mbairro
                ccpg-> cidade := mcidade
                ccpg-> uf := muf
                ccpg-> cep := mcep
                ccpg-> cpostal := mcpostal
                ccpg-> email := memail
                ccpg-> tel1 := mtel1
                ccpg-> tel2 := mtel2
                ccpg-> fax := mfax
                ccpg-> cgc := mcgc
                ccpg-> insc := minsc
                ccpg-> prazo_pag := mprazo_pag
                ccpg-> ct_gerente := mct_gerente
                ccpg-> ct_fatura := mct_fatura
                ccpg-> ct_cobran := mct_cobranca
                ccpg-> ct_vendedo := mct_vendedo
                ccpg-> limite := mlimite
                ccpg-> obs := mobs
                ccpg-> carteira := mcarteira
                ccpg-> banco := mbanco
                COMMIT
                UNLOCK
        ENDIF
ENDDO
RETURN NIL
********************************** f i m *******************************
