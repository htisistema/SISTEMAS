****************************
* EMITE RECIBO DA INSERCOM
****************************

FUNCTION saf63(mnome,mvalor,mdata)

LOCAL lci,cci,lba,cba,lin,lin1,lin2,lin3,rpreco,rrecibo,linhas := 0,;
      opcao,tela1,telarec,telaprin,mref,mref1,mref2,mref3,;
      mref4,mref5,mref6,traco,mquantd

PRIVATE mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35)

lci := 08
cci := 08
lba := 18
cba := 74
lin1 := '          Recebi de '
lin2 := ' a importancia de R$ '
lin3 := ' referente a '
opcao := SPACE(1)
traco := REPLI('-',80)
telarec := SAVESCREEN(01,00,24,79)
IF mnome = NIL
        mnome := SPACE(35)
        mnome := memp+resa+SPACE(35-LEN(memp+resa))
        mdata := mdata_sis
ENDIF
IF mvalor = NIL
        mvalor := 0
ENDIF
WHILE .T.
        mensagem('Preencha os campos  -  <ESC> p/Retornar')
        mref :=  'manutencao do sistema  SAF  (sistema  academico e'
        mref1 := 'financeiro) do mes                               '
        mref2 := SPACE(49)
        mref3 := SPACE(49)
        mref4 := SPACE(49)
        mref5 := SPACE(49)
        mquantd := 1
        setcor(3)
        botao(lci,cci,lba,cba)
        setcor(1)
        janela(lci,cba,' R E C I B O ')
        SET INTEN ON
        DEVPOS(lci+1,cci+2);DEVOUT('Data:')
        DEVPOS(lci+2,cci+2);DEVOUT('Nome:')
        DEVPOS(lci+3,cci+2);DEVOUT('Valor:')
        DEVPOS(lci+4,cci+2);DEVOUT('Referente a:')
        @ lci+1,cci+8 GET mdata PICT '99/99/99'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,telarec)
                RETURN NIL
        ENDIF

        @ lci+2,cci+8 GET mnome PICT '@!'
        @ lci+3,cci+9 GET mvalor PICT '9,999,999.99'
        @ lci+4,cci+15 GET mref
        @ lci+5,cci+15 GET mref1
        @ lci+6,cci+15 GET mref2
        @ lci+7,cci+15 GET mref3
        @ lci+8,cci+15 GET mref4
        @ lci+9,cci+15 GET mref5
 *      @ lci+10,cci+15 GET mref6
        READ
        IF LASTKEY() = 27
                LOOP
        ENDIF
*       mensagem('Quantidade de Copias:')
*       mquantd := 1
*       @ 23,COL()+1 GET mquantd PICT '9'
*       READ
*       IF LASTKEY() = 27
*               LOOP
*       ENDIF
        IF ! imp_arq('REC_INSER.REL','R')
                LOOP
        ENDIF
        rrecibo := lin1+RTRIM(mnome)+lin2+LTRIM(TRANSFORM(mvalor,'@E 9,999,999.99'))+' '+'('+RTRIM(EXT(mvalor,10))+')'+lin3         + RTRIM(mref) + RTRIM(mref1) + RTRIM(mref2) + RTRIM(mref3) + RTRIM(mref4) + RTRIM(mref5) + ', que damos plena quitacao da mesma'
*       SET DEVI TO PRINT
*       SET PRINT TO (cfg->prt_rel)
        FOR i = 1 TO mquantd
                insercom_b('',mnome,DTOC(mdata),DTOC(mdata),'CIBO',DTOC(mdata),mvalor,mref,mref1,mref2,mref3,mref4,mref5)
        NEXT
        EJECT
        SETPRC(00,00)
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
        RESTSCREEN(01,00,24,79,telarec)
        IF mimp_tipo = 2
                lertexto('REC_INSER.REL')
        ENDIF
        RETURN NIL
        mvalor := 0
        mnome := SPACE(35)
        LOOP

ENDDO
******************************* F I M ******************************8
* FUNCAO PARA IMPRESSAO DO BOLETO INSERCOM
******************************************
FUNCTION insercom_b(cli_bol,razao_bol,venc_bol,emissao_bol,numdoc_bol,datproc_bol,;
                    valor,obs1,obs2,obs3,obs4,obs5,obs6)

imprt(mtipo_imp,'N')
DEVPOS(PROW()  ,00);DEVOUT(PADC('��������������Ŀ     ��� ��� � ���� ���� ���� ���� ���� ����� ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('� ������������ ��     �  � � � ���� ��   ���� �    �  � � � � ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('�   ��������   ��    ��� � ��� ���� ���� � �� ���� ���� � � � ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('�    ������    ��    ���������������������������������������� ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('�   ��������   ��       Rua Barao da Vitoria, 295 - Sl.1007   ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('� ������������ ��            Sao Jose - Recife - PE           ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('�����������������            TeleFax: (081) 224.3567          ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC(' ����������������           C.G.C.: 02.702.271/0001-14        ',80))
DEVPOS(PROW()+1,00);DEVOUT('�������������������������������������������������������������������������Ŀ')
DEVPOS(PROW()+1,00);DEVOUT('�       R E C I B O   D E   P A G A M E N T O     �Vencimento: '+venc_bol)
DEVPOS(PROW(),74);DEVOUT('�')
DEVPOS(PROW()+1,00);DEVOUT('�������������������������������������������������������������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('�Cliente: '+cli_bol+' - '+ razao_bol)
DEVPOS(PROW(),74);DEVOUT('�')
DEVPOS(PROW()+1,00);DEVOUT('�������������������������������������������������������������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('� Data Emissao � No.Documento �Data Processamento � Valor da Fatura R$    �')
DEVPOS(PROW()+1,00);DEVOUT('�  '+emissao_bol)
DEVPOS(PROW(),15);DEVOUT('� RE'+numdoc_bol)
DEVPOS(PROW(),30);DEVOUT('�  '+emissao_bol)
DEVPOS(PROW(),50);DEVOUT('�  '+TRANSFORM(valor,'9,999,999.99'))
DEVPOS(PROW(),74);DEVOUT('�')
DEVPOS(PROW()+1,00);DEVOUT('�������������������������������������������������������������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('�Instrucoes:(Texto de Responsabilidade do Cedente)�(-) Desconto R$        �')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs1+                                        '�                       �')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs2+                                        '�����������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs3+                                        '�(+) Juros/Multa R$     �')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs4+                                        '�                       �')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs5+                                        '�����������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs6+                                        '�(+) Outros Acrescimo R$�')
DEVPOS(PROW()+1,00);DEVOUT('�                                                 �                       �')
DEVPOS(PROW()+1,00);DEVOUT('�������������������������������������������������������������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('� Recebimento:                                    �(=) Total a Pagar R$   �')
DEVPOS(PROW()+1,00);DEVOUT('� Data:    /    /      Por:                       �                       �')
DEVPOS(PROW()+1,00);DEVOUT('���������������������������������������������������������������������������')

DEVPOS(PROW()+2,00);DEVOUT(' ')

DEVPOS(PROW()+1,00);DEVOUT(PADC('��������������Ŀ     ��� ��� � ���� ���� ���� ���� ���� ����� ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('� ������������ ��     �  � � � ���� ��   ���� �    �  � � � � ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('�   ��������   ��    ��� � ��� ���� ���� � �� ���� ���� � � � ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('�    ������    ��    ���������������������������������������� ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('�   ��������   ��       Rua Barao da Vitoria, 295 - Sl.1007   ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('� ������������ ��            Sao Jose - Recife - PE           ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('�����������������            TeleFax: (081) 224.3567          ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC(' ����������������           C.G.C.: 02.702.271/0001-14        ',80))
DEVPOS(PROW()+1,00);DEVOUT('�������������������������������������������������������������������������Ŀ')
DEVPOS(PROW()+1,00);DEVOUT('�       R E C I B O   D E   P A G A M E N T O     �Vencimento: '+venc_bol)
DEVPOS(PROW(),74);DEVOUT('�')
DEVPOS(PROW()+1,00);DEVOUT('�������������������������������������������������������������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('�Cliente: '+cli_bol+' - '+ razao_bol)
DEVPOS(PROW(),74);DEVOUT('�')
DEVPOS(PROW()+1,00);DEVOUT('�������������������������������������������������������������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('� Data Emissao � No.Documento �Data Processamento � Valor da Fatura R$    �')
DEVPOS(PROW()+1,00);DEVOUT('�  '+emissao_bol)
DEVPOS(PROW(),15);DEVOUT('� RE'+numdoc_bol)
DEVPOS(PROW(),30);DEVOUT('�  '+emissao_bol)
DEVPOS(PROW(),50);DEVOUT('�  '+TRANSFORM(valor,'9,999,999.99'))
DEVPOS(PROW(),74);DEVOUT('�')
DEVPOS(PROW()+1,00);DEVOUT('�������������������������������������������������������������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('�Instrucoes:(Texto de Responsabilidade do Cedente)�(-) Desconto R$        �')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs1+                                        '�                       �')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs2+                                        '�����������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs3+                                        '�(+) Juros/Multa R$     �')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs4+                                        '�                       �')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs5+                                        '�����������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('�'+obs6+                                        '�(+) Outros Acrescimo R$�')
*DEVPOS(PROW()+1,00);DEVOUT('�* Valor referenta a taxa de manutencao do SIAC   �                       �')
*DEVPOS(PROW()+1,00);DEVOUT('�(Sistema Integrado Automacao Comercial). Apos    �����������������������Ĵ')
*DEVPOS(PROW()+1,00);DEVOUT('�vencimento cobrar multa de 2% ao mes mais 0,25%  �(+) Juros/Multa R$     �')
*DEVPOS(PROW()+1,00);DEVOUT('�ao dia.                                          �                       �')
*DEVPOS(PROW()+1,00);DEVOUT('�                                                 �����������������������Ĵ')
*DEVPOS(PROW()+1,00);DEVOUT('�                                                 �(+) Outros Acrescimo R$�')
DEVPOS(PROW()+1,00);DEVOUT('�                                                 �                       �')
DEVPOS(PROW()+1,00);DEVOUT('�������������������������������������������������������������������������Ĵ')
DEVPOS(PROW()+1,00);DEVOUT('� Recebimento:                                    �(=) Total a Pagar R$   �')
DEVPOS(PROW()+1,00);DEVOUT('� Data:    /    /      Por:                       �                       �')
DEVPOS(PROW()+1,00);DEVOUT('���������������������������������������������������������������������������')
EJECT
RETURN .T.
