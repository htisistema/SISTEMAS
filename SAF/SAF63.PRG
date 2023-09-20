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
DEVPOS(PROW()  ,00);DEVOUT(PADC('здддддддддддддд©     эээ эээ э ээээ ээээ ээээ ээээ ээээ эээээ ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('Ё ддээээээээдд Ёш     ш  ш ш ш шэээ шэ   шээш ш    ш  ш ш ш ш ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('Ё   дддээддд   Ёш    эшэ ш шэш эээш шэээ ш шэ шэээ шээш ш ш ш ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('Ё    ддээдд    Ёш    дддддддддддддддддддддддддддддддддддддддд ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('Ё   дддээддд   Ёш       Rua Barao da Vitoria, 295 - Sl.1007   ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('Ё ддээээээээдд Ёш            Sao Jose - Recife - PE           ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('юддддддддддддддыш            TeleFax: (081) 224.3567          ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC(' ъъъъъъъъъъъъъъъъ           C.G.C.: 02.702.271/0001-14        ',80))
DEVPOS(PROW()+1,00);DEVOUT('здддддддддддддддддддддддддддддддддддддддддддддддддбддддддддддддддддддддддд©')
DEVPOS(PROW()+1,00);DEVOUT('Ё       R E C I B O   D E   P A G A M E N T O     ЁVencimento: '+venc_bol)
DEVPOS(PROW(),74);DEVOUT('Ё')
DEVPOS(PROW()+1,00);DEVOUT('цдддддддддддддддддддддддддддддддддддддддддддддддддаддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('ЁCliente: '+cli_bol+' - '+ razao_bol)
DEVPOS(PROW(),74);DEVOUT('Ё')
DEVPOS(PROW()+1,00);DEVOUT('цддддддддддддддбддддддддддддддбдддддддддддддддддддбддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('Ё Data Emissao Ё No.Documento ЁData Processamento Ё Valor da Fatura R$    Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё  '+emissao_bol)
DEVPOS(PROW(),15);DEVOUT('Ё RE'+numdoc_bol)
DEVPOS(PROW(),30);DEVOUT('Ё  '+emissao_bol)
DEVPOS(PROW(),50);DEVOUT('Ё  '+TRANSFORM(valor,'9,999,999.99'))
DEVPOS(PROW(),74);DEVOUT('Ё')
DEVPOS(PROW()+1,00);DEVOUT('цддддддддддддддаддддддддддддддадддддддддддддддддддеддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('ЁInstrucoes:(Texto de Responsabilidade do Cedente)Ё(-) Desconto R$        Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs1+                                        'Ё                       Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs2+                                        'цддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs3+                                        'Ё(+) Juros/Multa R$     Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs4+                                        'Ё                       Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs5+                                        'цддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs6+                                        'Ё(+) Outros Acrescimo R$Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё                                                 Ё                       Ё')
DEVPOS(PROW()+1,00);DEVOUT('цдддддддддддддддддддддддддддддддддддддддддддддддддеддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('Ё Recebimento:                                    Ё(=) Total a Pagar R$   Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё Data:    /    /      Por:                       Ё                       Ё')
DEVPOS(PROW()+1,00);DEVOUT('юдддддддддддддддддддддддддддддддддддддддддддддддддаддддддддддддддддддддддды')

DEVPOS(PROW()+2,00);DEVOUT(' ')

DEVPOS(PROW()+1,00);DEVOUT(PADC('здддддддддддддд©     эээ эээ э ээээ ээээ ээээ ээээ ээээ эээээ ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('Ё ддээээээээдд Ёш     ш  ш ш ш шэээ шэ   шээш ш    ш  ш ш ш ш ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('Ё   дддээддд   Ёш    эшэ ш шэш эээш шэээ ш шэ шэээ шээш ш ш ш ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('Ё    ддээдд    Ёш    дддддддддддддддддддддддддддддддддддддддд ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('Ё   дддээддд   Ёш       Rua Barao da Vitoria, 295 - Sl.1007   ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('Ё ддээээээээдд Ёш            Sao Jose - Recife - PE           ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC('юддддддддддддддыш            TeleFax: (081) 224.3567          ',80))
DEVPOS(PROW()+1,00);DEVOUT(PADC(' ъъъъъъъъъъъъъъъъ           C.G.C.: 02.702.271/0001-14        ',80))
DEVPOS(PROW()+1,00);DEVOUT('здддддддддддддддддддддддддддддддддддддддддддддддддбддддддддддддддддддддддд©')
DEVPOS(PROW()+1,00);DEVOUT('Ё       R E C I B O   D E   P A G A M E N T O     ЁVencimento: '+venc_bol)
DEVPOS(PROW(),74);DEVOUT('Ё')
DEVPOS(PROW()+1,00);DEVOUT('цдддддддддддддддддддддддддддддддддддддддддддддддддаддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('ЁCliente: '+cli_bol+' - '+ razao_bol)
DEVPOS(PROW(),74);DEVOUT('Ё')
DEVPOS(PROW()+1,00);DEVOUT('цддддддддддддддбддддддддддддддбдддддддддддддддддддбддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('Ё Data Emissao Ё No.Documento ЁData Processamento Ё Valor da Fatura R$    Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё  '+emissao_bol)
DEVPOS(PROW(),15);DEVOUT('Ё RE'+numdoc_bol)
DEVPOS(PROW(),30);DEVOUT('Ё  '+emissao_bol)
DEVPOS(PROW(),50);DEVOUT('Ё  '+TRANSFORM(valor,'9,999,999.99'))
DEVPOS(PROW(),74);DEVOUT('Ё')
DEVPOS(PROW()+1,00);DEVOUT('цддддддддддддддаддддддддддддддадддддддддддддддддддеддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('ЁInstrucoes:(Texto de Responsabilidade do Cedente)Ё(-) Desconto R$        Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs1+                                        'Ё                       Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs2+                                        'цддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs3+                                        'Ё(+) Juros/Multa R$     Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs4+                                        'Ё                       Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs5+                                        'цддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('Ё'+obs6+                                        'Ё(+) Outros Acrescimo R$Ё')
*DEVPOS(PROW()+1,00);DEVOUT('Ё* Valor referenta a taxa de manutencao do SIAC   Ё                       Ё')
*DEVPOS(PROW()+1,00);DEVOUT('Ё(Sistema Integrado Automacao Comercial). Apos    цддддддддддддддддддддддд╢')
*DEVPOS(PROW()+1,00);DEVOUT('Ёvencimento cobrar multa de 2% ao mes mais 0,25%  Ё(+) Juros/Multa R$     Ё')
*DEVPOS(PROW()+1,00);DEVOUT('Ёao dia.                                          Ё                       Ё')
*DEVPOS(PROW()+1,00);DEVOUT('Ё                                                 цддддддддддддддддддддддд╢')
*DEVPOS(PROW()+1,00);DEVOUT('Ё                                                 Ё(+) Outros Acrescimo R$Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё                                                 Ё                       Ё')
DEVPOS(PROW()+1,00);DEVOUT('цдддддддддддддддддддддддддддддддддддддддддддддддддеддддддддддддддддддддддд╢')
DEVPOS(PROW()+1,00);DEVOUT('Ё Recebimento:                                    Ё(=) Total a Pagar R$   Ё')
DEVPOS(PROW()+1,00);DEVOUT('Ё Data:    /    /      Por:                       Ё                       Ё')
DEVPOS(PROW()+1,00);DEVOUT('юдддддддддддддддддддддддддддддддддддддддддддддддддаддддддддддддддддддддддды')
EJECT
RETURN .T.
