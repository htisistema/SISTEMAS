***********************************************
* FUNCAO PARA EMISSAO DE ETIQUETAS NORMAL PERSONALIZADA
***********************************************

FUNCTION sac5et41(tipo)
***************************
LOCAL MPRG:='SAC5ET41'

LOCAL lci,cci,lba,cba,opcao,mempresa,mdescr1,mdescr2,mdescr3,mdescr4,mdescr5,;
      mquantd,m_produto:={},mespaco,mcomprimir,mimp,mfila

lci := 09
cci := 05
lba := 18
cba := 79

tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        mdescr1 := SPACE(40)
        mdescr2 := SPACE(40)
        mdescr3 := SPACE(40)
        mdescr4 := SPACE(40)
        mdescr5 := SPACE(40)
        opcao := SPACE(1)
        mquantd := 1
        mespaco := 70
        mcomprimir := '17'
        mfila := 2
        ASIZE(m_produto,0)
        mensagem('Escolha as opcoes que deseja imprimir')
        botao(lci,cci,lba,cba)
        janela(lci,cba,' Opcoes para Etiquetas PADRAO PERSONALIZADAS ')
        DEVPOS(lci+1,cci+1);DEVOUT('Descricao.....:')
        DEVPOS(lci+2,cci+1);DEVOUT('Descricao.....:')
        DEVPOS(lci+3,cci+1);DEVOUT('Descricao.....:')
        DEVPOS(lci+4,cci+1);DEVOUT('Descricao.....:')
        DEVPOS(lci+5,cci+1);DEVOUT('Descricao.....:')
        DEVPOS(lci+6,cci+1);DEVOUT('Espaco entre ETIQUETAS....:')
        DEVPOS(lci+7,cci+1);DEVOUT('Quantidade de Etiquetas por PRODUTO:')
        DEVPOS(lci+8,cci+1);DEVOUT('Quantidade Fileira ETIQUETAS [1,2].:')
        @ lci+1,cci+17 GET mdescr1
        @ lci+2,cci+17 GET mdescr2
        @ lci+3,cci+17 GET mdescr3
        @ lci+4,cci+17 GET mdescr4
        @ lci+5,cci+17 GET mdescr5
        @ lci+6,cci+29 GET mespaco PICT '999' VALID IF(mespaco<46,.F.,.T.)
        @ lci+7,cci+38 GET mquantd PICT '999999'
        @ lci+8,cci+38 GET mfila PICT '99' VALID IF(mfila<1 .AND. mfila > 2,.F.,.T.)
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
*       impressora()
        mensagem('Confirma os dados para impressao das etiquetas [S/N]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        FOR i = 1 TO mquantd
                AADD(m_produto,{mdescr1,mdescr2,mdescr3,mdescr4,mdescr5})
        NEXT
        SET DEVI TO PRINT
        SET PRINT TO LPT1
        i := 0
        FOR i = 1 TO LEN(m_produto)
                imprt('M','C')
                DEVPOS(PROW(),00);DEVOUT(m_produto[i,1])
                IF i < LEN(m_produto) .AND. mfila == 2
                        DEVPOS(PROW(),mespaco);DEVOUT(LEFT(m_produto[i+1,1],46))
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_produto[i,2])
                IF i < LEN(m_produto) .AND. mfila == 2
                        DEVPOS(PROW(),mespaco);DEVOUT(m_produto[i+1,2])
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_produto[i,3])
                IF i < LEN(m_produto) .AND. mfila == 2
                        DEVPOS(PROW(),mespaco);DEVOUT(m_produto[i+1,3])
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_produto[i,4])
                IF i < LEN(m_produto) .AND. mfila == 2
                        DEVPOS(PROW(),mespaco);DEVOUT(m_produto[i+1,4])
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_produto[i,5])
                IF i < LEN(m_produto) .AND. mfila == 2
                        DEVPOS(PROW(),mespaco);DEVOUT(m_produto[i+1,5])
                ENDIF
                IF mfila == 2
                        i ++
                ENDIF
                DEVPOS(PROW()+2,00);DEVOUT(' ')
        NEXT
        imprt('M','N')
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDDO







