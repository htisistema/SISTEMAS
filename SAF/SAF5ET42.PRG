***********************************************
* FUNCAO PARA EMISSAO DE ETIQUETAS PEQUENAS PERSONALIZADA
***********************************************

FUNCTION sac5et42(tipo)
***************************
LOCAL MPRG:='SAC5ET42'

LOCAL lci,cci,lba,cba,opcao,mdescr1,mdescr2,mdescr3,mlinhas,;
      mquantd,m_produto:={}

lci := 09
cci := 05
lba := 15
cba := 79

tela := SAVESCREEN(01,00,24,79)
WHILE .T.
        mdescr1 := SPACE(17)
        mdescr2 := SPACE(17)
        mdescr3 := SPACE(17)
        mlinhas := 1
        opcao := SPACE(1)
        mquantd := 1
        ASIZE(m_produto,0)
        mensagem('Escolha as opcoes que deseja imprimir')
        botao(lci,cci,lba,cba)
        janela(lci,cba,' Opcoes para Etiquetas PEQUENAS PERSONALIZADA')
        DEVPOS(lci+1,cci+1);DEVOUT('Descricao.....:')
        DEVPOS(lci+2,cci+1);DEVOUT('Descricao.....:')
        DEVPOS(lci+3,cci+1);DEVOUT('Descricao.....:')
        DEVPOS(lci+4,cci+1);DEVOUT('Quantidade de Etiquetas por PRODUTO:')
        DEVPOS(lci+5,cci+1);DEVOUT('Quantidade linhas entre Etiquetas  :')
        @ lci+1,cci+17 GET mdescr1
        @ lci+2,cci+17 GET mdescr2
        @ lci+3,cci+17 GET mdescr3
        @ lci+4,cci+38 GET mquantd PICT '999999'
        @ lci+5,cci+38 GET mlinhas PICT '9'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                RETURN NIL
        ENDIF
        impressora()
        mensagem('Confirma os dados para impressao das etiquetas [S/N]:')
        opcao := 'S'
        @ 23,COL()+1 GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        FOR i = 1 TO mquantd
                AADD(m_produto,{mdescr1,mdescr2,mdescr3,mdescr4})
        NEXT
        SET DEVI TO PRINT
        SET PRINT TO LPT1
        i := 0
        imprt('M','C')
        FOR i = 1 TO LEN(m_produto)
                DEVPOS(PROW()+1,00);DEVOUT(LEFT(m_produto[i,2]+'-'+m_produto[i,3],17))
                IF i < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+1,2])
                ENDIF
                IF i+1 < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+2,2])
                ENDIF
                IF i+2 < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+3,2])
                ENDIF
                IF i+3 < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+4,2])
                ENDIF

                DEVPOS(PROW()+1,00);DEVOUT(m_produto[i,2])
                IF i < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+1,2])
                ENDIF
                IF i+1 < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+2,2])
                ENDIF
                IF i+2 < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+3,2])
                ENDIF
                IF i+3 < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+4,2])
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_produto[i,4])
                IF i < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+1,4])
                ENDIF
                IF i+1 < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+2,4])
                ENDIF
                IF i+2 < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+3,4])
                ENDIF
                IF i+3 < LEN(m_produto)
                        DEVPOS(PROW(),PCOL()+7);DEVOUT(m_produto[i+4,4])
                ENDIF
                i=i+4
                DEVPOS(PROW()+mlinhas,00);DEVOUT(' ')
        NEXT
        imprt('M','N')
        SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDDO






