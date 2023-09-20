***************************************
* DESPROTEGER E VERIFICA O COMPUTADOR
***************************************
FUNCTION admdbf(mdir,mreindex)
************

LOCAL cor,f

LOCAL matriz :={}

PRIVATE  mcaminho,msituacao,mtip_term,nmaxpsw:=8
// MODULO PRA ACESSAR ARQUIVOS .CDX
REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
//
// Abertura do arquivo de CONFIGURACAO
mcaminho := SPACE(30)
arqcfg()
IF ! AbriArq('saccfg','cfg');RETURN NIL;ENDIF
IF EMPTY(mcaminho)
        mcaminho := ALLTRIM(cfg->caminho)
ENDIF
mtip_term := 'S'
msituacao := 'M'
f := 0
DEVPOS(23,00);DEVOUT(STRZERO(f++,3)+'> ATIV    ')
IF ! FILE(mcaminho+'ativ.dbf')
        matriz:={ }
        AADD(matriz,{'codigo','C',2,0})
        AADD(matriz,{'atividade','C',40,0})
        DBCREATE(mcaminho+'ativ.dbf',matriz)
ENDIF
DEVPOS(23,00);DEVOUT(STRZERO(f++,3)+'> PROPOSTA')
IF ! FILE(mcaminho+'proposta.dbf')
        matriz:={ }
        AADD(matriz,{'codigo','C',5,0})
        AADD(matriz,{'data','D',8,0})
        AADD(matriz,{'numprop','C',10,0})
        AADD(matriz,{'valor','N',16,2})
        AADD(matriz,{'validade','D',8,0})
        AADD(matriz,{'fecha','C',1,0})
        DBCREATE(mcaminho+'proposta.dbf',matriz)
ENDIF
DEVPOS(23,00);DEVOUT(STRZERO(f++,3)+'> REGIAO   ')
IF ! FILE(mcaminho+'regiao.dbf')
        matriz:={ }
        AADD(matriz,{'codigo','C',2,0})
        AADD(matriz,{'regiao','C',30,0})
        AADD(matriz,{'codvend','C',3,0})
        DBCREATE(mcaminho+'regiao.dbf',matriz)
ENDIF
**********************************************************************************************
DEVPOS(23,00);DEVOUT(STRZERO(f++,3)+'> VENDMES')
IF ! FILE(mcaminho+'vendmes.dbf')
        matriz:={ }
        AADD(matriz,{'codvend','C',3,0})
        AADD(matriz,{'mes','C',5,0})
        AADD(matriz,{'visitas','N',5,0})
        AADD(matriz,{'vendas','N',5,0})
        AADD(matriz,{'propostas','N',5,0})
        AADD(matriz,{'faltas','N',5,0})
        DBCREATE(mcaminho+'vendmes.dbf',matriz)
ENDIF
**********************************************************************************************
DEVPOS(23,00);DEVOUT(STRZERO(f++,3)+'>VISITA   ')
IF ! FILE(mcaminho+'visita.dbf')
        matriz:={ }
        AADD(matriz,{'codigo','C',5,0})
        AADD(matriz,{'data','D',8,0})
        AADD(matriz,{'l1','C',60,0})
        AADD(matriz,{'l2','C',60,0})
        AADD(matriz,{'l3','C',60,0})
        AADD(matriz,{'l4','C',60,0})
        AADD(matriz,{'l5','C',60,0})
        AADD(matriz,{'obs','C',60,0})
        AADD(matriz,{'ok','L',8,0})
        AADD(matriz,{'flag','C',8,0})
        AADD(matriz,{'uvend','C',3,0})
        DBCREATE(mcaminho+'visita.dbf',matriz)
ENDIF
**********************************************************************************************
RETURN NIL

