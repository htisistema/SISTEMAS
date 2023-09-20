*******************************
* RECEBIMENTO DIVERSOS
*******************************

FUNCTION saf301
***************
LOCAL MPRG:='SAF301'
LOCAL opcao,tela,lci,cci,lba,cba,i,mdata,mdescri1,mdescri2,mtraco,;
      m_dup:={},m_po:={},mdocumento

PRIVATE mvalor:=0

IF ! ver_nivel(mprg,'RECEBIMENTO DIVERSOS (CAIXA)','135',nivel_acess)
        RETURN NIL
ENDIF
CLEAR GETS

lci = 15
cci = 21
lba = 21
cba = 79
tela = SAVESCREEN(01,00,24,79)
mtraco := REPLI('=',80)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! AbriArq('safcaixa','caix');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
WHILE .T.
        botao(lci,cci,lba,cba,,' Recebimentos Diversos ')
        SET INTEN ON
        mensagem('Preencha os Campos - <ESC> p/Retornar ')
        ASIZE(m_dup,0)
        ASIZE(m_po,0)
        mdata := mdata_sis
        mvalor := 0
        mdocumento := SPACE(10)
        mdescri1 := SPACE(40)
        mdescri2 := SPACE(40)
        opcao = 'S'
        DEVPOS(lci+1,cci+2);DEVOUT('Data.......:')
        DEVPOS(lci+2,cci+2);DEVOUT('Documento..:')
        DEVPOS(lci+3,cci+2);DEVOUT('Valor R$...:')
        DEVPOS(lci+4,cci+2);DEVOUT('Descricao..:')

        @ lci+1,cci+15 GET mdata
        @ lci+2,cci+15 GET mdocumento
        @ lci+3,cci+15 GET mvalor PICT '99,999,999.99' VALID IF(EMPTY(mvalor),.F.,.T.)
        @ lci+4,cci+15 GET mdescri1 PICT '@!'
        @ lci+5,cci+15 GET mdescri2 PICT '@!'
        READ
        IF LASTKEY() = 27
                RESTSCREEN(01,00,24,79,tela)
                CLOSE ALL
                RETURN NIL
        ENDIF
        mensagem('Confirma o Recebimento [S/n]:')
        opcao := 'S'
        @ 23,COL() GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF

        ********************
        SELE('caix');ORDSETFOCUS(1)
        ********************

        IF ADIREG()
                REPLA tipo WITH 'RE'
                REPLA data WITH mdata
                REPLA venci WITH mdata
                REPLA documento WITH mdocumento
                REPLA valor WITH mvalor
                REPLA descri1 WITH mdescri1
                REPLA descri2 WITH mdescri2
                REPLA cod_opera WITH cod_operado
                COMMIT
                UNLOCK
                AADD(m_dup,mdocumento+' '+LEFT(mdescri1,33)+' '+TRANSFORM(mvalor,'9,999.99'))
                AADD(m_po,{RECNO(),'X',mvalor,0,mvalor,'X'})
                mtipo_aut := 0
                mtela_aut := SAVESCREEN(00,00,24,79)
                botao(10,35,12,67,,' Opcao de Autenticacoes ')
                @ 11,36 PROMPT ' Autenticar '
                @ 11,50 PROMPT ' Imprimir Recibo '
                SET INTEN ON
                MENU TO mtipo_aut
                IF  mtipo_aut = 2
                        RESTSCREEN(00,00,24,79,mtela_aut)
                        mtela_aut := SAVESCREEN(00,00,24,79)
                        WHILE LASTKEY() <> 27
                                botao(10,10,12,60,,' IMPRESSAO DE RECIBO ')
                                DEVPOS(11,11);DEVOUT('Tecle <ENTER> p/Imprimir - <ESC> p/abandonar')
                                INKEY(0)
                                IF LASTKEY() = 27;EXIT;ENDIF
                                rec_dup(m_dup,m_po,1,0,mvalor,mdata)
                        ENDDO
                        RESTSCREEN(00,00,24,79,mtela_aut)
                ELSE
                        RESTSCREEN(00,00,24,79,mtela_aut)
                        mtela_aut := SAVESCREEN(00,00,24,79)
                        WHILE LASTKEY() <> 27
                                botao(10,10,13,60,,' AUTENTICACAO DE DOCUMENTO ')
                                DEVPOS(11,11);DEVOUT('Insira o Documento na impressora p/autenticar')
                                DEVPOS(12,11);DEVOUT('  <ENTER> p/Autenticar - <ESC> p/abandonar')
                                INKEY(0)
                                IF LASTKEY() = 27;EXIT;ENDIF
                                SET DEVI TO PRINT
                                SET PRINT TO LPT1
                                imprt('M','C')
                                DEVPOS(00,00);DEVOUT('RECDIV'+ALLTRIM(mdocumento)+LEFT(mdescri1,10)+cod_operado+DTOC(mdata)+TRANSFORM(mvalor,'999999.99'))
                                imprt('M','N',1)
                                SETPRC(00,00)
                                SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
                        ENDDO
                        RESTSCREEN(00,00,24,79,mtela_aut)
                ENDIF
        ELSE
                atencao('Nao foi possivel acessar o Arquivo !!!')
                LOOP
        ENDIF
ENDDO
RETURN NIL
********************************** f i m **********************************
************************************************
*FUNCAO P/IMPRIMIR RECIBO BAIXA DE DOC.RECEBER
************************************************
FUNCTION rec_dup(m_d,m_p,mqtd,mtot_m,mtot,mdat,mtipo_rel)
********************

LOCAL mtraco := REPLI('=',54),i:=0,mtipo_imp:='M',mextenso,mlinha,mlin,lin
GO m_p[1,1]
SET DEVICE TO PRINT
SET PRINT TO LPT1
*SET PRINT TO RECDUPR.REL
*IF cfg->tipfrmped = 'B'
        imprt(mtipo_imp,'C')
        DEVPOS(PROW()+1,00);DEVOUT(PADC(ALLTRIM(memp+resa),54))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM(mend_firm),54))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM(mcid_firm),54))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(RTRIM('Fone: '+mfone_firm),54))
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        IF mtipo_rel = NIL
                DEVPOS(PROW()+1,00);DEVOUT(PADC('RECIBO PAGAMENTO',54))
                DEVPOS(PROW()+1,00);DEVOUT(mtraco)
                DEVPOS(PROW()+1,00);DEVOUT(PADC('*** SEM VALOR FISCAL ***',54))
        ELSE
                DEVPOS(PROW()+1,00);DEVOUT(PADC('DEMONSTRATIVO DE DEBITO',54))
        ENDIF
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Data Emissao: '+DTOC(mdata_sis)+'     Data do Pagamento: '+DTOC(mdat))
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Documento')
        DEVPOS(PROW(),14);DEVOUT('Descricao')
        DEVPOS(PROW(),45);DEVOUT('Vlr.Total')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        FOR i = 1 TO LEN(m_d)
                IF EMPTY(m_d[i])
                        LOOP
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(m_d[i])
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',54))
        DEVPOS(PROW()+1,24);DEVOUT('Valor Total R$: '+TRANSFORM(mvalor,'999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        mextenso:= '('+RTRIM(EXT(ROUND(mvalor,2),10))+')'
        mlinha := MLCOUNT(mextenso,54)
        lin := 0
        FOR lin = 1 TO mlinha
                mlin := MEMOLINE(mextenso,54,lin,,10)
                DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(mlin),54))
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+3,00);DEVOUT(PADC('----------------------------------------',54))
        DEVPOS(PROW()+1,00);DEVOUT(PADC('       Local e data do recebimento      ',54))
        DEVPOS(PROW()+3,00);DEVOUT(PADC(REPLI('-',LEN(ALLTRIM(memp+resa))+4),54))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(ALLTRIM(memp+resa),54))
        imprt(mtipo_imp,'N',11)
        SETPRC(00,00)
        SET DEVICE TO SCREEN;SET PRINT TO;SET PRINT OFF
/*
ELSEIF cfg->tipfrmped = 'F' .OR. cfg->tipfrmped = 'G'
        mtraco := REPLI('=',80)
        imprt(mtipo_imp,'E')
        DEVPOS(PROW()+1,00);DEVOUT(memp+resa)
        imprt(mtipo_imp,'C')
        DEVPOS(PROW()+1,00);DEVOUT(RTRIM(set->end)+'-'+RTRIM(set->bairro)+'-'+RTRIM(set->cidade)+'-'+RTRIM(set->uf)+' - Fone: '+set->fone)
        imprt(mtipo_imp,'N')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        IF mtipo_rel = NIL
                DEVPOS(PROW()+1,00);DEVOUT(PADC('RECIBO PAGAMENTO',80))
        ELSE
                DEVPOS(PROW()+1,00);DEVOUT(PADC('DEMONSTRATIVO DE DEBITO',80))
        ENDIF
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Data: '+DTOC(mdata_sis)+'        Data do Pagamento: '+DTOC(mdat))
        DEVPOS(PROW()+1,00);DEVOUT('Cliente: '+dupr->fornec+'-'+dupr->cliente)
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Documento')
        DEVPOS(PROW(),14);DEVOUT('Vencimen')
        DEVPOS(PROW(),28);DEVOUT('Valor')
        DEVPOS(PROW(),34);DEVOUT('Juros+Mult')
        DEVPOS(PROW(),46);DEVOUT('Vlr.Total')
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        FOR i = 1 TO LEN(m_d)
*               IF m_p[i,2] <> 'X'
*                       LOOP
*               ENDIF
                IF EMPTY(m_d[i])
                        LOOP
                ENDIF
                DEVPOS(PROW()+1,00);DEVOUT(SUBSTR(m_d[i],7,55))
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT(REPLI('-',80))
        DEVPOS(PROW()+1,00);DEVOUT('Qtd.Doc.: '+TRANSFORM(mqtd,'99999'))
        DEVPOS(PROW(),45);DEVOUT(TRANSFORM(mtot,'999,999.99'))
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        mextenso:= '('+RTRIM(EXT(ROUND(mtot,2),10))+')'
        mlinha := MLCOUNT(mextenso,60)
        lin := 0
        FOR lin = 1 TO mlinha
                mlin := MEMOLINE(mextenso,60,lin,,10)
                DEVPOS(PROW()+1,00);DEVOUT(PADR(RTRIM(mlin),60))
        NEXT
        DEVPOS(PROW()+1,00);DEVOUT(mtraco)
        DEVPOS(PROW()+1,00);DEVOUT('Recebemos o valor acima descrito, pelo qual damos quitacao.')
        DEVPOS(PROW()+3,00);DEVOUT(PADC('----------------------------------------',80))
        DEVPOS(PROW()+1,00);DEVOUT(PADC('       Local e data do recebimento      ',80))
        DEVPOS(PROW()+3,00);DEVOUT(PADC(REPLI('-',LEN(ALLTRIM(memp+resa))+4),80))
        DEVPOS(PROW()+1,00);DEVOUT(PADC(ALLTRIM(memp+resa),80))
        EJECT
        SET DEVICE TO SCREEN;SET PRINT TO;SET PRINT OFF
ENDIF
*/
*lertexto('RECDUPR.REL')
RETURN NIL


