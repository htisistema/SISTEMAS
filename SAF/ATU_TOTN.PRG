**************************************************
* ATUALIZACAO DO ARQUIVO SACTOTNT   CPF E INSC. CGC
**************************************************

FUNCTION atu_totnt
******************

LOCAL mOPcpf:=' ',mOPinsc:=' ',mOPcgc:=' '

PUBLIC msituacao:='O',mcaminho:=' '

PRIVATE mcpf,mcgc,mcliente

// MODULO PRA ACESSAR ARQUIVOS .CDX
REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
//

IF ! AbriArq('sacforn','forn','E');RETURN NIL;ENDIF
IF ! AbriArq('saccli','cli','E');RETURN NIL;ENDIF
IF ! AbriArq('SACTOTNT','TOTNT','E');RETURN NIL;ENDIF

limpa(00,00,24,79)

DEVPOS(00,00);DEVOUT('Deseja atualizar o CPF :')
DEVPOS(01,00);DEVOUT('Deseja atualizar o CGC :')
DEVPOS(02,00);DEVOUT('Deseja atualizar o INSC:')
@ 00,24 GET mOPcpf PICT '@!' VALID mOPcpf $ 'X, '
@ 01,24 GET mOPcgc PICT '@!' VALID mOPcgc $ 'X, '
@ 02,24 GET mOPinsc PICT '@!' VALID mOPinsc $ 'X, '
READ
IF LASTKEY() = 27
        RETURN NIL
ENDIF
*********************
SELE('totnt');ORDSETFOCUS(1)
GO TOP
*********************

WHILE ! EOF()
        IF totnt->ent_sai = 'S'
                ver_cli(VAL(totnt->cod_cli))
                IF mOPcpf = 'X'
                        totnt->cpf_cli := cli->cpf
                ENDIF
                IF mOPcgc = 'X'
                        totnt->cgc_cli := cli->cgc
                ENDIF
                IF mOPinsc = 'X'
                        totnt->insc_cli := cli->insc
                ENDIF
                totnt->uf_cli := cli->uf
        ELSE
                ver_fab(VAL(totnt->cod_cli))
                IF mOPcpf = 'X'
                        totnt->cpf_cli := forn->cpf
                ENDIF
                IF mOPcgc = 'X'
                        totnt->cgc_cli := forn->cgc
                ENDIF
                IF mOPinsc = 'X'
                        totnt->insc_cli := forn->insc
                ENDIF
                totnt->uf_cli := forn->uf
        ENDIF
        SKIP
ENDDO
RETURN NIL
