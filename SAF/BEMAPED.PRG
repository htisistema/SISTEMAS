/*
ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³ Sistema   : AutoCaixa                                                 ³Ş
³ Programa  : BEMAPED                                                   ³Ş
³ Proposito : Funcao de Vendas do AutoCaixa P/PEDIDOS                   ³Ş
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙŞ
  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
FUNCTION bemaped

#include "\clipper5\inkey.ch"

LOCAL tela,mtot_ped:=0,mnum_ped:=0,xlin:=0,mpoint:=0,msub_total,mqtd
PRIVATE mcliente,mcpf,mcgc,mnum_ecf

IF ! abriarq('sacnoped','noped');RETURN NIL;ENDIF
IF ! abriarq('sacped_s','ped_s');RETURN NIL;ENDIF
IF ! abriarq('saccli','cli');RETURN NIL;ENDIF
IF ! abriarq('sacmerc','merc');RETURN NIL;ENDIF

tela := SAVESCREEN(00,00,24,79)
WHILE .T.
        mtot_ped := 0
        RESTSCREEN(00,00,24,79,tela)

        botao(10,20,17,77)
        DEVPOS(11,21);DEVOUT('Sequencia:')
        setcor(3)
        DEVPOS(12,21);DEVOUT('Pressione <ESC>p/continuar')
        setcor(1)
        @ 13,22 TO 13,76
        DEVPOS(14,21);DEVOUT('No.Pedido:')
        DEVPOS(15,21);DEVOUT('Cliente..:')
        DEVPOS(16,21);DEVOUT('Valor R$.:')
        WHILE LASTKEY() <> 27
                ****************
                SELE('noped')
                GO TOP
                ****************
                setcor(3)
                DEVPOS(11,32);DEVOUT(noped->numero)
                INKEY(1)
        ENDDO
        setcor(1)
        mtot_ped := 0
        mnum_ped := 0
        @ 14,32 GET mnum_ped PICT '999999' VALID IF(EMPTY(mnum_ped),.F.,.T.)
        READ
        IF LASTKEY() = 27
                mnum_ped := 0
                RESTSCREEN(00,00,24,79,tela)
                RETURN NIL
        ENDIF
        ****************
        SELE('ped_s');ORDSETFOCUS(1)
        GO TOP
        ****************
        IF ! ped_s->(DBSEEK(STRZERO(mnum_ped,6)))
                atencao('Este PEDIDO nao estar disponivel')
                mnum_ped := 0
                LOOP
        ENDIF
        setcor(3)
        DEVPOS(15,32);DEVOUT(ped_s->pcod_cli)
        ver_cli(VAL(ped_s->pcod_cli),15,39)
        mpoint := RECNO()
        WHILE ! EOF() .AND. STRZERO(mnum_ped,6) = ped_s->pnum_ped
                mtot_ped := mtot_ped + (ped_s->pquantd * ped_s->pvlr_fat)
                DEVPOS(16,32);DEVOUT(TRANSFORM(mtot_ped,'999,999.99'))
                SKIP
        ENDDO
        GO mpoint
        IF ped_s->ppag = '*'
                atencao('Este pedido ja foi PAGO')
*               LOOP
        ENDIF
        IF ped_s->ppag = 'C'
                atencao('Este pedido ja foi CANCELADO')
                LOOP
        ENDIF
        mensagem('Confirma o pedido [S/n]:')
        opcao:='S'
        @ 23,COL() GET opcao PICT '@!' VALID opcao $ 'S,N'
        READ
        IF LASTKEY() = 27 .OR. opcao = 'N'
                LOOP
        ENDIF
        //    ABRE O CUPOM FISCAL
        prepara_string := inicio_protocolo + "00" + fim_protocolo_driver
        com_c_imp(prepara_string,tamanho_de_retorno)
        tamanho_de_retorno := 14
        prepara_string := inicio_protocolo + "35|14" + fim_protocolo_driver
        cnumpdv := com_c_imp(prepara_string,tamanho_de_retorno)
        mnum_ecf:= com_c_imp(prepara_string,tamanho_de_retorno)
        tamanho_de_retorno := 6
        prepara_string := inicio_protocolo + "30" + fim_protocolo_driver
        cnumcupom:=com_c_imp(prepara_string,tamanho_de_retorno)
        tamanho_de_retorno := 0
        ****************
        SELE('ped_s');ORDSETFOCUS(1)
        GO mpoint
        ****************
        botao(09,00,17,79)
        xlin := 9
        WHILE STRZERO(mnum_ped,6) = ped_s->pnum_ped .AND. ! EOF()
                IF xlin>=17
                        scroll(09,00,17,79,1)
                else
                        xlin++
                ENDIF
                @ xlin,06 say ped_s->pcod_merc pict [99999]
                @ xlin,15 say ped_s->pmerc pict [@!]
                @ xlin,57 say ped_s->pquantd pict [@e 99999.999]
                @ xlin,67 say ped_s->pvlr_fat*ped_s->pquantd pict [@e 9,999,999.99]
                IF EMPTY(ped_s->pind_icms) .AND. EMPTY(ped_s->psit_trib)
                        mind := '01'
                ELSE
                        mind := ped_s->pind_icms
                ENDIF
                IF EMPTY(ped_s->psit_trib) .AND. EMPTY(ped_s->pind_icms)
                        msit := 'T'
                ELSE
                        msit := ped_s->psit_trib
                ENDIF
                mqtd :=SUBSTR(STRZERO(ped_s->pquantd,8,3),1,4)+SUBSTR(STRZERO(ped_s->pquantd,8,3),6)
                IF ped_s->psit_trib = 'T'
                        prepara_string := inicio_protocolo + "09|"+STRZERO(VAL(ped_s->pcod_merc),13)+"|"+;
                        LEFT(ped_s->pmerc,29)+"|"+mind+"|"+mqtd+"|"+;
                        STRZERO(IF(ped_s->ppre_venda < ped_s->pvlr_fat,ped_s->pvlr_fat,ped_s->ppre_venda)*100,8)+"|"+;
                        STRZERO(IF(ped_s->ppre_venda < ped_s->pvlr_fat,0,ped_s->pquantd*ped_s->ppre_venda - ped_s->pquantd*ped_s->pvlr_fat)*100,8) + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                ELSE
                        prepara_string := inicio_protocolo + "09|"+STRZERO(VAL(ped_s->pcod_merc),13)+"|"+;
                        LEFT(ped_s->pmerc,29)+"|"+ped_s->psit_trib+ped_s->psit_trib+"|"+mqtd+"|"+;
                        STRZERO(IF(ped_s->ppre_venda < ped_s->pvlr_fat,ped_s->pvlr_fat,ped_s->ppre_venda)*100,8)+"|"+;
                        STRZERO(IF(ped_s->ppre_venda < ped_s->pvlr_fat,0,ped_s->pquantd*ped_s->ppre_venda - ped_s->pquantd*ped_s->pvlr_fat)*100,8) + fim_protocolo_driver
                        com_c_imp(prepara_string,tamanho_de_retorno)
                ENDIF
                SKIP
        ENDDO
        SET DEVICE TO SCREEN
        SAC30(mnum_ped)
        tamanho_de_retorno := 14
        prepara_string := inicio_protocolo + "29" + fim_protocolo_driver
        msub_total := com_c_imp(prepara_string,tamanho_de_retorno)
        tamanho_de_retorno := 0
        prepara_string := inicio_protocolo + "10|0000|"+msub_total+"|A|**** Muito Obrigado, Volte sempre ****";
                          + chr(10) +  fim_protocolo_driver
        com_c_imp(prepara_string,tamanho_de_retorno)
ENDDO
RETURN NIL
