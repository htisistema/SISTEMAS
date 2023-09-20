/*
ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³ Sistema   : AutoCaixa                                                 ³Ş
³ Programa  : BEMAPED                                                   ³Ş
³ Proposito : Funcao de Vendas do AutoCaixa P/PEDIDOS                   ³Ş
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙŞ
  ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
FUNCTION schaped

#include "\clipper5\inkey.ch"

LOCAL tela,mtot_ped:=0,mnum_ped:=0,xlin:=0,mpoint:=0,msub_total,mqtd,ret:=0
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
                        ret := vnd_item(STRZERO(VAL(ped_s->pcod_merc),13),ped_s->pmerc,SUBSTR(STRZERO(ped_s->pquantd,8,3),1,4),SUBSTR(STRZERO(ped_s->pquantd,8,3),6),STRZERO(IF(ped_s->ppre_venda < ped_s->pvlr_fat,ped_s->pvlr_fat,ped_s->ppre_venda)*100,9),mind)
                        IF ret <> 0
                                atencao('Retorno da Impressora:'+ret)
                        ENDIF
                ELSE
                        ret := vnd_item(STRZERO(VAL(ped_s->pcod_merc),13),ped_s->pmerc,SUBSTR(STRZERO(ped_s->pquantd,8,3),1,4),SUBSTR(STRZERO(ped_s->pquantd,8,3),6),STRZERO(IF(ped_s->ppre_venda < ped_s->pvlr_fat,ped_s->pvlr_fat,ped_s->ppre_venda)*100,9),mind)
                        IF ret <> 0
                                atencao('Retorno da Impressora:'+ret)
                        ENDIF
                ENDIF
                SKIP
        ENDDO
        SET DEVICE TO SCREEN
        SAC30(mnum_ped)
        ret := pagcancpag(0,'A VISTA',mtot_ped,0,2)
        IF ret <> 0
                atencao('Retorno da impressora...')
                LOOP
        ENDIF
        ret := subtotal()
        IF ret <> 0
                atencao('Retorno da impressora...')
                LOOP
        ENDIF
        mensagem("Aguarde...Finalizando")
        ret := FimTrans( Descri1 )
        IF  ret <> 0 .AND. ret <> 69
                atencao( "Retorno Impressora", ret )
        ELSE
*               ret := AbreGaveta()
*               IF ( ret != 0 )
*                       atencao( "Retorno Impressora", ret )
*               ENDIF
                ret := LineFeed( 1, 10 )
                IF ret <> 0
                        atencao( "Retorno Impressora", ret )
                ENDIF
        ENDIF
ENDDO
RETURN NIL
