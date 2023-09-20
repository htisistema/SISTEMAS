
FUNCTION SIACVISI
#include 'INKEY.CH'

LOCAL mcgccpf:= SPACE(18),minsc := SPACE(18),i:=0
PRIVATE mfabrica:=SPACE(40)

SET DATE BRITI
SET EPOCH TO 1910
// MODULO PRA ACESSAR ARQUIVOS .CDX
REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
//
USE SACFORN ALIAS SIAC EXCLUSIVE NEW
USE clifor ALIAS VISI EXCLUSIVE NEW

WHILE ! EOF()
        limpa(00,00,24,79)
        SELE('siac')
        ADIREG()
        siac->cod_forn  := STRZERO(visi->codigo,4)
        siac->razao     := visi->nome
        siac->tipo      := '1.01'
        siac->data_cad  := DATE()
        siac->endereco  := visi->endereco+'-'+visi->complement
        siac->bairro    := visi->bairro
        siac->cidade    := visi->cidade
        siac->uf        := visi->estado
        siac->cep       := visi->cep
        siac->tel1      := visi->telefone_r
        siac->tel2      := visi->telefone_c
        siac->fax       := visi->fax
        siac->insc      := visi->inscricao_
        siac->cgc       := visi->cgc_ou_cpf
        siac->ct_gerente:= visi->forn_pesso
        siac->obs       := visi->observacoe
        mensagem('Registro: '+siac->cod_forn+' - '+siac->razao)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()
USE SACFORN ALIAS FORN EXCLUSIVE NEW
ORDCREATE('sacforn','codigo','forn->cod_forn')
USE SACMERC ALIAS SIAC EXCLUSIVE NEW
USE PRODUTO ALIAS VISI EXCLUSIVE NEW
i:=1
WHILE ! EOF()
        limpa(00,00,24,79)
        SELE('siac')
        ADIREG()
        siac->cod_barr  := visi->codigo_bar
        siac->gru_sub   := STRZERO(visi->grupo,3)+'01'
*       siac->cod_merc  := STRZERO(VAL(visi->codigo),5)
        siac->cod_merc  := STRZERO(i++,5)
        siac->merc      := visi->descricao_
        siac->data_cad  := DATE()
        siac->unidade   := 'UN'
        siac->disp      := 'S'
        siac->alimento  := 'N'
        siac->isento    := 'N'
        siac->pr_unit   := visi->preco
        siac->pr_venda  := visi->preco
        siac->cod_fab   := STRZERO(visi->fornecedor,4)
        mfabrica := SPACE(40)
        ver_fab(visi->fornecedor,00,01)
        siac->fabrica   := mfabrica
        siac->ind_icms  := '01'
        siac->sit_trib  := 'T'
        mensagem('Registro: '+siac->cod_merc+' - '+siac->merc)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()

USE SACGRUPO ALIAS SIAC EXCLUSIVE NEW
USE GRUPO ALIAS VISI EXCLUSIVE NEW

SELE('visi')
GO TOP
WHILE ! EOF()
        limpa(00,00,24,79)
        SELE('siac')
        ADIREG()
        siac->gru_sub   := STRZERO(visi->idt,3)
        siac->merc      := visi->nome
        siac->data_cad  := DATE()
        ADIREG()
        siac->gru_sub   := STRZERO(visi->idt,3)+'01'
        siac->merc      := visi->nome
        siac->data_cad  := DATE()
        mensagem('Registro: '+siac->gru_sub+' - '+siac->merc)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()
RETURN NIL

