
FUNCTION SIACVISI
#include 'INKEY.CH'

LOCAL mcgccpf:= SPACE(18),minsc := SPACE(18),i:=0
SET DATE BRITI
SET EPOCH TO 1910
// MODULO PRA ACESSAR ARQUIVOS .CDX
REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
//
USE SACFORN ALIAS SIAC EXCLUSIVE NEW
USE MSAC_FOR ALIAS VISI EXCLUSIVE NEW

WHILE ! EOF()
        limpa(00,00,24,79)
        SELE('siac')
        ADIREG()
        siac->cod_forn  := SUBSTR(visi->codigo,2)
        siac->razao     := visi->nome
        siac->tipo      := '1.01'
        siac->data_cad  := DATE()
        siac->endereco  := visi->endereco
        siac->bairro    := visi->bairro
        siac->cidade    := visi->cidade
        siac->uf        := visi->estado
        siac->cep       := visi->cep
        siac->tel1      := visi->fone
        siac->insc      := visi->inscr
        siac->cgc       := visi->cgc
        siac->ct_gerente:= visi->contato
        mensagem('Registro: '+siac->cod_forn+' - '+siac->razao)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()
USE SACMERC ALIAS SIAC EXCLUSIVE NEW
USE PRODUTO ALIAS VISI EXCLUSIVE NEW

WHILE ! EOF()
        limpa(00,00,24,79)
        SELE('siac')
        ADIREG()
        siac->gru_sub   := '00101'
        siac->cod_merc  := STRZERO(visi->codigo_t,5)
        siac->merc      := visi->descricao
        siac->data_cad  := DATE()
        siac->unidade   := visi->unidade
        siac->disp      := 'S'
        siac->alimento  := 'N'
        siac->isento    := 'N'
        siac->est_min   := visi->estmin
        siac->est_max   := visi->estmax
        siac->dat_ult_e := visi->dtcomp
        siac->pr_unit   := visi->vrcompr
        siac->cust_real := visi->vrcompr
        siac->pr_venda  := visi->vrvenda
        siac->saldo_mer := visi->atual
        siac->ind_icms  := '01'
        siac->sit_trib  := 'T'
        mensagem('Registro: '+siac->cod_merc+' - '+siac->merc)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()

USE SACCLI ALIAS SIAC EXCLUSIVE NEW
USE MSAC_CLI ALIAS VISI EXCLUSIVE NEW

SELE('visi')
GO TOP
WHILE ! EOF()
        limpa(00,00,24,79)
        SELE('siac')
        ADIREG()
        siac->cod_cli   := visi->codigo
        siac->razao     := visi->nome
        siac->nome      := visi->fantasia
        siac->data_cad  := visi->cli_desde
        siac->tipo      := 'C'
        siac->nascimento:= visi->dt_nasc
        siac->ult_comp  := visi->ult_compra
        siac->vlr_comp  := visi->vlr_ultcom
        siac->tot_comp  := visi->compra_acu
        siac->bairro    := visi->bairro
        siac->endereco  := visi->endereco
        siac->cidade    := visi->cidade
        siac->cep       := visi->cep
        siac->uf        := visi->estado
        siac->tel1      := visi->fone_res
        siac->fax       := visi->fax
        siac->insc      := visi->inscr_ci
        siac->contato   := visi->contato
        siac->limite    := visi->lim_credit
        siac->codvend   := STRZERO(VAL(visi->vendedor),3)
        siac->num_ped   := visi->ult_pedido
        siac->obs       := visi->obs1
        IF LEN(ALLTRIM(visi->cgc_cpf)) = 11
                siac->cpf := visi->cgc_cpf
        ELSE
                siac->cgc := visi->cgc_cpf
        ENDIF
        mensagem('Registro: '+siac->cod_cli+' - '+siac->razao)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()
/*

USE INSOPERA ALIAS SIAC EXCLUSIVE NEW
USE MSAC_VEN ALIAS VISI EXCLUSIVE NEW

WHILE ! EOF()
        limpa(00,00,24,79)
        SELE('siac')
        ADIREG()
        siac->scod_op   := STRZERO(VAL(visi->codigo),3)
        siac->snome     := visi->nome
        siac->sdata_cad := DATE()
        siac->scomissao := visi->comissao
        siac->scom_praz := visi->comissao1
        mensagem('Registro: '+siac->scod_op+' - '+siac->snome)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()

USE SACDUPR ALIAS SIAC EXCLUSIVE NEW
USE SIC_CHE ALIAS VISI EXCLUSIVE NEW

WHILE ! EOF()
        limpa(00,00,24,79)
        SELE('siac')
        ADIREG()
        siac->tipo      := 'CH'
        siac->tip_cli   := 'C'
        siac->fornec    := visi->codigo
        siac->cliente   := visi->nome
        siac->emissao   := visi->datalanc
        siac->num_ped   := visi->titulo
        siac->duplicata := visi->cheque
        siac->valor_dup := visi->valor
        siac->venc      := visi->data
        siac->valor     := visi->valor
        siac->banco     := visi->banco
        siac->agencia   := visi->agencia
        siac->c_c       := visi->conta
        IF visi->compensado = 'S'
                siac->datpag := visi->data
                siac->vlpago := visi->valor
                siac->pago   := 'B'
        ENDIF
        mensagem('Registro: '+siac->tipo+' - '+siac->duplicata)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()
*/
RETURN NIL

