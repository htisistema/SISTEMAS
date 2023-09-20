
FUNCTION SIACVISI
#include 'INKEY.CH'

LOCAL mcgccpf:= SPACE(18),minsc := SPACE(18),i:=0
SET DATE BRITI
SET EPOCH TO 1910
// MODULO PRA ACESSAR ARQUIVOS .CDX
REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
//
/*
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
*/
matriz:={ }
AADD(matriz,{'cod_cli','C',5,0})
AADD(matriz,{'cgccpf','C',14,0})
AADD(matriz,{'razao','C',40,0})
AADD(matriz,{'nome','C',40,0})
AADD(matriz,{'endereco','C',35,0})
AADD(matriz,{'bairro','C',20,0})
AADD(matriz,{'cidade','C',20,0})
AADD(matriz,{'uf','C',2,0})
AADD(matriz,{'cep','C',9,0})
AADD(matriz,{'cAIXA','C',9,0})
AADD(matriz,{'tel1','C',14,0})
AADD(matriz,{'fax','C',14,0})
AADD(matriz,{'insc','C',19,0})
AADD(matriz,{'end_cob','C',35,0})
AADD(matriz,{'bairro_cob','C',20,0})
AADD(matriz,{'cidade_cob','C',20,0})
AADD(matriz,{'uf_cob','C',2,0})
AADD(matriz,{'cep_cob','C',9,0})
AADD(matriz,{'cAIXACOB','C',9,0})
AADD(matriz,{'data_cad','C',8,0})
AADD(matriz,{'ult_comp','C',8,0})
AADD(matriz,{'contato','C',15,0})

DBCREATE('clifor.dbf',matriz)
USE CLIFOR ALIAS VISI EXCLUSIVE NEW
APPEND FROM mcclien.txt DELIMITED
USE SACCLI ALIAS SIAC EXCLUSIVE NEW

SELE('visi')
GO TOP
WHILE ! EOF()
        limpa(00,00,24,79)
        SELE('siac')
        ADIREG()
        siac->cod_cli   := STRZERO(VAL(visi->cod_cli),5)
        siac->razao     := visi->razao
        siac->nome      := visi->nome
        siac->data_cad  := CTOD(visi->data_cad)
        siac->tipo      := 'C'
        siac->endereco  := visi->endereco
        siac->bairro    := visi->bairro
        siac->cidade    := visi->cidade
        siac->cep       := visi->cep
        siac->uf        := visi->uf
        siac->tel1      := visi->tel1
        siac->fax       := visi->fax
        siac->insc      := visi->insc
        siac->contato   := visi->contato
        siac->end_cob   := visi->end_cob
        siac->bairro_cob:= visi->bairro_cob
        siac->cidade_cob:= visi->cidade_cob
        siac->uf_cob    := visi->uf_cob
        siac->cep_cob   := visi->cep_cob
        siac->contato   := visi->contato
        siac->ult_comp  := CTOD(visi->ult_comp)
        IF LEN(ALLTRIM(visi->cgccpf)) = 14
                siac->cgc := visi->cgccpf
        ELSEIF LEN(ALLTRIM(visi->cgccpf)) < 14
                siac->cpf := visi->cgccpf
        ENDIF
        mensagem('Registro: '+siac->cod_cli+' - '+siac->razao)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()
/*
USE SACFORN ALIAS SIAC EXCLUSIVE NEW
USE CLIFOR   ALIAS VISI EXCLUSIVE NEW

WHILE ! EOF()
        limpa(00,00,24,79)
        IF visi->tipo_c_f = 'F'
                SKIP
                LOOP
        ENDIF
        SELE('siac')
        ADIREG()
        siac->cod_forn  := STRZERO(visi->codigo_c_f,4)
        siac->razao     := visi->nome_c_f
        siac->tipo      := 'FO'
        siac->data_cad  := DATE()
        siac->endereco  := visi->endereco
        siac->bairro    := visi->bairro
        siac->cidade    := visi->cidade
        siac->uf        := visi->estado
        siac->cep       := visi->cep
        siac->tel1      := visi->fone1
        siac->tel2      := visi->fone2
        siac->fax       := visi->fax
        siac->insc      := visi->inscest
        IF LEN(visi->cgc_cpf) <= 14
                siac->cpf    := SUBSTR(visi->cgc_cpf,1,3)+SUBSTR(visi->cgc_cpf,5,3)+SUBSTR(visi->cgc_cpf,9,3)+SUBSTR(visi->cgc_cpf,13,3)
        ELSE
                siac->cgc    := SUBSTR(visi->cgc_cpf,1,2)+SUBSTR(visi->cgc_cpf,4,3)+SUBSTR(visi->cgc_cpf,8,3)+SUBSTR(visi->cgc_cpf,12,4)+SUBSTR(visi->cgc_cpf,17,2)
                siac->insc   := minsc
        ENDIF
        siac->ct_gerente:= visi->contato
        mensagem('Registro: '+siac->cod_forn+' - '+siac->razao)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()

USE SACDUPR ALIAS SIAC EXCLUSIVE NEW
USE RECEBER ALIAS VISI EXCLUSIVE NEW

WHILE ! EOF()
        limpa(00,00,24,79)
        SELE('siac')
        ADIREG()
        siac->tipo      := 'DU'
        siac->tip_cli   := 'C'
        siac->fornec    := STRZERO(visi->codigo_c_f,5)
        siac->cliente   := visi->nome_c_f
        siac->emissao   := visi->emissao
        siac->num_ped   := visi->codigo_n_f
        siac->duplicata := visi->dupl1
        siac->valor_dup := visi->valor1
        siac->venc      := visi->venc1
        siac->valor     := visi->valor1
        IF ! EMPTY(visi->dupl2)
                ADIREG()
                siac->tipo      := 'DU'
                siac->tip_cli   := 'C'
                siac->fornec    := STRZERO(visi->codigo_c_f,5)
                siac->cliente   := visi->nome_c_f
                siac->emissao   := visi->emissao
                siac->num_ped   := visi->codigo_n_f
                siac->duplicata := visi->dupl2
                siac->valor_dup := visi->valor2
                siac->venc      := visi->venc2
                siac->valor     := visi->valor2
        ENDIF
        mensagem('Registro: '+siac->tipo+' - '+siac->duplicata)
        SELE('visi')
        SKIP
ENDDO
DBCLOSEALL()
*/

RETURN NIL

