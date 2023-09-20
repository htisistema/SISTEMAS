*********************
* MENU DE MANUTENCAO
*********************
#include "inkey.ch"              // constantes de codigos das teclas
#include "Fileio.ch"             // funcoes de leitura/gravacao de arquivo
#include "set.ch"                // constantes da funcao SETKEY()
#define COM_CALE

FUNCTION sac0(mdir)
***************
LOCAL MPRG:='MANUTEN'

LOCAL opcao,tela,tecla,lci,cci,lba,cba,mop
PRIVATE msituacao := 'O',cod_operado:='   ',mdata_sis:=DATE(),memp_resa:='MANUTENCAO'

lci:=09
cci:=27
lba:=12
cba:=54
tela = SAVESCREEN(01,00,24,79)
opcao:=0
mop := ' '
******* T I P O   D E   S I S T E M A **************
mversao := 'v7.90'
sistema := '"SIAC" Sistema Integrado Automacao Comercial'
mfirma_ins := 'INSERCOM Informatica Ltda.'
****************************************************
// MODULO PRA ACESSAR ARQUIVOS .CDX
REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
//
SET MESSAGE TO 23 CENTER
SET CONS OFF
SET WRAP ON
SET SCOREBOARD OFF
SET DATE BRITI
SET DELE ON
SET INTEN ON
SET EPOCH TO 1911

setcor(3)
limpa(01,00,24,79)
SETCOLOR('W+/B,x')
limpa(00,00,00,79)
limpa(23,00,23,79)
DEVPOS(24,00);DEVOUT(PADL(mfirma_ins,80))
DEVPOS(24,00);DEVOUT(sistema+' '+mversao)
setcor(3)
limpa(02,00,02,79)
BANNER(03,'MANUTENCAO',setcor(5))
setcor(1)
opcao:=0
IF mdir <> 'MANUADM'
        CLS
        QUIT
ENDIF
IF ! AbriArq('saccfg','cfg');RETURN NIL;ENDIF
mcaminho := ALLTRIM(cfg->caminho)
*alt_senha(1)
WHILE .T.
        @ 23,00 CLEAR TO 23,79
        botao(lci,cci,lba,cba)
        @ lci+1,cci+1 PROMPT ' Atualizar p/arq.morto  ' MESSAGE '** Atualizacao de Arquivos p/melhor performace, <ESC> p/Retorno **'
        @ lci+2,cci+1 PROMPT ' Gerar Parcela Pagamento' MESSAGE '** Gerar parcela de pagamento <ESC>p/Retorno **'
        SET INTEN ON
        MENU TO opcao

        IF LASTKEY() = 27
                mop := mensagem1('Confirma o FIM DO SISTEMA [S/n]:','S','S,N')
                IF mop = 'S'
                        CLS
                        QUIT
                ENDIF
        ENDIF
        DO CASE
                CASE opcao = 1
        *               ATUALIZACAO DOS ARQ'S
                        sacatual()
                CASE opcao = 2
        *               RECIBO DA INSERCOM
                        IF ver_serie() = '975976'
                                sacparce()
                        ENDIF
        ENDCASE
ENDDO
RETURN NIL

*************************** F I M **************************************



