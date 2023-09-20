*********************************************
* FUNCAO P/IMPRIMIR BOLETOS BANCARIOS
*********************************************

FUNCTION safbolea(mtip)
*****************
LOCAL mprg:='SAFBOLEA'
LOCAL tela,opcao,lci,cci,lba,cba,i:=0,mfaixa
PRIVATE mparcela,mparcela1,msele,morde,mvalor,mvlr_bolsa:=0,mvenc,mdata,;
        mtipo_imp:='M',mimp_tipo:=0,marq:=SPACE(35),mnum_conta:=SPACE(16),mmat_aluno:=SPACE(6),;
        mgstt:=SPACE(4),n_documento,mresponsa,mcpf,mend,mobs1,mobs2,mobs3,mobs4,;
        mobs5,mobs6,mobs7,mflag_imp:=' ',mtipo:=mtip,mtipo_doc:='ME'

IF ! ver_nivel(mprg,'EMISSAO DE BOLETO BANCARIOS','125')
        RETURN NIL
ENDIF

lci := 06
cci := 01
lba := 18
cba := 79
tela := SAVESCREEN(00,00,24,79)
*--------------------------------------------------------------------------
CLOSE ALL
IF ! abriarq('tabgstt','tbgstt');RETURN NIL;ENDIF
IF ! abriarq('tabmensa','tabmen');RETURN NIL;ENDIF
IF ! abriarq('faixa','fx');RETURN NIL;ENDIF
IF ! abriarq('caddisc','disc');RETURN NIL;ENDIF
IF ! abriarq('cadnota','nota');RETURN NIL;ENDIF
IF ! abriarq('cadaluno','aluno');RETURN NIL;ENDIF
IF ! abriarq('cdmensal','cdmen');RETURN NIL;ENDIF
IF ! abriarq('nobrega','nobre');RETURN NIL;ENDIF
*--------------------------------------------------------------------------
**************
SELE('nobre')
**************
mdata      := mdata_sis
mmat_aluno := SPACE(6)
mgstt      := SPACE(4)
mvenc     := CTOD('  /  /  ')
n_documento:=SPACE(8)
mvalor     := 0
mobs1      := SPACE(50)
mobs2      := SPACE(50)
mobs3      := SPACE(50)
mobs4      := SPACE(50)
mobs5      := SPACE(50)
mobs6      := SPACE(50)
mobs7      := SPACE(50)
*mobs1      := '- Ate o vencimento receber o valor R$:         '
*mobs2      := '- Ate 30 dias apos vencimento receber R$:      '
*mobs3      := '- Apos 30 dias do vencimento receber o valor do'
*mobs4      := '  documento mais multa de 2% ao mes e juros de '
*mobs5      := '  1% ao mes                                    '
*mobs6      := '- Apos 60 dias sera encaminhado para protesto  '
*mobs7      := '                                             '
@ 23,00 CLEAR TO 23,79
setcor(3)
botao(lci,cci,lba,cba)
setcor(1)
DEVPOS(lci+1,cci+1);DEVOUT('Numero Documento..:')
DEVPOS(lci+1,cci+50);DEVOUT('Emissao...:')
DEVPOS(lci+2,cci+1);DEVOUT('Matricula do Aluno:')
DEVPOS(lci+3,cci+1);DEVOUT('Digite o GSTT.....:')
DEVPOS(lci+4,cci+1);DEVOUT('Valor do Documento:')
DEVPOS(lci+4,cci+50);DEVOUT('Vencimento:')
DEVPOS(lci+5,cci+1);DEVOUT('Instrucoes:')
@ lci+1,cci+21 GET n_documento PICT '@!'
@ lci+1,cci+62 GET mdata
@ lci+2,cci+21 GET mmat_aluno PICT '999999' VALID IF(EMPTY(mmat_aluno),.T.,ver_aluno(mmat_aluno,,lci+2,cci+29))
@ lci+3,cci+21 GET mgstt PICT '@!' VALID ver_gstt(mgstt)
@ lci+4,cci+21 GET mvalor PICT '999,999.99'
@ lci+4,cci+62 GET mvenc VALID IF(mvenc<mdata,.F.,.T.)
@ lci+5,cci+13 GET mobs1
@ lci+6,cci+13 GET mobs2
@ lci+7,cci+13 GET mobs3
@ lci+8,cci+13 GET mobs4
@ lci+9,cci+13 GET mobs5
@ lci+10,cci+13 GET mobs6
@ lci+11,cci+13 GET mobs7
READ
IF LASTKEY() = 27
        RESTSCREEN(00,00,24,79,tela)
        RETURN NIL
ENDIF
opcao := ' '
opcao := mensagem1('Confirma a Operacao:','S','S,N')
IF LASTKEY() = 27 .OR. opcao = 'N'
        RESTSCREEN(00,00,24,79,tela)
        RETURN NIL
ENDIF
msele := SELE()
morde := INDEXORD()
mseq := 1
mlin := SAVESCREEN(24,00,24,79)
i:= 0
IF ! imp_arq('BOLETO.REL')
        RESTSCREEN(00,00,24,79,tela)
        RETURN NIL
ENDIF
mresponsa  := aluno->responsa
mcpf       := aluno->r_cpf
mend       := RTRIM(aluno->r_rua)+', '+RTRIM(aluno->r_num)+'-'+RTRIM(aluno->r_apto)+' - '+RTRIM(aluno->r_bairro)+' - '+RTRIM(aluno->r_cidade)+' - '+aluno->r_estado
IF mtipo = 'ITAU' .OR. mtipo = 'BILBAO'
        mflag_imp := '*'
        bol_itau()
ELSEIF mtipo = 'UNIBANCO'
        bol_unib()
        IF meject = 'S';EJECT;ENDIF
ELSEIF mtipo = 'RURAL'
        bol_rural()
        IF meject = 'S';EJECT;ENDIF
ELSEIF mtipo = 'REAL'
        bol_real()
        IF meject = 'S';EJECT;ENDIF
ELSEIF mtipo = 'BANDEPE' .OR. mtipo = 'HSBC'
        bol_bandep()
ELSEIF mtipo = 'BRADESCO' .OR. mtipo = 'BB'
        mflag_imp := '*'
        bol_brades()
ENDIF
SETPRC(00,00)
EJECT
SET DEVI TO SCREEN;SET PRINT TO;SET PRINT OFF
IF mimp_tipo = 2
        lertexto('BOLETO.REL')
ENDIF
RESTSCREEN(00,00,24,79,tela)
SET CENTURY OFF
RETURN NIL
********************************* F I M ********************************

