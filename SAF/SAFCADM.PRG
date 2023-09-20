*******************************
* INCLUSAO DE MENSALIDADES - CADMENSAL
*******************************

FUNCTION safcadm(mdir)
**************

LOCAL i,cont,matric,mgstt,mparc,mano,mparcela,mbolsa,mvalor,m_parc:={},p:=0

cont   := 0
matric := SPACE(6)
mgstt  := SPACE(4)
mparc  := SPACE(2)
mano   := SPACE(2)
mparcela := SPACE(2)
mbolsa := SPACE(1)

REQUEST DBFCDX
RDDSETDEFAULT('DBFCDX')
SET DELE ON
SET DATE BRITI
CLEAR ALL
@ 00,00 CLEAR TO 24,79
IF mdir = NIL
        DEVPOS(2,00);DEVOUT('Informe o ano')
        RETURN NIL
ENDIF
USE cdmensal  alias cdmen EXCLUSIVE new
USE cadaluno  alias aluno shared new
USE tabmensa  alias tabmen shared new
USE faixa     alias fx     shared new
*****************
SELE('aluno');ORDSETFOCUS(4)
*****************
GO TOP

DEVPOS(2,00);DEVOUT('Lendo arquivo CADALUNO           Registro No.:')
WHILE ! EOF()
        IF ! EMPTY(aluno->dt_tranf)
                SKIP
                LOOP
        ENDIF

        ASIZE(m_parc,0)
        cont ++
        DEVPOS(2,48);DEVOUT(cont)
        DEVPOS(3,48);DEVOUT(aluno->matricula)

        matric := aluno->matricula
        mgstt  := aluno->gstt

        *****************
        SELE('cdmen');ORDSETFOCUS(1)
        *****************
        GO TOP
        mano  := mdir

        IF DBSEEK(aluno->matricula)
                WHILE cdmen->matricula = matric .AND. ! EOF()
                        mparc := cdmen->parcela
                        AADD(m_parc,cdmen->parcela)
                        cdmen->data_venc  := CTOD('05/'+cdmen->parcela+'/'+mano)
                        DBUNLOCK()
                        SKIP
                ENDDO
        ELSE
                mparc := SPACE(2)
        ENDIF
   
        IF LEN(m_parc) = 0
                *****************
                SELE('aluno');ORDSETFOCUS(4)
                *****************
                SKIP
                LOOP
        ENDIF
        p:=0
        i:=0
*       FOR i := 1 TO (12 - VAL(mparc))
        FOR i := 1 TO 12
                p := ASCAN(m_parc,STRZERO(i,2))
                IF  p > 0
                        LOOP
                ENDIF
                IF ! ADIREG()
                        atencao('Nao foi possivel criar o registro')
                        LOOP
                ENDIF
                mparcela          := STRZERO(i,2)
                cdmen->tipo       := 'ME'
                cdmen->gstt       := mgstt
                cdmen->matricula  := matric
                mbolsa            := ver_faixa(mparcela)
                cdmen->bolsa      := mbolsa
                cdmen->parcela    := mparcela
                cdmen->data_venc  := CTOD('05/'+mparcela+'/'+mano)
                mvalor := vlr_men(SUBSTR(aluno->gstt,1,2)+SUBSTR(aluno->gstt,4),mparcela)
                cdmen->vlr_parc   := mvalor
                IF faixa_vlr(mbolsa) = 100
                        cdmen->desc_bolsa := mvalor
                ELSE
                        cdmen->desc_bolsa := mvalor * (faixa_vlr(mbolsa)/100)
                ENDIF
                cdmen->flag       := ' '
                DBUNLOCK()
        NEXT
        DBCOMMITALL()
        *****************
        SELE('aluno');ORDSETFOCUS(4)
        *****************
        SKIP

ENDDO
RETURN NIL
