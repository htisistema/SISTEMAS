ECHO OFF
cls
clipper @insercom /n
copy insercom.obj c:\clipper5\obj
clipper @siacadm /n
clipper @sac1adm /n
clipper @sac2adm /n
clipper @sacnota /n
RTLINK /FREE FI siacadm,sac1adm,sac2adm,sacnota,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52,relogio

