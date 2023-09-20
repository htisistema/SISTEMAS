ECHO OFF
cls
clipper @insercom /n
copy insercom.obj c:\clipper5\obj
clipper @saccons /n
RTLINK /FREE FI saccons,insercom,amdk6 LIB clipper,extend,dbfcdx,novterm,GASPRO52,relogio

