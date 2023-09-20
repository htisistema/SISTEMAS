@echo off
cls
clipper @safindex /n
rtlink fi safindex LIB clipper,extend,novterm,dbfcdx,gaspro52 OUT safindex
del safindex.obj

