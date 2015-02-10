#******************************************************************
# ILP - - Implantation d'un langage de programmation.
# Copyright (C) 2004-2010 <Christian.Queinnec@lip6.fr>
# $Id: Makefile 1249 2012-09-27 06:49:51Z queinnec $
# GPL version>=2
#****************************************************************** 

# On efface les resultats de compilation pour faciliter le changement
# de machine (et d'architecture (Intel*86 vers powerpc par exemple)).
# En fait, il serait mieux de stocker les .o et .a dans un
# sous-repertoire mais comment automatiser (de facon portable) cela
# sous un OS non Unix ?

work : clean libilp.a
	-[ -f gc.a -a -d include ] || make compile.gc
# Pas de -o sur MacOSX:
	uname -s -r -v -m > HOSTTYPE	

# Effacer les resultats de compilation

clean :
	-rm -f *.o libilp.a HOSTTYPE gc-7.2/gc.a

# Les bibliotheques d'execution sont compilees en mode debug, c'est
# plus utile pour les exercices.

CC	=	gcc

# Pas de glibc pour MacOSX, on suppose que l'existence de /Library indique
# que c'est un Mac.
CFLAGS	=	-Wall -std=c99 -pedantic \
		-g \
 `if [ -d /Library ] ; then : ; else if pkg-config --exists glib-2.0 ; then pkg-config --cflags --libs glib-2.0 ; fi ; fi`

# On compile tout ce qui est en C (sauf ilpHash.c non porte sur MacOSX)
# et sauf les template*.c qui sont bien sur incomplets.

CFILES	=	$(shell ls ilp*.c)

# On place ilpBasicError dans l'archive et non ilpError (afin de
# pouvoir en changer s'il le faut).

ARFILES	=	ilp.o

# Ne pas oublier ranlib, c'est utile sur MacOSX.

libilp.a : ${CFILES:.c=.o}
	ar cvr libilp.a ${ARFILES}
	-ranlib libilp.a

# Autres dependances:

ilp.o : ilp.c ilp.h

# L'option +gc de compileThenRun.sh utilise le GC de Boehm qu'il faut 
# aussi installer (cf. C.readme):

compile.gc : gc-7.2/gc.a
	mv gc-7.2/gc.a . 
	mv gc-7.2/include .
	rm -rf gc-7.2
gc-7.2/gc.a : gc-7.2d.tgz
	[ -d gc-7.2 ] || tar xzf gc-7.2d.tgz
	cd gc-7.2/ && make -f Makefile.direct test
# QNC 2010oct: OK sur amd64 (ubuntu 9.10)
# QNC 2010oct: 6.8 ne compile pas sur MacOSX 10.6.4
# QNC 2012aug: 7.2d OK sur amd64 (Debian 6) et sur MacOSX 10.6.8

# end of Imakefile
