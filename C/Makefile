#******************************************************************
# ILP9 - Implantation d'un langage de programmation.
# by Christian.Queinnec@paracamplus.com
# See http://mooc.paracamplus.com/ilp9
# GPL version 3
#****************************************************************** 

# Clean the directory in order to ease porting to another type of
# machine. (MacOSX to Linux for instance).

work : clean libilp.a
	-[ -f gc.a -a -d include ] || make compile.gc
# no switch -o on MacOSX:
	uname -s -r -v -m > HOSTTYPE	

clean :
	-rm -f *.o libilp.a HOSTTYPE gc-7.2/gc.a
	-rm -rf include

# Libraries will be compiled in debug mode to help students.

CC	=	gcc

# No glibc on MacOSX, we suppose that the mere existence of /Library tells
# us that we are on a MacOSX.
CFLAGS	=	-Wall -std=c99 -pedantic \
		-g \
 `if [ -d /Library ] ; then : ; else if pkg-config --exists glib-2.0 ; then pkg-config --cflags --libs glib-2.0 ; fi ; fi`

# Compile every C file

CFILES	=	$(shell ls ilp*.c)

ARFILES	=	ilp.o

# Don't forget ranlib, this is useful on MacOSX.

libilp.a : ${CFILES:.c=.o}
	ar cvr libilp.a ${ARFILES}
	-ranlib libilp.a

# Other dependencies:

ilp.o : ilp.c ilp.h

# The +gc switch in compileThenRun.sh means that we want to use Boehm's GC:

compile.gc : gc-7.2/gc.a
	mv gc-7.2/gc.a . 
	mv gc-7.2/include .
	rm -rf gc-7.2
gc-7.2/gc.a : gc-7.2f.tgz
	[ -d gc-7.2 ] || tar xzf gc-7.2f.tgz
	cd gc-7.2/ && ./configure && make && make check
	cd gc-7.2/ && make -f Makefile.direct

# end of Imakefile
