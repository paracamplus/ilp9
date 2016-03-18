#! /bin/bash
# -*- coding: utf-8 -*-
#******************************************************************
# ILP9 - Implantation d'un langage de programmation.
# by Christian.Queinnec@paracamplus.com
# See http://mooc.paracamplus.com/ilp9
# GPL version 3
#****************************************************************** 

usage () {
    cat <<EOF
Usage: $0 [ +gc ] [ +v ] foo.c [baz.c -Dq=3 ...]
Compile les fichiers C, cree puis lance l'executable /tmp/test$USER
Ce script suppose que tous les fichiers .c, .h, .a sont sous le
repertoire parent de ce script (celui ou se trouvent C/ et Samples/).

+gc est une option incluant le GC de Boehm (si utilisable)
+v montre sans l'executer la commande gcc synthetisee
foo.c est le resultat de la compilation d'ILP
bar.o sont des modules compiles a utiliser plutot que ceux de libilp.a
   [libilp.a contient les modules par defaut d'ILP1. Pour certaines
    versions d'ILP, il faut des modules plus appropries.]
hux.h est un fichier d'entete a inclure pour la compilation de foo.c
baz.c est un fichier C a compiler en meme temps que foo.c
-Dvar=val  variable cpp supplementaire a passer a gcc
    
Ce script est egalement sensible aux variables d'environnement:
  OTHER_LIBS   ajout de bibliotheques supplementaires
EOF
}

# Les variables contrôlant si l'on utilise le GC de Boehm:
#    Chemin relatif vers la bibliotheque statique:
LIB_GC=gc.a
#    Incorpore-t-on un GC ou pas ?
WITH_GC=false
# Montre-t-on la commande gcc synthetisee
VERBOSE=false

# Y a-t-il des options comme +gc ou +v en tete des options ?
while [ $# -gt 0 ]
do 
    case "$1" in 
        +gc)
            WITH_GC=true
            shift
            ;;
        +v)
            VERBOSE=true
            shift
            ;;
        *)
            break
            ;;
    esac
done

CFLAGS='-Wall -Wno-unused-variable -Wno-unused-but-set-variable -Wno-unused-label -std=c99 -pedantic -g '
# NOTE: Il y a des tests avec des variables inutilisees, ne pas
# attirer l'attention dessus.

# On se déplace dans le répertoire contenant C, Java, Samples.

cd `dirname $0`/../
if [ -d C -a -d Samples ]
then true
else
    echo "Je ne trouve pas les repertoires C et Samples" >&2
    exit 9
fi

# Sommes-nous sur la meme machine ?
SAME_HOSTTYPE=false
if [ -r C/HOSTTYPE ]
then if [ "$(< C/HOSTTYPE)" = "`uname -s -r -v -m`" ]
     then SAME_HOSTTYPE=true
     fi
else
    uname -s -r -v -m > C/HOSTTYPE	
fi

# On essaye toujours de regenerer libilp.a
( cd C
  make libilp.a >/dev/null 
  if [ ! -r ./libilp.a ]
  then 
      echo "Compilation incorrecte de C/ilp.[ch]" >&2
      exit 15
  fi
)

# On essaye de compiler le GC mais seulement si l'on n'a pas deja essaye.
# NOTA si `pwd` contient des blancs, la compilation ne reussira pas.
RECOMPILE_GC=false
if $SAME_HOSTTYPE
then if [ ! -r C/$LIB_GC ]
     then 
        if [ -r C/compilingGC... ]
        then
            echo "Compilation du GC incorrecte!" >&2
            WITH_GC=false
        else
            RECOMPILE_GC=true
        fi
    fi
else
    RECOMPILE_GC=true
fi

if $RECOMPILE_GC
then
    echo "Compilation du GC" >&2
    touch C/compilingGC... 
    (cd C ; make compile.gc)
    if [ -r C/$LIB_GC ]
    then 
        rm C/compilingGC... 
    else
        echo "GC non compilable: option +gc non possible!" >&2
        WITH_GC=false
    fi
fi

# Collecter les fichiers (.o, .a .h ou .c) à incorporer:
FILES=${FILES:-}
for file in "$@"
do
  case "$file" in
      -D*)
          CFLAGS="$CFLAGS $file "
          ;;
      *.[coah])
          FILES="$FILES $file"
          if [ ! -r "$file" ]
          then 
              echo Fichier introuvable: $file >&2
              exit 7
          fi
          ;;
      *)
          FILES="$FILES $file"
          ;;
  esac
done

OTHER_LIBS=${OTHER_LIBS:- -lm}
if $WITH_GC
then OTHER_LIBS="$OTHER_LIBS C/$LIB_GC"
fi

AOUT=${TMPDIR:-/tmp}/test$USER
if [ -r $AOUT ]
then rm -f $AOUT
fi

if $VERBOSE
then 
    echo gcc ${CFLAGS} -o $AOUT \
    -I. -IC \
    $FILES C/libilp.a $OTHER_LIBS '&&' $AOUT
fi

#echo Trying to compile $FILE ... >&2
gcc ${CFLAGS} -o $AOUT \
    -I. -IC \
    $FILES C/libilp.a $OTHER_LIBS && \
$AOUT

# end of compileThenRun.sh
