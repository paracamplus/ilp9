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

# Y a-t-il des options comme +gc ou +v ?
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

COMMAND_DIR=`dirname $0`
case "$COMMAND_DIR" in
    # rendre le repertoire qui contient compileThenRun.sh absolu:
    /*)
        true
        ;;
    *)
        COMMAND_DIR=`pwd -P`/$COMMAND_DIR
        ;;
esac

# Si les .c ne sont pas compilés pour l'architecture courante, on les
# recompile à la volée.
( cd $COMMAND_DIR/
  RECOMPILE=true
  if [ -r ./HOSTTYPE ]
  then if [ "$(< ./HOSTTYPE)" = "`uname -s -r -v -m`" ]
       then RECOMPILE=false
       fi
  fi
  if $RECOMPILE
  then 
      echo "Compilation de la bibliotheque d'execution d'ILP..." >&2
      make clean
      if make work
      then :
      else
          echo "GC non compilable: option +gc non possible!" >&2
          exit 13
      fi
  fi 1>&2
)

if [ ! -r $COMMAND_DIR/libilp.a ]
then 
    echo Bibliotheque introuvable: $COMMAND_DIR/libilp.a >&2
    exit 3
fi

if $WITH_GC
then
    if [ -r $COMMAND_DIR/$LIB_GC ]
    then
        CFLAGS="-DWITH_GC $CFLAGS"
    else
        echo "GC introuvable (consulter Makefile): $COMMAND_DIR/$LIB_GC" >&2
        # On continue sans GC puisque non compilé!
        WITH_GC=false
    fi
fi

# Collecter les fichiers (.o, .a .h ou .c) à incorporer et les rendre absolus:
FILES=${FILES:-}
for file
do
  case "$file" in
      -D*)
          CFLAGS="$CFLAGS $file "
          ;;
      /*.[coah])
          FILES="$FILES $file"
          if [ ! -r $file ]
          then 
              echo Fichier introuvable: $file >&2
              exit 7
          fi
          ;;
      *.[coah])
          file=`pwd -P`/$file
          FILES="$FILES $file"
          if [ ! -r $file ]
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
then OTHER_LIBS="$OTHER_LIBS $COMMAND_DIR/$LIB_GC"
fi

AOUT=${TMPDIR:-/tmp}/test$USER
if [ -r $AOUT ]
then rm -f $AOUT
fi

if $VERBOSE
then 
    echo gcc ${CFLAGS} -o $AOUT \
    -I. -I$COMMAND_DIR \
    $FILES $COMMAND_DIR/libilp.a $OTHER_LIBS '&&' $AOUT
fi

#echo Trying to compile $FILE ... >&2
gcc ${CFLAGS} -o $AOUT \
    -I. -I$COMMAND_DIR \
    $FILES $COMMAND_DIR/libilp.a $OTHER_LIBS && \
$AOUT

# end of compileThenRun.sh
