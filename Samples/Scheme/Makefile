# ******************************************************************
# ILP -- Implantation d'un langage de programmation.
# GPL version>=2
# ******************************************************************

work : generate.xml

# Convertir des programmes écrits en syntaxe Scheme (avec tout plein
# de parenthèses) en des fichiers XML conformes aux grammaires d'ILP.
# C'est quand même nettement plus simple d'écrire comme cela! Cette
# règle engendre également les valeurs et les impressions attendues
# pour ces programmes.

SCHEME	=	bigloo -i

generate.xml : scm2xml.scm
	{ echo "(load \"scm2xml.scm\")" ; \
	  for s in u*-[0-9].scm ; do \
	     echo "(quote \"Convertir $$s en $${s%.scm}.xml\")" ; \
	     echo "(handle-file \"$$s\" \"../$${s%.scm}\")" ; \
	  done ; } | ${SCHEME}

JAVA	=	java
JING	=	../../Java/jars/jing.jar

# Un test par-ci par-la.

BASE	=	u57-2

test.by.hand :
	{ echo '(load "scm2xml.scm")' ; \
	  echo '(handle-file "${BASE}.scm" "../${BASE}")' ; } | \
		${SCHEME}
	${JAVA} -jar ${JING} ../grammar9.rng ../${FILE}.xml
	cd ../ && cat ${BASE}.xml ${BASE}.result ${BASE}.print
# Apres avoir chargé le fichier scm2xml.scm, la fonction handle-file prend
# deux arguments: le fichier à traiter, le fichier où stocker le résultat.

# ########################################## Compilation
# Compilation du programme Scheme de conversion scm -> xml en Java.

scm2xml.java : 
# -gjvm 
	bigloo -jvm -c -o scm2xml.java -call/cc -v3 -Wall -jvm-jar \
		scm2xml.scm scm2xml-*.scm

# end of Imakefile
