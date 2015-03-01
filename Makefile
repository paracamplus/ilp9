#******************************************************************
# ILP9 - Implantation d'un langage de programmation.
# by Christian.Queinnec@paracamplus.com
# See http://mooc.paracamplus.com/ilp9
# GPL version 3
#****************************************************************** 

work : grammar9.rng validate.xml.files recompile.C.code

JAVA		=	java
TRANG		=	Java/jars/trang.jar
JING		=	Java/jars/jing.jar
CODING		=	utf-8

.SUFFIXES: .rnc .rng .xsd .dtd
.rnc.rng :
	${JAVA} -jar ${TRANG} \
		-i encoding=${CODING} \
		-o encoding=${CODING} \
		 $*.rnc $*.rng
.rnc.xsd :
	${JAVA} -jar ${TRANG} \
		-i encoding=${CODING} \
		-o encoding=${CODING} \
		 $*.rnc $*.xsd
.rnc.dtd :
	${JAVA} -jar ${TRANG} \
		-i encoding=${CODING} \
		-o encoding=${CODING} \
		 $*.rnc $*.dtd

grammar9.rng : grammar9.rnc

validate.xml.files : grammar9.rng generate.xml.files
	for p in Samples/*.xml ; do \
	  echo Validating $$p with grammar9 ; \
	  ${JAVA} -jar ${JING} grammar9.rng $$p ; \
	done

generate.xml.files :
	-cd Samples/ && rm *.xml *.print *.result
	cd Samples/Scheme ; make 

recompile.C.code :
	cd C/ && make

# end of Makefile
