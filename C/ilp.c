/* -*- coding: utf-8 -*- 
 * ******************************************************************
 * ILP -- Implantation d'un langage de programmation.
 * Copyright (C) 2004 <Christian.Queinnec@lip6.fr>
 * $Id: ilp.c 1111 2011-10-17 05:51:28Z queinnec $
 * GPL version>=2
 * ******************************************************************/

/** Ce fichier constitue la bibliothèque d'exécution d'ILP. */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "ilp.h"
#include "ilpAlloc.h"
#include "ilpBasicError.h"

/** Booléens.
 * 
 * On alloue statiquement les deux booléens.
 * Pas possible encore en C d'allouer le flottant pi statiquement.
 * On l'alloue donc dynamiquement mais comme un singleton (cf. ilp_pi).
 */

struct ILP_Object ILP_object_true = {
     ILP_BOOLEAN_KIND,
     { ILP_BOOLEAN_TRUE_VALUE }
};

struct ILP_Object ILP_object_false = {
     ILP_BOOLEAN_KIND,
     { ILP_BOOLEAN_FALSE_VALUE }
};

/** Une fonction pour stopper abruptement l'application. */

ILP_Object
ILP_die (char *message)
{
     fputs(message, stderr);
     fputc('\n', stderr);
     fflush(stderr);
     exit(EXIT_FAILURE);
}

/** Ce n'est pas une vraie allocation mais une simple conversion. */

ILP_Object
ILP_make_boolean (int b)
{
     if ( b ) {
          return ILP_TRUE;
     } else {
          return ILP_FALSE;
     }
}

ILP_Object
ILP_make_integer (int d)
{
     ILP_Object result = ILP_AllocateInteger();
     result->_content.asInteger = d;
     return result;
}

ILP_Object
ILP_make_float (double d)
{
     ILP_Object result = ILP_AllocateFloat();
     result->_content.asFloat = d;
     return result;
}

ILP_Object
ILP_pi ()
{
     static ILP_Object object_pi = NULL;
     if ( object_pi == NULL ) {
          object_pi = ILP_make_float(ILP_PI_VALUE);
     }
     return object_pi;
}


ILP_Object
ILP_make_string (char *s)
{
     int size = strlen(s);
     ILP_Object result = ILP_AllocateString(size);
     result->_content.asString._size = size;
     memmove(result->_content.asString.asCharacter, s, size);
     return result;
}

/** String primitives */

ILP_Object
ILP_concatenate_strings (ILP_Object o1, ILP_Object o2)
{
     int size1 = o1->_content.asString._size;
     int total_size = size1 + o2->_content.asString._size;
     ILP_Object result = ILP_AllocateString(total_size);
     memmove(&(result->_content.asString.asCharacter[0]), 
             o1->_content.asString.asCharacter,
             o1->_content.asString._size);
     memmove(&(result->_content.asString.asCharacter[size1]),
             o2->_content.asString.asCharacter,
             o2->_content.asString._size);
     return result;
}

/** Opérateurs unaires. 
 La négation de flottant manquait comme vu par <Clement.Bossut@etu.upmc.fr> */

ILP_Object
ILP_make_opposite (ILP_Object o)
{
     if ( ILP_isInteger(o) ) {
          ILP_Object result = ILP_AllocateInteger();
          result->_content.asInteger = (- o->_content.asInteger);
          return result;
     } else if ( ILP_isFloat(o) ) {
          ILP_Object result = ILP_AllocateFloat();
          result->_content.asFloat = (- o->_content.asFloat);
          return result;
     } else {
          return ILP_domain_error("Not a number", o);
     }
}

ILP_Object
ILP_make_negation (ILP_Object o)
{
     /* Opere sur toutes les representations possibles de Vrai */
     if ( ILP_isEquivalentToTrue(o) ) {
          return ILP_FALSE;
     } else {
          return ILP_TRUE;
     }
}

/** Opérateurs binaires. */

/* DefineOperator(addition, +) est incorrect car + représente également
 * la concaténation des chaînes de caractères. */

ILP_Object
ILP_make_addition (ILP_Object o1, ILP_Object o2)
{
  if ( ILP_isInteger(o1) ) {
    if ( ILP_isInteger(o2) ) {
      ILP_Object result = ILP_AllocateInteger();
      result->_content.asInteger = o1->_content.asInteger + o2->_content.asInteger;
      return result;
    } else if ( ILP_isFloat(o2) ) {
      ILP_Object result = ILP_AllocateFloat();
      result->_content.asFloat = o1->_content.asInteger + o2->_content.asFloat;
      return result;
    } else {
      return ILP_domain_error("Not a number", o2);
    }
  } else if ( ILP_isFloat(o1) ) {
    if ( ILP_isInteger(o2) ) {
      ILP_Object result = ILP_AllocateFloat();
      result->_content.asFloat = o1->_content.asFloat + o2->_content.asInteger;
      return result;
    } else if ( ILP_isFloat(o2) ) {
      ILP_Object result = ILP_AllocateFloat();
      result->_content.asFloat = o1->_content.asFloat + o2->_content.asFloat;
      return result;
    } else {
      return ILP_domain_error("Not a number", o2);
    }

  } else if ( ILP_isString(o1) ) {
    if ( ILP_isString(o2) ) {
      return ILP_concatenate_strings(o1, o2);
    } else {
      return ILP_domain_error("Not a string", o2);
    }
  } else {
    return ILP_domain_error("Not addable", o1);
  }
}

#define DefineOperator(name,op)                                        \
ILP_Object                                                             \
ILP_make_##name (ILP_Object o1, ILP_Object o2)                         \
{                                                                      \
     if ( ILP_isInteger(o1) ) {                                        \
          if ( ILP_isInteger(o2) ) {                                   \
               ILP_Object result = ILP_AllocateInteger();              \
               result->_content.asInteger =                            \
                    o1->_content.asInteger op o2->_content.asInteger;  \
               return result;                                          \
          } else if ( ILP_isFloat(o2) ) {                              \
               ILP_Object result = ILP_AllocateFloat();                \
               result->_content.asFloat =                              \
                    o1->_content.asInteger op o2->_content.asFloat;    \
               return result;                                          \
          } else {                                                     \
               return ILP_domain_error("Not a number", o2);            \
          }                                                            \
     } else if ( ILP_isFloat(o1) ) {                                   \
          if ( ILP_isInteger(o2) ) {                                   \
               ILP_Object result = ILP_AllocateFloat();                \
               result->_content.asFloat =                              \
                    o1->_content.asFloat op o2->_content.asInteger;    \
               return result;                                          \
          } else if ( ILP_isFloat(o2) ) {                              \
               ILP_Object result = ILP_AllocateFloat();                \
               result->_content.asFloat =                              \
                    o1->_content.asFloat op o2->_content.asFloat;      \
               return result;                                          \
          } else {                                                     \
               return ILP_domain_error("Not a number", o2);            \
          }                                                            \
     } else {                                                          \
          return ILP_domain_error("Not a number", o1);                 \
     }                                                                 \
}

DefineOperator(subtraction, -)
DefineOperator(multiplication, *)
DefineOperator(division, /)

/* DefineOperator(modulo, %) est incorrect car le modulo ne se prend
 * que sur de entiers. */

ILP_Object
ILP_make_modulo (ILP_Object o1, ILP_Object o2)
{
     if ( ILP_isInteger(o1) ) {
          if ( ILP_isInteger(o2) ) {
               ILP_Object result = ILP_AllocateInteger();
               result->_content.asInteger =
                    o1->_content.asInteger % o2->_content.asInteger;
               return result;
          } else {
               return ILP_domain_error("Not an integer", o2);
          }
     } else {
          return ILP_domain_error("Not an integer", o1);
     }
}

#define DefineComparator(name,op)                                      \
ILP_Object                                                             \
ILP_compare_##name (ILP_Object o1, ILP_Object o2)                      \
{                                                                      \
     if ( ILP_isInteger(o1) ) {                                        \
          if ( ILP_isInteger(o2) ) {                                   \
               return ILP_make_boolean(                                \
                    o1->_content.asInteger op o2->_content.asInteger); \
          } else if ( ILP_isFloat(o2) ) {                              \
               return ILP_make_boolean(                                \
                    o1->_content.asInteger op o2->_content.asFloat);   \
          } else {                                                     \
               return ILP_domain_error("Not a number", o2);            \
          }                                                            \
     } else if ( ILP_isFloat(o1) ) {                                   \
          if ( ILP_isInteger(o2) ) {                                   \
               return ILP_make_boolean(                                \
                    o1->_content.asFloat op o2->_content.asInteger);   \
          } else if ( ILP_isFloat(o2) ) {                              \
               return ILP_make_boolean(                                \
                    o1->_content.asFloat op o2->_content.asFloat);     \
          } else {                                                     \
               return ILP_domain_error("Not a number", o2);            \
          }                                                            \
     } else {                                                          \
          return ILP_domain_error("Not a number", o1);                 \
     }                                                                 \
}

DefineComparator(less_than, <)
DefineComparator(less_than_or_equal, <=)
DefineComparator(equal, ==)
DefineComparator(greater_than, >)
DefineComparator(greater_than_or_equal, >=)
DefineComparator(not_equal, !=)

/** Primitives */

ILP_Object
ILP_newline () 
{
     fputc('\n', stdout);
     return ILP_FALSE;
}

ILP_Object
ILP_print (ILP_Object o)
{
  switch (o->_kind) {
  case ILP_INTEGER_KIND: {
    fprintf(stdout, "%d", o->_content.asInteger);
    break;
  }
  case ILP_FLOAT_KIND: {
    /* Supprimer les blancs du debut */
    char buffer[15];
    char *p = &buffer[0];
    sprintf(buffer, "%12.5g", o->_content.asFloat);
    while ( isspace(*p) ) {
      p++;
    }
    fprintf(stdout, "%s", p);
    break;
  }
  case ILP_BOOLEAN_KIND: {
    fprintf(stdout, "%s", (ILP_isTrue(o) ? "true" : "false"));
    break;
  }
  case ILP_STRING_KIND: {
    fprintf(stdout, "%s", o->_content.asString.asCharacter);
    break;
  }
  default: {
    fprintf(stdout, "<0x%x:0x%p>", o->_kind, (void*) o);
    break;
  }
  }
  return ILP_FALSE;
}

/* end of ilp.c */
