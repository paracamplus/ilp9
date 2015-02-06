/* -*- coding: utf-8 -*- $Id: ilp.h 1246 2012-09-17 15:39:53Z queinnec $ */

#ifndef ILP_H
#define ILP_H

#include <stdlib.h>
#include <stdio.h>

/** Definition additionnelle pour enrichir ILP_Object */

#ifndef ILP_OTHER_KINDS
#define ILP_OTHER_KINDS
#define ILP_OTHER_STRUCTS
#endif /* ILP_OTHER_KINDS */

/** Le type général des fonctions d'ILP (d'arité quelconque). */

typedef struct ILP_Object* (*ILP_general_function)();

/** Il y a cinq types de valeurs pour l'instant repérées par ces
 * constantes (elles figureront dans le champ _kind d'ILP_Object.
 */

enum ILP_Kind {
     ILP_BOOLEAN_KIND       = 0xab010ba,
     ILP_INTEGER_KIND       = 0xab020ba,
     ILP_FLOAT_KIND         = 0xab030ba,
     ILP_STRING_KIND        = 0xab040ba,
     ILP_PRIMITIVE_KIND     = 0xab050ba
     ILP_OTHER_KINDS
};

/** Toutes les valeurs manipulées ont cette forme. 
 *
 * Un premier champ indique leur nature ce qui permet de décoder le
 * champ qui suit. Les chaînes de caractères sont préfixées par leur
 * longueur.

 * NOTE: asBoolean est la première possibilité ce qui est nécessaire pour 
 * l'allocation statiques des booléens (cf. ilp.c). Ce ne l'était pas 
 * auparavant ce qui créait une bogue pour MacOSX (ILP_TRUE valait faux!).
 */

typedef struct ILP_Object {
     enum ILP_Kind      _kind;
     union {
          unsigned char asBoolean;
          int           asInteger;
          double        asFloat;
          struct asString {
               int      _size;
               char     asCharacter[1];
          } asString;
          struct asPrimitive {
               ILP_general_function  _code;
          } asPrimitive;
          ILP_OTHER_STRUCTS
     }                  _content;
} *ILP_Object;

/** -------------------------------------------
 * Des macros pour manipuler toutes ces valeurs. 
 */

/** Booléens. */

/** Il y a deux sortes de booléens et ces deux constantes les repèrent. */

enum ILP_BOOLEAN_VALUE {
     ILP_BOOLEAN_FALSE_VALUE = 0,
     ILP_BOOLEAN_TRUE_VALUE  = 1
};

#define ILP_Boolean2ILP(b) \
  ILP_make_boolean(b)

#define ILP_isBoolean(o) \
  ((o)->_kind == ILP_BOOLEAN_KIND)

#define ILP_isTrue(o) \
  (((o)->_kind == ILP_BOOLEAN_KIND) && \
   ((o)->_content.asBoolean))

#define ILP_TRUE  (&ILP_object_true)
#define ILP_FALSE (&ILP_object_false)

#define ILP_isEquivalentToTrue(o) \
  ((o) != ILP_FALSE)

#define ILP_CheckIfBoolean(o) \
  if ( ! ILP_isBoolean(o) ) { \
       ILP_domain_error("Not a boolean", o); \
  };

/** Entiers */

#define ILP_Integer2ILP(i) \
  ILP_make_integer(i)

#define ILP_AllocateInteger() \
  ILP_malloc(sizeof(struct ILP_Object), ILP_INTEGER_KIND)

#define ILP_isInteger(o) \
  ((o)->_kind == ILP_INTEGER_KIND)

#define ILP_CheckIfInteger(o) \
  if ( ! ILP_isInteger(o) ) { \
       ILP_domain_error("Not an integer", o); \
  };

/** Flottants */

#define ILP_Float2ILP(f) \
  ILP_make_float(f)

#define ILP_AllocateFloat() \
  ILP_malloc(sizeof(struct ILP_Object), ILP_FLOAT_KIND)

#define ILP_isFloat(o) \
  ((o)->_kind == ILP_FLOAT_KIND)

#define ILP_CheckIfFloat(o) \
  if ( ! ILP_isFloat(o) ) { \
       ILP_domain_error("Not a float", o); \
  };

#define ILP_PI_VALUE 3.1415926535
#define ILP_PI (ILP_pi())

/** Chaînes de caractères */

#define ILP_String2ILP(s) \
  ILP_make_string(s)

#define ILP_AllocateString(length) \
  ILP_malloc(sizeof(struct ILP_Object) \
             + (sizeof(char) * (length)), ILP_STRING_KIND)

#define ILP_isString(o) \
  ((o)->_kind == ILP_STRING_KIND)

#define ILP_CheckIfString(o) \
  if ( ! ILP_isString(o) ) { \
       ILP_domain_error("Not a string", o); \
  };

/** Opérateurs unaires */

#define ILP_Opposite(o) \
  ILP_make_opposite(o)

#define ILP_Not(o) \
  ILP_make_negation(o)

/** Opérateurs binaires */

#define ILP_Plus(o1,o2) \
  ILP_make_addition(o1, o2)

#define ILP_Minus(o1,o2) \
  ILP_make_subtraction(o1, o2)

#define ILP_Times(o1,o2) \
  ILP_make_multiplication(o1, o2)

#define ILP_Divide(o1,o2) \
  ILP_make_division(o1, o2)

#define ILP_Modulo(o1,o2) \
  ILP_make_modulo(o1, o2)

/** modulo a faire */

#define ILP_LessThan(o1,o2) \
  ILP_compare_less_than(o1,o2)

#define ILP_LessThanOrEqual(o1,o2) \
  ILP_compare_less_than_or_equal(o1,o2)

#define ILP_GreaterThan(o1,o2) \
  ILP_compare_greater_than(o1,o2)

#define ILP_GreaterThanOrEqual(o1,o2) \
  ILP_compare_greater_than_or_equal(o1,o2)

#define ILP_Equal(o1,o2) \
  ILP_compare_equal(o1,o2)

#define ILP_NotEqual(o1,o2) \
  ILP_compare_not_equal(o1,o2)

/** Primitives:
 * Les fonctions ILP_print() et ILP_newline() sont définies dans ilp.c
 */

#define ILP_globalIfInitialized(n) \
     (((n)!=NULL)?(n):(ILP_error("Uninitialized " #n " variable!")))

typedef ILP_Object (*ILP_Primitive) ();

extern struct ILP_Object ILP_object_true;
extern struct ILP_Object ILP_object_false;
extern struct ILP_Object ILP_object_pi;
extern ILP_Object ILP_die (char *message);
extern ILP_Object ILP_make_boolean (int b);
extern ILP_Object ILP_make_integer (int d);
extern ILP_Object ILP_make_float (double d);
extern ILP_Object ILP_pi ();
extern ILP_Object ILP_make_string (char *s);
extern ILP_Object ILP_make_opposite (ILP_Object o);
extern ILP_Object ILP_make_negation (ILP_Object o);
extern ILP_Object ILP_make_addition (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_concatenate_strings (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_make_subtraction (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_make_multiplication (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_make_division (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_make_modulo (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_compare_less_than (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_compare_less_than_or_equal (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_compare_equal (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_compare_greater_than (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_compare_greater_than_or_equal (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_compare_not_equal (ILP_Object o1, ILP_Object o2);
extern ILP_Object ILP_newline ();
extern ILP_Object ILP_print (ILP_Object o);

#endif /* ILP_H */

/* end of ilp.h */
