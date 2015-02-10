/*  -*- coding: utf-8 -*- $Id: ilpObj.h 1238 2012-09-12 15:31:08Z queinnec $ */

#ifndef ILPOBJ_H
#define ILPOBJ_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

/** Compatibilite inter-plateforme */
#if !defined(__APPLE_CC__)
extern int snprintf(char *str, size_t size, const char *format, ...);
#endif

/** Les fonctions d'ILP sont représentées en C par des fonctions qui
 * prennent des ILP_Object et renvoie un ILP_Object. */

typedef struct ILP_Object* (*ILP_general_function)();

/** Il y a deux sortes de booléens et ces deux constantes les repèrent. */

enum ILP_BOOLEAN_VALUE {
     ILP_BOOLEAN_FALSE_VALUE = 0,
     ILP_BOOLEAN_TRUE_VALUE  = 1
};


#define ILP_EXCEPTION_BUFFER_LENGTH    1000
#define ILP_EXCEPTION_CULPRIT_LENGTH     10


/** Toutes les valeurs manipulées ont cette forme: 
 * un entête indiquant la classe suivi des champs appropriés.
 */

typedef struct ILP_Object {
     struct ILP_Class*  _class;
     union {
          unsigned char asBoolean;
          int           asInteger;
          double        asFloat;
          struct asString {
               int      _size;
               char     asCharacter[1];
          } asString;
          struct asException {
               char                message[ILP_EXCEPTION_BUFFER_LENGTH];
               struct ILP_Object*  culprit[ILP_EXCEPTION_CULPRIT_LENGTH];
          } asException;
          struct asClass {
               struct ILP_Class*   super;
               char*               name;
               int                 fields_count;
               struct ILP_Field*   last_field;
               int                 methods_count;
               ILP_general_function method[1];
          } asClass;
          struct asMethod {
               struct ILP_Class*   class_defining;
               char*               name;
               short               arity;
               short               index;
          } asMethod;
          struct asField {
               struct ILP_Class*   defining_class;
               struct ILP_Field*   previous_field;
               char*               name;
               short               offset;
          } asField;
          struct asInstance {
               struct ILP_Object*  field[1];
          } asInstance;
     }                  _content;
} *ILP_Object;

/** On identifie ces structures car la sémantique de C ne permet de
 * faire d'allocations statiques pour une variante d'union (autre que
 * la première). */

typedef struct ILP_Exception {
     struct ILP_Class* _class;
     union {
          struct asException_ {
               char                message[ILP_EXCEPTION_BUFFER_LENGTH];
               struct ILP_Object*  culprit[ILP_EXCEPTION_CULPRIT_LENGTH];
          } asException;
     }                  _content;
} *ILP_Exception;

typedef struct ILP_Class {
     struct ILP_Class* _class;
     union {
          struct asClass_ {
               struct ILP_Class*   super;
               char*               name;
               int                 fields_count;
               struct ILP_Field*   last_field;
               int                 methods_count;
               ILP_general_function method[2];
          } asClass;
     }                  _content;
} *ILP_Class;

typedef struct ILP_Method {
     struct ILP_Class* _class;
     union {
          struct asMethod_ {
               struct ILP_Class*   class_defining;
               char*               name;
               short               arity;
               short               index;
          } asMethod;
     }                  _content;
} *ILP_Method;

typedef struct ILP_Field {
     struct ILP_Class* _class;
     union {
          struct asField_ {
               struct ILP_Class*   defining_class;
               struct ILP_Field*   previous_field;
               char*               name;
               short               offset;
          } asField;
     }                  _content;
} *ILP_Field;

/** Engendrer le type des classes à i méthodes. 
 *
 * C'est nécessaire car gcc maintenant supprime les initialisations
 * superflues. Il faut donc soit allouer et initialiser dynamiquement
 * les classes soit créer autant de types que nécessaires (comment
 * éviter les doublons ?)
 */ 

#define ILP_GenerateClass(i) \
typedef struct ILP_Class##i {                                   \
     struct ILP_Class* _class;                                  \
     union {                                                    \
          struct asClass_##i {                                  \
               struct ILP_Class*   super;                       \
               char*               name;                        \
               int                 fields_count;                \
               struct ILP_Field*   last_field;                  \
               int                 methods_count;               \
               ILP_general_function method[i];                  \
          } asClass;                                            \
     }                  _content;                               \
} *ILP_Class##i

#define ILP_FindAndCallSuperMethod(i) \
  (((ilp_SuperMethod != NULL) \
    ? (*ILP_find_and_call_super_method##i) \
    : (*ILP_dont_call_super_method) )( \
   ilp_Self, ilp_CurrentMethod, ilp_SuperMethod, ilp_CurrentArguments))

extern ILP_Object ILP_find_and_call_super_method0(
     ILP_Object self,
     ILP_Method current_method,
     ILP_general_function super_method,
     ILP_Object arguments[] );
extern ILP_Object ILP_find_and_call_super_method1(
     ILP_Object self,
     ILP_Method current_method,
     ILP_general_function super_method,
     ILP_Object arguments[] );
extern ILP_Object ILP_find_and_call_super_method2(
     ILP_Object self,
     ILP_Method current_method,
     ILP_general_function super_method,
     ILP_Object arguments[] );
extern ILP_Object ILP_find_and_call_super_method3(
     ILP_Object self,
     ILP_Method current_method,
     ILP_general_function super_method,
     ILP_Object arguments[] );
extern ILP_Object ILP_dont_call_super_method(
     ILP_Object self,
     ILP_Method current_method,
     ILP_general_function super_method,
     ILP_Object arguments[] );

/** -------------------------------------------
 * Des macros pour manipuler toutes ces valeurs. 
 */

#define ILP_IsA(o,c) \
  ILP_is_a(o, c)

/** Booléens. */

#define ILP_Boolean2ILP(b) \
  ILP_make_boolean(b)

#define ILP_isBoolean(o) \
  ((o)->_class == &ILP_object_Boolean_class)

#define ILP_isTrue(o) \
  (((o)->_class == &ILP_object_Boolean_class) && \
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
  ILP_malloc(sizeof(struct ILP_Object), &ILP_object_Integer_class)

#define ILP_isInteger(o) \
  ((o)->_class == &ILP_object_Integer_class)

#define ILP_CheckIfInteger(o) \
  if ( ! ILP_isInteger(o) ) { \
       ILP_domain_error("Not an integer", o); \
  };

/** Flottants */

#define ILP_Float2ILP(f) \
  ILP_make_float(f)

#define ILP_AllocateFloat() \
  ILP_malloc(sizeof(struct ILP_Object), &ILP_object_Float_class)

#define ILP_isFloat(o) \
  ((o)->_class == &ILP_object_Float_class)

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
             + (sizeof(char) * (length)), &ILP_object_String_class)

#define ILP_isString(o) \
  ((o)->_class == &ILP_object_String_class)

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

/** Constantes de classes. */

extern struct ILP_Class ILP_object_Object_class;
extern struct ILP_Class ILP_object_Class_class;
extern struct ILP_Class ILP_object_Method_class;
extern struct ILP_Class ILP_object_Field_class;
extern struct ILP_Class ILP_object_Integer_class;
extern struct ILP_Class ILP_object_Float_class;
extern struct ILP_Class ILP_object_Boolean_class;
extern struct ILP_Class ILP_object_String_class;
extern struct ILP_Class ILP_object_Exception_class;
extern struct ILP_Field ILP_object_super_field;
extern struct ILP_Field ILP_object_defining_class_field;
extern struct ILP_Method ILP_object_print_method;
extern struct ILP_Method ILP_object_classOf_method;

/** Primitives. */

extern struct ILP_Object ILP_object_true;
extern struct ILP_Object ILP_object_false;
extern ILP_Object ILP_die (char *message);
extern ILP_Object ILP_make_boolean (int b);
extern ILP_Object ILP_make_integer (int d);
extern ILP_Object ILP_make_float (double d);
extern ILP_Object ILP_pi ();
extern ILP_Object ILP_make_string (char *s);
extern ILP_Object ILP_make_opposite (ILP_Object o);
extern ILP_Object ILP_make_negation (ILP_Object o);
extern ILP_Object ILP_make_addition (ILP_Object o1, ILP_Object o2);
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
extern ILP_Object ILP_print (ILP_Object self);
extern ILP_Object ILP_classOf (ILP_Object self);

extern ILP_Object ILP_malloc (int size, ILP_Class class);
extern ILP_Object ILP_make_instance (ILP_Class class);
extern int /* boolean */ ILP_is_a (ILP_Object o, ILP_Class class);
extern ILP_general_function ILP_find_method (ILP_Object receiver,
                                             ILP_Method method,
                                             int argc);

/** Mecanisme d'allocation */

#ifdef WITH_GC
   /* Le GC de Boehm se trouve là: */
#  include "include/gc.h"
#  define ILP_START_GC GC_init()
#  define ILP_MALLOC GC_malloc
#else 
#  define ILP_START_GC
#  define ILP_MALLOC malloc
#endif

/** Exceptions. */

struct ILP_catcher {
     struct ILP_catcher *previous;
     jmp_buf _jmp_buf;
};

extern struct ILP_catcher *ILP_current_catcher;
extern ILP_Object ILP_current_exception;
extern ILP_Object ILP_throw (ILP_Object exception);
extern void ILP_establish_catcher (struct ILP_catcher *new_catcher);
extern void ILP_reset_catcher (struct ILP_catcher *catcher);
extern ILP_Object ILP_error (char *message);
extern ILP_Object ILP_domain_error (char *message, ILP_Object o);

#define ILP_UnknownFieldError(f, o) \
  ILP_domain_error("Unfound field " f, o);

#endif /* ILPOBJ_H */

/* end of ilpObj.h */
