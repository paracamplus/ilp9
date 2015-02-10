/* -*- coding: utf-8 -*-
   $Id: ilpObj.c 1057 2011-08-19 12:14:07Z queinnec $ */
/* ******************************************************************
 * ILP -- Implantation d'un langage de programmation.
 * Copyright (C) 2004 <Christian.Queinnec@lip6.fr>
 * $Id: ilpObj.c 1057 2011-08-19 12:14:07Z queinnec $
 * GPL version>=2
 * ******************************************************************/

/** Ce fichier constitue la bibliothèque d'exécution d'ILP6. */

#include "ilp.h"

char *ilpObj_Id = "$Id: ilpObj.c 1057 2011-08-19 12:14:07Z queinnec $";

/** Les classes de base. */

extern struct ILP_Class ILP_object_Object_class;

struct ILP_Class ILP_object_Class_class = {
     &ILP_object_Class_class,
     { { &ILP_object_Object_class,
         "Class",
         1,
         &ILP_object_super_field,
         2,
         { ILP_print,
           ILP_classOf } } }
};

struct ILP_Class ILP_object_Object_class = {
     &ILP_object_Class_class,
     { { NULL,
         "Object",
         0,
         NULL,
         2,
         { ILP_print,
           ILP_classOf } } }
};

struct ILP_Class ILP_object_Method_class = {
     &ILP_object_Class_class,
     { { &ILP_object_Object_class,
         "Method",
         0,
         NULL,
         3,
         { ILP_print,
           ILP_classOf } } }
};

struct ILP_Class ILP_object_Field_class = {
     &ILP_object_Class_class,
     { { &ILP_object_Object_class,
         "Field",
         1,
         &ILP_object_defining_class_field,
         2,
         { ILP_print,
           ILP_classOf } } }
};

struct ILP_Class ILP_object_Integer_class = {
     &ILP_object_Class_class,
     { { &ILP_object_Object_class,
         "Integer",
         0,
         NULL,
         2,
         { ILP_print,
           ILP_classOf } } }
};

struct ILP_Class ILP_object_Float_class = {
     &ILP_object_Class_class,
     { { &ILP_object_Object_class,
         "Float",
         0,
         NULL,
         2,
         { ILP_print,
           ILP_classOf } } }
};

struct ILP_Class ILP_object_Boolean_class = {
     &ILP_object_Class_class,
     { { &ILP_object_Object_class,
         "Boolean",
         0,
         NULL,
         2,
         { ILP_print,
           ILP_classOf } } }
};

struct ILP_Class ILP_object_String_class = {
     &ILP_object_Class_class,
     { { &ILP_object_Object_class,
         "String",
         0,
         NULL,
         2,
         { ILP_print,
           ILP_classOf } } }
};

struct ILP_Class ILP_object_Exception_class = {
     &ILP_object_Class_class,
     { { &ILP_object_Object_class,
         "Exception",
         0,
         NULL,
         2,
         { ILP_print,
           ILP_classOf } } }
};

/** Les champs prédéfinis. 
 *
 * Tous les champs des structures C qui ne mènent pas à des valeurs
 * d'ILP ne sont pas considérés comme des champs. C'est pourquoi (1)
 * ces champs sont regroupés en tête de structure et (2) il y en a peu.
 */

struct ILP_Field ILP_object_super_field = {
     &ILP_object_Field_class,
     { { &ILP_object_Class_class,
         NULL,
         "super",
         0 } }
};

struct ILP_Field ILP_object_defining_class_field = {
     &ILP_object_Field_class,
     { { &ILP_object_Field_class,
         NULL,
         "defining_class",
         0 } }
};

struct ILP_Field ILP_object_previous_field_field = {
     &ILP_object_Field_class,
     { { &ILP_object_Field_class,
         &ILP_object_defining_class_field,
         "previous_field",
         1 } }
};

struct ILP_Field ILP_object_class_defining_field = {
     &ILP_object_Field_class,
     { { &ILP_object_Method_class,
         NULL,
         "class_defining",
         0 } }
};

/** Les méthodes prédéfinies. 
 *
 * Il n'y en a que deux (pour l'instant) d'arité nulle:
 *     o.print() qui imprime l'objet
 *     o.getClass() qui renvoie sa classe.
 */

struct ILP_Method ILP_object_print_method = {
     &ILP_object_Method_class,
     { { &ILP_object_Object_class,
         "print",
         1,       /* arité (incluant self) */
         0        /* offset */
     } }
};

struct ILP_Method ILP_object_classOf_method = {
     &ILP_object_Method_class,
     { { &ILP_object_Object_class,
         "classOf",
         1,       /* arité (incluant self) */
         1        /* offset */
     } }
};

/** Booléens.
 * 
 * On alloue statiquement les deux booléens. 
 */

struct ILP_Object ILP_object_true = {
     &ILP_object_Boolean_class,
     { ILP_BOOLEAN_TRUE_VALUE }
};

struct ILP_Object ILP_object_false = {
     &ILP_object_Boolean_class,
     { ILP_BOOLEAN_FALSE_VALUE }
};

/** Exceptions
 *
 * L'exception courante. 
 * NOTA: il serait mieux qu'elle soit dynamiquement allouée. 
 */

static struct ILP_Exception ILP_the_exception =  {
     &ILP_object_Exception_class,
     { { "", 
         { NULL } } }
};

/** Ces variables globales contiennent:
 *  -- le rattrapeur d'erreur courant
 *  -- l'exception courante (lorsque signalée)
 */

static struct ILP_catcher ILP_the_original_catcher = {
     NULL
};
struct ILP_catcher *ILP_current_catcher = &ILP_the_original_catcher;

ILP_Object ILP_current_exception = NULL;

/** Signaler une exception. */

ILP_Object
ILP_throw (ILP_Object exception)
{
     ILP_current_exception = exception;
     if ( ILP_current_catcher == &ILP_the_original_catcher ) {
          ILP_die("No current catcher!");
     };
     longjmp(ILP_current_catcher->_jmp_buf, 1);
     /** UNREACHABLE */
     return NULL;
}

/** Chaîner le nouveau rattrapeur courant avec l'ancien. */

void
ILP_establish_catcher (struct ILP_catcher *new_catcher)
{
     new_catcher->previous = ILP_current_catcher;
     ILP_current_catcher = new_catcher;
}

/** Remettre en place un rattrapeur. */

void
ILP_reset_catcher (struct ILP_catcher *catcher)
{
     ILP_current_catcher = catcher;
}

/** 
 * Signalement d'une erreur.
 */

ILP_Object
ILP_error (char *message)
{
     snprintf(ILP_the_exception._content.asException.message,
              ILP_EXCEPTION_BUFFER_LENGTH,
              "Error: %s\n",
              message);
     fprintf(stderr, "%s", ILP_the_exception._content.asException.message);
     ILP_the_exception._content.asException.culprit[0] = NULL;
     return ILP_throw((ILP_Object) &ILP_the_exception);
}

/** Une fonction pour signaler qu'un argument n'est pas du type attendu. */

ILP_Object
ILP_domain_error (char *message, ILP_Object o)
{
     snprintf(ILP_the_exception._content.asException.message,
              ILP_EXCEPTION_BUFFER_LENGTH,
              "Domain error: %s\nCulprit: 0x%p\n",
              message, (void*) o);
     fprintf(stderr, "%s", ILP_the_exception._content.asException.message);
     ILP_the_exception._content.asException.culprit[0] = o;
     ILP_the_exception._content.asException.culprit[1] = NULL;
     return ILP_throw((ILP_Object) &ILP_the_exception);
}

/** Une fonction pour stopper abruptement l'application. */

ILP_Object
ILP_die (char *message)
{
     fputs(message, stderr);
     fputc('\n', stderr);
     fflush(stderr);
     exit(EXIT_FAILURE);
}

/** Vérifier si une instance est d'une certaine classe. 
 * Cet algorithme est linéaire: on peut faire mieux! */

int /* boolean */
ILP_is_a (ILP_Object o, ILP_Class class)
{
     ILP_Class oclass = o->_class;
     if ( oclass == class ) {
          return 1;
     } else {
          oclass = oclass->_content.asClass.super;
          /* Object a NULL pour superclasse. */
          while ( oclass ) {
               if ( oclass == class ) {
                    return 1;
               };
               oclass = oclass->_content.asClass.super;
          }
          return 0;
     }
}

static int
ILP_is_subclass_of (ILP_Class oclass, ILP_Class otherclass)
{
     if ( oclass == otherclass ) {
          return 1;
     } else {
          oclass = oclass->_content.asClass.super;
          /* Object a NULL pour superclasse. */
          while ( oclass ) {
               if ( oclass == otherclass ) {
                    return 1;
               };
               oclass = oclass->_content.asClass.super;
          }
          return 0;
     }
}          

/** Déterminer une méthode. */

ILP_general_function
ILP_find_method (ILP_Object receiver,
                 ILP_Method method,
                 int argc)
{
     ILP_Class oclass = receiver->_class;
     if ( ! ILP_is_subclass_of(oclass, 
                               method->_content.asMethod.class_defining) ) {
          /* Signaler une absence de méthode */
          snprintf(ILP_the_exception._content.asException.message,
                   ILP_EXCEPTION_BUFFER_LENGTH,
                   "No such method %s\nCulprit: 0x%p\n",
                   method->_content.asMethod.name, 
                   (void*) receiver);
          /*DEBUG*/
          fprintf(stderr, "%s", ILP_the_exception._content.asException.message);
          ILP_the_exception._content.asException.culprit[0] = receiver;
          ILP_the_exception._content.asException.culprit[1] = 
               (ILP_Object) method;
          ILP_the_exception._content.asException.culprit[2] = NULL;
          ILP_throw((ILP_Object) &ILP_the_exception);
          /* UNREACHED */
          return NULL;          
     };
     if ( argc != method->_content.asMethod.arity ) {
          /* Signaler une erreur d'arité */
          snprintf(ILP_the_exception._content.asException.message,
                   ILP_EXCEPTION_BUFFER_LENGTH,
                   "Method %s arity error: %d instead of %d\nCulprit: 0x%p\n",
                   method->_content.asMethod.name, 
                   argc,
                   method->_content.asMethod.arity,
                   (void*) receiver);
          /*DEBUG*/
          fprintf(stderr, "%s", ILP_the_exception._content.asException.message);
          ILP_the_exception._content.asException.culprit[0] = receiver;
          ILP_the_exception._content.asException.culprit[1] = 
               (ILP_Object) method;
          ILP_the_exception._content.asException.culprit[2] = NULL;
          ILP_throw((ILP_Object) &ILP_the_exception);
          /* UNREACHED */
          return NULL;
     };
     {
          int index = method->_content.asMethod.index;
          return oclass->_content.asClass.method[index];
     }
}

#define DefineSuperMethodCaller(i) \
ILP_Object \
ILP_find_and_call_super_method##i ( \
     ILP_Object self, \
     ILP_Method current_method, \
     ILP_general_function super_method, \
     ILP_Object arguments[1] ) \
{ \
     /* assert( super_method != NULL ); */ \
     switch ( i ) { \
          case 0: { \
               return (*super_method)(self); \
          } \
          case 1: { \
               return (*super_method)(self, arguments[1]); \
          } \
          case 2: { \
               return (*super_method)(self, arguments[1], arguments[2]); \
          } \
          case 3: { \
               return (*super_method)(self, arguments[1],  \
                                            arguments[2],  \
                                            arguments[3]); \
          } \
          default: { \
               snprintf(ILP_the_exception._content.asException.message, \
                        ILP_EXCEPTION_BUFFER_LENGTH, \
                        "Cannot invoke supermethod %s\nCulprit: 0x%p\n", \
                        current_method->_content.asMethod.name,  \
                        (void*) self ); \
               /*DEBUG*/ \
               fprintf(stderr, "%s", ILP_the_exception._content.asException.message); \
               ILP_the_exception._content.asException.culprit[0] = self; \
               ILP_the_exception._content.asException.culprit[1] =  \
                    (ILP_Object) current_method; \
               ILP_the_exception._content.asException.culprit[2] = NULL; \
               ILP_throw((ILP_Object) &ILP_the_exception); \
               /* UNREACHED */ \
               return NULL; \
          } \
     } \
}

DefineSuperMethodCaller(0)
DefineSuperMethodCaller(1)
DefineSuperMethodCaller(2)
DefineSuperMethodCaller(3)

ILP_Object 
ILP_dont_call_super_method (
     ILP_Object self,
     ILP_Method current_method,
     ILP_general_function super_method,
     ILP_Object arguments[1] )
{
     /* assert ( super_method == NULL ); */
     snprintf(ILP_the_exception._content.asException.message,
              ILP_EXCEPTION_BUFFER_LENGTH,
              "No supermethod %s\nCulprit: 0x%p\n",
              current_method->_content.asMethod.name, 
              (void*) self );
     /*DEBUG*/
     fprintf(stderr, "%s", ILP_the_exception._content.asException.message);
     ILP_the_exception._content.asException.culprit[0] = self;
     ILP_the_exception._content.asException.culprit[1] = 
          (ILP_Object) current_method;
     ILP_the_exception._content.asException.culprit[2] = NULL;
     ILP_throw((ILP_Object) &ILP_the_exception);
     /* UNREACHED */
     return NULL;
}

/** Allocateurs. ILP_malloc est paramétré par l'allocateur de bas
 * niveau employé, par défaut: malloc() mais ce peut être GC_malloc() 
 * Cf. ilpObj.h pour plus de détails sur l'emploi d'un GC. */

ILP_Object
ILP_malloc (int size, ILP_Class class)
{
     ILP_Object result = ILP_MALLOC(size);
     if ( result == NULL ) {
          return ILP_die("Memory exhaustion");
     };
     result->_class = class;
     return result;
}

ILP_Object
ILP_make_instance (ILP_Class class) 
{
     int size = sizeof(ILP_Class);
     size += sizeof(ILP_Object) * class->_content.asClass.fields_count;
     return ILP_malloc(size, class);
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

static ILP_Object
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

/** Opérateurs unaires. */

ILP_Object
ILP_make_opposite (ILP_Object o)
{
     ILP_CheckIfInteger(o);
     {
          ILP_Object result = ILP_AllocateInteger();
          result->_content.asInteger = (- o->_content.asInteger);
          return result;
     }
}

ILP_Object
ILP_make_negation (ILP_Object o)
{
     ILP_CheckIfBoolean(o);
     {
          if ( ILP_isTrue(o) ) {
               return ILP_FALSE;
          } else {
               return ILP_TRUE;
          }
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
               result->_content.asInteger =
                    o1->_content.asInteger + o2->_content.asInteger;
               return result;
          } else if ( ILP_isFloat(o2) ) {
               ILP_Object result = ILP_AllocateFloat();
               result->_content.asFloat =
                    o1->_content.asInteger + o2->_content.asFloat;
               return result;
          } else {
               return ILP_domain_error("Not a number", o2);
          }
     } else if ( ILP_isFloat(o1) ) {
          if ( ILP_isInteger(o2) ) {
               ILP_Object result = ILP_AllocateFloat();
               result->_content.asFloat =
                    o1->_content.asFloat + o2->_content.asInteger;
               return result;
          } else if ( ILP_isFloat(o2) ) {
               ILP_Object result = ILP_AllocateFloat();
               result->_content.asFloat =
                    o1->_content.asFloat + o2->_content.asFloat;
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

/** Imprimer le contenu d'une instance. */

void
ILP_print_fields (ILP_Object o,
                  ILP_Field last)
{
     if ( last == NULL ) {
          return;
     };
     ILP_print_fields(o, last->_content.asField.previous_field);
     fprintf(stdout, ":%s=", last->_content.asField.name);
     ILP_print(o->_content.asInstance.field[last->_content.asField.offset]);
}

/** Cette fonction peut être utilisée comme une méthode. Elle imprime
 * le receveur sur le flux de sortie. */

ILP_Object
ILP_print (ILP_Object self)
{
     if ( self->_class == &ILP_object_Integer_class ) {
          fprintf(stdout, "%d", self->_content.asInteger);
     } else if ( self->_class == &ILP_object_Float_class ) {
          fprintf(stdout, "%12.5g", self->_content.asFloat);
     } else if ( self->_class == &ILP_object_Boolean_class ) {
          fprintf(stdout, "%s", (ILP_isTrue(self) ? "true" : "false"));
     } else if ( self->_class == &ILP_object_String_class ) {
          fprintf(stdout, "%s", self->_content.asString.asCharacter);
     } else if ( self->_class == &ILP_object_Class_class ) {
          fprintf(stdout, "<Class:%s>", self->_content.asClass.name);
     } else if ( self->_class == &ILP_object_Method_class ) {
          fprintf(stdout, "<Method:%s>", self->_content.asMethod.name);
     } else if ( self->_class == &ILP_object_Field_class ) {
          fprintf(stdout, "<Field:%s>", self->_content.asField.name);
     } else {
          fprintf(stdout, "<%s", self->_class->_content.asClass.name);
          ILP_print_fields(self, self->_class->_content.asClass.last_field);
          fprintf(stdout, ">");
     }
     return ILP_FALSE;
}

/** Cette fonction renvoie la classe du receveur. La classe est
 * également un objet obéissant au modèle ObjVlisp. */

ILP_Object
ILP_classOf (ILP_Object self) 
{
     return (ILP_Object) (self->_class);
}

/* end of ilpObj.c */
