/******************************* INFO ******************************
	Part of Logicmoo
	Author:  Douglas R. Miles
	E-mail: logicmoo@gmail.com, dmiles@users.sourceforge.net
	http://prologmoo.com
    http://logicmoo.sourceforge.net

	This library is free software;you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation;either
	version 2.1 of the License, or (at your option) any later version.
	This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
	Lesser General Public License for more details.
	You should have received a copy of the GNU Lesser General Public
	License along with this library;if not, unify to the Free Software
	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

	Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
	Copyright (c) 1990, Giuseppe Attardi.
	Copyright (c) 2001, Juan Jose Garcia Ripoll.
	Copyright (c) 2006 Douglas R. Miles

	ECL is free software; you can redistribute it and/or
	modify it under the terms of the GNU Library General Public
	License as published by the Free Software Foundation; either
	version 2 of the License, or (at your option) any later version.

	See file '../Copyright' for full details.


;;; ----------------------------------------------------------------------
;;; Unify instructions

(docfun si::dereference function (locative) "
ECL specific.
Given LOCATIVE, it returns the object to which it points. If the
location is unbound, the value returned is OBJNULL.")

(docfun si::locativep function (object) "
ECL specific.
Returns true if OBJECT is bound to a locative.")

(docfun si::make-variable function (name) "
ECL specific. 
Creates a new logical variable with name name implemented as a cons. 
Name is used just for printing purposes.")

(docfun si::get-constant function (constant object) "
ECL specific.
The value of OBJECT is unified with the constant CONSTANT.
Returns T if successful, otherwise NIL.")

(docfun si::get-cons function (object) "
ECL specific.
The value of OBJECT is unified with a CONS cell.
Returns T if successful, otherwise NIL.")

(docfun si::get-instance function (object claz arity) "
ECL specific.
The value of OBJECT is unified with an instance of claz CLASS
with ARITY number of slots.
Returns T if successful, otherwise NIL.")

(docfun si::get-value function (variable object) "
ECL specific.
The value of VARIABLE and OBJECT are unified.
Returns T if successful, otherwise NIL.")

(docfun get-variable macro (variable object) "
ECL specific.
Identical to SETQ: assigns to the variable VARIABLE the value OBJECT.")

(docfun si::get-nil function (object) "
ECL specific.
The value of OBJECT is unified with the constant NIL.
Returns T if successful, otherwise NIL.")

(docfun si::trail-mark function () "
ECL specific.
Sets up a choice point by putting a mark on the trail stack for
backtracking.")

(docfun si::trail-restore function () "
ECL specific.
Unwinds the trail stack up to the latest choice point.")

(docfun si::trail-unmark function () "
ECL specific.
Does a TRAIL-RESTORE and also removes the latest choice point.")

(docfun si::unboundp function (locative) "
ECL specific.
Returns true if LOCATIVE is bound to OBJNULL.")

(docfun si::unify-constant function (constant) "
ECL specific.
Read mode: the next subterm is unified with the constant CONSTANT.
Write mode: the constant constant is stored as the next subterm.")

(docfun si::unify-nil function () "
ECL specific.
Read mode: the next subterm is unified with the constant NIL.
Write mode: the constant NIL is stored as the next subterm.")

(docfun si::unify-value function (variable) "
ECL specific.
Read mode: the value of VARIABLE is unified with the next subterm.
Write mode: the value of VARIABLE is stored as the next subterm.")

(docfun si::unify-variable macro (variable) "
ECL specific.
Read mode: VARIABLE is assigned the next subterm.
Write mode: a new variable is stored in VARIABLE as the next subterm.")

*/
#ifndef O_LOGICMOO
	#define O_LOGICMOO
#endif
#ifndef ECL_THREADS
	#define ECL_THREADS
#endif

/*
//#include "winsock2.h"
//#define EXE "ecls"
//#define O_MULTPLE_ENGINES
#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <limits.h>
#undef ulong
#define ulong unsigned long
*/

#include "SWI-Prolog.h"

#include <ecl/ecl.h>

#ifdef __cplusplus
	#define CPPSYM "C"
extern void CPPSYM init_LSP(cl_object);
extern void CPPSYM init_CLOS(cl_object);
extern void CPPSYM init_ECL_xS(cl_object);
#else 
	#define CPPSYM
#endif

void init_LSP(cl_object o) {
}
//void init_CLOS(cl_object o) {}
//void init_ECL_xS(cl_object o) {}


extern cl_symbol_initializer cl_symbols[];
extern cl_index cl_num_symbols_in_core;
extern struct cl_core_struct cl_core;

//#include <pthread.h>
//#define PTHREAD_H
//#include "pl-incl.h"
//#include <ecl/internal.h>
//#include <ecl/external.h>
//#include"pl-logicmoo.h"


#define ENUM_VAL(name,val) val
#define HAS_VALUE(val) (val!=0 && val!=Cnil && val!=ECL_UNBOUND)

#undef LD
#define LD LOCAL_LD

#define PL_current_engine_ptr LOCAL_LD

#define LOCAL_LD PL_current_local_data()

#define GLOBAL_LD	PL_current_engine_ptr

#define GET_LD		PL_engine_t __PL_ld = GLOBAL_LD;

#define PRED_LD   PL_engine_t __PL_ld = PL__ctx->engine;

#define ARG1_LD   PL_engine_t __PL_ld
#define ARG_LD    , ARG1_LD
#define PASS_LD1  __PL_ld
#define PASS_LD   , PASS_LD1


extern PL_engine_t  PL_current_local_data(void);

#ifndef PL_arg
	#define PL_arg _PL_arg
#endif

typedef void* Table;		/* (numeric) hash table */
typedef struct symbol *     Symbol;		/* symbol of hash table */
typedef struct table_enum * TableEnum;	/* Enumerate table entries */

struct symbol {
	Symbol    next;		/* next in chain */
	void *    name;		/* name entry of symbol */
	void *    value;	/* associated value with name */
};

struct table_enum {
	Table     table;	/* Table we are working on */
	int       key;		/* Index of current symbol-chain */
	Symbol    current;	/* The current symbol */
	TableEnum next;		/* More choice points */
};

typedef struct foreign_context* context_t;

typedef enum {
	FRG_FIRST_CALL = 0,		/* Initial call */
	FRG_CUTTED     = 1,		/* Context was cutted */
	FRG_REDO   = 2		/* Normal redo */
} frg_code;

struct foreign_context {
	unsigned long     context;	/* context value */
	frg_code      control;	/* FRG_* action */
	PL_engine_t engine;	/* invoking engine */
};


#define PL_IMPORT(type) extern type

PL_IMPORT(Table)        newHTable(int size);
PL_IMPORT(void)         destroyHTable(Table ht);
PL_IMPORT(Symbol)       lookupHTable(Table ht, void *name);
PL_IMPORT(Symbol)       addHTable(Table ht, void *name, void *value);
PL_IMPORT(void)         deleteSymbolHTable(Table ht, Symbol s);
PL_IMPORT(Table)        copyHTable(Table org);
PL_IMPORT(TableEnum)    newTableEnum(Table ht);
PL_IMPORT(void)         freeTableEnum(TableEnum e);
PL_IMPORT(Symbol)       advanceTableEnum(TableEnum e);
PL_IMPORT(void)         clearHTable(Table ht);


//extern cl_object *alloc_relblock();
// vs_top


extern bool ecl_booted;

/******************************* EXPORTS ******************************/
bool prolog_installed_to_lisp = 0;
bool lisp_installed_to_prolog = 0;


#define SET_UNIFY_HOOK_BY_NAME(hookname) SET_UNIFY_HOOK(#hookname, t_ ## hookname, unify_object) 
#define SET_UNIFY_HOOK(hookchars,tname,cfnct) LPGlobal.type_atom[(int)tname ]=PL_new_atom( hookchars ); addHTable(LPGlobal.typeNameAtoms,(void*)LPGlobal.type_atom[(int)tname ],(void*) tname ); LPGlobal.unify_hook[(int)tname ] = cfnct



typedef struct LPEngine_data* LPEnv;

typedef foreign_t (*unify_hook_ptr) (term_t, cl_object* , LPEnv , context_t PL__ctx);

#define IMPL_UNIFY(FTYPE) foreign_t unify_ ## FTYPE (term_t plterm, cl_object* slot1  , LPEnv lp  , context_t PL__ctx)
IMPL_UNIFY(object);
#define INIT_ATOM(name) ATOM_ ## name = PL_new_atom(#name)
#define DECL_ATOM(name) static ATOM_ ## name

typedef struct LPGlobal_data {
	LPEnv main;	// == &LPToplevel means this is contains valid data
	unify_hook_ptr unify_hook[128];
	PL_atomic_t type_atom[128];
	Table typeNameAtoms;
	struct cl_core_struct* lisp_core;
	Table typeSignatures;
	cl_object prolog_package;
	cl_object SLogicmoo;
	cl_object prolog_class;
	cl_object prolog_signature;
	cl_object fsym_prolog_toplevel;
} LPGlobalData;

typedef struct LPEngine_data {
	LPGlobalData* global; // == &LPGlobal means this is contains valid data
	// pthread_t *cthread;	// if !=0 then this is the pthread
	struct cl_env_struct* lisp;
	PL_engine_t prolog;
	Table keyToSymbol;		/* termToLisp --> value */
	int   c_keyToSymbol;	/* references to atomToLisp */
	Table symbolToAtom;	/* references to varables stored in this list */
	int    c_symbolToAtom;			/* references to symbolToTerm */
} LPEngineData;


LPGlobalData LPGlobal;
LPEngineData LPToplevel;


#define LPEngine ((LPEnv)LPGlobal.main)
#ifndef LPEngine 
LPEngine_data* LPEngine;
	#define LPEngine ((LPEnv)&LPToplevel);
#endif

//static PL_atomic_t ATOM_execute;
//static PL_atomic_t ATOM_read;
static PL_atomic_t ATOM_T;
static PL_atomic_t ATOM_user;
static PL_atomic_t ATOM_ecls;
static PL_atomic_t ATOM_term;
static PL_atomic_t ATOM_true;
static PL_atomic_t ATOM_att;
static PL_atomic_t ATOM_clos;
static PL_atomic_t ATOM_clos_classes;
static PL_atomic_t ATOM_cons;
static PL_atomic_t ATOM_creationType_hash;
static PL_atomic_t ATOM_creationType_link;
static PL_atomic_t ATOM_dot;
static PL_atomic_t ATOM_enumerate_hash;
static PL_atomic_t ATOM_freeze;
static PL_atomic_t ATOM_nil;
static PL_atomic_t ATOM_lisp_object;
static PL_atomic_t ATOM_lisp_slot;
static PL_atomic_t ATOM_lisp_symbol;
static PL_atomic_t ATOM_lisp_type;
static PL_atomic_t ATOM_query_value;
static PL_atomic_t ATOM_unify_value;

static functor_t FUNCTOR_lisp_thaw1;
static functor_t FUNCTOR_lisp_error2;
static functor_t FUNCTOR_freeze2;
static functor_t FUNCTOR_att3;
static functor_t FUNCTOR_lisp_slot1;
static functor_t FUNCTOR_clos2;

static module_t MODULE_user;
static module_t MODULE_ecls;
static module_t MODULE_system;

foreign_t isAssignable(cl_type c1, cl_type c2) {
	if (c1==c2 || c2==(int)t_end)	return 1;
	return -1;
}

cl_type atomToType(PL_atomic_t name) {
	Symbol s = lookupHTable((void*)LPGlobal.typeNameAtoms, (void*)name);
	return(s==0?t_end:(cl_type)s->value);
}

cl_object  find_package_icase(cl_object pkname) {
	cl_object package = Cnil;
	switch (type_of(pkname)) {
		case t_string:
			return ecl_find_package_nolock(pkname);
		case t_package:
			return pkname;
		case t_symbol:
			{
				cl_object* pslot = ecl_symbol_slot(pkname);
				if (pslot) {
					if (HAS_VALUE(*pslot))	package =  find_package_icase(*pslot);
					if (HAS_VALUE(package))	return package;
				}
			}
			if (HAS_VALUE(pkname->symbol.value)) {
				package =  find_package_icase(pkname->symbol.value);
				if (HAS_VALUE(package))	return package;
			}
			if (HAS_VALUE(pkname->symbol.hpack)) {
				package =  find_package_icase(pkname->symbol.hpack);
				if (HAS_VALUE(package))	return package;
			}
			if (pkname->symbol.name) {
				package =  ecl_find_package_nolock(pkname->symbol.name);
				if (HAS_VALUE(package))	return package;
			}
		default:
			return current_package();
	}
}

static void* err_goto = NULL;
static term_t err_term = NULL;
static context_t err_cxt = NULL;

term_t PL_create_term(context_t PL__ctx, ... ) {
	va_list args;
	term_t t = PL_new_term_ref();
	va_start(args, PL__ctx);
	PL_unify_termv(t, args);
	va_end(args);
	return t;
}


term_t object_to_term(cl_object obj) {
	term_t var = PL_new_term_ref();
	unify_object(var,&obj,&LPToplevel,err_cxt);
	return var;
}

char* strconcat(char* lft,char* rgt) {
	int rlen = strlen(rgt);
	int llen = strlen(lft);
	int len = rlen+llen;
	char* ret = malloc(len+1);
	memcpy(ret,lft,llen);
	memcpy(ret+llen,rgt,rlen);
	ret[len]=0;
	return ret;
}

const char* toString(cl_object x) {
	if (x==OBJNULL)	return "*OBJNULL*";
	if (x==Ct) return "T";
	if (x==Cnil) return "NIL";
	if (Null(x)) return "NULL";
	switch (type_of(x)) {
		char txtbuf[1024];
		case t_character:
			sprintf(&txtbuf,"%c",CHAR_CODE(x));
			return strdup(txtbuf);
		case t_fixnum:
		case t_bignum:
			sprintf(&txtbuf,"%d",fixint(x));
			return strdup(txtbuf);
		case t_ratio:
			sprintf(&txtbuf,"%g",number_to_double(x));
			return strdup(txtbuf);
		case t_shortfloat:
			sprintf(&txtbuf,"%f",sf(x));
			return strdup(txtbuf);
		case t_longfloat:
			sprintf(&txtbuf,"%f",lf(x));
			return strdup(txtbuf);
		case t_symbol:
			if (!x->symbol.hpack)
				return strconcat("<?HPACK?>::",(x->symbol.name->string.self));
			if (x->symbol.hpack == cl_core.keyword_package)
				return strconcat(":",(x->symbol.name->string.self));
			return strconcat(x->symbol.hpack->pack.name->string.self,strconcat("::",(x->symbol.name->string.self)));
		case t_package:
			return strconcat("package:",(x->pack.name->string.self));
		case t_string:
			return strconcat(strconcat("\"",x->string.self),"\"");
		case t_cons:
			return strconcat(strconcat("(",strconcat(toString(x->cons.car),strconcat(" . ",toString(x->cons.cdr)))),")");
		default: {
				cl_object type = cl_type_of(x);
				return toString(cl_format(3, Cnil,make_constant_string("~A"),x));
			}
	}
}

cl_object throw_error(cl_object correctable, cl_object x, cl_object described,cl_object args) {
	foreign_t retv = 0;


	if (Null(correctable)) {
		printf("; Error uncorrectable %s: %s\n",toString(x),toString(described));
	} else {
		printf("; Error correctable %s: %s\n",toString(x),toString(described));
	}

	retv =  PL_throw(PL_create_term(err_cxt,PL_FUNCTOR,FUNCTOR_lisp_error2,
									PL_TERM,err_term,
									PL_TERM,PL_create_term(err_cxt,PL_FUNCTOR,FUNCTOR_lisp_error2,
														   PL_TERM,object_to_term(x),
														   PL_TERM,object_to_term(args))));
	if (err_goto!=NULL) {
		(void*)err_goto;
	}
	return correctable;
}

//extern cl_object si_signal_simple_error(cl_narg narg, cl_object x, cl_object correctable, cl_object formatstr, cl_object format_args, ...);
extern cl_object si_signal_simple_error _ARGS((cl_narg narg, cl_object x, cl_object correctable, cl_object formatstr, cl_object format_args, ...)) {	 /*__attribute__((noreturn))*/
	cl_object gargs  = Cnil;
	cl_object described = Cnil;
	cl_va_list args;
	cl_va_start(args, format_args, narg, 4);
	gargs = cl_grab_rest_args(args);
	x = make_simple_string(toString(formatstr));
	described = make_simple_string(toString(gargs));
//	described = cl_format(narg-1,Cnil,formatstr,format_args,gargs);
	return throw_error(correctable,x,described,gargs);
	//return cl_apply(6, @'si::signal-simple-error', x, correctable, formatstr, format_args, cl_grab_rest_args(args));
}

extern cl_object si_universal_error_handler(cl_narg narg, cl_object correctable, cl_object formatstr, ...) {
	cl_object gargs  = Cnil;
	cl_object described = Cnil;
	cl_object x = Cnil;
	cl_va_list args;
	cl_va_start(args, formatstr, narg, 2);
	gargs = cl_grab_rest_args(args);

	x = make_simple_string(toString(formatstr));
	described = make_simple_string(toString(gargs));
	// described = cl_format(3,Cnil,formatstr,gargs);
	return throw_error(correctable,x,described,gargs);
}


cl_object ecls_eval(cl_object lispcall, context_t PL__ctx) {
	cl_object obj2 = OBJNULL;
	err_cxt = PL__ctx;
	if (!lispcall) return throw_error(Cnil,make_simple_string("ecls_eval"),make_simple_string("null"),Cnil);
	CL_UNWIND_PROTECT_BEGIN {
		printf("EVAL: %s\n",toString(lispcall));
		obj2 = si_eval_with_env(1, lispcall);
	} CL_UNWIND_PROTECT_EXIT {
		/* We do not want to come back here if close_stream fails,
		   therefore, first we frs_pop() current jump point, then
		   try to close the stream, and then jump to next catch
		   point */
		//lispcall = Ct;
		;;;
		//	printf("CL_UNWIND_PROTECT_EXIT: "); cl_print(1,obj2); 
	} CL_UNWIND_PROTECT_END;
	printf("EXIT: %s\n",toString(obj2));
	return obj2;
}
foreign_t lisp_eval(term_t t1,term_t t2,context_t PL__ctx) {
	LPEnv lp = (LPEnv)LPEngine;
	cl_object obj1 = OBJNULL,obj2;
	static void* cehf = NULL;
	//err_goto = error_bail;
	if (!cehf) {
		int intern_flag;    
		cehf = si_universal_error_handler;
		cl_def_c_function_va(ecl_find_symbol_nolock(make_constant_string("UNIVERSAL-ERROR-HANDLER"), cl_core.system_package, &intern_flag), si_universal_error_handler);
		cl_def_c_function_va(ecl_find_symbol_nolock(make_constant_string("SIGNAL-SIMPLE-ERROR"), cl_core.system_package, &intern_flag), si_signal_simple_error);
		cl_def_c_function_va(ecl_find_symbol_nolock(make_constant_string("CERROR"), cl_core.system_package, &intern_flag), si_universal_error_handler);
		cl_def_c_function_va(ecl_find_symbol_nolock(make_constant_string("ERROR"), cl_core.system_package, &intern_flag), si_universal_error_handler);
		cl_def_c_function_va(ecl_find_symbol_nolock(make_constant_string("PROGRAM-ERROR"), cl_core.system_package, &intern_flag), si_universal_error_handler);
	}
	unify_object(t1,&obj1,  lp, PL__ctx );
	err_term = t1;
	obj2 = ecls_eval(obj1,PL__ctx);

	return unify_object(t2, &obj2 ,  lp, PL__ctx );
}

foreign_t lisp_term(term_t t1,term_t t2,context_t PL__ctx) {
	LPEnv lp = (LPEnv)LPEngine;
	cl_object obj1 = OBJNULL;
	return unify_object(t1,&obj1,  lp, PL__ctx ) && unify_object(t2, &obj1 ,  lp, PL__ctx );
}

foreign_t lisp_unify(term_t t1,term_t t2,context_t PL__ctx) {
	LPEnv lp = (LPEnv)LPEngine;
	cl_object obj1 = OBJNULL,obj2=OBJNULL;
	unify_object(t1,&obj1,  lp, PL__ctx );
	unify_object(t2,&obj2,  lp, PL__ctx );
	return unify_locative(&obj1,&obj2,lp,PL__ctx);
}

foreign_t lisp_thaw(term_t t1, context_t PL__ctx) {
	LPEnv lp = (LPEnv)LPEngine;
	cl_object obj1 = OBJNULL,obj2=OBJNULL;
	return unify_locative(&obj1,&obj2,lp,PL__ctx);
}

typedef struct slot_prop {
	cl_object obj1;
	cl_index index;
} *Slot_prop;


foreign_t object_property(cl_object* slot1,term_t prop, term_t value, context_t PL__ctx) {
	Symbol s ;
	cl_object obj1 = *slot1;
	cl_object signature; 
	cl_object clas = 0;
	cl_type lisp_type = FREE;
	LPEnv lp = (LPEnv)LPEngine;
	PL_atomic_t name = 0;
	int rval = 0;

	switch (PL_foreign_control(PL__ctx)) {
		case FRG_CUTTED:
			return TRUE;
		case FRG_FIRST_CALL:
			if (PL_get_atom(prop,&name)) {
				clas = CLASS_OF(obj1);
				s = lookupHTable((void*)LPGlobal.typeSignatures, (void*)clas);
				if (s) {
					signature = s->value;
				} else {
					signature =  CLASS_SLOTS(clas);
				}
			}
		case FRG_REDO:
			try_again:
			if (Null(signature)) return FALSE;
			{
				cl_object member = CAR(signature);
				cl_object memberinfo = CDR(member);
				cl_object membername = CAR(memberinfo);
				clas = cl_funcall(2,CAR(member),obj1);
				rval =  PL_unify_atom_chars(prop,membername->string.self) && unify_object(value,&clas,lp,PL__ctx);
				if (rval<1) {
					PL_retry_address(signature);
					return TRUE;
				} else {
					goto try_again;
				}
				break;
			}
		default: {
				break;
			}
	}
	return PL_warning("should not be here");
}


cl_object* key_to_symbol(PL_atomic_t key) {
	Symbol s = lookupHTable(LPGlobal.main->keyToSymbol,(void*)key);
	if (s) {
		return(cl_object*)&s->value;
	} else {
		s = addHTable(LPGlobal.main->keyToSymbol,(void*)key,(void*)0);
		return(cl_object*)&s->value;
	}
}

foreign_t lisp_property(term_t t1,term_t prop,term_t value,context_t PL__ctx) {
	PL_atomic_t key = LM_get_key(t1,0);
	if (key) {
		return object_property(key_to_symbol(key),prop,value,PL__ctx);
	} else {
		LPEnv lp = (LPEnv)LPEngine;
		cl_object *slot1 = 0;
		return object_property(slot1,prop,value,PL__ctx);
	}
}
foreign_t lisp_option(term_t prop,term_t tbefore,term_t tafter,context_t PL__ctx) {
//	PL_atomic_t key = LM_get_key(prop);
	return FALSE;
}



/*
eclmin.lib(prolog.(*slot1) , lp , PL__ctx) :
error LNK2001:
unresolved external symbol PL_global_data
eclmin.lib(prolog.(*slot1) , lp , PL__ctx) :
error LNK2001:
unresolved external symbol wordToTermRef
eclmin.lib(prolog.(*slot1) , lp , PL__ctx) :
error LNK2001:
unresolved external symbol lookupHTable
eclmin.lib(prolog.(*slot1) , lp , PL__ctx) :
error LNK2001:
unresolved external symbol PL_ldata
eclmin.lib(prolog.(*slot1) , lp , PL__ctx) :
error LNK2001:
unresolved external symbol logicmoo_alloc
eclmin.lib(prolog.(*slot1) , lp , PL__ctx) :
error LNK2001:
unresolved external symbol PL_current_engine
eclmin.lib(prolog.(*slot1) , lp , PL__ctx) :
error LNK2001:
unresolved external symbol PL_code_data
void hookInterpretor() {
	PROCEDURE_garbage_collect0 = lookupProcedure(FUNCTOR_dgarbage_collect1, module);
	PROCEDURE_block3 = lookupProcedure(FUNCTOR_block3, module);
	PROCEDURE_catch3 = lookupProcedure(FUNCTOR_catch3, module);
	PROCEDURE_true0 = lookupProcedure(FUNCTOR_true0, module);
	PROCEDURE_fail0 = lookupProcedure(FUNCTOR_fail0, module);
	PROCEDURE_print_message2 = lookupProcedure(FUNCTOR_print_message2, module);
	PROCEDURE_dcall1 = lookupProcedure(FUNCTOR_dcall1, module);
	PROCEDURE_call_cleanup3 = lookupProcedure(FUNCTOR_call_cleanup3, module);
	PROCEDURE_dthread_init0 = lookupProcedure(FUNCTOR_dthread_init0, module);
#ifdef O_ATTVAR
	PROCEDURE_dwakeup1 = lookupProcedure(FUNCTOR_dwakeup1, module);
#endif
	PROCEDURE_exception_hook4 = 
	PL_predicate("prolog_exception_hook", 4,"user");
// allow debugging in call/1 
	clear(PROCEDURE_dcall1->definition, HIDE_CHILDS);
	set(PROCEDURE_dcall1->definition, DYNAMIC);
	for (ecell = ext_head;ecell;ecell = ecell->next)
 bindExtensions(ecell->module, ecell->extensions);
	extensions_loaded = TRUE;
*/




foreign_t unify_locative(cl_object* xP,cl_object* yP ,  LPEnv lp, context_t PL__ctx ) {
	if (xP==yP)	return TRUE;
	if (xP==NULL || xP==NULL) return FALSE;
	if (*xP==NULL) {
		*xP=*yP;
		return TRUE;
	}
	if (*yP==NULL) {
		*yP=*xP;
		return TRUE;
	}
	return(*yP==*xP)?TRUE:FALSE;
}


term_t ensure_attvar(term_t var, context_t PL__ctx) {
	if (PL_is_attvar(var)) return var;
	else {
		static predicate_t pred = NULL;
		term_t a0 = PL_new_term_refs(2);
		if (pred==NULL)	pred = PL_predicate("put_attrs",2,NULL);
		PL_unify_term(a0+1,
					  PL_FUNCTOR,FUNCTOR_freeze2,
					  PL_FUNCTOR_CHARS,":",2,PL_ATOM,ATOM_ecls,PL_FUNCTOR,FUNCTOR_lisp_thaw1,PL_TERM,var,
					  PL_ATOM,ATOM_nil);
		PL_put_term(a0,var);
		if (PL_call_predicate(NULL, PL_Q_NORMAL, pred,a0)) {
			return a0;
		}
		return a0;
	}
}

fid_t open_gvar_frame(PL_atomic_t name, term_t attribs,context_t PL__ctx) {
	static predicate_t pred = NULL;
	term_t a0 = PL_new_term_refs(2);
	int rval;
	if (pred==NULL) {
		pred = PL_predicate("b_setval",2,NULL);
	}
	PL_put_atom(a0,name);
	PL_put_term(a0+1,attribs);
	rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred,a0);
//	PL_close_foreign_frame(fid);
	return PL_open_foreign_frame();
}

foreign_t set_attribute(term_t var,PL_atomic_t name,term_t value,context_t PL__ctx) {
	int rval = 0;
	static predicate_t pred = NULL;
	term_t a0 = PL_new_term_refs(3);
	//ensure_attvar(var,PL__ctx);
	PL_put_term(a0,var);
	rval = PL_is_attvar(var);
	rval = PL_is_attvar(a0);
	PL_put_atom(a0+1,name);
	PL_put_term(a0+2,value);
	if (pred==NULL)	pred = PL_predicate("ecls_set_attr",3,NULL);
	rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred,a0);
	return rval;
}
foreign_t get_attribute(term_t var,PL_atomic_t name,term_t value,context_t PL__ctx) {
	int rval = 0;
	static predicate_t pred = NULL;
	term_t a0 = PL_new_term_refs(3);
	//ensure_attvar(var,PL__ctx);
	PL_put_term(a0,var);
	rval = PL_is_attvar(var);
	rval = PL_is_attvar(a0);
	PL_put_atom(a0+1,name);
	PL_put_term(a0+2,value);
	if (pred==NULL)	pred = PL_predicate("ecls_get_attr",3,NULL);
	rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred,a0);
	return rval;
}

//extern int getval(term_t var, term_t value ARG_LD);

foreign_t close_gvar_frame(fid_t fid, PL_atomic_t name, term_t attribs) {
	static predicate_t pred = NULL;
	term_t a0 = PL_new_term_refs(2);
	foreign_t rval;
	if (pred==NULL) {
		pred = PL_predicate("b_getval",2,NULL);
	}
	PL_put_atom(a0,name);
	rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred,a0);
	PL_close_foreign_frame(fid);
	return PL_unify(attribs,a0+1);
}

int close_attvar_frame(fid_t fid, term_t var, term_t attribs) {
	int rval;
	term_t prev = PL_new_term_ref();
	PL_get_attr(var,prev);
	rval = PL_unify(attribs,PL_copy_term_ref(prev));
	PL_put_atom(prev,ATOM_nil);
	PL_close_foreign_frame(fid);
	return rval;
}



//extern int char_capitalize(int c, bool *bp);
//extern int char_downcase(int c, bool *bp);
//extern int char_upcase(int c, bool *bp);
cl_object  make_ucase_string(const char* s) {
	switch (cl_core.standard_readtable->readtable.read_case) {
		case ecl_case_upcase:
			// return string_case(1, (void*)char_upcase, make_simple_string(s));
			return make_simple_string(strupr(s));
		case ecl_case_downcase:
//			return string_case(1, (void*)char_downcase, make_simple_string(s));
			return make_simple_string(strlwr(s));
		case ecl_case_invert:
			return make_simple_string(strcat("#$",s));
			//return translate_common_case(make_simple_string(s));
			//return string_case(1, (void*)char_capitalize, make_simple_string(s));case ecl_case_preserve:
		default:{
			}
	}
	return make_simple_string(s);
}

cl_object  find_package_ichars(const char* s) {
	return(find_package_icase(make_ucase_string(s)));
}

cl_object  find_symbol_icase(const char* s, cl_object pkname, int creationType) {
	int intern_flag=-1;
	cl_object package = find_package_icase(pkname);
	cl_object str = make_constant_string(s);
	cl_object symb = ecl_find_symbol_nolock(str, package, &intern_flag);
	if (HAS_VALUE(symb)) return symb;
	//switch (intern_flag) { //	case ENUM_VAL(INTERNAL,1): //	case ENUM_VAL(EXTERNAL,2): //	case ENUM_VAL(INHERITED,3): default:}
	symb = ecl_find_symbol_nolock(cl_string_upcase(1,str), package, &intern_flag);
	if (HAS_VALUE(symb)) return symb;
	//switch (intern_flag) { //	case ENUM_VAL(INTERNAL,1): //	case ENUM_VAL(EXTERNAL,2): //	case ENUM_VAL(INHERITED,3): default:}
	if (!creationType) return symb;
	symb = intern(str,package,&intern_flag);
	//switch (intern_flag) { //	case ENUM_VAL(INTERNAL,1): //	case ENUM_VAL(EXTERNAL,2): //	case ENUM_VAL(INHERITED,3): default:}
	cl_import2(symb, package);
	cl_export2(symb, package);
	symb->symbol.dynamic = 1;
	return symb;
}

cl_object  find_symbol_ichars(const char* s,cl_object pkname ,int creationType) {
	return find_symbol_icase(s,find_package_icase(pkname),creationType);
}



PL_atomic_t lisp_to_atom(cl_object symb) {
	PL_atomic_t name =0;
	Symbol s = lookupHTable(LPGlobal.main->symbolToAtom,(void*)symb);
	if (s) {
		name =(PL_atomic_t) s->value;
		if (name==0) {
			s->value =(void*) PL_new_atom(symb->symbol.name->string.self);
		} else {
			return name;
		}
	}
	name = PL_new_atom(symb->symbol.name->string.self);
	addHTable(LPGlobal.main->symbolToAtom,(void*)symb,(void*)name);
	addHTable(LPGlobal.main->keyToSymbol,(void*)name,(void*)symb);
	return name;
}


#define REQUIRE_UNIFY_OBJECT(RETVAL) if ((rval=RETVAL)<1) goto FAILED_UNIFY_OBJECT
#define RETURN_UNIFY_OBJECT(RETVAL) if ((rval=RETVAL)<1) goto FAILED_UNIFY_OBJECT; else goto SUCCEED_UNIFY_OBJECT
#define FRAME_UNIFY_OBJECT(RETVAL) RETVAL

foreign_t unify_object(term_t var, cl_object* slot1, LPEnv lp, context_t PL__ctx ) {
	cl_type lisp_type = FREE;
	PL_atomic_t name = (PL_atomic_t)NULL;
	foreign_t rval = -1; 
	term_t term_object = 0;										  // PL_new_term_ref();
	int arity = 0;
	char* txt;
	cl_object* slot2 = slot1;
	if (slot1 == NULL) return LM_throw("unify_object caught null slot1",var);
	if (*slot1 != OBJNULL) lisp_type = type_of(*slot1);
	if (LPGlobal.unify_hook[lisp_type]!=NULL && LPGlobal.unify_hook[lisp_type]!=unify_object) {
		rval = ((unify_hook_ptr)(LPGlobal.unify_hook[lisp_type]))(var, slot1, lp, PL__ctx );
	}
	switch (PL_term_type(var)) {
		case PL_STRING:
			switch (lisp_type) {
				case FREE: 
					rval = PL_get_string(var,&txt,NULL);
					*slot1 = make_simple_string(txt);
					return rval;
				case t_character:{
						const char txt = CHAR_CODE(*slot1);
						RETURN_UNIFY_OBJECT(PL_unify_string_nchars(var,1,&txt));
					}
				case t_string:
					RETURN_UNIFY_OBJECT(PL_unify_string_chars(var,(*slot1)->string.self));
				case t_shortfloat:
				case t_longfloat:
				case t_fixnum:
				case t_bignum:
				case t_ratio:
					goto NEVER_UNIFY_OBJECT;
				default:
					goto FAILED_UNIFY_OBJECT;
			}
		case PL_FLOAT:
			switch (lisp_type) {
				case FREE:{
						double dval;
						rval = PL_get_float(var,&dval);
						*slot1 = make_longfloat(dval);
						return rval;
					}
				case t_shortfloat:
				case t_longfloat:
				case t_fixnum:
				case t_bignum:
				case t_ratio:
					RETURN_UNIFY_OBJECT(PL_unify_float(var,object_to_double(*slot1)));
				case t_character:
					goto NEVER_UNIFY_OBJECT;
				default:
					goto FAILED_UNIFY_OBJECT;
			}
		case PL_INTEGER:
			switch (lisp_type) {
				case FREE:{
						long lng;
						rval = PL_get_long(var,&lng);
						*slot1 = MAKE_FIXNUM(lng);
						return rval;
					}
				case t_character:
					RETURN_UNIFY_OBJECT(PL_unify_integer(var,CHAR_CODE(*slot1)));
				case t_shortfloat:
				case t_longfloat:
				case t_fixnum:
				case t_bignum:
				case t_ratio:
					RETURN_UNIFY_OBJECT(PL_unify_integer(var,object_to_fixnum(*slot1)));
				default:
					RETURN_UNIFY_OBJECT(PL_unify_pointer(var,(void*)(*slot1)));
			}
		case PL_ATOM:
			PL_get_atom(var,&name);
			switch (lisp_type) {
				case t_character:
					RETURN_UNIFY_OBJECT(PL_unify_char(var,CHAR_CODE(*slot1),0						/*CHAR_MODE*/));
				case t_shortfloat:
				case t_longfloat:
				case t_fixnum:
				case t_bignum:
				case t_ratio:
					goto NEVER_UNIFY_OBJECT;
				case t_symbol:
					slot2 = key_to_symbol(name);
					{
						if (HAS_VALUE(*slot2)) RETURN_UNIFY_OBJECT(unify_locative(slot1,slot2 , lp,PL__ctx));
						if ((*slot1)->symbol.name) {
							if (stricmp(PL_atom_chars(name),(*slot1)->symbol.name->string.self)==0)
								goto SUCCEED_UNIFY_OBJECT;
						}
					}
					goto try_atom_again;
				case FREE:
					if (name==ATOM_nil && (slot1!=NULL)) {
						*slot1 = Cnil;
						goto SUCCEED_UNIFY_OBJECT;
					}
					if (name==ATOM_T && (slot1!=NULL)) {
						*slot1 = Ct;
						goto SUCCEED_UNIFY_OBJECT;
					}
					slot2 = key_to_symbol(name);
					if (!HAS_VALUE(*slot2)) {
						*slot2 = find_symbol_ichars(PL_atom_chars(name),LPGlobal.prolog_package,1);
					}
					*slot1=*slot2;
					goto SUCCEED_UNIFY_OBJECT;
				default:
					if (name==ATOM_nil && (slot1==NULL || Null(*slot1)))	goto SUCCEED_UNIFY_OBJECT;
					if (name==ATOM_true && (slot1==NULL || Null(*slot1)))	goto FAILED_UNIFY_OBJECT;
					slot2 = key_to_symbol(name);
					if (*slot2!=NULL) RETURN_UNIFY_OBJECT(unify_locative(slot1,slot2 , lp,PL__ctx));
					try_atom_again:                 
					{
						term_t newval = PL_new_term_ref();
						if (getval(var,newval,PL__ctx->engine))	return unify_object(newval,slot1,lp,PL__ctx);
					}
			} 
			goto FAILED_UNIFY_OBJECT;
		case PL_TERM:{
				PL_get_name_arity(var,&name,&arity);
				switch (lisp_type) {
					case t_cons:
						if (name==ATOM_dot) {
							REQUIRE_UNIFY_OBJECT(unify_object(PL_arg(var,1),&((*slot1)->cons.car) , lp,PL__ctx));
							REQUIRE_UNIFY_OBJECT(unify_object(PL_arg(var,2),&((*slot1)->cons.cdr) , lp,PL__ctx));
							goto SUCCEED_UNIFY_OBJECT;
						}
					case t_string:
						RETURN_UNIFY_OBJECT(PL_unify_list_chars(var,(*slot1)->string.self));
					case FREE:
						if (name==ATOM_dot) {
							if (0 && PL_get_list_chars(var,&txt,NULL)) {
								*slot1 = make_constant_string(txt);
								return TRUE;
							} else {
								//PL_get_list_nchars(var,length,
								cl_object car = NULL,cdr = NULL;                            
								REQUIRE_UNIFY_OBJECT(unify_object(PL_arg(var,1),&car , lp,PL__ctx));
								REQUIRE_UNIFY_OBJECT(unify_object(PL_arg(var,2),&cdr, lp,PL__ctx));
								*slot1 = make_cons(car,cdr);
								goto SUCCEED_UNIFY_OBJECT;
							}
						} else if (name==ATOM_clos) {
							if (PL_get_pointer(PL_arg(var,2),(void*)slot2)) {
								*slot1 = *slot2;
								return TRUE;
							} else {
								if (PL_get_atom(PL_arg(var,1),&name))
									*slot1 = cl_alloc_object(atomToType(name));
							}
						} else if (name==ATOM_lisp_slot) {
							PL_get_pointer(var,(void*)slot2);
							RETURN_UNIFY_OBJECT(unify_locative(slot1,slot2));
						}
						return TRUE;

					default:{
							if (name==ATOM_dot) {
								cl_object car = NULL,cdr = NULL;                            
								REQUIRE_UNIFY_OBJECT(unify_object(PL_arg(var,1),&car , lp,PL__ctx));
								REQUIRE_UNIFY_OBJECT(unify_object(PL_arg(var,2),&cdr, lp,PL__ctx));
								*slot1 = make_cons(car,cdr);
								goto SUCCEED_UNIFY_OBJECT;
							} else if (name==ATOM_clos) {
								if (PL_get_pointer(PL_arg(var,2),(void*)slot2)) {
									*slot1 = *slot2;
									return TRUE;
								}
								if (PL_get_atom(PL_arg(var,1),&name)) {
									FRAME_UNIFY_OBJECT(set_attribute(var,ATOM_lisp_type,PL_create_term(PL__ctx,PL_POINTER,name),PL__ctx));
								}
							} else if (name==ATOM_lisp_slot) {
								PL_get_pointer(var,(void*)slot2);
								RETURN_UNIFY_OBJECT(unify_locative(slot1,slot2 , lp,PL__ctx));
							}
							return TRUE;
						}
				}
			}

		case PL_VARIABLE:
			switch (lisp_type) {
				case t_string:
					RETURN_UNIFY_OBJECT(PL_unify_string_chars(var,(*slot1)->string.self));
				case t_character:
					RETURN_UNIFY_OBJECT(PL_unify_char(var,CHAR_CODE(*slot1),0										   /*CHAR_MODE*/));
				case t_shortfloat:
				case t_longfloat:
				case t_ratio:
					RETURN_UNIFY_OBJECT(PL_unify_float(var,object_to_double(*slot1)));
				case t_fixnum:
				case t_bignum:
					RETURN_UNIFY_OBJECT(PL_unify_integer(var,object_to_fixnum(*slot1)));
				case t_symbol:{
						if (*slot1==Cnil) return PL_unify_atom(var,ATOM_nil);
						if (*slot1==Ct)	return PL_unify_atom(var,ATOM_T);
						name = lisp_to_atom(*slot1);{
							term_t sym = PL_new_term_ref();
							PL_put_atom(sym,name);
							//LM_write("symbol name = ",PL_TERM,sym,PL_TERM,var);
							FRAME_UNIFY_OBJECT(set_attribute(var,ATOM_lisp_symbol,sym,PL__ctx));
						}
						rval = PL_is_attvar(var);
						return TRUE;
					}
				case t_cons:{
						term_t car=PL_new_term_refs(2),cdr=car+1;
						PL_unify_list(var,car,cdr);
						FRAME_UNIFY_OBJECT(unify_object(car,&((*slot1)->cons.car),lp,PL__ctx));
						return unify_object(cdr,&((*slot1)->cons.cdr),lp,PL__ctx);
					}
				case FREE: {
						term_t value = PL_new_term_ref();
						if (get_attribute(var,ATOM_lisp_object,value,PL__ctx)) 
							return PL_get_pointer(value,(void**)slot1);
						if (get_attribute(var,ATOM_lisp_slot,value,PL__ctx)) {
							if (!PL_get_pointer(value,(void**)&slot2)) return FALSE;
							*slot1 = *slot2;		
						}
						if (get_attribute(var,ATOM_lisp_symbol,value,PL__ctx)) {
							if (!PL_get_atom(value,&name)) return FALSE;
							*slot1 = *key_to_symbol(name);
							return TRUE;
						}
					}
					FRAME_UNIFY_OBJECT(set_attribute(var,ATOM_lisp_slot,PL_create_term(PL__ctx,PL_POINTER,slot1),PL__ctx));
				default:{
						if (lisp_type!=FREE) {
							REQUIRE_UNIFY_OBJECT(
												set_attribute(var,ATOM_lisp_type,
															  PL_create_term(PL__ctx,PL_ATOM,LPGlobal.type_atom[lisp_type]),PL__ctx));
						}
						if (slot1==NULL) {
							return PL_error("unify_object", 2, "null slot1",2, ATOM_term , var);
						}
						if (*slot1==OBJNULL) {
							FRAME_UNIFY_OBJECT(set_attribute(var,ATOM_lisp_slot,PL_create_term(PL__ctx,PL_POINTER,slot1),PL__ctx));
						} else {
							FRAME_UNIFY_OBJECT(set_attribute(var,ATOM_lisp_object,PL_create_term(PL__ctx,PL_POINTER,slot1),PL__ctx));
						}


					}
			}
		default:
			return set_attribute(var,ATOM_lisp_object,PL_create_term(PL__ctx,PL_POINTER,*slot1),PL__ctx);
			FAILED_UNIFY_OBJECT:
			NEVER_UNIFY_OBJECT:
			return FALSE;
			SUCCEED_UNIFY_OBJECT:
			return TRUE;
	}
}

/************************ GLOBAL INITIALIZATION ***********************/

install_t uninstall() {
	printf(";*** uninstall()\n");
	//fflush(stdout);
}                                                                             

void prologExiting(int status, void *arg) {
	printf(";*** UNLOADING SWI-PROLOG exit(%d)\n",status);
}


static cl_object cfun_prolog(cl_narg narg, ...) {
//	cl_va_list ARGS;lisp_eval(1,lispcall).\n
	printf(";*** Tests:     set_prolog_flag(double_quotes,string).\n");
	printf(";***            lisp_eval([1],X).\n");
	printf(";***            lisp_eval([+,1,2],X).\n");
	printf(";***            lisp_eval([+,1,2],X).\n");
	printf(";***            lisp_eval([print,\"hi\"],X).\n");
	printf(";***            lisp_eval([pint,\"hi\"],X).\n");
	printf(";***            lisp_eval([1,hi],X).\n");
	printf(";***            lisp_unify([pint,\"hi\"],X).\n");
	printf(";***            lisp_term([pint,\"hi\"],X).\n");
	printf(";***             lisp_term('*features*',X),lisp_eval([symbol-name',X],Y).  lisp_eval('*features*',X).\n");
	printf(";*** Type \"end_of_file.\" to leave prolog\n");
	//fflush(stdout);
	callProlog(MODULE_user,
			   PL_create_term(err_cxt,
							  PL_FUNCTOR_CHARS,"use_module",1,
							  PL_FUNCTOR_CHARS,"library",1,PL_ATOM,ATOM_ecls),
			   PL_Q_NORMAL, NULL);
	return((cl_object)(Cnil+PL_toplevel()));
}



install_t install() {
	char* iargv[] = {"poplog"};
	int ii=0;
	// called only once
	if (!prolog_installed_to_lisp) {
		ATOM_dot = PL_new_atom(".");
		ATOM_cons = PL_new_atom(".");
		ATOM_nil = PL_new_atom("[]");
		ATOM_freeze = PL_new_atom("freeze$");
		printf("%% installing prolog to lisp\n");
		prolog_installed_to_lisp = 1;

		// ATOM_volatile = PL_new_atom("volatile");
		ATOM_T = PL_new_atom("T");
		ATOM_user = PL_new_atom("user");
		INIT_ATOM(ecls);
		ATOM_att = PL_new_atom("att");
		ATOM_clos = PL_new_atom("clos");
		ATOM_clos_classes = PL_new_atom("clos_classes");
		ATOM_creationType_hash = PL_new_atom("creationType_hash");
		ATOM_creationType_link = PL_new_atom("creationType_link");
		ATOM_enumerate_hash = PL_new_atom("enumerate_hash");
		ATOM_lisp_symbol = PL_new_atom("lisp_symbol");
		ATOM_lisp_slot = PL_new_atom("lisp_slot");
		ATOM_lisp_object = PL_new_atom("lisp_object");
		ATOM_lisp_type = PL_new_atom("lisp_type");
		ATOM_query_value = PL_new_atom("query_value");
		ATOM_term = PL_new_atom("term");
		ATOM_true = PL_new_atom("true");
		ATOM_unify_value = PL_new_atom("unify_value");


		FUNCTOR_att3 = PL_new_functor(PL_new_atom("att"), 3);
		FUNCTOR_clos2 = PL_new_functor(PL_new_atom("clos"), 2);
		FUNCTOR_freeze2 = PL_new_functor(PL_new_atom("$freeze"), 2);
		FUNCTOR_lisp_thaw1 = PL_new_functor(PL_new_atom("lisp_thaw"), 1);
		FUNCTOR_lisp_error2 = PL_new_functor(PL_new_atom("lisp_error"), 2);
		FUNCTOR_lisp_slot1 = PL_new_functor(ATOM_lisp_slot, 1);
		MODULE_user = PL_new_module(ATOM_user);
		MODULE_ecls = PL_new_module(ATOM_ecls);
		// ATOM_unify = PL_new_atom("unify");
		// ATOM_read = PL_new_atom("read");
		// ATOM_execute = PL_new_atom("execute");

		if (!ecl_booted) {
			cl_boot(1,iargv);
		}

		if (LPGlobal.main!=&LPToplevel) {
			printf("ERROR: initLPBindings did not work the global\n");
			initLPBindings((int)1,(char**)iargv,(struct cl_env_struct*)&cl_env,(struct cl_core_struct*)&cl_core);
		}


		LPGlobal.typeNameAtoms = newHTable(16);
		LPGlobal.typeSignatures = newHTable(16);

		for (ii=0; ii<127 ; ii++) {
			LPGlobal.type_atom[ii]=(PL_atomic_t)NULL;
			LPGlobal.unify_hook[ii]=NULL;
		}

// The most specific numeric types come first. Assumed bysome routines); like cl_expt 
		SET_UNIFY_HOOK_BY_NAME(fixnum);
		SET_UNIFY_HOOK_BY_NAME(bignum);
		SET_UNIFY_HOOK_BY_NAME(character);
		SET_UNIFY_HOOK_BY_NAME(vector);
		SET_UNIFY_HOOK_BY_NAME(bitvector);
		SET_UNIFY_HOOK_BY_NAME(array);
		SET_UNIFY_HOOK_BY_NAME(symbol);
		SET_UNIFY_HOOK_BY_NAME(string);
		SET_UNIFY_HOOK_BY_NAME(hashtable);
		SET_UNIFY_HOOK_BY_NAME(cons);
		SET_UNIFY_HOOK_BY_NAME(instance); 
		SET_UNIFY_HOOK_BY_NAME(cons);
		SET_UNIFY_HOOK_BY_NAME(fixnum);                                                                         
		SET_UNIFY_HOOK_BY_NAME(character);                                                                      
		SET_UNIFY_HOOK_BY_NAME(bignum);                                                                         
		SET_UNIFY_HOOK_BY_NAME(ratio);                                                                          
		SET_UNIFY_HOOK_BY_NAME(shortfloat);                                                                      
		SET_UNIFY_HOOK_BY_NAME(longfloat);                                                                          
		SET_UNIFY_HOOK_BY_NAME(complex);                                                                         
		SET_UNIFY_HOOK_BY_NAME(symbol);                                                                             
		SET_UNIFY_HOOK_BY_NAME(package);                                                                            
		SET_UNIFY_HOOK_BY_NAME(hashtable);                                                                          
		SET_UNIFY_HOOK_BY_NAME(array);                                                                              
		SET_UNIFY_HOOK_BY_NAME(vector);                                                                             
		SET_UNIFY_HOOK_BY_NAME(string);                                                                             
		SET_UNIFY_HOOK_BY_NAME(bitvector);                                                                          
		SET_UNIFY_HOOK_BY_NAME(stream);                                                                             
		SET_UNIFY_HOOK_BY_NAME(random);                                                                                  
		SET_UNIFY_HOOK_BY_NAME(readtable);                                                                               
		SET_UNIFY_HOOK_BY_NAME(pathname);                                                                                
		SET_UNIFY_HOOK_BY_NAME(bytecodes);                                                                               
		SET_UNIFY_HOOK_BY_NAME(cfun);                                                                                
		SET_UNIFY_HOOK_BY_NAME(cclosure);                                                                               
#ifdef CLOS
		SET_UNIFY_HOOK_BY_NAME(instance);                                                                               
#else
		SET_UNIFY_HOOK_BY_NAME(structure);                                                                              
#endif /// CLOS 
#ifdef ECL_THREADS
		SET_UNIFY_HOOK_BY_NAME(process);
		SET_UNIFY_HOOK_BY_NAME(lock);
#endif
		SET_UNIFY_HOOK_BY_NAME(codeblock);                                                                          
		SET_UNIFY_HOOK_BY_NAME(foreign);                                                                           
		SET_UNIFY_HOOK_BY_NAME(end);
		SET_UNIFY_HOOK_BY_NAME(other);
		SET_UNIFY_HOOK_BY_NAME(contiguous);                                                                                
		SET_UNIFY_HOOK("FREE",FREE,unify_object);                                                                           

		//PL_current_global_data()->debug_level = 9;

		//"SI::UNIVERSAL-ERROR-HANDLER"
		PL_register_foreign_in_module("ecls","$lisp_eval", 2, lisp_eval, PL_FA_NONDETERMINISTIC);
		PL_register_foreign_in_module("ecls","$lisp_unify", 2, lisp_unify, PL_FA_NONDETERMINISTIC);
		PL_register_foreign_in_module("ecls","$lisp_term", 2, lisp_term, PL_FA_NONDETERMINISTIC);
		PL_register_foreign_in_module("ecls","$lisp_thaw", 1, lisp_term, PL_FA_NONDETERMINISTIC);
//		PL_register_foreign("lisp_symbol", 2, lisp_symbol, PL_FA_NONDETERMINISTIC);
		PL_register_foreign_in_module("ecls","$lisp_property", 3, lisp_property, PL_FA_NONDETERMINISTIC);
		PL_register_foreign_in_module("ecls","$lisp_option", 3, lisp_option, PL_FA_NONDETERMINISTIC);
		PL_on_halt(prologExiting,NULL);
	} else {
		printf("% poplog alreading installed\n");
	}
}


int initLPBindings(int narg, char **argv,struct cl_env_struct* env, struct cl_core_struct* core) {
	int iargc = 1;
	int type_i = 0;
	printf(";** initLPBindings(%d,[%s|...],...,...)\n",narg,argv[0]);
	if (LPGlobal.main!=&LPToplevel) {
		printf(";*** INSTALLING GLOBALS\n");
		//fflush(stdout);
		LPGlobal.main = &LPToplevel;
		//LPGlobal.prolog_code = PL_current_code_data();
		//LPGlobal.prolog_core = PL_current_global_data();	 
		LPGlobal.main->keyToSymbol = newHTable(64);
		LPGlobal.main->symbolToAtom = newHTable(64);																																			 // PL_global_data; //lookupHTable
		LPGlobal.lisp_core = core;
		LPGlobal.prolog_package = make_package(make_simple_string("PROLOG"),Cnil,LPGlobal.lisp_core->packages);
		printf(";**** PACKAGE PROLOG::\n");
		LPGlobal.fsym_prolog_toplevel = _intern("TOPLEVEL", LPGlobal.prolog_package);
		cl_def_c_function_va(LPGlobal.fsym_prolog_toplevel, cfun_prolog);
	}
	if (LPEngine==NULL) {
		LPEngine = &LPToplevel;
		printf(";*** FOUND THREAD\n");
	}
	if (LPEngine->global != &LPGlobal) {
		printf(";*** INSTALLING LP-CURRENT\n");
		//fflush(stdout);
		LPEngine->global = &LPGlobal;
		LPEngine->lisp = env;
		LPEngine->prolog = NULL;
	}
	if (LPEngine->prolog==NULL) {
		char* iargv[] = {"poplog"};
		if (!(iargc = PL_is_initialised(&iargc,(char ***)iargv))) {
			printf(";*** INITALIZING SWI-PROLOG\n");
			if (!(iargc = PL_initialise(narg,argv))) {
				printf(";* FAILED SWI-PROLOG\n");
			} else {
				printf(";*** SWI-PROLOG Loaded\n");
			}
		}
	}

	LPEngine->prolog = LD;
	if (LPEngine->prolog) {
		printf(";*** FOUND SWI-PROLOG ENGINE\n");
	} else {
		printf(";*** CREATING SWI-PROLOG ENGINE\n");
		LPEngine->prolog = PL_create_engine(NULL);
	}
	if (LPEngine->prolog==NULL) {
		printf(";*** ERROR SWI-PROLOG ENGINE\n");
		return -1;
	} else {
		install();
		printf(";**** Use: (PROLOG::TOPLEVEL)\n");
	}

#ifdef O_LOGICMOO
	LPGlobal.SLogicmoo = _intern("LOGICMOO", LPGlobal.prolog_package);
	//register_root(&LOGICMOO);
#endif
	//fflush(stdout);
	return iargc;
}


extern struct cl_core_struct cl_core;

int main(int argc, char **args) {
	cl_object top_level;
	/* This should be always the first call */
	cl_boot(argc, args);
	initLPBindings(argc,args,(struct cl_env_struct*) &cl_env, (struct cl_core_struct*) &cl_core);
	/* We are computing unnormalized numbers at some point */
	//si_trap_fpe(Ct, Cnil);
	top_level = _intern("TOP-LEVEL", cl_core.system_package);
	cl_def_c_function(top_level, cfun_prolog, 0);
	funcall(1, top_level);
	return(0);
}

