/*****************************************************************************/
/*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  */
/*  Copyright (C) 2009-2012  Stefano Zacchiroli <zack@upsilon.cc>            */
/*                                                                           */
/*  This library is free software: you can redistribute it and/or modify     */
/*  it under the terms of the GNU Lesser General Public License as           */
/*  published by the Free Software Foundation, either version 3 of the       */
/*  License, or (at your option) any later version.  A special linking       */
/*  exception to the GNU Lesser General Public License applies to this       */
/*  library, see the COPYING file for more information.                      */
/*****************************************************************************/

// TODO should check / handle exceptions for all invoked caml_callback-s
// TODO better management of g_error() (not all should be fatal)
// TODO property-by-property access for preamble (as per packages)

#include <stdio.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "cudf-private.h"	// instantiate OCaml-related ADTs
#include "cudf-variants.h"
#include "cudf.h"

#define Val_none	Val_int(0)
#define Some_val(v)	Field(v,0)

/* field indexes in the return type of {!Cudf_parser.parse_from_file},
 * {!Cudf_parser.load_from_file}, and {!Cudf_parser.load_solution_from_file} */
#define FIELD_PRE	0
#define FIELD_UNIV	1	// universe for load_*, package list for parse_*
#define FIELD_REQ	2	// unused for load_solution_from_file

/* field indexes in {!Cudf.package} */
#define FIELD_PKG	0
#define FIELD_VERSION	1
#define FIELD_DEPS	2
#define FIELD_CONFL	3
#define FIELD_PROV	4
#define FIELD_INST	5
#define FIELD_WASINST	6
#define FIELD_KEEP	7
#define FIELD_PKGEXTRA	8

/* field indexes in {!Cudf.request} */
#define FIELD_REQID	0
#define FIELD_REQINST	1
#define FIELD_REQREM	2
#define FIELD_REQUP	3
#define FIELD_REQEXTRA	4

/* field indexes in {!Cudf.preamble} */
#define FIELD_PREID	0
#define FIELD_TYPEDECL	1
#define FIELD_UCHECK	2
#define FIELD_SCHECK	3
#define FIELD_RCHECK	4

/* field indexes in the return type of {!Cudf_checker.is_solution} and
 * {!Cudf_checker.is_consistent} */
#define FIELD_ISSOL	0

/* Initialize a pointer to an OCaml value */
#define NEW_MLVAL(p)					\
	do { p = malloc(sizeof(value));			\
		caml_register_global_root(p); }		\
	while (0)

/* Free a pointer to an OCaml value */
#define FREE_MLVAL(p)				\
	do { free(p);				\
		caml_remove_global_root(p); }	\
	while (0)

/** generic OCaml binding helpers */

#if 0
static int caml_list_length(value l) {
	int length = 0;

	while (l != Val_emptylist) {
		length++;
		l = Field(l, 1);
	}
	return length;
}
#endif

/** CUDF-specific binding helpers */

static int relop_val(value v) {
	CAMLparam1(v);
	int op;

	switch (Int_val(v)) {
	case MLPVAR_Eq : op = RELOP_EQ ; break ;
	case MLPVAR_Neq : op = RELOP_NEQ ; break ;
	case MLPVAR_Geq : op = RELOP_GEQ ; break ;
	case MLPVAR_Gt : op = RELOP_GT ; break ;
	case MLPVAR_Leq : op = RELOP_LEQ ; break ;
	case MLPVAR_Lt : op = RELOP_LT ; break ;
	default :
		g_error("Internal error: unexpected variant for \"relop\": %d",
			Int_val(v));
	}

	CAMLreturnT(int, op);
}

cudf_vpkg_t *cudf_vpkg_val(value ml_vpkg) {
	CAMLparam1(ml_vpkg);
	CAMLlocal1(ml_constr);
	cudf_vpkg_t *vpkg;

	vpkg = malloc(sizeof(cudf_vpkg_t));
	vpkg->name = strdup(String_val(Field(ml_vpkg, 0)));
	if (Field(ml_vpkg, 1) != Val_none) {	/* version constraint */
		ml_constr = Some_val(Field(ml_vpkg, 1));
		vpkg->relop = relop_val(Field(ml_constr, 0));
		vpkg->version = Int_val(Field(ml_constr, 1));
	} else {	/* no version constraint */
		vpkg->relop = 0;
		vpkg->version = -1;
	}

	CAMLreturnT(cudf_vpkg_t *, vpkg);
}

cudf_vpkglist_t cudf_vpkglist_val(value ml_vpkgs) {
	CAMLparam1(ml_vpkgs);
	CAMLlocal1(ml_vpkg);
	GList *l = NULL;
	cudf_vpkg_t *vpkg;
	
	while (ml_vpkgs != Val_emptylist) {
		ml_vpkg = Field(ml_vpkgs, 0);
		vpkg = cudf_vpkg_val(ml_vpkg);
		l = g_list_append(l, vpkg);
		ml_vpkgs = Field(ml_vpkgs, 1);
	}
	CAMLreturnT(cudf_vpkglist_t, l);
}

cudf_vpkgformula_t cudf_vpkgformula_val(value ml_fmla) {
	CAMLparam1(ml_fmla);
	CAMLlocal2(ml_and, ml_or);
	GList *and_l = NULL;	/* top-level formula (CNF) */
	GList *or_l;		/* OR-ed deps */
	/* ml_and: iterates over OR-ed deps (which are AND-ed together) */
	/* ml_or: iterates over vpkg-s (which are OR-ed together) */
	cudf_vpkg_t *vpkg;

	ml_and = ml_fmla;
	while (ml_and != Val_emptylist) {
		ml_or = Field(ml_and, 0);
		or_l = NULL;
		while (ml_or != Val_emptylist) {
			vpkg = cudf_vpkg_val(Field(ml_or, 0));
			or_l = g_list_append(or_l, vpkg);
			ml_or = Field(ml_or, 1);
		}
		and_l = g_list_append(and_l, or_l);
		ml_and = Field(ml_and, 1);
	}

	CAMLreturnT(cudf_vpkgformula_t, and_l);
}

cudf_value_t *cudf_value_val(value ml_v) {
	CAMLparam1(ml_v);
	CAMLlocal1(ml_payload);
	cudf_value_t *v;
	int typ;

	v = malloc(sizeof(cudf_value_t));
	typ = Int_val(Field(ml_v, 0));
	ml_payload = Field(ml_v, 1);

	v->typ = typ;
	switch (typ) {
	case MLPVAR_Int :
		v->typ = TYPE_INT;
		v->val.i = Int_val(ml_payload);
		break;
	case MLPVAR_Posint :
		v->typ = TYPE_POSINT;
		v->val.i = Int_val(ml_payload);
		break;
	case MLPVAR_Nat :
		v->typ = TYPE_NAT;
		v->val.i = Int_val(ml_payload);
		break;
	case MLPVAR_Bool :
		v->typ = TYPE_BOOL;
		v->val.i = Bool_val(ml_payload);
		break;
	case MLPVAR_String :
		v->typ = TYPE_STRING;
		v->val.s = strdup(String_val(ml_payload));
	case MLPVAR_Pkgname :
		v->typ = TYPE_PKGNAME;
		v->val.s = strdup(String_val(ml_payload));
	case MLPVAR_Ident :
		v->typ = TYPE_IDENT;
		v->val.s = strdup(String_val(ml_payload));
		break;
	case MLPVAR_Enum :
		v->typ = TYPE_ENUM;
		/* Skip enum list and jump to the actual enum.  Enum list is
		 * currently not accessible using C bindings. */
		v->val.s = strdup(String_val(Field(ml_payload, 1)));
		break;
	case MLPVAR_Vpkg :
		v->typ = TYPE_VPKG;
		v->val.vpkg = cudf_vpkg_val(ml_payload);
		break;
	case MLPVAR_Veqpkg :
		v->typ = TYPE_VEQPKG;
		v->val.vpkg = cudf_vpkg_val(ml_payload);
		break;
	case MLPVAR_Vpkglist :
		v->typ = TYPE_VPKGLIST;
		v->val.vpkgs = cudf_vpkglist_val(ml_payload);
		break;
	case MLPVAR_Veqpkglist :
		v->typ = TYPE_VEQPKGLIST;
		v->val.vpkgs = cudf_vpkglist_val(ml_payload);
		break;
	case MLPVAR_Vpkgformula :
		v->typ = TYPE_VPKGFORMULA;
		v->val.f = cudf_vpkgformula_val(ml_payload);
		break;
	case MLPVAR_Typedecl :
		v->typ = TYPE_TYPEDECL;
		break;
	default :
		g_error("Internal error: unexpected variant for type: %d", typ);
	}

	CAMLreturnT(cudf_value_t *, v);
}

/** libCUDF binding public interface */

void cudf_init() {
	char *fake_argv[] = {"", NULL};
	static int cudf_initialized = 0;

	if (cudf_initialized)
		return;

	caml_startup(fake_argv);
	cudf_initialized = 1;
}

cudf_doc_t *cudf_parse_from_file(char *fname) {
	CAMLparam0();
	CAMLlocal2(ml_doc, ml_pkgs);
	static value *closure_f = NULL;
	cudf_doc_t *doc;
	GList *l = NULL;
	cudf_package_t pkg;
  
	doc = malloc(sizeof(cudf_doc_t));
	if (closure_f == NULL)
		closure_f = caml_named_value("parse_from_file");
	ml_doc = caml_callback(*closure_f, caml_copy_string(fname));
  
	NEW_MLVAL(doc->preamble);			/* preamble */
	if (Field(ml_doc, FIELD_PRE) != Val_none) {
		doc->has_preamble = 1;
		*(doc->preamble) = Some_val(Field(ml_doc, FIELD_PRE));
	} else {
		doc->has_preamble = 0;
		*(doc->preamble) = Val_none;
	}

	NEW_MLVAL(doc->request);			/* request */
	if (Field(ml_doc, FIELD_REQ) != Val_none) {
		doc->has_request = 1;
		*(doc->request) = Some_val(Field(ml_doc, FIELD_REQ));
	} else {
		doc->has_request = 0;
		*(doc->request) = Val_none;
	}

	ml_pkgs = Field(ml_doc, FIELD_UNIV);		/* packages */
	while (ml_pkgs != Val_emptylist) {
		NEW_MLVAL(pkg);
		*pkg = Field(ml_pkgs, 0);
		l = g_list_prepend(l, pkg);
		ml_pkgs = Field(ml_pkgs, 1);
	}
	doc->packages = g_list_reverse(l);

	CAMLreturnT(cudf_doc_t *, doc);
}

cudf_t *cudf_load_from_file(char *fname) {
	CAMLparam0();
	CAMLlocal1(ml_cudf);
	static value *closure_f = NULL;
	cudf_t *cudf;
  
	cudf = malloc(sizeof(cudf_t));
	if (closure_f == NULL)
		closure_f = caml_named_value("load_from_file");
	ml_cudf = caml_callback(*closure_f, caml_copy_string(fname));

	NEW_MLVAL(cudf->preamble);			/* preamble */
	if (Field(ml_cudf, FIELD_PRE) != Val_none) {
		cudf->has_preamble = 1;
		*(cudf->preamble) = Some_val(Field(ml_cudf, FIELD_PRE));
	} else {
		cudf->has_preamble = 0;
		*(cudf->preamble) = Val_none;
	}

	NEW_MLVAL(cudf->request);			/* request */
	if (Field(ml_cudf, FIELD_REQ) != Val_none) {
		cudf->has_request = 1;
		*(cudf->request) = Some_val(Field(ml_cudf, FIELD_REQ));
	} else {
		cudf->has_request = 0;
		*(cudf->request) = Val_none;
	}

	NEW_MLVAL(cudf->universe);			/* universe */
	*(cudf->universe) = Field(ml_cudf, FIELD_UNIV);

	CAMLreturnT(cudf_t *, cudf);
}

cudf_t *cudf_load_solution_from_file(char *fname, cudf_universe_t ref_univ) {
	CAMLparam0();
	CAMLlocal1(ml_cudf);
	static value *closure_f = NULL;
	cudf_t *cudf;
  
	cudf = malloc(sizeof(cudf_t));
	if (closure_f == NULL)
		closure_f = caml_named_value("load_solution_from_file");
	ml_cudf = caml_callback2(*closure_f, caml_copy_string(fname),
				 *ref_univ);

	NEW_MLVAL(cudf->preamble);			/* preamble */
	if (Field(ml_cudf, FIELD_PRE) != Val_none) {
		cudf->has_preamble = 1;
		*(cudf->preamble) = Some_val(Field(ml_cudf, FIELD_PRE));
	} else {
		cudf->has_preamble = 0;
		*(cudf->preamble) = Val_none;
	}

	NEW_MLVAL(cudf->request);			/* request */
	cudf->has_request = 0;		/* solutions have no request */
	*(cudf->request) = Val_none;

	NEW_MLVAL(cudf->universe);			/* universe */
	*(cudf->universe) = Field(ml_cudf, FIELD_UNIV);

	CAMLreturnT(cudf_t *, cudf);
}

char *cudf_pkg_name(cudf_package_t pkg) {
	return String_val(Field(*pkg, FIELD_PKG));
}

int cudf_pkg_version(cudf_package_t pkg) {
	return Int_val(Field(*pkg, FIELD_VERSION));
}

int cudf_pkg_installed(cudf_package_t pkg) {
	return Int_val(Field(*pkg, FIELD_INST));
}

int cudf_pkg_was_installed(cudf_package_t pkg) {
	return Int_val(Field(*pkg, FIELD_WASINST));
}

int cudf_pkg_keep(cudf_package_t pkg) {
	CAMLparam0();
	CAMLlocal1(keep);
	int k;

	keep = Field(*pkg, FIELD_KEEP);
	switch (Int_val(keep)) {
	case MLPVAR_Keep_none : k = KEEP_NONE ; break ;
	case MLPVAR_Keep_version : k = KEEP_VERSION ; break ;
	case MLPVAR_Keep_package : k = KEEP_PACKAGE ; break ;
	case MLPVAR_Keep_feature : k = KEEP_FEATURE ; break ;
	default :
		g_error("Internal error: unexpected variant for \"keep\": %d",
			Int_val(keep));
	}

	CAMLreturnT(int, k);
}

cudf_vpkgformula_t cudf_pkg_depends(cudf_package_t pkg) {
	return cudf_vpkgformula_val(Field(*pkg, FIELD_DEPS));
}

cudf_vpkglist_t cudf_pkg_conflicts(cudf_package_t pkg) {
	return cudf_vpkglist_val(Field(*pkg, FIELD_CONFL));
}

cudf_vpkglist_t cudf_pkg_provides(cudf_package_t pkg) {
	return cudf_vpkglist_val(Field(*pkg, FIELD_PROV));
}

char *cudf_pkg_property(cudf_package_t pkg, const char *prop) {
	CAMLparam0();
	CAMLlocal1(prop_val);
	static value *closure_f = NULL;
  
	if (closure_f == NULL)
		closure_f = caml_named_value("lookup_package_property");
	prop_val = caml_callback2_exn(*closure_f, *pkg, caml_copy_string(prop));
	CAMLreturnT(char *,
		    Is_exception_result(prop_val) ? NULL :
		    strdup(String_val(prop_val)));
}

char *cudf_req_property(cudf_request_t req, const char *prop) {
	CAMLparam0();
	CAMLlocal1(prop_val);
	static value *closure_f = NULL;
  
	if (closure_f == NULL)
		closure_f = caml_named_value("lookup_request_property");
	prop_val = caml_callback2_exn(*closure_f, *req, caml_copy_string(prop));
	CAMLreturnT(char *,
		    Is_exception_result(prop_val) ? NULL :
		    strdup(String_val(prop_val)));
}

cudf_vpkglist_t cudf_req_install(cudf_request_t req) {
	return cudf_vpkglist_val(Field(*req, FIELD_REQINST));
}

cudf_vpkglist_t cudf_req_remove(cudf_request_t req) {
	return cudf_vpkglist_val(Field(*req, FIELD_REQREM));
}

cudf_vpkglist_t cudf_req_upgrade(cudf_request_t req) {
	return cudf_vpkglist_val(Field(*req, FIELD_REQUP));
}

char *cudf_pre_property(cudf_preamble_t pre, const char *prop) {
	CAMLparam0();
	CAMLlocal1(prop_val);
	static value *closure_f = NULL;
  
	if (closure_f == NULL)
		closure_f = caml_named_value("lookup_preamble_property");
	prop_val = caml_callback2_exn(*closure_f, *pre, caml_copy_string(prop));
	CAMLreturnT(char *,
		    Is_exception_result(prop_val) ? NULL :
		    strdup(String_val(prop_val)));
}

cudf_extra_t cudf_pkg_extra(cudf_package_t pkg) {
	CAMLparam0();
	CAMLlocal2(ml_extras, ml_prop);
	GHashTable *h = NULL;

	h = g_hash_table_new_full(g_str_hash, g_str_equal,
				  g_free, (GDestroyNotify) cudf_free_value);

	ml_extras = Field(*pkg, FIELD_PKGEXTRA);
	while (ml_extras != Val_emptylist) {
		ml_prop = Field(ml_extras, 0);
		g_hash_table_insert(h, strdup(String_val(Field(ml_prop, 0))),
				    cudf_value_val(Field(ml_prop, 1)));
		ml_extras = Field(ml_extras, 1);
	}
	CAMLreturnT(cudf_extra_t, h);
}


/** Universe management */

cudf_universe_t cudf_load_universe(GList *packages) {
	CAMLparam0();
	CAMLlocal2(ml_pkgs, cons);
	static value *closure_f = NULL;
	GList *l = packages;
	cudf_universe_t univ = NULL;

	ml_pkgs = Val_emptylist;
	while (l != NULL) {
		cons = caml_alloc(2, 0);
		Store_field(cons, 0, * (cudf_package_t) g_list_nth_data(l, 0));
		Store_field(cons, 1, ml_pkgs);
		ml_pkgs = cons;
		l = g_list_next(l);
	}

	if (closure_f == NULL)
		closure_f = caml_named_value("load_universe");
	NEW_MLVAL(univ);
	*univ = caml_callback(*closure_f, ml_pkgs);

	CAMLreturnT(cudf_universe_t, univ);
}

int cudf_universe_size(cudf_universe_t univ) {
	static value *closure_f = NULL;

	if (closure_f == NULL)
		closure_f = caml_named_value("universe_size");

	return Int_val(caml_callback(*closure_f, *univ));
}

int cudf_installed_size(cudf_universe_t univ) {
	static value *closure_f = NULL;

	if (closure_f == NULL)
		closure_f = caml_named_value("installed_size");

	return Int_val(caml_callback(*closure_f, *univ));
}

int cudf_is_consistent(cudf_universe_t univ) {
	static value *closure_f = NULL;

	if (closure_f == NULL)
		closure_f = caml_named_value("is_consistent");

	return Bool_val(Field(caml_callback(*closure_f, *univ), FIELD_ISSOL));
}

int cudf_is_solution(cudf_t *cudf, cudf_universe_t sol) {
	CAMLparam0();
	CAMLlocal1(ml_cudf);
	static value *closure_f = NULL;

	if (closure_f == NULL)
		closure_f = caml_named_value("is_solution");
	if (! cudf->has_request)
		g_error("Given CUDF has no request: cannot compare it with a solution.");
	ml_cudf = caml_alloc(2, 0);
	Store_field(ml_cudf, 0, *(cudf->universe));
	Store_field(ml_cudf, 1, *(cudf->request));

	CAMLreturnT(int,
		    Bool_val(Field(caml_callback2(*closure_f, ml_cudf, *sol),
				   FIELD_ISSOL)));
}

/** Memory management.
    free-like functions to free binding-specific data structures */

void cudf_free_doc(cudf_doc_t *doc) {
	GList *l;

	if (doc == NULL)
		return;

	FREE_MLVAL(doc->preamble);
	FREE_MLVAL(doc->request);
	l = doc->packages;
	while (l != NULL) {
		FREE_MLVAL(g_list_nth_data(l, 0));
		l = g_list_next(l);
	}
	g_list_free(l);
	free(doc);
}

void cudf_free_cudf(cudf_t *cudf) {
	if (cudf == NULL)
		return;

	FREE_MLVAL(cudf->preamble);
	FREE_MLVAL(cudf->request);
	FREE_MLVAL(cudf->universe);
	free(cudf);
}

void cudf_free_universe(cudf_universe_t univ) {
	if (univ == NULL)
		return;

	FREE_MLVAL(univ);
}

void cudf_free_vpkg(cudf_vpkg_t *vpkg) {
	if (vpkg == NULL)
		return;

	if (vpkg->name != NULL)
		free(vpkg->name);
	free(vpkg);
}

void cudf_free_vpkglist(cudf_vpkglist_t vpkgs) {
	GList *l = vpkgs;

	while (l != NULL) {
		cudf_free_vpkg(g_list_nth_data(l, 0));
		l = g_list_next(l);
	}
	g_list_free(vpkgs);
}

void cudf_free_vpkgformula(cudf_vpkgformula_t fmla) {
	GList *l = fmla;

	while (l != NULL) {
		cudf_free_vpkglist(g_list_nth_data(l, 0));
		l = g_list_next(l);
	}
	g_list_free(fmla);
}

void cudf_free_value(cudf_value_t *v) {
	int typ;

	if (v == NULL)
		return;

	typ = v->typ;
	switch (typ) {
	case TYPE_INT :
	case TYPE_POSINT :
	case TYPE_NAT :
	case TYPE_BOOL :
		break;	/* integers don't require any freeing */
	case TYPE_STRING :
	case TYPE_PKGNAME :
	case TYPE_IDENT :
	case TYPE_ENUM :
		free(v->val.s);
		break;
	case TYPE_VPKG :
	case TYPE_VEQPKG :
		cudf_free_vpkg(v->val.vpkg);
		break;
	case TYPE_VPKGLIST :
	case TYPE_VEQPKGLIST :
		cudf_free_vpkglist(v->val.vpkgs);
		break;
	case TYPE_VPKGFORMULA :
		cudf_free_vpkgformula(v->val.f);
		break;
	case TYPE_TYPEDECL :
		break;
	default :
		g_error("Internal error: unexpected variant for type: %d", typ);
	}

	free(v);
}

void cudf_free_extra(cudf_extra_t extra) {
	g_hash_table_destroy(extra);
}

