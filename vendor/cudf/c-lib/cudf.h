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

#ifndef _CUDF_H
#define _CUDF_H

#include <glib.h>

#ifndef _CUDF_PRIVATE_H
/* Abstract data types. You should access them only with the functions given
   below. Really (or you will face the anger of OCaml GC).*/

typedef void *cudf_preamble_t;	/* preamble of a CUDF document */
typedef void *cudf_request_t;	/* request of a CUDF document */
typedef void *cudf_universe_t;	/* package universe (i.e. all known packages) */
typedef void *cudf_package_t;	/* single package from the universe */

#endif

typedef GList *cudf_packages_t;	/* List of CUDF packages */

typedef struct __cudf_doc {
	int has_preamble;	/* Whether user request was provided or not */
	int has_request;	/* Whether request was provided or not */
	cudf_preamble_t preamble;	/* Preamble (iff has_preamble != 0) */
	cudf_request_t request;	/* User request (iff has_request != 0) */
	cudf_packages_t packages;	/* List of packages */
} cudf_doc_t;

typedef struct __cudf {
	int has_preamble;	/* Whether user request was provided or not */
	int has_request;	/* Whether request was provided or not */
	cudf_preamble_t preamble;	/* Preamble (iff has_preamble != 0) */
	cudf_request_t request;	/* User request (iff has_request != 0) */
	cudf_universe_t universe; /* Abstract package universe */
} cudf_t;


/* Initialization */

/* Call cudf_init() before doing anything else with libCUDF. (Or you will get a
 * segfault, you've been warned.) */
void cudf_init();


/* Parsing */

/* Parse a CUDF document from file, without doing any further processing. */
cudf_doc_t *cudf_parse_from_file(char *fname);

/* Load a CUDF document from file, i.e. parse it and then store the contained
 * packages as an universe structure.
 * 
 * Note: to load solutions you should prefer cudf_load_solution_from_file,
 * which can be invoked after CUDF document loading. */
cudf_t *cudf_load_from_file(char *fname);

/* Load from file a CUDF universe representing a solution to an upgrade
 * scenario. Solution format is as per Appendix B of CUDF 2.0 spec
 * (i.e. package/version pairs, together with installation status).
 *
 * @param ref_univ is the reference universe to be used to expand package
 *   information, usually it is the universe of the original CUDF
 */
cudf_t *cudf_load_solution_from_file(char *fname, cudf_universe_t ref_univ);


/* Package predicate
   Examples:
   - bar	--->	{ name="bar" ; relop=0 ; version=UNSPECIFIED }
   - foo >= 2	--->	{ name="foo" ; relop=RELOP_GEQ ; version=2 }
*/
typedef struct __cudf_vpkg {
	char *name;	/* Package name */
	int relop;	/* Version constraint operator, see RELOP_* constants.
			   0 (i.e. RELOP_NOP) means no constraint */
	int version;	/* Version constraint value (iff constr != 0) */
} cudf_vpkg_t;

typedef GList *cudf_vpkglist_t;		/* List of cudf_vpkg */

/* Hash table mapping property names (char *) to typed values (cudf_value_t). */
typedef GHashTable *cudf_extra_t;

/* List of (cudf_vpkg_t *) lists.
   CNF encoding: the inner lists are OR-ed, while the outer are AND-ed */
typedef GList *cudf_vpkgformula_t;

/* Version comparison operators */
#define RELOP_EQ	1	/* "=" */
#define RELOP_NEQ	2	/* "!=" */
#define RELOP_GEQ	3	/* ">=" */
#define RELOP_GT	4	/* ">" */
#define RELOP_LEQ	5	/* "<=" */
#define RELOP_LT	6	/* "<" */
#define RELOP_NOP	0	/* dummy operator */

/* CUDF types */
#define TYPE_INT		1	/* type "int" */
#define TYPE_POSINT		2	/* type "posint" */
#define TYPE_NAT		3	/* type "nat" */
#define TYPE_BOOL		4	/* type "bool" */
#define TYPE_STRING		5	/* type "string" */
#define TYPE_ENUM		6	/* type "enum" (whichever enum list) */
#define TYPE_PKGNAME		7	/* type "pkgname" */
#define TYPE_IDENT		8	/* type "ident" */
#define TYPE_VPKG		9	/* type "vpkg" */
#define TYPE_VPKGFORMULA	10	/* type "vpkgformula" */
#define TYPE_VPKGLIST		11	/* type "vpkglist" */
#define TYPE_VEQPKG		12	/* type "veqpkg" */
#define TYPE_VEQPKGLIST		13	/* type "veqpkglist" */
#define TYPE_TYPEDECL		14	/* type "typedecl" */
#define TYPE_NOTYPE		0	/* dummy type */


/* Typed CUDF value */
typedef struct __cudf_value {
	int typ;	/* CUDF type, one of the TYPE_* constants */
	union {
		int i;
		char *s;
		cudf_vpkg_t *vpkg;
		cudf_vpkgformula_t f;
		cudf_vpkglist_t vpkgs;
		/* cudf_typedecl types; */	/* currently not supported */
	} val;	/* CUDF value
		   depending on typ above, one of the above union field is set:
		            typ       | val field
		     -----------------+-------------------
		     TYPE_INT         | int i
		     TYPE_POSINT      | int i
		     TYPE_NAT         | int i
		     TYPE_BOOL        | int i
		     TYPE_STRING      | char *s
		     TYPE_ENUM        | char *s
		     TYPE_PKGNAME     | char *s
		     TYPE_IDENT       | char *s
		     TYPE_VPKG        | cudf_vpkg_t *pkg
		     TYPE_VEQPKG      | cudf_vpkg_t *pkg
		     TYPE_VPKGLIST    | cudf_vpkglist_t pkgs
		     TYPE_VEQPKGLIST  | cudf_vpkglist_t pkgs
		     TYPE_VPKGFORMULA | cudf_vpkgformula_t f
		     TYPE_TYPEDECL    | cudf_typedecl_t types
		*/
} cudf_value_t;

/* Macros for accessing cudf_package values */

/* Get package name of a cudf_pkg */
char *cudf_pkg_name(cudf_package_t pkg);

/* Get package version of a cudf_pkg */
int cudf_pkg_version(cudf_package_t pkg);

/* Get (current) installation status of a cudf_pkg */
int cudf_pkg_installed(cudf_package_t pkg);

/* Get (past) installation status of a cudf_pkg */
int cudf_pkg_was_installed(cudf_package_t pkg);

/* Possible values returned by cudf_pkg_keep() */
#define KEEP_NONE	0	/* keep: none */
#define KEEP_VERSION	1	/* keep: version */
#define	KEEP_PACKAGE	2	/* keep: package */
#define	KEEP_FEATURE	3	/* keep: feature */

/* Get "keep" property from a cudf_pkg. See KEEP_* macros */
int cudf_pkg_keep(cudf_package_t pkg);

/* Get dependencies of a package */
cudf_vpkgformula_t cudf_pkg_depends(cudf_package_t pkg);

/* Get conflicts of a package */
cudf_vpkglist_t cudf_pkg_conflicts(cudf_package_t pkg);

/* Get provided features of a package */
cudf_vpkglist_t cudf_pkg_provides(cudf_package_t pkg);

/* Get extra properties of a package. */
cudf_extra_t cudf_pkg_extra(cudf_package_t pkg);

/* Lookup package property by name. Returned string should be manually freed.
   Return NULL if the property is missing (and has no default value). */
char *cudf_pkg_property(cudf_package_t pkg, const char *prop);

/* Lookup request property by name. Returned string should be manually freed.
   Return NULL if the property is missing (and has no default value). */
char *cudf_req_property(cudf_request_t req, const char *prop);

/* Get install section of the request. */
cudf_vpkglist_t cudf_req_install(cudf_request_t req) ;

/* Get upgrade section of the request. */
cudf_vpkglist_t cudf_req_upgrade(cudf_request_t req) ;

/* Get remove section of the request. */
cudf_vpkglist_t cudf_req_remove(cudf_request_t req) ;

/* Lookup preamble property by name. Returned string should be manually freed.
   Return NULL if the property is missing (and has no default value). */
char *cudf_pre_property(cudf_preamble_t pre, const char *prop);


/* Universe management */

/* @param packages list of (pointers to) cudf_package-s; the packages member of
    a cudf_doc structure is a suitable value
   @return a freshly allocated universe, which should be freed when no longer
    needed using cudf_free_universe */
cudf_universe_t cudf_load_universe(GList *packages);

/* Return the number of packages in the given universe. */
int cudf_universe_size(cudf_universe_t univ);

/* Return the number of installed packages in the given universe. */
int cudf_installed_size(cudf_universe_t univ);

/* Check whether the package status of the given universe is consistent
 * (i.e. dependencies and conflicts or all installed packages are
 * respected). */
int cudf_is_consistent(cudf_universe_t univ);

/* Check whether the given universe contains a proper solution for the given
 * CUDF (i.e. its package status is consistent and satisfies user request).
 *
 * Solution should normally be obtained via cudf_load_solution_from_file(), and
 * passing cudf->universe to it, e.g.:
 *
 *   cudf = cudf_load_from_file(...);
 *   sol = cudf_load_solution_from_file(..., cudf->universe);
 *   ok = is_solution(cudf, sol);
 */
int cudf_is_solution(cudf_t *cudf, cudf_universe_t solution);


/* Memory management */

void cudf_free_doc(cudf_doc_t *doc);
void cudf_free_cudf(cudf_t *cudf);
void cudf_free_universe(cudf_universe_t univ);
void cudf_free_vpkg(cudf_vpkg_t *vpkg);
void cudf_free_vpkglist(cudf_vpkglist_t l);
void cudf_free_vpkgformula(cudf_vpkgformula_t fmla);
void cudf_free_value(cudf_value_t *val);
void cudf_free_extra(cudf_extra_t extra);


#endif	/* end of cudf.h */

