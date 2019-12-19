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

/* Compile with:

   cc -o c-test c-test.c `pkg-config --cflags cudf` `pkg-config --libs cudf`
*/

#include <stdio.h>
#include <stdlib.h>
#include <glib.h>

#include <cudf.h>

/* Print to stdout a relational operator (on versions) */
void print_relop(int relop) {
	switch (relop) {
	case RELOP_EQ : printf("=") ; break ;
	case RELOP_NEQ : printf("!=") ; break ;
	case RELOP_GEQ : printf(">=") ; break ;
	case RELOP_GT : printf(">") ; break ;
	case RELOP_LEQ : printf("<=") ; break ;
	case RELOP_LT : printf("<") ; break ;
	case RELOP_NOP :
	default :
		g_error("Unexpected integer, which is not a RELOP_*: %d",
			relop);
	}
}

/* Print to stdout a package version predicate */
void print_vpkg(cudf_vpkg_t *vpkg) {
	if (vpkg == NULL)
		return;

	printf("%s", vpkg->name);
	if (vpkg->relop) {
		printf(" ");
		print_relop(vpkg->relop);
		printf(" %d", vpkg->version);
	}
}

/* Print to stdout a list of package predicates, separated by a given
   separator */
void print_vpkglist(cudf_vpkglist_t l, const char *sep) {
	cudf_vpkg_t *vpkg;
	GList *last;

	last = g_list_last(l);
	while (l != NULL) {
		vpkg = g_list_nth_data(l, 0);
		print_vpkg(vpkg);
		if (l != last)
			printf(sep);
		l = g_list_next(l);
	}
}

/* Print to stdout a package formula */
void print_vpkgformula(cudf_vpkgformula_t fmla) {
	GList *last;

	last = g_list_last(fmla);
	while (fmla != NULL) {
		print_vpkglist(g_list_nth_data(fmla, 0), " | ");
		if (fmla != last)
			printf(", ");
		fmla = g_list_next(fmla);
	}
}

/* Print to stdout a CUDF preamble */
void print_preamble(cudf_doc_t *doc) {
	char *s;
	char *props[] = { "preamble", "property", "univ-checksum",
			  "status-checksum", "req-checksum" };
	int i;

	if (! doc->has_preamble)
		return;

	for (i=0 ; i<5 ; i++) {
		s = cudf_pre_property(doc->preamble, props[i]);
		printf("  %s: %s\n", props[i], s);
		free(s);
	}
}

/* Print to stdout a CUDF request */
void print_request(cudf_doc_t *doc) {
	char *s;

	if (! doc->has_request)
		return;

	s = cudf_req_property(doc->request, "request");
	printf("  request: %s\n", s);
	free(s);

	printf("  install: ");
	print_vpkglist(cudf_req_install(doc->request), ", ");
	printf("\n");

	printf("  upgrade: ");
	print_vpkglist(cudf_req_upgrade(doc->request), ", ");
	printf("\n");

	printf("  remove: ");
	print_vpkglist(cudf_req_remove(doc->request), ", ");
	printf("\n");
}

/* Print to stdout a possible value of the "keep" package property */
void print_keep(int keep) {
	switch (keep) {
	case KEEP_NONE : printf("  keep: version\n"); break;
	case KEEP_VERSION : printf("  keep: version\n"); break;
	case KEEP_PACKAGE : printf("  keep: package\n"); break;
	case KEEP_FEATURE : printf("  keep: feature\n"); break;
	default : g_error("Unexpected \"keep\" value: %d", keep);
	}
}

void print_value(cudf_value_t *v) {
	int typ;

	if (v == NULL)
		return;

	typ = v->typ;
	switch (typ) {
	case TYPE_INT :
	case TYPE_POSINT :
	case TYPE_NAT :
		printf("%d", v->val.i);
		break;
	case TYPE_BOOL :
		printf("%s", v->val.i ? "true" : "false");
		break;
	case TYPE_STRING :
	case TYPE_PKGNAME :
	case TYPE_IDENT :
	case TYPE_ENUM :
		printf("%s", v->val.s);
		break;
	case TYPE_VPKG :
	case TYPE_VEQPKG :
		print_vpkg(v->val.vpkg);
		break;
	case TYPE_VPKGLIST :
	case TYPE_VEQPKGLIST :
		print_vpkglist(v->val.vpkgs, ", ");
		break;
	case TYPE_VPKGFORMULA :
		print_vpkgformula(v->val.f);
		break;
	case TYPE_TYPEDECL :
		break;
	default :
		g_error("Internal error: unexpected variant for type: %d", typ);
	}
}

/* Print to stdout a generic property, i.e. a pair <name, typed value> */
void print_property(gpointer k, gpointer v, gpointer user_data) {
	printf("  %s: ", (char *) k);
	print_value(v);
	printf("\n");
}

/* Print to stdout a set of extra properties */
#define print_extra(e)	(g_hash_table_foreach(e, print_property, NULL))

/* Print to stdout a CUDF package */
void print_pkg(cudf_package_t pkg) {
	cudf_vpkgformula_t fmla;
	cudf_vpkglist_t vpkglist;
	cudf_extra_t extra;

	printf("  package: %s\n", cudf_pkg_name(pkg));
	printf("  version: %d\n", cudf_pkg_version(pkg));
	printf("  installed: %s\n",
	       cudf_pkg_installed(pkg) ? "true" : "false");
	printf("  was-installed: %s\n",
	       cudf_pkg_was_installed(pkg) ? "true" : "false");

	fmla = cudf_pkg_depends(pkg);
	printf("  depends: ");
	print_vpkgformula(fmla);
	printf("\n");
	cudf_free_vpkgformula(fmla);

	vpkglist = cudf_pkg_conflicts(pkg);	/* conflicts */
	printf("  conflicts: ");
	print_vpkglist(vpkglist, ", ");
	printf("\n");
	cudf_free_vpkglist(vpkglist);

	vpkglist = cudf_pkg_provides(pkg);	/* provides */
	printf("  provides: ");
	print_vpkglist(vpkglist, ", ");
	printf("\n");
	cudf_free_vpkglist(vpkglist);

	print_keep(cudf_pkg_keep(pkg));		/* keep */

	extra = cudf_pkg_extra(pkg);		/* extra properties */
	print_extra(extra);
	printf("\n");
	cudf_free_extra(extra);
}

int main(int argc, char **argv) {
	cudf_doc_t *doc = NULL;
	cudf_t *cudf = NULL, *sol = NULL;
	cudf_package_t pkg;
	cudf_universe_t univ = NULL;
	GList *l = NULL;

	cudf_init();
	if (argc < 2) {
		printf("Usage: %s CUDF_FILE [ SOLUTION_FILE ]\n", argv[0]);
		exit(2);
	}

	g_message("Parsing CUDF document %s ...", argv[1]);
	doc = cudf_parse_from_file(argv[1]);
	printf("Has preamble: %s\n", doc->has_preamble ? "yes" : "no");
	if (doc->has_preamble) {
		printf("Preamble: \n");
		print_preamble(doc);
		printf("\n");
	}
	printf("Has request: %s\n", doc->has_request ? "yes" : "no");
	if (doc->has_request) {
		printf("Request: \n");
		print_request(doc);
		printf("\n");
	}

	printf("Universe:\n");
	l = doc->packages;
	while (l != NULL) {
		pkg = (cudf_package_t) g_list_nth_data(l, 0);
		print_pkg(pkg);
		l = g_list_next(l);
	}
	g_message("Try packages -> universe conversion ...");
	univ = cudf_load_universe(doc->packages);
	printf("Universe size: %d/%d (installed/total)\n",
	       cudf_installed_size(univ), cudf_universe_size(univ));
	printf("Universe consistent: %s\n", cudf_is_consistent(univ) ?
	       "yes" : "no");

	g_message("Freeing memory ...");
	cudf_free_universe(univ);
	cudf_free_doc(doc);

	g_message("Try direct CUDF loading ...");
	cudf = cudf_load_from_file(argv[1]);
	printf("Universe size: %d/%d (installed/total)\n",
	       cudf_installed_size(cudf->universe),
	       cudf_universe_size(cudf->universe));
	printf("Universe consistent: %s\n",
	       cudf_is_consistent(cudf->universe) ? "yes" : "no");
	if (argc >= 3) {
		g_message("Loading solution %s ...", argv[2]);
		sol = cudf_load_solution_from_file(argv[2], cudf->universe);
		printf("Is solution: %s\n",
		       cudf_is_solution(cudf, sol->universe) ? "yes" : "no");
	}
	g_message("Freeing memory (direct loading)...");
	cudf_free_cudf(sol);
	cudf_free_cudf(cudf);
	g_message("All done.");

	exit(0);
}
