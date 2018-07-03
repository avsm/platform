/**************************************************************************************/
/*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     */
/*  Copyright (C) 2009 Mancoosi Project                                               */
/*                                                                                    */
/*  This library is free software: you can redistribute it and/or modify              */
/*  it under the terms of the GNU Lesser General Public License as                    */
/*  published by the Free Software Foundation, either version 3 of the                */
/*  License, or (at your option) any later version.  A special linking                */
/*  exception to the GNU Lesser General Public License applies to this                */
/*  library, see the COPYING file for more information.                               */
/**************************************************************************************/

#include <stdint.h>
#include <errno.h>


#include <rpmtypes.h>
#include <rpmio.h>
#include <rpmtag.h>
#include <rpmdb.h>

#define _RPMEVR_INTERNAL
#include <rpmevr.h>

/* ocamlc sets this variable but it is not compatible 
 * with fts.h */
#undef __USE_FILE_OFFSET64 
#define _RPMGI_INTERNAL
#include <rpmgi.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

#define Val_none Val_int(0)

void raise_Eof () {
  static value * exn = NULL;
  if (exn == NULL)
      exn = caml_named_value ("hdlist.eof");
  raise_constant (*exn);
}

#define VARIANT_S 0
#define VARIANT_L 1
#define VARIANT_D 2

CAMLprim value Val_some( value v ) {
  CAMLparam1( v );
  CAMLlocal1( some );
  some = caml_alloc(1, 0);
  Store_field( some, 0, v );
  CAMLreturn(some);
}

CAMLprim value tuple( value a, value b) {
  CAMLparam2( a, b );
  CAMLlocal1( res );
  res = caml_alloc_tuple(2);
  Store_field (res, 0, a);
  Store_field (res, 1, b);
  CAMLreturn(res);
}

CAMLprim value append( value hd, value tl ) {
  CAMLparam2( hd , tl );
  CAMLlocal1( res );
  res = tuple( hd, tl );
  CAMLreturn(res);
}

CAMLprim value string_variant(value s) {
  CAMLparam1( s );
  CAMLlocal1( v );
  v = caml_alloc(1, VARIANT_S);
  Store_field(v, 0, s);
  CAMLreturn(v);
}

CAMLprim value list_variant_L(value l) {
  CAMLparam1( l );
  CAMLlocal1( v );
  v = caml_alloc(1, VARIANT_L);
  Store_field(v, 0, l);
  CAMLreturn(v);
}

CAMLprim value list_variant_D(value l) {
  CAMLparam1( l );
  CAMLlocal1( v );
  v = caml_alloc(1, VARIANT_D);
  Store_field(v, 0, l);
  CAMLreturn(v);
}

CAMLprim value get_filedeps(Header h) {
  CAMLparam0 ();
  CAMLlocal2( hd, tl );
  CAMLlocal1 ( tmp );
  tl = Val_emptylist;

  HE_t he = (HE_t)memset(alloca(sizeof(*he)), 0, sizeof(*he));
  he->tag = RPMTAG_FILEPATHS;
  if(headerGet(h, he, 0)) {
    int i = 0;
    while (i < he->c) {
      tmp = caml_copy_string(he->p.argv[i]);
      tl = append(tmp,tl);
      i++;
    }
  }

  CAMLreturn(tl);
}

CAMLprim value get_deps(Header h, rpmTag tag) {
  CAMLparam0 ();
  CAMLlocal4( hd, tl, constr, t );
  CAMLlocal1( tmp );
  const char *name, *version;
  rpmsenseFlags flag;

  tl = Val_emptylist;
  constr = Val_none;
  const rpmds deps = rpmdsNew(h, tag, 0);
  while (rpmdsNext(deps) != -1) {

    flag = rpmdsFlags(deps);
    if (!(flag & RPMSENSE_RPMLIB) && !(flag & RPMSENSE_MISSINGOK)) {
      name = rpmdsN(deps);
      constr = Val_none;
      if ((flag & RPMSENSE_EQUAL) || 
          (flag & RPMSENSE_LESS) ||
          (flag & RPMSENSE_GREATER)) {
        if ((version = rpmdsEVR(deps)) != NULL) {
          tmp = caml_copy_string(version);
          t = tuple(Val_int(flag),tmp);
          constr = Val_some(t);
        }
      }
      tmp = caml_copy_string(name);
      hd = tuple(tmp,constr);
      tl = append(hd,tl);
    }
  };
  (void) rpmdsFree(deps);

  CAMLreturn(tl);
}

char * headerGetEVR(Header h) {
    const char * V = NULL;
    const char * R = NULL;
    size_t nb = 0;
    char * EVR, * t;
    char E[10];

    HE_t he = (HE_t)memset(alloca(sizeof(*he)), 0, sizeof(*he));

    he->tag = RPMTAG_EPOCH;
    (void) headerGet(h, he, 0);
    rpmuint32_t epoch = (he->p.ui32p ? he->p.ui32p[0] : 0);
    (void) _free(he->p.ptr);
    sprintf(E,"%d:",epoch);

    (void) headerNEVRA(h, NULL, NULL, &V, &R, NULL);

    //if (E)      nb += strlen(E) + 1;
    nb += strlen(E) + 1;
    if (V)      nb += strlen(V);
    if (R)      nb += strlen(R) + 1;

    nb++;
    EVR = t = malloc(nb);
    *t = '\0';
    //if (E)      t = stpcpy( stpcpy(t, E), ":");
    t = stpcpy( t, E);
    if (V)      t = stpcpy( t, V);
    if (R)      t = stpcpy( stpcpy(t, "-"), R);

    // E = _free(E);
    V = _free(V);
    R = _free(R);
    return EVR;
}

#define gi_val(v) ((rpmgi)(Field((v), 0)))

value rpm_parse_paragraph (value _gi) {
  CAMLparam1 ( _gi );
  CAMLlocal2 ( hd, tl );
  CAMLlocal2 ( h1, t1 );
  CAMLlocal2 ( k, v );
  CAMLlocal1 ( tmp );
  tl = Val_emptylist;

  rpmgi gi = gi_val(_gi);

  if ((rpmgiNext(gi)) != RPMRC_OK) raise_Eof();
  Header h = rpmgiHeader(gi);
  HE_t he = (HE_t)memset(alloca(sizeof(*he)), 0, sizeof(*he));

  he->tag = RPMTAG_NAME;
  (void) headerGet(h, he, 0);
  k = caml_copy_string("Package");
  tmp = caml_copy_string(he->p.str);
  v = string_variant(tmp);
  hd = tuple(k,v);
  tl = append(hd,tl);
/*
  k = caml_copy_string("Version");
  tmp = caml_copy_string(headerGetEVR(h));
  hd = tuple(k,string_variant(tmp));
  tl = append(hd,tl);
*/
  k = caml_copy_string("Version");
  tmp = caml_copy_string(headerGetEVR(h));
  hd = tuple(k,string_variant(tmp));
  tl = append(hd,tl);

  he->tag = RPMTAG_ARCH;
  (void) headerGet(h, he, 0);
  k = caml_copy_string("Architecture");
  tmp = caml_copy_string(he->p.str);
  v = string_variant(caml_copy_string(he->p.str));
  hd = tuple(k,v);
  tl = append(hd,tl);

  k = caml_copy_string("Requires");
  v = get_deps(h,RPMTAG_REQUIRENAME);
  hd = tuple(k,list_variant_D(v));
  tl = append(hd,tl);

  k = caml_copy_string("Provides");
  v = get_deps(h,RPMTAG_PROVIDENAME);
  hd = tuple(k,list_variant_D(v));
  tl = append(hd,tl);

  k = caml_copy_string("Suggests");
  v = get_deps(h,RPMTAG_SUGGESTSNAME);
  hd = tuple(k,list_variant_D(v));
  tl = append(hd,tl);

  k = caml_copy_string("Enhances");
  v = get_deps(h,RPMTAG_ENHANCESNAME);
  hd = tuple(k,list_variant_D(v));
  tl = append(hd,tl);

  k = caml_copy_string("Conflicts");
  v = get_deps(h,RPMTAG_CONFLICTNAME);
  hd = tuple(k,list_variant_D(v));
  tl = append(hd,tl);

  // XXX add Linktos ???
  k = caml_copy_string("Files");
  v = get_filedeps(h);
  hd = tuple(k,list_variant_L(v));
  tl = append(hd,tl);

/*
  // XXX this part should be in a separate function !!
  t1 = Val_emptylist;
  he = (HE_t)memset(alloca(sizeof(*he)), 0, sizeof(*he));
  he->tag = RPMTAG_FILEPATHS;
  if(headerGet(h, he, 0)) {
    int i = 0;
    while (i < he->c) {
      h1 = caml_copy_string(he->p.argv[i]);
      t1 = append(h1,t1);
      i++;
    }
  }

  hd = tuple(k,list_variant_L(t1));
  tl = append(hd,tl);
*/

  CAMLreturn(tl);
}

value rpm_open_hdlist (value file_name) {
  CAMLparam1 (file_name);
  CAMLlocal1 (result);
  rpmgi gi = NULL;
  rpmts ts = NULL;
  FD_t fd;

  fd = Fopen (String_val (file_name), "r");
  if (!fd) caml_failwith (strerror (errno));

  ts = rpmtsCreate();
  rpmtsSetRootDir(ts, NULL);
  gi = rpmgiNew(ts, RPMDBI_HDLIST, NULL, 0);

  rpmtsSetVSFlags(ts,
      _RPMVSF_NOSIGNATURES |
      RPMVSF_NOHDRCHK |
      _RPMVSF_NOPAYLOAD |
      _RPMVSF_NOHEADER);
  gi->active = 1;
  gi->fd = fd;

  caml_register_generational_global_root(&result);
  result = caml_alloc_small(1, Abstract_tag);
  Field(result, 0) = (value) gi;

  CAMLreturn(result);
}

value rpm_close_hdlist (value _gi) {
  CAMLparam1 (_gi);
  (void) rpmgiFree(gi_val(_gi));
  caml_remove_generational_global_root(&_gi);
  CAMLreturn(Val_unit);
}

value rpm_vercmp ( value x, value y ) {
  CAMLparam2 ( x , y );
  CAMLlocal1 ( res );
  res = rpmvercmp ( (char *) x , (char *) y );
  CAMLreturn (Val_int(res));
}

value rpm_EVRcmp ( value x, value y ) {
  CAMLparam2 ( x , y );
  CAMLlocal1 ( res );
  res = rpmEVRcmp ( (char *) x , (char *) y );
  CAMLreturn (Val_int(res));
}
