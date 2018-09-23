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

#define _GNU_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include <rpm/rpmtypes.h>
#include <rpm/rpmlib.h>
#include <rpmtag.h>
#include <rpm/header.h>
#include <rpm/rpmfi.h>
#include <rpm/rpmts.h>

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

value get_filedeps(Header h) {
  CAMLparam0 ();
  const char *fname;
  CAMLlocal2( hd, tl );
  tl = Val_emptylist;
  rpmts ts = rpmtsCreate();
  rpmfi fi = rpmfiNew(ts, h, RPMTAG_BASENAMES, RPMFI_NOHEADER);
  while (rpmfiNext(fi) != -1) {
    fname = rpmfiFN(fi);
    hd = caml_copy_string(fname);
    tl = append(hd,tl);
  }
  rpmfiFree(fi);
  CAMLreturn(list_variant_L(tl));
}

#define fd_val(v) ((FD_t)(Field((v), 0)))

CAMLprim value rpm_parse_paragraph (value fd) {
  char *s;
  CAMLparam1 ( fd );
  CAMLlocal2 ( hd, tl );
  CAMLlocal2 ( k, v);
  
  FD_t _fd = fd_val(fd);
  Header h;

  if ((h = headerRead(_fd, HEADER_MAGIC_YES)) == NULL) raise_Eof();

  tl = Val_emptylist;

  k = caml_copy_string("Package");
  s = headerGetAsString(h,RPMTAG_NAME);
  v = caml_copy_string(s);
  hd = tuple(k,string_variant(v));
  tl = append(hd,tl);
/*
  uint32_t e;
  k = caml_copy_string("Epoch");
  e = headerGetNumber(h, RPMTAG_EPOCH);
  (void) sprintf(s,"%d",e);
  v = caml_copy_string(s);
  hd = tuple(k,string_variant(v));
  tl = append(hd,tl);

//this is a mandriva specific tag...
#define RPMTAG_DISTEPOCH 1218
  k = caml_copy_string("DistEpoch");
  e = headerGetNumber(h, RPMTAG_DISTEPOCH);
  (void) sprintf(s,"%d",e);
  v = caml_copy_string(s);
  hd = tuple(k,string_variant(v));
  tl = append(hd,tl);

  k = caml_copy_string("VersionString");
  s = headerGetAsString(h, RPMTAG_VERSION);
  v = caml_copy_string(s);
  free(s);
  hd = tuple(k,string_variant(v));
  tl = append(hd,tl);

  k = caml_copy_string("Release");
  s = headerGetAsString(h, RPMTAG_RELEASE);
  v = caml_copy_string(s);
  hd = tuple(k,string_variant(v));
  tl = append(hd,tl);
*/
  k = caml_copy_string("Version");
  s = headerGetAsString(h, RPMTAG_EVR);
  v = caml_copy_string(s);
  hd = tuple(k,string_variant(v));
  tl = append(hd,tl);

  k = caml_copy_string("Architecture");
  s = headerGetAsString(h,RPMTAG_ARCH);
  v = caml_copy_string(s);
  hd = tuple(k,string_variant(v));
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
  v = get_deps(h,RPMTAG_SUGGESTS);
  hd = tuple(k,list_variant_D(v));
  tl = append(hd,tl);

  k = caml_copy_string("Enhances");
  v = get_deps(h,RPMTAG_ENHANCES);
  hd = tuple(k,list_variant_D(v));
  tl = append(hd,tl);

  k = caml_copy_string("Conflicts");
  v = get_deps(h,RPMTAG_CONFLICTNAME);
  hd = tuple(k,list_variant_D(v));
  tl = append(hd,tl);

  k = caml_copy_string("Obsoletes");
  v = get_deps(h,RPMTAG_OBSOLETENAME);
  hd = tuple(k,list_variant_D(v));
  tl = append(hd,tl);

  k = caml_copy_string("Files");
  v = get_filedeps(h);
  hd = tuple(k,v);
  tl = append(hd,tl);

  if (h != NULL) (void) headerFree (h);

  CAMLreturn(tl);
}

CAMLprim value rpm_open_hdlist (value file_name) {
  CAMLparam1 (file_name);
  CAMLlocal1 (result);
  FD_t fd;

  fd = Fopen (String_val (file_name), "r");
  if (!fd) caml_failwith (strerror (errno));

  result = alloc_small(1, Abstract_tag);
  Field(result, 0) = (value) fd;

  CAMLreturn(result);
}

CAMLprim value rpm_close_hdlist (value fd) {
  CAMLparam1 (fd);
  Fclose (fd_val(fd));
  CAMLreturn(Val_unit);
}

CAMLprim value rpm_vercmp ( value x, value y ) {
  CAMLparam2 ( x , y );
  CAMLlocal1 ( res );
  res = rpmvercmp ( (char *) x , (char *) y );
  CAMLreturn (Val_int(res));
}

/*
value rpm_EVRcmp ( value x, value y ) {
  CAMLparam2 ( x , y );
  CAMLlocal1 ( res );
  res = rpmEVRcmp ( (char *) x , (char *) y );
  CAMLreturn (Val_int(res));
}

int rpmVersionCompare(Header first, Header second)
{
    // Missing epoch becomes zero here, which is what we want
    uint32_t epochOne = headerGetNumber(first, RPMTAG_EPOCH);
    uint32_t epochTwo = headerGetNumber(second, RPMTAG_EPOCH);
    int rc;

    if (epochOne < epochTwo)
        return -1;
    else if (epochOne > epochTwo)
        return 1;

    rc = rpmvercmp(headerGetString(first, RPMTAG_VERSION),
                   headerGetString(second, RPMTAG_VERSION));
    if (rc)
        return rc;

    return rpmvercmp(headerGetString(first, RPMTAG_RELEASE),
                     headerGetString(second, RPMTAG_RELEASE));
}
*/
/*
CAMLprim value rpm_parseEVR ( value s ) {
  CAMLparam1 ( s );
  CAMLlocal3 ( e, v , r );
  CAMLlocal1( res );
  char *aEVR = (char *) s;
  const char *aE, *aV, *aR;

  parseEVR(aEVR, &aE, &aV, &aR);
  e = caml_copy_string(aE);
  v = caml_copy_string(aV);
  r = caml_copy_string(aR);

  res = caml_alloc_tuple(3);
  Store_field (res, 0, e);
  Store_field (res, 1, v);
  Store_field (res, 2, r);

  CAMLreturn (res);
}
*/
