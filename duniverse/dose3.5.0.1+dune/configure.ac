AC_INIT(dose3, 5.0.1, pietro.abate@inria.fr)
AC_COPYRIGHT(Copyright 2009-2016 Mancoosi Project)

AC_CONFIG_MACRO_DIR([m4])

AC_CHECK_OCAML_OS_TYPE
if test "$OCAMLC" = "no"; then
 AC_MSG_ERROR([You must install the OCaml compiler to compile dose3])
fi

AX_COMPARE_VERSION([$OCAMLVERSION], [lt], [3.12.1],
  AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported]))

AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.00.0])
AC_SUBST(OCAML_4,"$ax_compare_version")
AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.01.0])
AC_SUBST(OCAML_4_01,"$ax_compare_version")
AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.02.0])
AC_SUBST(OCAML_4_02,"$ax_compare_version")

AC_CHECK_PROG(CPPO_CHECK,cppo,yes)
if test x"$CPPO_CHECK" != x"yes" ; then
    AC_MSG_ERROR([You must install the cppo pre-processor to compile dose3])
fi
# this option cause problems with old versions of cppo
CPPOFLAGS="-V OCAML:$OCAMLVERSION"

AC_PROG_OCAMLLEX
if test "$OCAMLEX" = "no"; then
  AC_MSG_ERROR([Cannot find ocamllex])
fi

AC_PROG_OCAMLYACC
if test "$OCAMYACC" = "no"; then
  AC_MSG_ERROR([Cannot find ocamlyacc])
fi

AC_PROG_FINDLIB
AC_SUBST(OCAMLFIND)
if test "$OCAMLFIND" = "no"; then
 AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_ARG_WITH(bytecodeonly,
        [ --with-bytecodeonly ],
        OCAMLBEST=byte,
        OCAMLBEST="$OCAMLBEST")
if test "$OCAMLBEST" = "byte" ; then
  OCAMLEXT=byte
  OCAMLOPT=
  AC_SUBST(OCAMLOPT)
else
  OCAMLEXT=native
fi

#defaults
USE_RPM=use_
LINK_RPM=link_
HAS_RPM=no
PKG_BZ2=pkg_
PKG_ZIP=pkg_
PKG_PARMAP=pkg_
PKG_XML=pkg_
PKG_CUDF=use_cudf
PKG_CURL=pkg_
PKG_OUNIT=pkg_
PKG_BENCHMARK=pkg_
DOC_RPM=""

AC_ARG_WITH(libcudf,
        [ --without-libcudf ],
        HAS_LIBCUDF=no,
        HAS_LIBCUDF=yes)
AC_SUBST(HAS_LIBCUDF)
if test "$HAS_LIBCUDF" = "yes" ; then
  AC_CHECK_OCAML_PKG([cudf])
  if (test "$OCAML_PKG_cudf" = "no"); then
    AC_MSG_NOTICE([you asked to use an external libcudf, but ocamlfind cannot find it])
    AC_MSG_ERROR([Please specify --without-libcudf to compile cudf locally])
    PKG_CUDF=use_cudf
  else
    PKG_CUDF="package(cudf)"
  fi
else
  if test -f "cudf/cudf.mli" ; then
    AC_MSG_NOTICE([using local libcudf])
  else
    AC_MSG_ERROR([you asked to compile cudf locally, but the cudf submodule is not present])
  fi
fi

AC_CHECK_OCAML_PKG([extlib])
if test "$OCAML_PKG_extlib" = "no"; then
 AC_MSG_ERROR([Please install OCaml findlib module 'extlib'.])
fi

AC_CHECK_OCAML_PKG([re.pcre])
if test "$OCAML_PKG_re.pcre" = "no"; then
 AC_MSG_ERROR([Please install OCaml findlib module 're.pcre'.])
fi

AC_CHECK_OCAML_PKG([ocamlgraph])
if test "$OCAML_PKG_ocamlgraph" = "no"; then
 AC_MSG_ERROR([Please install OCaml findlib module 'ocamlgraph'.])
fi
TMPVERSION=`$OCAMLFIND query -format %v ocamlgraph | sed 's/\.//g'`
CONFIG_OCAMLGRAPH="-D 'OCAMLGRAPHVERSION $TMPVERSION'"

AC_ARG_WITH(parmap,
        [ --with-parmap ],
        HAS_PARMAP=yes,
        HAS_PARMAP=no)
AC_SUBST(HAS_PARMAP)
if test "$HAS_PARMAP" = "yes" ; then
  AC_CHECK_OCAML_PKG([parmap])
  if test "$OCAML_PKG_parmap" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'parmap'.])
  fi
  CONFIG_PARMAP="-D HASPARMAP"
  PKG_PARMAP="package(parmap)"
  PARMAP=parmap
fi

AC_ARG_WITH(zip,
        [ --with-zip ],
        HAS_ZIP=yes,
        HAS_ZIP=no)
AC_SUBST(HAS_ZIP)
if test "$HAS_ZIP" = "yes" ; then
  AC_CHECK_OCAML_PKG([zip])
  if test "$OCAML_PKG_zip" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'zip'.])
  fi
  CONFIG_ZIP="-D HASZIP"
  PKG_ZIP="package(zip)"
  ZIP=zip
fi

AC_ARG_WITH(bz2,
        [ --with-bz2 ],
        HAS_BZ2=yes,
        HAS_BZ2=no)
AC_SUBST(HAS_BZ2)
if test "$HAS_BZ2" = "yes" ; then
  AC_CHECK_OCAML_PKG([bz2])
  if test "$OCAML_PKG_bz2" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'bz2'.])
  fi
  CONFIG_BZ2="-D HASBZ2"
  PKG_BZ2="package(bz2)"
  BZ2=bz2
fi

AC_ARG_WITH(curl,
        [ --with-curl ],
        HAS_CURL=yes,
        HAS_CURL=no)
AC_SUBST(HAS_CURL)
if test "$HAS_CURL" = "yes" ; then
  AC_CHECK_OCAML_PKG([curl])
  if test "$OCAML_PKG_curl" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'curl'.])
  fi
  PKG_CURL="package(curl)"
fi

AC_ARG_WITH(xml,
        [ --with-xml ],
        HAS_XML=yes,
        HAS_XML=no)
AC_SUBST(HAS_XML)
if test "$HAS_XML" = "yes" ; then
  AC_CHECK_OCAML_PKG([expat])
  if test "$OCAML_PKG_expat" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'expat'.])
  fi
  AC_CHECK_OCAML_PKG([xml-light])
  if test "$OCAML_PKG_xml-light" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'xml-light'.])
  fi
  PKG_XML="package(expat), package(xml-light)"
fi

AC_ARG_WITH(oUnit,
        [ --with-oUnit ],
        HAS_OUNIT=yes,
        HAS_OUNIT=no)
AC_SUBST(HAS_OUNIT)
if test "$HAS_OUNIT" = "yes" ; then
  AC_CHECK_OCAML_PKG([oUnit])
  if test "$OCAML_PKG_oUnit" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'oUnit'.])
  fi
  PKG_OUNIT="pkg_oUnit"
fi

AC_ARG_WITH(benchmark,
        [ --with-benchmark ],
        HAS_BENCHMARK=yes,
        HAS_BENCHMARK=no)
AC_SUBST(HAS_BENCHMARK)
if test "$HAS_BENCHMARK" = "yes" ; then
  AC_CHECK_OCAML_PKG([benchmark])
  if test "$OCAML_PKG_benchmark" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'benchmark'.])
  fi
  PKG_BENCHMARK="pkg_benchmark"
fi

AS_IF([test "$OCAMLBEST" = "opt"],[OCAMLBESTCC=$OCAMLOPT],[OCAMLBESTCC=$OCAMLC])
OCAML_CC="$($OCAMLBESTCC -config | fgrep native_c_compiler | sed -e "s/native_c_compiler: \(.*\) .*/\1/")"
AC_PROG_CC(["${OCAML_CC}" gcc cl cc])

AC_HEADER_STDC

RPMPATH="-I/usr/include/rpm"
CFLAGS="$CFLAGS $RPMPATH"
CPPFLAGS="$CPPFLAGS $RPMPATH"

AC_ARG_WITH(rpm4,
        [ --with-rpm4 ],
        HAS_RPM4=yes,
        HAS_RPM4=no)
if test "$HAS_RPM4" = "yes" ; then
  AC_MSG_NOTICE([RPM support enabled])
  AC_CHECK_HEADERS([rpm/rpmio.h],,AC_MSG_ERROR([missing rpmio.h]))
  AC_CHECK_HEADERS([rpm/rpmtypes.h],,AC_MSG_ERROR([missing rpmtypes.h]))
  CONFIG_RPM="-D HASRPM"
  USE_RPM="use_rpm, use_rpm4"
  DOC_RPM="#rpm/Packages
  #rpm/Hdlists
  #rpm/Version
  #rpm/Rpmcudf"
  LINK_RPM="link_rpm4"
  HAS_RPM=yes
fi

AC_ARG_WITH(rpm5,
        [ --with-rpm5 ],
        HAS_RPM5=yes,
        HAS_RPM5=no)
if test "$HAS_RPM5" = "yes"; then
  AC_MSG_NOTICE([RPM 5 support enabled])
  CFLAGS="-I/usr/include/rpm"
  LDFLAG="-Lrpm -Lrpmio"
  AC_CHECK_HEADERS([rpm/rpmio.h],,AC_MSG_ERROR([missing rpmio.h]))
  CONFIG_RPM="-D HASRPM"
  USE_RPM="use_rpm, use_rpm5"
  DOC_RPM="#rpm/Packages
  #rpm/Hdlists
  #rpm/Version
  #rpm/Rpmcudf"
  LINK_RPM="link_rpm5"
  HAS_RPM=yes
fi

OCAML_SYSTEM="$($OCAMLBESTCC -config | fgrep system | sed -e "s/system: \(.*\)/\1/")"
AC_SUBST(OCAML_SYSTEM)

AS_IF([test "${OCAML_OS_TYPE}" = "Win32"],[
  AC_MSG_CHECKING([for a workable solution for ln -s])
  ln -s configure conftestLink
  AS_IF([test "`cmd /c dir conftestLink 2>/dev/null | fgrep SYMLINK`" = ""],[LN=cp],[LN="ln -s"])
  AC_MSG_RESULT([$LN])
],[
  LN="ln -s"
])
AC_SUBST(LN)

AC_SUBST(CPPOFLAGS)
AC_SUBST(CONFIG_ZIP)
AC_SUBST(PKG_ZIP)
AC_SUBST(ZIP)

AC_SUBST(CONFIG_PARMAP)
AC_SUBST(PKG_PARMAP)
AC_SUBST(PARMAP)

AC_SUBST(CONFIG_BZ2)
AC_SUBST(PKG_BZ2)
AC_SUBST(BZ2)

AC_SUBST(LINK_RPM)
AC_SUBST(CONFIG_RPM)
AC_SUBST(INCLUDE_RPM)
AC_SUBST(USE_RPM)
AC_SUBST(LINK_RPM)
AC_SUBST(HAS_RPM)
AC_SUBST(DOC_RPM)

AC_SUBST(INCLUDE_CUDF)
AC_SUBST(PKG_CUDF)

AC_SUBST(CONFIG_OCAMLGRAPH)

AC_SUBST(PKG_CURL)

AC_SUBST(PKG_XML)

LDFLAGS="-fstack-protector"

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(LDFLAGS)

AC_SUBST(OCAMLEXT)

AC_ARG_WITH(experimental,
        [ --with-experimental ],
        BUILD_EXP=yes,
        BUILD_EXP=no)
if test "$BUILD_EXP" = "yes" ; then
  AC_CONFIG_FILES([
    experimental/Makefile
  ])
fi

AC_CONFIG_FILES([
  Makefile.config
  _tags
  META
  dose3.odocl
  common/versionInfo.ml
  applications/dose-tests.list
])
AC_OUTPUT

make printconf
