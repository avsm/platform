Summary: CUDF (Common Upgradeability Description Format) tools and libraries
Name: cudf
Version: 0.6
Release: 1
Source: https://gforge.inria.fr/frs/?group_id=4385
URL: http://www.mancoosi.org/cudf/
License: LGPL
Group: Development/Libraries
BuildRequires: ocaml ocaml-findlib ocaml-extlib-devel
BuildRoot: %{_tmppath}/%{name}-root

%description
CUDF (for Common Upgradeability Description Format) is a format for describing
upgrade scenarios in package-based Free and Open Source Software distribution.

libCUDF is a library to manipulate so called CUDF documents. A CUDF document
describe an upgrade problem, as faced by package managers in popular
package-based GNU/Linux distributions.

%package tools
Summary: CUDF (Common Upgradeability Description Format) command-line tools

%description tools
CUDF (for Common Upgradeability Description Format) is a format for describing
upgrade scenarios in package-based Free and Open Source Software distribution.

libCUDF is a library to manipulate so called CUDF documents. A CUDF document
describe an upgrade problem, as faced by package managers in popular
package-based GNU/Linux distributions.

This package contains command line tools to manipulate CUDF and related
documents. In particular it contains cudf-check, which enables checking of
document properties such as installation consistency and matching of problems
with their solutions.

%package devel
Summary: CUDF (Common Upgradeability Description Format) C development stuff

%description devel
CUDF (for Common Upgradeability Description Format) is a format for describing
upgrade scenarios in package-based Free and Open Source Software distribution.

libCUDF is a library to manipulate so called CUDF documents. A CUDF document
describe an upgrade problem, as faced by package managers in popular
package-based GNU/Linux distributions.

This package contains the development stuff needed to use libCUDF in your C
programs.

%package ocaml-devel
Summary: CUDF (Common Upgradeability Description Format) OCaml development stuff

%description ocaml-devel
CUDF (for Common Upgradeability Description Format) is a format for describing
upgrade scenarios in package-based Free and Open Source Software distribution.

libCUDF is a library to manipulate so called CUDF documents. A CUDF document
describe an upgrade problem, as faced by package managers in popular
package-based GNU/Linux distributions.

This package contains the development stuff needed to use libCUDF in your OCaml
programs.

%prep
%setup -q

%build
make all c-lib
which /usr/bin/ocamlopt > /dev/null && make opt c-lib-opt

%install
rm -rf "$RPM_BUILD_ROOT"
make install				\
    DESTDIR="$RPM_BUILD_ROOT"		\
    LIBDIR="%{_libdir}"			\
    OCAMLLIBDIR="%{_libdir}/ocaml"

%check
make test

%clean
rm -rf "$RPM_BUILD_ROOT"

%files tools
%defattr(-,root,root)
%{_bindir}/cudf-check
%{_bindir}/cudf-parse-822

%files devel
%defattr(-,root,root)
%{_includedir}/cudf.h
%{_libdir}/*.a
%{_libdir}/pkgconfig/cudf.pc

%files ocaml-devel
%defattr(-,root,root)
%{_libdir}/ocaml/cudf

%changelog
* Tue Dec 22 2009 Stefano Zacchiroli <zack@pps.jussieu.fr>
- use default rpm installation paths (in particular, /usr/lib64 on x86_64)

* Sat Dec 19 2009 Stefano Zacchiroli <zack@pps.jussieu.fr>
- various adjustments (deps, description, native code, ...)

* Fri Dec 18 2009 Jeff Johnson <jbj@rpm5.org>
- create.
