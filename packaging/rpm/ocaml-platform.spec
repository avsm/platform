# OCaml has a bytecode backend that works on anything with a C
# compiler, and a native code backend available on a subset of
# architectures.  A further subset of architectures support native
# dynamic linking.

Name:           ocaml-platform
Version:        0.1
Release:        1%{?dist}

Summary:        OCaml compiler and programming environment

License:        QPL and (LGPLv2+ with exceptions)

URL:            http://www.ocaml.org

Source0:        ocaml-platform.tar.gz

BuildRequires:  gcc

Requires:       gcc

%description
OCaml is a high-level, strongly-typed, functional and object-oriented
programming language from the ML family of languages.

This package comprises two batch compilers (a fast bytecode compiler
and an optimizing native-code compiler), an interactive toplevel system,
parsing tools (Lex,Yacc), a replay debugger, a documentation generator,
and a comprehensive library.

%prep
%setup -q

%build
make

%install
mkdir -p $RPM_BUILD_ROOT/usr/bin
cp _build/default/output/* $RPM_BUILD_ROOT/%{_bindir}

%files
%{_bindir}/dune
%{_bindir}/jbuilder
%{_bindir}/ocamlmerlin
%{_bindir}/odoc
%{_bindir}/opam-installer
%{_bindir}/mdx
%{_bindir}/dune-release
%{_bindir}/ocamlformat
%{_bindir}/ocamlmerlin-server
%{_bindir}/opam
%{_bindir}/utop

%changelog
* Fri Aug 31 2018 Jon Ludlam <jonathan.ludlam@citrix.com> - 0.1-1
- Initial package
