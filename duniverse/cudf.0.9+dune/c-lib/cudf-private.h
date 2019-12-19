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

#ifndef _CUDF_PRIVATE_H
#define _CUDF_PRIVATE_H

/* Instantiation of OCaml-related abstract data types. To be used only libcudf
   implementation itself. Rationale: all use of OCaml's values from C is very
   subtle and should be avoided in final C applications; additionally, hiding
   "value" type removes the need of having OCaml C headers installed. */

typedef value *cudf_preamble_t;
typedef value *cudf_request_t;
typedef value *cudf_universe_t;
typedef value *cudf_package_t;

#endif
