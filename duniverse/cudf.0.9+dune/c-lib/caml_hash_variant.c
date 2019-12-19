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

#include <caml/mlvalues.h>
#include <stdio.h>

int main(int argc, char **argv)
{
	char *shortname, *variant;

	if (argc == 2) {
		variant = argv[1];
		shortname = argv[1];
	} else if (argc == 3) {
		variant = argv[1];
		shortname = argv[2];
	} else {
		printf("Usage: hash_variant VARIANT [SHORT_NAME]\n");
		exit(2);
	}

	printf("#define\tMLPVAR_%s\t(%d)\t/* caml hash for \"`%s\" */\n",
	       shortname, Int_val(caml_hash_variant(variant)), variant);
	return 0;
}
