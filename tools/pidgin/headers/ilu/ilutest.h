#define ILU_PUBLIC extern
#define ILU_PUBLIC_CLASS class
#define ILU_STDCALL

/* A few tests of ugly macros from Xerox's ILU. */
#include "iluerror.h"

ILU_DECL_PARMLESS_ERR(unknown);	/* not used */

typedef enum {
  ilu_bpm_duh,			/* It should be pretty obvious */
  ilu_bpm_true			/* Attempting RPC on true server */
  /* Many errors snipped... */
}               ilu_bad_param_Minor;

ILU_DECL_ERR(bad_param)
{
  ilu_bad_param_Minor minor;
} ILU_END_DECL_ERR;
