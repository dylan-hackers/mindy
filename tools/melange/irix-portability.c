#include "stdlib.h"

#include "stdio.h"

#include "runtime.h"

#include "math.h"

extern descriptor_t dylanZtrue;	/* #t */

#define obj_True dylanZtrue.heapptr
extern descriptor_t dylanZfalse;	/* #f */

#define obj_False dylanZfalse.heapptr

#define GENERAL_ENTRY(func) \
    ((entry_t)SLOT(func, void *, 8))
#define GENERIC_ENTRY(func) \
    ((entry_t)SLOT(func, void *, 32))


/* Define Constant $default-defines */

/* $default-defines allocated as melange_cZportabilityZCdefault_defines */


/* Define Constant irix-include-directories */

/* irix-include-directories allocated as melange_cZportabilityZirix_include_directories */


/* Top level form. */

extern descriptor_t melange_cZportabilityZirix_include_directories;	/* irix-include-directories */

extern descriptor_t melange_cZc_lexerZinclude_path;	/* include-path */

extern descriptor_t melange_cZdylan_visceraZpush_last;	/* push-last */

/* form at {<unknown-source-location> instance} */
void melange_cZUNKNOWN_7(descriptor_t *orig_sp)
{
    descriptor_t *cluster_0_top;
    long L_size; /* size */
    long L_state; /* state */
    long L_state_2; /* state */
    descriptor_t L_PCTelement; /* %element */
    descriptor_t L_temp;
    descriptor_t L_temp_2; /* temp */


    /* #line 164 "./vector.dylan" */
    /* #line {Class <compound-macro-source-location>} */
    L_size = SLOT(melange_cZportabilityZirix_include_directories.heapptr, long, 4);
    L_state = 0;
    while (1) {
	/* #line {Class <unknown-source-location>} */
	L_state_2 = L_state;

	/* #line 148 "./cmp.dylan" */
	if ((L_state_2 < L_size)) {

	    /* #line 129 "./vector.dylan" */
	    L_PCTelement = SLOT(melange_cZportabilityZirix_include_directories.heapptr, descriptor_t, 8 + L_state_2 * sizeof(descriptor_t));

	    /* #line 116 "./irix-portability.dylan" */
	    if ((L_temp = melange_cZc_lexerZinclude_path).heapptr == NULL) abort();
	    L_temp_2 = L_temp;

	    /* #line 116 "./irix-portability.dylan" */
	    orig_sp[0] = L_temp_2;
	    orig_sp[1] = L_PCTelement;
	    /* push-last */
	    GENERAL_ENTRY(melange_cZdylan_visceraZpush_last.heapptr)(orig_sp + 2, melange_cZdylan_visceraZpush_last.heapptr, 2);
	    /* #line {Class <compound-macro-source-location>} */
	    L_state = (L_state_2 + 1);
	}
	else {
	    goto block0;
	}
    }
  block0:;
    return;
}


/* Define Constant $integer-size */

/* $integer-size is 4 */


/* Define Constant $short-int-size */

/* $short-int-size is 2 */


/* Define Constant $long-int-size */

/* $long-int-size is 4 */


/* Define Constant $longlong-int-size */

/* $longlong-int-size is 8 */


/* Define Constant $char-size */

/* $char-size is 1 */


/* Define Constant $float-size */

/* $float-size is 4 */


/* Define Constant $double-float-size */

/* $double-float-size is 8 */


/* Define Constant $long-double-size */

/* $long-double-size is 16 */


/* Define Constant $enum-size */

/* $enum-size is 4 */


/* Define Constant $pointer-size */

/* $pointer-size is 4 */


/* Define Constant $function-pointer-size */

/* $function-pointer-size is 4 */

