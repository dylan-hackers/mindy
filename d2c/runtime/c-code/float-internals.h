#ifndef INFINITY
# define INFINITY (1.0F/0.0F)
#endif


#if defined(__i386__)
# define HAVE_LDBL_UNION
union ldbl {
  long double v;
  struct {
    unsigned :32;		/* mantissa */
    unsigned :32;		/* mantissa */
    unsigned sbex:16;		/* sign and biased exponent */
    unsigned :16;		/* empty */
  } s;
};
# define ldbl_get_exponent(l)	(((l).s.sbex & 0x7FFF) - 0x3FFE)
# define ldbl_set_exponent(l,e)	((l).s.sbex = ((l).s.sbex & 0x8000) \
				            | (e + 0x3FFE))

#if defined(__CYGWIN__)
# undef HAVE_LOGL
#endif

#elif defined(__sparc__)
# define HAVE_LDBL_UNION
union ldbl {
  long double v;
  struct {
    unsigned sbex:16;		/* sign and biased exponent */
    unsigned :16;		/* mantissa */
    unsigned :32;		/* mantissa */
    unsigned :32;		/* mantissa */
    unsigned :32;		/* mantissa */
  } s;
};
# define ldbl_get_exponent(l)	(((l).s.sbex & 0x7FFF) - 0x3FFE)
# define ldbl_set_exponent(l,e)	((l).s.sbex = ((l).s.sbex & 0x8000) \
				            | (e + 0x3FFE))

#endif

