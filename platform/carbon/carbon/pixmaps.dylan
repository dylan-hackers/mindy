module: carbon

define functional class <PixMap>	( <statically-typed-pointer> )
end class <PixMap>;

define functional class  <PixMapHandle>	( <statically-typed-pointer> )
end class <PixMapHandle>;

content-size( type == <PixMap> )
=> ( result :: <integer> )
	c-expr( int:, "sizeof( PixMap )" );
end method content-size;

content-size( type == <PixMapHandle> )
=> ( result :: <integer> )
	4;
end method content-size;

define method CopyBits( srcBits :: <PixMap>, dstBits :: <PixMap>,
                                 const Rect *           srcRect,
                                 const Rect *           dstRect,
                                 short                  mode,
                                 RgnHandle              maskRgn) /* can be NULL */          ONEWORDINLINE(0xA8EC);


EXTERN_API( void )
CopyMask                        (const BitMap *         srcBits,
                                 const BitMap *         maskBits,
                                 const BitMap *         dstBits,
                                 const Rect *           srcRect,
                                 const Rect *           maskRect,
                                 const Rect *           dstRect)                            ONEWORDINLINE(0xA817);

