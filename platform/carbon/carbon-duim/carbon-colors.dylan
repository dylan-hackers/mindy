Module:       carbon-duim
Synopsis:     Carbon back-end
Author:	      David Gray, Scott McKay, Andy Armstrong, Rob Myers
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Colour management

define sealed class <carbon-palette> (<basic-palette>)
  sealed slot port :: false-or(<port>),
    required-init-keyword: port:,
    setter: %port-setter;
end class <carbon-palette>;

define method make-palette
    (_port :: <carbon-port>, #key color?, dynamic?, #all-keys)
 => (palette :: <carbon-palette>)
  make(<carbon-palette>, port: _port, color?: color?, dynamic?: dynamic?)
end method make-palette;



define constant $native-black = make( <RGBColor*>, red: 0, green: 0, blue: 0);
define constant $native-white = make( <RGBColor*>, red: 65535, green: 65535, blue: 65535);


define method convert-color-to-native(mirror, r, g, b) 
=> (native :: <RGBColor*>)
  ignore(mirror);
  make(<RGBColor*>, red: truncate(r * 65535),
        green: truncate(g * 65535),
        blue: truncate(b * 65535));
end method convert-color-to-native;

define constant $1f/65535 = 1.0 / 65535.0;

define method native-color-rgb(colour :: <RGBColor*>)
=>(r ::<float>, g :: <float>, b :: <float>)
  values(colour.red-value * $1f/65535, 
          colour.blue-value * $1f/65535, 
          colour.green-value * $1f/65535);
end method native-color-rgb;

define method %color->native-color(color :: <rgb-color>)
=> (result :: <RGBColor*>)
  let (r :: <single-float>, g :: <single-float>, b :: <single-float>)
    = color-rgb(color);
  make(<RGBColor*>, red: truncate(r * 65535),
        green: truncate(g * 65535),
        blue: truncate(b * 65535));
end method %color->native-color;
