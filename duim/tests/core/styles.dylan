Module:       DUIM-TEST-SUITE
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// 
define sideways method make-test-instance
    (class == <color>) => (color :: <color>)
  make(<color>, red: 1, green: 0, blue: 0)
end method make-test-instance;

/// DC tests

define duim-dcs class-test <brush> ()
  //---*** Fill this in...
end class-test <brush>;

define duim-dcs class-test <color-not-found> ()
  //---*** Fill this in...
end class-test <color-not-found>;

define duim-dcs class-test <color> ()
  //---*** Fill this in...
end class-test <color>;

define duim-dcs class-test <contrasting-color> ()
  //---*** Fill this in...
end class-test <contrasting-color>;

define duim-dcs class-test <device-font> ()
  //---*** Fill this in...
end class-test <device-font>;

define duim-dcs class-test <dynamic-color> ()
  //---*** Fill this in...
end class-test <dynamic-color>;

define duim-dcs class-test <image> ()
  //---*** Fill this in...
end class-test <image>;

define duim-dcs class-test <ink> ()
  //---*** Fill this in...
end class-test <ink>;

define duim-dcs class-test <palette-full> ()
  //---*** Fill this in...
end class-test <palette-full>;

define duim-dcs class-test <palette> ()
  //---*** Fill this in...
end class-test <palette>;

define duim-dcs class-test <pattern> ()
  //---*** Fill this in...
end class-test <pattern>;

define duim-dcs class-test <pen> ()
  //---*** Fill this in...
end class-test <pen>;

define duim-dcs class-test <stencil> ()
  //---*** Fill this in...
end class-test <stencil>;

define duim-dcs class-test <text-style> ()
  //---*** Fill this in...
end class-test <text-style>;



/// Text style tests

define test default-text-styles-test ()
  let text-style = $default-text-style;
  let button-text-style = make-text-style("courier", #f, #f, #f, #f);
  let button = make-test-pane(<push-button>, text-style: button-text-style);
  // check-equal("pane text style",
  //             pane-text-style(button), button-text-style);
  let layout = make-test-pane(<row-layout>, children: vector(button));
  let frame
    = make-test-frame(<test-frame>, layout: layout, text-style: text-style);
  let top-sheet = top-level-sheet(frame);
  let medium = sheet-medium(top-sheet);
  check-equal("top-level-sheet default text-style",
	      medium-text-style(medium), text-style);
  frame
end test default-text-styles-test;


/// Color tests

define test color-equality-test ()
  let color = make-rgb-color(1, 0, 0);
  check-equal("make-rgb-color = $red", color, $red);
  let color = make-rgb-color(0, 1, 0);
  check-equal("make-rgb-color = $green", color, $green);
  let color = make-rgb-color(0, 0, 1);
  check-equal("make-rgb-color = $blue", color, $blue);
end test color-equality-test;

define test default-colors-test ()
  let foreground = $red;
  let background = $green;
  let button-foreground = $blue;
  let button-background = $black;
  let button
    = make-test-pane(<push-button>,
		     foreground: button-foreground,
		     background: button-background);
  check-equal("pane foreground",
	      default-foreground(button), button-foreground);
  check-equal("pane background",
	      default-background(button), button-background);
  let layout = make-test-pane(<row-layout>, children: vector(button));
  let frame
    = make-test-frame(<test-frame>,
		      layout: layout,
		      foreground: foreground,
		      background: background);
  let top-sheet = top-level-sheet(frame);
  let medium = sheet-medium(top-sheet);
  check-equal("top-level-sheet default foreground",
	      medium-foreground(medium), foreground);
  check-equal("top-level-sheet default background",
	      medium-background(medium), background);
  frame
end test default-colors-test;


/// Define the colors test suite

define suite duim-colors-suite ()
  test default-text-styles-test;
  test color-equality-test;
  test default-colors-test;
end suite duim-colors-suite;
