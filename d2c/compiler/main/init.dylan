module: init
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/init.dylan,v 1.1 1994/12/12 13:01:25 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

begin
  let table = $Dylan-Module.module-syntax-table;
  table[#"%%begin"] := <begin-token>;
  table[#"%%bind-exit"] := <bind-exit-token>;
  table[#"%%class"] := <class-token>;
  table[#"%%cleanup"] := <cleanup-token>;
  table[#"%%constant"] := <constant-token>;
  table[#"%%create"] := <create-token>;
  table[#"%%finally"] := <finally-token>;
  table[#"%%for"] := <for-token>;
  table[#"%%from"] := <from-token>;
  table[#"%%else"] := <else-token>;
  table[#"%%export"] := <export-token>;
  table[#"%%if"] := <if-token>;
  table[#"%%in"] := <in-token>;
  table[#"%%library"] := <library-token>;
  table[#"%%method"] := <method-token>;
  table[#"%%module"] := <module-token>;
  table[#"%%mv-call"] := <mv-call-token>;
  table[#"%%set"] := <set-token>;
  table[#"%%unwind-protect"] := <uwp-token>;
  table[#"%%use"] := <use-token>;
  table[#"%%variable"] := <variable-token>;
  table[#"%%while"] := <while-token>;
  done-initializing-module-system();
end;
