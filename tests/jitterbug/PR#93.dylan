module: PR-93
copyright: Copyright © 2002 Colin Walters <walters@debian.org>
synopsis: interpret dylan (really scheme)
license: public domain

define class <scheme-object> (<object>)
end class <scheme-object>;

define class <scheme-identifier> (<scheme-object>)
  slot value :: <symbol>, required-init-keyword: #"value";
end class <scheme-identifier>;

define class <scheme-environment> (<scheme-object>)
  slot env :: <table>, init-value: make(<object-table>);
  slot next :: false-or(<scheme-environment>), required-init-keyword: #"next";
end class <scheme-environment>;

define inline method env-get(env :: <scheme-environment>, name :: <scheme-identifier>)
 => (val :: <scheme-object>)
  let tab = env.env;
  let val = tab[name];
  if (val)
    val
  elseif (env.next ~= #f)
    env-get(env.next, name);
  else
    signal(make(<simple-error>));
  end if;
end method env-get;
