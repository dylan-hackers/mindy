module: PR-93

define inline method env-get(env)
 => (val)
  if (env)
    env-get(env);
  else
    signal(make(<simple-error>));
  end if;
end method env-get;
