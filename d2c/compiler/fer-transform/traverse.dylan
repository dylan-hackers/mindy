module: fer-transform

// These are helper functions to allow you to easily traverse the tree
// of compound regions, etc. looking for specific features
//
define function traverse-component
    (component :: <component>, what-for :: <class>, payload :: <function>) 
 => ();
  
  for (function-region in component.all-function-regions)
    traverse(component, function-region, what-for, payload);
  end for;
end function traverse-component;

// Handle <body-region>, <block-region>, <function-region>, and <loop-region>
//
define method traverse 
    (component :: <component>, region :: <body-region>, 
     what-for :: <class>, payload :: <function>) 
 => ();

  if (instance?(region, what-for))
    payload(component, region);
  end if;
    
  traverse(component, region.body, what-for, payload);
end method traverse;

// Handle <if-region>
//
define method traverse 
    (component :: <component>, region :: <if-region>, 
     what-for :: <class>, payload :: <function>) 
 => ();

  if (instance?(region, what-for))
    payload(component, region);
  end if;

  traverse(component, region.then-region, what-for, payload);
  traverse(component, region.else-region, what-for, payload);
end method traverse;

// Handle <compound-region>
//
define method traverse 
    (component :: <component>, region :: <compound-region>, 
     what-for :: <class>, payload :: <function>) 
 => ();
  
  if (instance?(region, what-for))
    payload(component, region);
  end if;

  for (r in region.regions)
    traverse(component, r, what-for, payload);
  end for;
end method traverse;

// Handle <exit>
//
define method traverse 
    (component :: <component>, region :: <exit>, 
     what-for :: <class>, payload :: <function>) 
 => ();

  if (instance?(region, what-for))
    payload(component, region);
  end if;
end method traverse;

// Handle <simple-region>
//
define method traverse
    (component :: <component>, region :: <simple-region>, 
     what-for :: <class>, payload :: <function>) 
 => ();

  if (instance?(region, what-for))
    payload(component, region);
  end if;

  for (assign = region.first-assign then assign.next-op,
       while: assign)
    traverse(component, assign, what-for, payload)
  end for;
end method traverse;

// Assignments:
//
define method traverse
    (component :: <component>, assignment :: <assignment>, 
     what-for :: <class>, payload :: <function>) 
 => ();

  if (instance?(assignment, what-for))
    payload(component, assignment);
  end if;

  // LHS variables
  for (defined-var = assignment.defines then defined-var.definer-next,
	 while: defined-var)
    traverse(component, defined-var, what-for, payload);
  end for;

  // RHS expression
  traverse(component, assignment.depends-on.source-exp, what-for, payload);

end method traverse;

// Variables:
//
define method traverse
    (component :: <component>, variable :: <abstract-variable>,
     what-for :: <class>, payload :: <function>)
 => ();

  if (instance?(variable, what-for))
    payload(component, variable);
  end if;
end method traverse;

// Operations:
//
define method traverse
    (component :: <component>, operation :: <operation>,
     what-for :: <class>, payload :: <function>)
 => ();

  if (instance?(operation, what-for))
    payload(component, operation);
  end if;

  for (operand = operation.depends-on then operand.dependent-next,
       while: operand)
    traverse(component, operand.source-exp, what-for, payload);
  end for;
end method traverse;

// Unclaimed <leaf>s:
//
define method traverse
    (component :: <component>, leaf :: <leaf>,
     what-for :: <class>, payload :: <function>)
 => ();

  if (instance?(leaf, what-for))
    payload(component, leaf);
  end if;
end method traverse;
