module: representation

define abstract class <representation> (<object>)
end;

define generic pick-representation
    (type :: <ctype>, optimize-for :: one-of(#"speed", #"space"))
    => res :: <representation>;

define generic representation-alignment (rep :: <representation>)
    => alignment :: <fixed-integer>;

define generic representation-size (rep :: <representation>)
    => size :: <fixed-integer>;

define generic representation-has-bottom-value? (rep :: <representation>)
    => res :: <boolean>;
