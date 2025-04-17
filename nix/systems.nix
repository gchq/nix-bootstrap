# A minimal subset of numtide/flake-utils
{
  forEachSystem = systems: f:
    builtins.foldl' (
      attrs: system: let
        ret = f system;
      in
        builtins.foldl' (
          attrs: key:
            attrs
            // {
              ${key} =
                (attrs.${key} or {})
                // {
                  ${system} = ret.${key};
                };
            }
        )
        attrs (builtins.attrNames ret)
    ) {}
    systems;

  system = allSystems:
    builtins.listToAttrs
    (builtins.map (system: {
        name = system;
        value = system;
      })
      allSystems);
}
