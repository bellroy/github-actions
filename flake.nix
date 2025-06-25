{
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs =
    inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      src = ./.;
      supportedCompilers = [
        "ghc96"
        "ghc98"
      ];
      defaultCompiler = "ghc96";
    };
}
