{
  description = "Cairo + Guile";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs { inherit system; };
  in {
    devShells.default = with pkgs;
      mkShell {
        packages = [
          guile
          guile-cairo
        ];

        shellHook = ''
          export INFOPATH=${guile-cairo}/share/info:$INFOPATH
          export GUILE_LOAD_PATH="$PWD:$GUILE_LOAD_PATH"
        '';
      };
  });
}
