{
  description = "Cairo + Guile";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    guile-pstk = {
      url = "github:KikyTokamuro/guile-pstk";
      flake = false;
    };
  };

  outputs = { nixpkgs, flake-utils, guile-pstk, ... }: flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs { inherit system; };
  in {
    devShells.default = with pkgs;
      mkShell {
        packages = [
          guile
          guile-cairo
          tcl-9_0
          tk-9_0
        ];

        shellHook = ''
          export INFOPATH=${guile-cairo}/share/info:$INFOPATH
          export GUILE_LOAD_PATH="$PWD:${guile-pstk}:$GUILE_LOAD_PATH"
        '';
      };
  });
}
