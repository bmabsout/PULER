{
  description = "A very basic flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          extensions = (with pkgs.vscode-extensions; [
            bbenoist.nix
            haskell.haskell
            justusadam.language-haskell
          ]);
          
          vscodium-with-extensions = pkgs.vscode-with-extensions.override {
            vscode = pkgs.vscodium;
            vscodeExtensions = extensions;
          };
          ghc-with-deps = pkgs.ghc.withPackages (ps: with ps; [
            containers
            mtl
            megaparsec
            prettyprinter
            repline
            filepath
            text
            transformers
            parser-combinators
            recursion-schemes
            lattices
            data-partition
            deriving-compat
            appendmap
            polysemy
            polysemy-plugin
            transformers-compat
            polysemy-zoo
          ]);
      in {

         devShell = pkgs.mkShell {
           buildInputs = [
             ghc-with-deps
             pkgs.haskell-language-server
             vscodium-with-extensions
             pkgs.python3
           ];
         };
        apps.x86_64-linux.puler = {
          type="app";
          program = "${pkgs.writeShellApplication {
            name= "PULER";
            text= "${ghc-with-deps}/bin/ghc -O2 src/Main.hs -o ./bin/puler";
          }}/bin/puler";
        };
      }


    );
}
