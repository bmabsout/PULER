{ compiler ? "ghc865", nixpkgs ? import (builtins.fetchTarball {
  # Descriptive name to make the store path easier to identify
  name = "nixpkgs";
  # Commit hash for nixos-unstable as of 2018-09-12
  url = https://github.com/nixos/nixpkgs/archive/16dd1df08174e007d664c808712a639db5eff49e.tar.gz;
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1sjvhzii7cml897r7wjhv99mdkz6x0y9x5jh13gx84n24gdrscn7";
}) {}}:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, mtl, prettyprinter, parser-combinators
      , recursion-schemes, deriving-compat, repline, stdenv, text, transformers-compat,
      transformers, appendmap, ghcid, callPackage, lattices, data-partition, megaparsec,
      polysemy, polysemy-plugin, polysemy-zoo, filepath
      }:
      mkDerivation {
        pname = "Puler";
        version = "0.3.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers mtl megaparsec prettyprinter repline filepath 
          text transformers parser-combinators recursion-schemes lattices data-partition
          deriving-compat appendmap polysemy polysemy-plugin transformers-compat polysemy-zoo
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackagesOld = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  haskellPackages = haskellPackagesOld.extend(self: super: {
                    });

  drv = (haskellPackages.callPackage f {});
  ghcide = (import (builtins.fetchTarball "https://github.com/cachix/ghcide-nix/tarball/f940ec611cc6914693874ee5e024eba921cab19e") {}).ghcide-ghc865;
in

  if pkgs.lib.inNixShell then haskellPackages.shellFor {
      packages = p: [drv];
      buildInputs = with pkgs; [ghcid ghcide gcc libffcall cabal-install
      python3];
    } else drv

