let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/7eeacecff44e05a9fd61b9e03836b66ecde8a525.tar.gz";
    sha256 = "sha256:0f6nv0pgk58d1962r8vswi7ks59fryh0yrdk99d30b3qj11a2045";
  };
  pkgs = import nixpkgs { config = { }; overlays = [ ]; };
in


pkgs.mkShellNoCC rec {
  nativeBuildInputs = with pkgs; [
    cabal-install
    ghcid
    ghc
    zlib
    (haskell.lib.justStaticExecutables pkgs.haskell.packages.ghc94.ormolu_0_5_2_0)
    (haskell.lib.justStaticExecutables haskellPackages.cabal-fmt)
    nixpkgs-fmt
    treefmt
    shellcheck
    jq
    gcc
  ];

  # Ensure that libz.so and other libraries are available to TH
  # splices, cabal repl, etc.
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeBuildInputs;
}
