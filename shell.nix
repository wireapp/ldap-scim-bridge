let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/4f31540079322e6013930b5b2563fd10f96917f0.tar.gz";
    sha256 = "sha256:12748r3h44hy3a41slm5hcihn1nhrxjlgp75qz6iwzazkxnclx00";
  };
  pkgs = import nixpkgs { config = { }; overlays = [ ]; };
in


pkgs.mkShellNoCC rec {
  nativeBuildInputs = with pkgs; [
    cabal-install
    ghcid
    ghc
    zlib
    (haskell.lib.justStaticExecutables haskellPackages.ormolu)
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
