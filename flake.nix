{
    description = "An Experimental Monad for Clash";

    inputs = {
        nixpkgs.url = "nixpkgs/nixos-23.05";
        silicon.url = "github:exclusive-and/silicon";
    };

    outputs = { self, nixpkgs, silicon, ... }:
    let
        system = "x86_64-linux";

        pkgs = import nixpkgs
        {
            inherit system;
            overlays = [ silicon.overlays.default ];
        };

        ghc = pkgs.silicon-depends.override
        {
            overrides = hself: hsuper:
            {
                silicon = silicon.packages.${system}.default;
            };
        };

        clash-monad = ghc.callCabal2nix "clash-monad" ./. {};

        clash-monad-env = ghc.shellFor
        {
            packages = _: [ clash-monad ];

            nativeBuildInputs = [
                pkgs.haskell.packages.ghc96.cabal-install
                pkgs.haskell.packages.ghc96.haskell-language-server
            ];
        };
    in
    {
        packages.${system}.default = clash-monad;
        devShells.${system}.default = clash-monad-env;
    };
}
