{ 
  description = "My NixOs Configuration using home-manager with flakes";


  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {url = "github:nix-community/home-manager/master"; inputs.nixpkgs.follows = "nixpkgs";
    };

    code-stats-vim = {
      url = "gitlab:code-stats/code-stats-vim";
      flake = false;
    };

    secrets.url = "path:./modules/secrets";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, home-manager, nixpkgs, code-stats-vim, secrets, emacs-overlay, ... }: 
  let
    overlays = [
      emacs-overlay.overlay
    ] ++ (import ./modules/overlays);

    specialArgs = {
      inherit home-manager
      code-stats-vim;

      secrets = import secrets;
      themes = import ./modules/themes;
    };

    homeConfigurations = {
      iliayar = home-manager.lib.homeManagerConfiguration rec {
        system = "x86_64-linux";
        extraSpecialArgs = specialArgs // {
          wallpapers = (pkgs.callPackage (import ./modules/themes/wallpapers.nix) { });
        };
        pkgs = import nixpkgs {
          inherit system;
        };
        homeDirectory = "/home/iliayar";
        username = "iliayar";
        configuration = { pkgs, config, ... }: {
          imports = [
            ./hosts/dellLaptop/home.nix
          ];

          nixpkgs.config = {
            allowUnfree = true;
          };
          nixpkgs.overlays = overlays;
        };
      };
    };

    dellLaptop = 
      let
        system = "x86_64-linux";

        modules = [
          ./hosts/dellLaptop/configuration.nix
          {
            nixpkgs.config = {
              allowUnfree = true;
            };
            nixpkgs.overlays = overlays;
          }
        ];
      in
        nixpkgs.lib.nixosSystem {
          inherit
            system
            modules
            specialArgs;
        };
  in {
    nixosConfigurations.NixLaptop = dellLaptop;
    inherit homeConfigurations;
  };
}
