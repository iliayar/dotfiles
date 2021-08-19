{ 
  description = "My NixOs Configuration using home-manager with flakes";


  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # theme = {
    #   background = "#1c1e1f";
    #   foreground = "#f8f8f2";
    #   color1     = "#e3276b";
    #   color2     = "#a6e22e";
    #   color3     = "#f4bf75";
    # };
  };

  outputs = { self, home-manager, nixpkgs, ... }: 
  let
    specialArgs = {
      inherit home-manager;
    };

    homeConfigurations = {
      iliayar = home-manager.lib.homeManagerConfiguration rec {
        system = "x86_64-linux";
        extraSpecialArgs = specialArgs;
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
          }
        ];
      in
        nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };
  in {
    nixosConfigurations.nixos = dellLaptop;
    inherit homeConfigurations;
  };
}
