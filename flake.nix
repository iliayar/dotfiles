{
  description = "My NixOs Configuration using home-manager with flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nur.url = "github:nix-community/NUR";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    codestats-nvim = {
      url = "github:YannickFricke/codestats.nvim";
      flake = false;
    };

    typst-preview-nvim = {
        # url = "path:/home/iliayar/Repos/typst-preview.nvim";  
        url = "github:iliayar/typst-preview.nvim";
        flake = false;
    };

    secrets.url = "git+ssh://git@github.com/iliayar/dotfiles-secrets.git";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    libxft-bgra = {
      url = "github:uditkarode/libxft-bgra";
      flake = false;
    };

    org-roam-ui = {
      url = "github:org-roam/org-roam-ui";
      flake = false;
    };

    lean4-mode = {
      url = "github:leanprover-community/lean4-mode";
      flake = false;
    };

    picom-jonaburg = {
      url = "github:jonaburg/picom";
      flake = false;
    };

    tlpui-src = {
      url = "github:d4nj1/TLPUI";
      flake = false;
    };

    rust-blocks = {
      url = "github:iliayar/rust-blocks";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    uci = {
      url = "git+ssh://git@github.com/iliayar/uci.git";
      # url = "path:/home/iliayar/Repos/microci";
      # inputs.nixpkgs.follows = "nixpkgs";
    };

    pyprland-newest = {
      url = "github:hyprland-community/pyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pyprland-my = {
      url = "github:iliayar/pyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nwg-displays = {
      url = "github:nwg-piotr/nwg-displays";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    denv = {
      url = "github:iliayar/env.nix";
      # url = "path:/home/iliayar/Repos/env.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wezterm-newest = {
      url = "github:wez/wezterm?dir=nix";
      flake = true;

      # NOTE: This is vital. To match GL versions,
      # because wezterm links to its own libGL, but
      inputs.nixpkgs.follows = "nixpkgs";
    };

    remote-nvim = {
      url = "github:amitds1997/remote-nvim.nvim";
      # url = "path:/home/iliayar/Repos/remote-nvim.nvim";
      flake = false;
    };

    coq-lsp-nvim = {
      url = "github:tomtomjhj/coq-lsp.nvim";
      flake = false;
    };

    deploy-rs.url = "github:serokell/deploy-rs";
    swww = {
      url = "github:LGFae/swww";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ghostty-newest = { url = "github:ghostty-org/ghostty"; };

    systec-can = {
      url =
        "https://www.systec-electronic.com/media/default/Redakteur/produkte/Interfaces_Gateways/sysWORXX_USB_CANmodul_Series/Downloads/SO-1139-systec_can.tar.bz2";
      flake = false;
    };

    cfcli = {
      url = "git+https://git.sr.ht/~iliayar/cfcli";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zen-browser = {
        url = "github:youwen5/zen-browser-flake";
        inputs.nixpkgs.follows = "nixpkgs";
    };

    curd = {
        url ="github:Wraient/curd";
    };

    cangjie-nvim = {
        # url = "path:/home/iliayar/Repos/Cangjie/cangjie.nvim";
        url = "git+https://gitcode.com/iliayar/cangjie.nvim.git";
    };
  };

  outputs = { self, home-manager, nixpkgs, nur, secrets, denv, emacs-overlay, rust-overlay, deploy-rs, ... }@inputs:
    let
      config = system:
        let
          overlays = [ emacs-overlay.overlay rust-overlay.overlays.default ]
            ++ (import ./modules/overlays (inputs // { inherit system; }));

          nixpkgs-config = {
            inherit system overlays;
            config.allowUnfree = true;
            config.permittedInsecurePackages = [ "electron-25.9.0" ];
          };

          pkgs = import nixpkgs nixpkgs-config;

          mylib = import ./modules/lib.nix { inherit pkgs; };

          themes = import ./modules/themes { inherit mylib; };

          specialArgs = inputs // {
            secrets = import secrets;
            themes = themes;
          };

          makeHomeProfileImpl = profile:
            home-manager.lib.homeManagerConfiguration rec {
              inherit pkgs;
              extraSpecialArgs = specialArgs // { inherit pkgs system; };
              modules = [
                denv.homeManagerModules.default
                ./modules
                profile
              ];
            };

          makeHomeProfile = name: makeHomeProfileImpl ./profiles/${name}.nix;

          makeProfiles = pkgs.lib.foldl
            (acc: name: acc // { ${name} = makeHomeProfile name; })
            { };

          dellLaptop =
            let
              modules = [
                nur.modules.nixos.default
                ./hosts/dellLaptop/configuration.nix
                ./cachix.nix
                {
                  nixpkgs = nixpkgs-config;
                  nix = {
                    gc = {
                      automatic = true;
                      options = "--delete-older-than 3d";
                    };
                  };
                }
              ];
            in
            nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };

          pc =
            let
              modules = [
                # nur.nixosModules.nur
                ./hosts/pc/configuration.nix
                ./cachix.nix
                {
                  nixpkgs = nixpkgs-config;
                  nix = {
                    gc = {
                      automatic = true;
                      options = "--delete-older-than 3d";
                    };
                  };
                }
              ];
            in
            nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };

          lenovoLaptop =
            let
              modules = [
                ./hosts/lenovoLaptop/configuration.nix
                ./cachix.nix
                {
                  nixpkgs = nixpkgs-config;
                  nix = {
                    gc = {
                      automatic = true;
                      options = "--delete-older-than 3d";
                    };
                  };
                }
              ];
            in
            nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };

          homeSrv =
            let
              modules = [
                ./hosts/homeSrv/configuration.nix
                ./cachix.nix
                {
                  nixpkgs = nixpkgs-config;
                  nix = {
                    gc = {
                      automatic = true;
                      options = "--delete-older-than 3d";
                    };
                  };
                }
              ];
            in
            nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };

          homeConfigurations = (makeProfiles [
            "heavy"
            "ubuntu-virt"
            "work"
            "work-linux"
            "home-server-ci"
            "home-server"
            "pc"
          ]) // {
            wsl = home-manager.lib.homeManagerConfiguration rec {
              inherit pkgs;
              extraSpecialArgs = specialArgs // { inherit pkgs system; };
              modules = [ ./hosts/wsl/home.nix ];
            };
          };

          nixosConfigurations = {
            NixLaptop = dellLaptop;
            NixLenovo = lenovoLaptop;
            NixServer = homeSrv;
            NixPC = pc;
          };

          deployPkgs = import nixpkgs {
            inherit system;
            overlays = [
              deploy-rs.overlays.default
              (self: super: {
                deploy-rs = {
                  inherit (pkgs) deploy-rs;
                  lib = super.deploy-rs.lib;
                };
              })
            ];
          };

          deploySystemPaths = {
            homeServer = deployPkgs.deploy-rs.lib.activate.nixos
              nixosConfigurations.NixServer;
            homeServerCi = deployPkgs.deploy-rs.lib.activate.home-manager
              homeConfigurations.home-server-ci;
            homeServerDev = deployPkgs.deploy-rs.lib.activate.home-manager
              homeConfigurations.home-server;
          };

        in
        {
          inherit homeConfigurations nixosConfigurations deploySystemPaths;
          makeHomeConfiguration = makeHomeProfileImpl;
        };
    in
    {
      makeHomeConfiguration."x86_64-linux" =
        (config "x86_64-linux").makeHomeConfiguration;

      nixosConfigurations = {
        NixLaptop = (config "x86_64-linux").nixosConfigurations.NixLaptop;
        NixPC = (config "x86_64-linux").nixosConfigurations.NixPC;
        NixServer = (config "x86_64-linux").nixosConfigurations.NixServer;
      };

      homeConfigurations = {
        heavy = (config "x86_64-linux").homeConfigurations.heavy;
        pc = (config "x86_64-linux").homeConfigurations.pc;
        home-server = (config "x86_64-linux").homeConfigurations.home-server;
      };

      deploy.nodes = {
        homeServer = {
          hostname = "home.iliayar.net";
          profilesOrder = [ "system" "ci" "iliayar" ];
          profiles.system = {
            user = "root";
            sshUser = "root";
            path = (config "x86_64-linux").deploySystemPaths.homeServer;
          };
          profiles.ci = {
            user = "ci";
            sshUser = "ci";
            path = (config "x86_64-linux").deploySystemPaths.homeServerCi;
          };
          profiles.dev = {
            user = "iliayar";
            sshUser = "iliayar";
            path = (config "x86_64-linux").deploySystemPaths.homeServerDev;
          };
        };
      };
    };
}
