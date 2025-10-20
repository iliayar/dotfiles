{ pkgs, ... }:
final: prev: {
  org-roam-ui = final.trivialBuild {
    pname = "org-roam-ui";
    version = "2021-08-23";
    src = pkgs.fetchFromGitHub {
      owner = "org-roam";
      repo = "org-roam-ui";
      rev = "5ac74960231db0bf7783c2ba7a19a60f582e91ab";
      sha256 = "sha256-dCoEQRi86eMerkMQPy3Ow/Kj9kzHxXRSrDk4cl8uLHo=";
    };
    packageRequires = [final.f final.websocket final.org-roam final.simple-httpd];
    postInstall = ''
      cp -r out $out/share/emacs/site-lisp
    '';
  };

  lean4-mode = final.trivialBuild {
    pname = "lean4-mode";
    version = "2024-06-23";
    src = pkgs.fetchFromGitHub {
      owner = "leanprover-community";
      repo = "lean4-mode";
      rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
      sha256 = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
    };
    packageRequires = [final.dash final.flycheck final.lsp-mode final.magit-section];
    postInstall = ''
      cp -r $src/data $out/share/emacs/site-lisp/
    '';
  };
  alabaster-theme = final.trivialBuild {
    pname = "alabaster-theme";
    version = "2025-10-18";
    src = pkgs.fetchFromGitHub {
      owner = "uzhne";
      repo = "alabaster-emacs";
      rev = "06321a2756961a54022c4c54b08536c4b9f7b4a1";
      sha256 = "sha256-yFpwlQ6t1HajD3jIcnVSXKBXTl/5aBJEcL/2aGViOAI=";
    };
    packageRequires = [];
    postInstall = ''
      cp -r $src $out/share/emacs/site-lisp/
    '';
  };
  stimmung-themes = final.trivialBuild {
    pname = "stimmung-themes";
    version = "2025-10-18";
    src = pkgs.fetchFromGitHub {
      owner = "motform";
      repo = "stimmung-themes";
      rev = "bb3410593bb7ecf3c4094396f488e5c64efdb051";
      sha256 = "sha256-05C0Wf1QJVwio08yO8DC1mWRoFtBo1xiL79KknXQA50=";
    };
    packageRequires = [];
    postInstall = ''
      cp -r $src $out/share/emacs/site-lisp/
    '';
  };
}
