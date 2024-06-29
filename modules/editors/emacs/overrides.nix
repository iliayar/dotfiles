{ org-roam-ui, lean4-mode, ... }:
final: prev: {
  org-roam-ui = final.trivialBuild {
    pname = "org-roam-ui";
    version = "2021-08-23";
    src = org-roam-ui;
    packageRequires = [final.f final.websocket final.org-roam final.simple-httpd];
    postInstall = ''
    cp -r out $out/share/emacs/site-lisp
    '';
  };

  lean4-mode = final.trivialBuild {
    pname = "lean4-mode";
    version = "2024-06-23";
    src = lean4-mode;
    packageRequires = [final.dash final.flycheck final.lsp-mode final.magit-section];
    postInstall = ''
    cp -r $src/data $out/share/emacs/site-lisp/
    '';
  };
}
