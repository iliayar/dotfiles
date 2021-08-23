{ org-roam-ui, ... }:
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

}
