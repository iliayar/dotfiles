{ pkgs, tlpui-src, ... }:
pkgs.python3Packages.buildPythonApplication {
  name = "tlpui";

  nativeBuildInputs = with pkgs; [
    tlp
  ];

  buildInputs = with pkgs; [
    gtk3
    gobject-introspection
    wrapGAppsHook
  ];

  propagatedBuildInputs = with pkgs; [
    python3Packages.pygobject3
  ];

  patches = [
    (pkgs.writeTextFile {
      name = "tlp.patch";
      text = ''
diff --git a/tlpui/file.py b/tlpui/file.py
index 15770fb..71818c5 100644
--- a/tlpui/file.py
+++ b/tlpui/file.py
@@ -5,7 +5,7 @@ import re
 from sys import stdout
 from subprocess import check_output, STDOUT, CalledProcessError
 from io import open
-from os import access, W_OK, close, path
+from os import access, W_OK, close, path, environ
 from tempfile import mkstemp
 from .config import TlpConfig, ConfType
 from . import settings
@@ -28,7 +28,7 @@ def get_tlp_config_defaults(tlpversion: str):
 
     if tlpversion not in ["0_8", "0_9", "1_0", "1_1", "1_2"]:
         # update default values with intrinsic ones
-        intrinsic_defaults_path = f"{settings.FOLDER_PREFIX}/usr/share/tlp/defaults.conf"
+        intrinsic_defaults_path = f"${pkgs.tlp}/share/tlp/defaults.conf"
         tlpconfig_defaults.update(extract_default_tlp_configs(intrinsic_defaults_path))
 
     return tlpconfig_defaults
      '';
    })
  ];

  doCheck = false;

  src = tlpui-src;
}
