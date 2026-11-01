{
  lib,
  rustPlatform,
  pkg-config,
  libgit2,
  zlib,
  herdr-nvim-src,
}:

# The herdr half of herdr-nvim, laid out as a herdr plugin directory:
# $out/share/herdr-nvim is what `herdr plugin link` takes. The binary finds the
# plugin's lua (injected into its sidebar nvim) by walking up from itself.
# The nvim half is built from the same `src` in modules/editors/neovim.
rustPlatform.buildRustPackage {
  pname = "herdr-nvim";
  inherit (lib.importTOML "${herdr-nvim-src}/herdr-plugin.toml") version;

  src = herdr-nvim-src;
  cargoLock.lockFile = "${herdr-nvim-src}/Cargo.lock";

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [
    libgit2
    zlib
  ];
  env.LIBGIT2_NO_VENDOR = "1";

  # the test suite drives a live herdr server and nvim
  doCheck = false;

  postInstall = ''
    root=$out/share/herdr-nvim
    mkdir -p $root/bin
    cp -r herdr-plugin.toml lua plugin doc $root/
    mv $out/bin/herdr-nvim $root/bin/herdr-nvim
    ln -s $root/bin/herdr-nvim $out/bin/herdr-nvim

    # the binary is prebuilt here; herdr runs [[build]] only on GitHub installs
    sed -i '/^\[\[build\]\]/,/^$/d' $root/herdr-plugin.toml
  '';

  meta = {
    description = "Neovim sidebar and agent annotations for herdr";
    homepage = "https://github.com/ChmaraX/herdr-nvim";
    license = lib.licenses.mit;
    mainProgram = "herdr-nvim";
  };
}
