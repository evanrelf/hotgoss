{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenv.mkDerivation {
  pname = "maelstrom";
  version = "0.2.3";
  src =
    builtins.fetchTarball {
      url = "https://github.com/jepsen-io/maelstrom/releases/download/v0.2.3/maelstrom.tar.bz2";
      sha256 = "sha256:1hkczlbgps3sl4mh6hk49jimp6wmks8hki0bqijxsqfbf0hcakwq";
    };
  nativeBuildInputs = [ pkgs.makeWrapper ];
  dontBuild = true;
  installPhase = ''
    mkdir -p $out/bin/
    mv lib $out/lib
    cat <<EOF > $out/bin/maelstrom
      exec ${pkgs.jdk}/bin/java \
        -Djava.awt.headless=true \
        -jar "$out/lib/maelstrom.jar" \
        "\$@"
    EOF
    chmod +x $out/bin/maelstrom
    wrapProgram $out/bin/maelstrom \
      --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.gnuplot pkgs.jdk ]}
  '';
}
