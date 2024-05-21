{ jdk ? "graalvm-ce" }:

let
  config = {
    packageOverrides = p: rec {
      java = p.${jdk};

      sbt = p.sbt.overrideAttrs (
        old: rec {
          patchPhase = ''
            echo -java-home ${java} >> conf/sbtopts
          '';
        }
      );
    };
  };

  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";

  pkgs = import nixpkgs { inherit config; };
in
pkgs.mkShell {
  name = "scala-shell";

  buildInputs = [
    pkgs.${jdk}
    pkgs.sbt
  ];
}
