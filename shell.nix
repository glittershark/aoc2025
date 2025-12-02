with (import <nixpkgs> { });

mkShell {
  buildInputs = [
    (pkgs.runCommand "swipl-prolog" { } ''
      mkdir -p $out/bin
      ln -s ${swi-prolog}/bin/* $out/bin/
      ln -s ${swi-prolog}/bin/swipl $out/bin/prolog
    '')
  ];
}
