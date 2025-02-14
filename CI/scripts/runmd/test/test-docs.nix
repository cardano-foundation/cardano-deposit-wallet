{ self, pkgs, runmd, recipe }:
pkgs.runCommand "docs-check" { } ''
  export PATH=${runmd}/bin:$PATH
  runmd -r ${self}/test/${recipe} -d ${self} | bash
  touch $out
''
