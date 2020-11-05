{ pkgs }:

let
  lib = pkgs.lib;

  nsc =
    let
      baseDir = ./.stack-work/install;

      nsc-path = f: "${baseDir}/${f}/bin/nsc";
      readDir = f: builtins.attrNames (builtins.readDir "${baseDir}/${f}");

      arch = lib.elemAt (readDir "") 0;
      hash = lib.elemAt (readDir "${arch}") 0;
      version = lib.elemAt (readDir "${arch}/${hash}") 0;
    in
      nsc-path "${arch}/${hash}/${version}";
in
pkgs.mkShell {
  name = "nsc";
  version = "1.0.0";

  buildInputs = with pkgs; [
    glibc
    binutils
  ];

  buildPhase = ''
    (echo "Running \`nsc\`:"; ${nsc} ./test/singleRet.nst) && \
          (echo -e "\n\nRunning \`readelf\`:"; readelf -a ./test.o) && \
          (echo -e "\n\nRunning \`objdump\`:"; objdump -d -j'.text' ./test.o) && \
          (echo -e "\n\nTry linking \`test.o\`:"; ld ./test.o ${pkgs.glibc}/lib/crt1.o ${pkgs.glibc}/lib/crti.o -lc -o a.out) && \
          (echo -e "\n\nExecuting `a.out`:"; ./a.out)
  '';
}
