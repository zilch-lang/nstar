{ pkgs
, runReadelf ? false
}:

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
    binutils-unwrapped
    gcc-unwrapped
  ];

  buildPhase = ''
    (echo "Running \`nsc\`:"; ${nsc} ./test/bashTrue.nst -o object.o) && \
  ''
  + (if runReadelf
     then ''
          (echo -e "\n\nRunning \`readelf\`:"; readelf -a ./object.o) && \
          (echo -e "\n\nRunning \`objdump\`:"; objdump -d -j'.text' ./object.o) && \
     '' else "")
  + ''
          (echo -e "\n\nTry linking \`object.o\`:"; gcc ./object.o -lc -o a.out) && \
          (echo -e "\n\nExecuting \`a.out\`:"; ./a.out)

     (echo -en "\n\nExit code: "; echo $?)
  '';
}
