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
    (echo "Running \`nsc\`:"; ${nsc} ./test/singleRet.nst) && \
  ''
  + (if runReadelf
     then ''
          (echo -e "\n\nRunning \`readelf\`:"; readelf -a ./test.o) && \
          (echo -e "\n\nRunning \`objdump\`:"; objdump -d -j'.text' ./test.o) && \
     '' else "")
  + ''
          (echo -e "\n\nTry linking \`test.o\`:"; ld ${pkgs.glibc}/lib/crt1.o ${pkgs.glibc}/lib/crti.o ${pkgs.gcc-unwrapped}/lib/gcc/*/*/crtbegin.o ./test.o ${pkgs.gcc-unwrapped}/lib/gcc/*/*/crtend.o ${pkgs.glibc}/lib/crtn.o -lc -o a.out) && \
          (echo -e "\n\nExecuting \`a.out\`:"; ./a.out)

     (echo -en "\n\nExit code: "; echo $?)
  '';
}
