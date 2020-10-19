let nixPkgs = import <nixpkgs> {};

    sys-packages = with nixPkgs; [
        gmp
    ];

    tools = with nixPkgs; [
        stack
    ];
in nixPkgs.mkShell {
    name = "skully";
    buildInputs = sys-packages ++ tools;

    shellHook = ''
        export LANG=C.UTF-8
    '';
}
