{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "org-moderncv";
  src = pkgs.lib.sourceByRegex ./. [
    "^org-cv-utils.el$"
    "^org-moderncv.el$"
  ];
  buildInputs = [
    (pkgs.emacsWithPackages (epkgs: []))
  ];
  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el 2> stderr.txt
    cat stderr.txt
    ! grep -q ': Warning:' stderr.txt
  '';
  installPhase = ''
    LISPDIR=$out/share/emacs/site-lisp
    install -d $LISPDIR
    install *.el *.elc $LISPDIR
  '';
}
