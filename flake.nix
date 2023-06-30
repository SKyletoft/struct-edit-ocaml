{
	description = "A very basic flake";

	inputs = {
		nixpkgs.url     = "github:nixos/nixpkgs/nixpkgs-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem(system:
			let
				pkgs = nixpkgs.legacyPackages.${system};
				lib = nixpkgs.lib;
			in rec {
				packages = {
					struct-edit = pkgs.stdenv.mkDerivation {
						pname = "struct-edit";
						version = "0.0.1";
						src = ./haskell-ver;
						nativeBuildInputs = [ custom-haskell ];
						buildInputs = with pkgs; [
							# Packages listed by ldd when built with nix build
							ncurses
							gmp
							libffi
							glibc
						];
						installPhase = ''
							mkdir -p $out
							cp Main $out/struct-edit
						'';
					};
					default = packages.struct-edit;
				};
				custom-haskell = pkgs.haskell.packages.ghc92.ghc.withPackages(pkgs: with pkgs; [
					pretty

					hindent
					stylish-haskell
					QuickCheck
					haskell-language-server
				]);
				custom-ocaml = with pkgs; [
					dune_3
					ocaml
					ocamlformat
					nodePackages.ocaml-language-server
					ocamlPackages.odoc
					ocamlPackages.utop
					ocamlPackages.merlin
				];
				devShells.default = pkgs.mkShell {
					nativeBuildInputs = custom-ocaml ++ (with pkgs; [
						custom-haskell
						gnumake
					]);
					shellHook = ''
						alias orun="dune build && dune exec ./bin/main.exe"
					'';
				};
			}
		);
}
