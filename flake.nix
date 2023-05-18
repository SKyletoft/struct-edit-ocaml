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
					struct-edit = {};
					default = packages.struct-edit;
				};
				devShells.default = pkgs.mkShell {
					nativeBuildInputs = with pkgs; [
						dune_3
						ocaml
						ocamlformat
						nodePackages.ocaml-language-server
						ocamlPackages.odoc
						ocamlPackages.utop
						ocamlPackages.merlin
					];
					shellHook = ''
						alias orun="dune build && dune exec ./bin/main.exe"
					'';
				};
			}
		);
}
