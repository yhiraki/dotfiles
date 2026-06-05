{
  description = "yhiraki dotfiles (ansible -> nix migration)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nixpkgs に無い zsh プラグインは flake input(flake=false)で取り込み、
    # flake.lock でバージョン固定する（git clone をやめて再現性を得る）。
    git-open = {
      url = "github:paulirish/git-open";
      flake = false;
    };
    zsh-simple-prompt = {
      url = "github:yhiraki/zsh-simple-prompt";
      flake = false;
    };

    # NOTE: nix-darwin / emacs-overlay は Mac 移行・emacs 最終ステップで追加する
    # nix-darwin = { url = "github:LnL7/nix-darwin"; inputs.nixpkgs.follows = "nixpkgs"; };
    # emacs-overlay = { url = "github:nix-community/emacs-overlay"; inputs.nixpkgs.follows = "nixpkgs"; };
  };

  outputs =
    { nixpkgs, home-manager, ... }@inputs:
    let
      mkHome =
        system: modules:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          extraSpecialArgs = { inherit inputs; };
          inherit modules;
        };
    in
    {
      # WSL 先行・最小スコープ。Mac は後で darwinConfigurations を追加。
      homeConfigurations."yuta@yuta-pc" = mkHome "x86_64-linux" [
        ./home/common.nix
        ./home/wsl.nix
      ];
    };
}
