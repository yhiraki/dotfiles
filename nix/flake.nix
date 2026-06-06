{
  description = "yhiraki dotfiles (ansible -> nix migration)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.05";
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

    # NOTE: emacs-overlay は emacs 最終ステップで追加する
    # emacs-overlay = { url = "github:nix-community/emacs-overlay"; inputs.nixpkgs.follows = "nixpkgs"; };
  };

  outputs =
    { nixpkgs, home-manager, nix-darwin, ... }@inputs:
    let
      # 共通の home-manager モジュール群（プラットフォーム別を末尾に足す）
      homeModules = extra: [ ./home/common.nix ] ++ extra;

      # 実行ユーザー名（--impure 必須）。公開 repo にユーザー名をハードコードしないため、
      # 環境変数 SUDO_USER（sudo 経由）→ USER の順で実行時に取得する。
      username =
        let
          sudoUser = builtins.getEnv "SUDO_USER";
          user = if sudoUser != "" then sudoUser else builtins.getEnv "USER";
        in
        if user == "" then
          throw "ユーザー名を取得できません。--impure 付きで実行してください（make switch 推奨）"
        else
          user;
    in
    {
      # ---- WSL(Linux): home-manager standalone ----
      # 適用: make switch（--impure でユーザー名を環境変数から取得）
      homeConfigurations."wsl" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
        extraSpecialArgs = { inherit inputs username; };
        modules = homeModules [ ./home/wsl.nix ];
      };

      # ---- Mac: nix-darwin + home-manager(module) ----
      # 適用: make switch（--impure 付きで実行され、ユーザー名は環境変数から取得）
      # ユーザー名を repo にハードコードしない（公開 dotfiles のため隠蔽）。
      # 私用/仕事どちらの Mac でも実行ユーザーに自動で一致する。
      darwinConfigurations =
        let
          mkMac =
            username:
            nix-darwin.lib.darwinSystem {
              specialArgs = { inherit inputs username; };
              modules = [
                ./darwin/configuration.nix
                home-manager.darwinModules.home-manager
                {
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.extraSpecialArgs = { inherit inputs username; };
                  # Mac には ansible の symlink(~/.zshenv 等)が存在し衝突するため、
                  # 初回 switch で .bak に自動退避させる（rm 手作業を不要に）。
                  home-manager.backupFileExtension = "bak";
                  home-manager.users.${username}.imports = homeModules [ ./home/darwin.nix ];
                }
              ];
            };
        in
        {
          "macbook" = mkMac username;
        };
    };
}
