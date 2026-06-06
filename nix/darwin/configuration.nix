{ pkgs, ... }:
{
  # ===== nix-darwin システム設定（Mac） =====
  # 適用: darwin-rebuild switch --flake ./nix#macbook

  nixpkgs.hostPlatform = "aarch64-darwin";

  # Determinate Nix で nix を導入している場合、nix-darwin に nix デーモンを
  # 二重管理させると衝突する。Determinate に委ねるため nix.enable = false。
  # （vanilla nix なら true に戻してよい）
  nix.enable = false;

  # 一部 system.defaults / homebrew 等が要求する主ユーザ指定
  system.primaryUser = "yuta";
  users.users.yuta = {
    home = "/Users/yuta";
  };

  # ログインシェル zsh が nix のパスを拾えるよう /etc/zshrc を整える
  programs.zsh.enable = true;

  # ===== mac-defaults role の移植 =====
  system.defaults = {
    finder.AppleShowAllFiles = true;

    # 旧 ansible の com.apple.trackpad.scaling=5 を NSGlobalDomain 経由で再現。
    # （CustomUserPreferences は任意 defaults を書ける escape hatch。
    #   値は好みで調整可。標準のトラックパッド速度上限は 3.0 前後）
    CustomUserPreferences = {
      NSGlobalDomain = {
        "com.apple.trackpad.scaling" = 5.0;
      };
    };
  };

  # ===== brew-free =====
  # CLI は nixpkgs(home-manager 側 home.packages)で導入。
  # karabiner-elements は DriverKit システム拡張のため手動インストール（nix 管理外）。
  # → homebrew モジュールは意図的に未使用。

  # Mac 固有 CLI（必要に応じて追加）。skhd はホットキー常駐なので将来 services.skhd 化。
  environment.systemPackages = with pkgs; [
    terminal-notifier
  ];

  # nix-darwin の状態バージョン（初回構築時の値で固定）
  system.stateVersion = 5;
}
