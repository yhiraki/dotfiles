{
  pkgs,
  lib,
  username,
  ...
}:
{
  # ===== nix-darwin システム設定（Mac） =====
  # 適用: make switch（username は flake.nix の mkMac から渡る）

  nixpkgs.hostPlatform = "aarch64-darwin";

  # vanilla(公式インストーラ) Nix なので nix デーモンは nix-darwin に管理させる。
  # （Determinate Nix を使う場合は二重管理が衝突するため false にすること）
  nix.enable = true;

  nix.settings = {
    # flake 運用（nix run / darwin-rebuild --flake / nix flake update）に必須
    experimental-features = [
      "nix-command"
      "flakes"
    ];
  }
  # 企業プロキシ(TLS インスペクション)環境向け: Keychain の CA を合成した
  # バンドルが用意されていればそれを使う。無い環境では何もしない。
  # バンドルの作り方:
  #   security find-certificate -a -p /Library/Keychains/System.keychain \
  #     | cat /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt - \
  #     | sudo tee /etc/nix/ca-bundle.crt
  # （--impure 前提。pathExists が flake の純粋評価では使えないため）
  // lib.optionalAttrs (builtins.pathExists "/etc/nix/ca-bundle.crt") {
    ssl-cert-file = "/etc/nix/ca-bundle.crt";
  };

  # 一部 system.defaults / homebrew 等が要求する主ユーザ指定
  system.primaryUser = username;
  users.users.${username} = {
    home = "/Users/${username}";
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
