{ pkgs, username, ... }:
{
  # Mac 固有の home-manager 設定。共通部分は common.nix に集約。
  # repoDir は common.nix が ${config.home.homeDirectory}/src/... で組むので
  # GOPATH=$HOME 前提のまま Mac(/Users/<username>) でも一致する。
  # username は flake.nix の mkMac から extraSpecialArgs で渡る。

  home.username = username;
  home.homeDirectory = "/Users/${username}";

  # Mac でのみ使う CLI。
  home.packages = with pkgs; [
    # pngpaste: クリップボードの画像を PNG 保存。org スクショ取り込みで利用
    pngpaste
    # sox: 録音→文字起こしワークフローの音声処理（Mac でのみ使用）
    sox
  ];
}
