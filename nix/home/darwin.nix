{ ... }:
{
  # Mac 固有の home-manager 設定。共通部分は common.nix に集約。
  # repoDir は common.nix が ${config.home.homeDirectory}/src/... で組むので
  # GOPATH=$HOME 前提のまま Mac(/Users/yuta) でも一致する。

  home.username = "yuta";
  home.homeDirectory = "/Users/yuta";

  # 例: Mac 固有パッケージ(terminal-notifier 等)はここに追加していく。
}
