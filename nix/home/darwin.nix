{ username, ... }:
{
  # Mac 固有の home-manager 設定。共通部分は common.nix に集約。
  # repoDir は common.nix が ${config.home.homeDirectory}/src/... で組むので
  # GOPATH=$HOME 前提のまま Mac(/Users/<username>) でも一致する。
  # username は flake.nix の mkMac から extraSpecialArgs で渡る。

  home.username = username;
  home.homeDirectory = "/Users/${username}";

  # 例: Mac 固有パッケージ(terminal-notifier 等)はここに追加していく。
}
