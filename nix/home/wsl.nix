{ ... }:
{
  # WSL(Linux) 固有設定。Mac 側は将来 darwin.nix を別に用意し、
  # 共通部分は common.nix に集約して isDarwin で分岐する。

  home.username = "yuta";
  home.homeDirectory = "/home/yuta";

  # 例: WSL 固有の direnv / mise などはここに追加していく。
}
