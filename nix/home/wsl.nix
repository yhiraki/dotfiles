{ username, ... }:
{
  # WSL(Linux) 固有設定。共通部分は common.nix に集約。
  # username は flake.nix から extraSpecialArgs で渡る（実行時に環境変数から取得）。

  home.username = username;
  home.homeDirectory = "/home/${username}";

  # WSLg + systemd 環境では /run/user/$UID 内の wayland ソケット symlink が
  # 用意されず、pgtk(GTK)ビルドの Emacs などが Wayland に繋げず X11 へ
  # フォールバックして警告を出す。実ソケットを絶対パスで直接指すことで回避する
  # (libwayland は絶対パスの WAYLAND_DISPLAY を XDG_RUNTIME_DIR を介さず使う)。
  home.sessionVariables.WAYLAND_DISPLAY = "/mnt/wslg/runtime-dir/wayland-0";

  # 例: WSL 固有の direnv / mise などはここに追加していく。
}
