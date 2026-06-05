{ pkgs, config, inputs, ... }:
let
  # repo の実体パス。Mac/WSL とも GOPATH=$HOME 配下に置いている前提。
  # 設定本体はここを直接 source して「編集→即反映」を維持する（out-of-store）。
  repoDir = "${config.home.homeDirectory}/src/github.com/yhiraki/dotfiles";

  # `mise activate zsh` の出力をビルド時に焼き込み、起動時は静的 source のみ
  # にして実行時フォークを撲滅する。ディレクトリ毎の .tool-versions 自動切替は
  # 出力内の precmd/chpwd フック(_mise_hook→hook-env)がランタイムで担うので維持。
  miseActivate = pkgs.runCommand "mise-activate.zsh" { } ''
    export HOME="$TMPDIR"
    ${pkgs.mise}/bin/mise activate zsh > $out
  '';
in
{
  # Mac / WSL 共通の home-manager 設定。

  home.stateVersion = "25.05";

  programs.home-manager.enable = true;

  # ansible packages role の CLI 群を nixpkgs へ。
  # （karabiner は手動。GUI/system 拡張は nix 管理外）
  home.packages = with pkgs; [
    coreutils
    findutils
    gnused
    gnutar
    gnugrep
    gnupg
    ripgrep
    fd
    fzf
    ghq
    wget
    gibo
    mise
  ];

  # direnv hook を nix が生成（旧 .zshrc の `eval "$(direnv hook zsh)"` を置換）
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";

    # zsh-autosuggestions（旧 plugins_repo の clone を置換）
    autosuggestion.enable = true;

    # nixpkgs に無いプラグインは flake input から（git clone をやめ再現性を確保）
    plugins = [
      {
        name = "fast-syntax-highlighting";
        src = pkgs.zsh-fast-syntax-highlighting;
        file = "share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh";
      }
      {
        name = "git-open";
        src = inputs.git-open;
      }
      {
        name = "zsh-simple-prompt";
        src = inputs.zsh-simple-prompt;
      }
    ];

    # 補完初期化（旧 .zshrc 末尾の compinit ブロックを置換）。
    # zsh-completions と自前補完を fpath に積んでから compinit。
    completionInit = ''
      fpath=(
        ${pkgs.zsh-completions}/share/zsh/site-functions
        ''${ZDOTDIR:-$HOME}/completion
        $HOME/.zfunc
        $fpath
      )
      local zcd="''${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
      [[ -d "$zcd" ]] || mkdir -p "$zcd"
      autoload -Uz compinit
      compinit -d "$zcd/zcompdump"
    '';

    # 旧 .zshenv（PATH 構築など）を repo 実体から source（live edit 維持）。
    # repo .zshenv が system パスを最前列に前置するため、source 後に
    # nix profile を再優先化する（typeset -U で重複は除去済み）。
    envExtra = ''
      source ${repoDir}/zsh/.zshenv
      export PATH="${config.home.profileDirectory}/bin:$PATH"
    '';

    # mise はビルド時焼き込み版を source（起動フォーク無し）→ 手書き設定を source。
    # PATH 確定後(envExtra の後)に走るので __MISE_ORIG_PATH も正しく取れる。
    initContent = ''
      source ${miseActivate}
      source ${repoDir}/zsh/rc.local.zsh
    '';
  };
}
