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

  # direnv hook も同様にビルド時焼き込み（起動時 `eval "$(direnv hook zsh)"` の
  # フォークを撲滅）。.envrc 評価の precmd フック(_direnv_hook)はランタイムで動く。
  direnvHook = pkgs.runCommand "direnv-hook.zsh" { } ''
    export HOME="$TMPDIR"
    ${pkgs.direnv}/bin/direnv hook zsh > $out
  '';

  # emacs: emacs/ 直下の「設定」項目だけを ~/.emacs.d/ へ個別 out-of-store
  # symlink する。elpa/eln-cache/custom.el/
  # org-roam.db 等のランタイム生成物は ~/.emacs.d に実体として残し汚さない。
  # 新規 top-level 設定を emacs/ に足したらここにも追記する。
  emacsConfigItems = [
    "early-init.el"
    "init.el"
    "elisp"
    "image"
    "org-protocol"
    "org-templates"
    "snippets"
    "templates"
    "tests"
  ];
  emacsFiles = builtins.listToAttrs (map
    (item: {
      name = ".emacs.d/${item}";
      value.source =
        config.lib.file.mkOutOfStoreSymlink "${repoDir}/emacs/${item}";
    })
    emacsConfigItems);
in
{
  # Mac / WSL 共通の home-manager 設定。

  home.stateVersion = "25.05";

  programs.home-manager.enable = true;

  # switch のたびに出る「未読のお知らせが N 件」ナグを抑制（changelog 通知）。
  # 読みたいときは `home-manager news --flake ./nix#wsl`。
  news.display = "silent";

  # Mac/WSL 共通の CLI 群。Mac 専用(pngpaste 等)は darwin.nix に置く。
  # GUI/システム拡張(karabiner 等)は nix 管理外で手動導入。
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
    git
    git-filter-repo
    gh
    # go は nix 版を baseline に。プロジェクト毎のバージョンは mise 側で上書き。
    go
    tmux
    # emacs 30.2(native-comp+tree-sitter 標準)。
    # WSL=pgtk(GUI+端末両対応・WSLg/Wayland)、Mac=NS。
    (if stdenv.isDarwin then emacs30 else emacs30-pgtk)
    ffmpeg
    imagemagick
    pandoc
    shellcheck
    shfmt
    yq
    # cmigemo: emacs migemo 検索用の C/Migemo 実装
    cmigemo
    # poppler_utils: pdftotext/pdftoppm 等。文字起こし/PDF 処理で利用
    poppler_utils
    # librsvg: org/emacs のインライン SVG 描画
    librsvg
    # postgresql: psql/pg_dump 等のクライアント目的（サーバは起動しない）
    postgresql_17
  ];

  # ~/.gitconfig は ~/.gitconfig.local を include するだけ（内容は手書き側に集約）。
  # 手書き本体(.gitconfig.local)と .gitexclude は repo 実体を out-of-store symlink。
  home.file = {
    ".gitconfig".text = ''
      [include]
      	path = ~/.gitconfig.local
    '';
    ".gitconfig.local".source =
      config.lib.file.mkOutOfStoreSymlink "${repoDir}/.gitconfig.local";
    ".gitexclude".source =
      config.lib.file.mkOutOfStoreSymlink "${repoDir}/.gitexclude";

    # 手書き .tmux.conf を repo 実体から out-of-store symlink（編集即反映）
    ".tmux.conf".source =
      config.lib.file.mkOutOfStoreSymlink "${repoDir}/.tmux.conf";
  } // emacsFiles;

  # direnv: nix-direnv の direnvrc は使うが、シェル統合(eval hook)は HM に
  # 注入させず、起動フォークを避けるためビルド時焼き込み版(direnvHook)を source。
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = false;
  };

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";

    autosuggestion.enable = true;

    # nixpkgs に無いプラグインは flake input から取り込み（flake.lock で固定）
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

    # PATH 構築等の .zshenv を repo 実体から source（live edit 維持）。
    # .zshenv が system パスを最前列に前置するため、source 後に
    # nix profile を再優先化する（typeset -U で重複は除去済み）。
    envExtra = ''
      source ${repoDir}/zsh/.zshenv
      export PATH="${config.home.profileDirectory}/bin:$PATH"
    '';

    # mise はビルド時焼き込み版を source（起動フォーク無し）→ 手書き設定を source。
    # PATH 確定後(envExtra の後)に走るので __MISE_ORIG_PATH も正しく取れる。
    initContent = ''
      source ${miseActivate}
      source ${direnvHook}
      source ${repoDir}/zsh/rc.local.zsh
    '';
  };
}
