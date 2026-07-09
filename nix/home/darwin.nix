{ pkgs, username, ... }:
let
  # dualtap: マイク+システム音を同時録音する自作 CLI。nixpkgs に無いため
  # GitHub Release の署名済みユニバーサルバイナリを取得して入れる。
  # 更新時は version と sha256 を bump する（sha256 は Homebrew formula の値、
  # または `nix-prefetch-url <tarball url>` で取得）。
  dualtap = pkgs.stdenvNoCC.mkDerivation rec {
    pname = "dualtap";
    version = "0.1.4";
    src = pkgs.fetchurl {
      url = "https://github.com/yhiraki/dualtap/releases/download/v${version}/dualtap-v${version}-macos-universal.tar.gz";
      sha256 = "f108a9f57dc1dc16e96cbcc99424144d7615518ff9f566d725bb08f439dc118a";
    };
    sourceRoot = ".";
    dontFixup = true; # ad-hoc 署名を保つ（strip/install_name_tool で壊さない）
    installPhase = ''
      mkdir -p $out/bin
      install -m755 dualtap $out/bin/dualtap
    '';
  };
in
{
  # Mac 固有の home-manager 設定。共通部分は common.nix に集約。
  # repoDir は common.nix が ${config.home.homeDirectory}/src/... で組むので
  # GOPATH=$HOME 前提のまま Mac(/Users/<username>) でも一致する。
  # username は flake.nix の mkMac から extraSpecialArgs で渡る。

  home.username = username;
  home.homeDirectory = "/Users/${username}";

  # Mac でのみ使う CLI。
  home.packages = (with pkgs; [
    # pngpaste: クリップボードの画像を PNG 保存。org スクショ取り込みで利用
    pngpaste
    # sox: 録音→文字起こしワークフローの音声処理（Mac でのみ使用）
    sox
  ]) ++ [
    # dualtap: 録音ワークフローの録音エンジン
    dualtap
  ];
}
