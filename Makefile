# dotfiles エントリポイント。
# 適用は OS を見て home-manager(WSL/Linux) / darwin-rebuild(macOS) を出し分ける。

NIX := ./nix
# nix-darwin のブートストラップ用 rev（flake input と揃える）
DARWIN_REV := nix-darwin-25.05
DARWIN_TARGET := macbook
# vanilla Nix はデフォルトで flakes が無効なので、初回ブートストラップ時に明示する。
# --impure は flake がユーザー名を環境変数(SUDO_USER/USER)から取るために必要
# （公開 repo にユーザー名をハードコードしないため）。
NIX_FLAKE_OPTS := --extra-experimental-features 'nix-command flakes' --impure

.PHONY: help switch update gc

help: ## このヘルプを表示
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2}'

switch: ## 構成を適用（OS 判定で home-manager / darwin-rebuild。Mac 初回は nix run で自動ブートストラップ）
ifeq ($(shell uname),Darwin)
	@if command -v darwin-rebuild >/dev/null 2>&1; then \
		sudo darwin-rebuild switch --impure --flake $(NIX)#$(DARWIN_TARGET); \
	elif command -v nix >/dev/null 2>&1; then \
		echo "==> darwin-rebuild 未導入。nix run で nix-darwin をブートストラップします"; \
		sudo "$$(command -v nix)" run $(NIX_FLAKE_OPTS) nix-darwin/$(DARWIN_REV)#darwin-rebuild -- switch --impure --flake $(NIX)#$(DARWIN_TARGET); \
	else \
		echo "ERROR: nix が見つかりません。Nix 未インストール、または新しいターミナルで PATH 未反映の可能性。"; \
		echo "  - 既にインストール済みなら: 新しいターミナルを開いて再実行"; \
		echo "  - 未インストールなら: sh <(curl -L https://nixos.org/nix/install)"; \
		exit 1; \
	fi
else
	home-manager switch --impure --flake $(NIX)#wsl
endif

update: ## flake inputs（nixpkgs / home-manager 等）を更新
	nix flake update --flake $(NIX)

gc: ## 古い世代を削除して store を掃除
	nix-collect-garbage -d
