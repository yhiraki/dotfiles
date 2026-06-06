# dotfiles エントリポイント（旧 ansible/Makefile の後継）。
# 適用は OS を見て home-manager(WSL/Linux) / darwin-rebuild(macOS) を出し分ける。

NIX := ./nix

.PHONY: help switch update gc

help: ## このヘルプを表示
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2}'

switch: ## 構成を適用（OS 判定で home-manager / darwin-rebuild）
ifeq ($(shell uname),Darwin)
	sudo darwin-rebuild switch --flake $(NIX)#macbook
else
	home-manager switch --flake $(NIX)#yuta@yuta-pc
endif

update: ## flake inputs（nixpkgs / home-manager 等）を更新
	nix flake update --flake $(NIX)

gc: ## 古い世代を削除して store を掃除
	nix-collect-garbage -d
