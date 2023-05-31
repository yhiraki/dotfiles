PHONY: build
build: requirements.yaml
	ansible-playbook dotfiles.yaml

PHONY: test
test: requirements.yaml
	ansible-playbook test.yaml

requirements.yaml:
	ansible-galaxy install -r requirements.yaml
