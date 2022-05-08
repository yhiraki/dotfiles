all:
	docker run --rm \
	-v $(shell cd ..; pwd):/code \
	-w /code/ansible \
	ansible \
	env TERM=xterm-256color make ansible

ansible:
	ansible-galaxy install -r requirements.yaml
	ansible-playbook hoge.yaml
