# Build complex_modifications JSON from src/*.yaml (requires yq: brew install yq)

.PHONY: build clean

build:
	@bash scripts/build_complex_modifications.sh

clean:
	@for f in assets/complex_modifications/*.json; do \
		if [ -f "$$f" ]; then \
			base=$$(basename "$$f" .json); \
			if [ -f "src/$$base.yaml" ]; then \
				rm -f "$$f" && echo "Removed $$f"; \
			fi; \
		fi; \
	done
