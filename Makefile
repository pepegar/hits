run: front back
	@echo "all"
	stack exec hits

front:
	@echo "building frontend"
	cd frontend && npm run build && mv dist/hits.js ../static

back:
	echo "building backend!"
	stack build
