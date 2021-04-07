CC := clang
CFLAGS := -std=c99 -W -Wall -pedantic

PACKCC := build/packcc/build/clang/release/bin/packcc
PARSER := build/gen/pcc_parser.c

.DEFAULT_GOAL := kitc
all: kitc

kitc: build/kitc
.PHONY: kitc

build/kitc: build/eval
	build/eval bin/kitc/main.kit

build/eval: configure ${PARSER}
	${CC} -Ibuild/gen -Isrc/bootstrap -Isrc/bootstrap/ast -Isrc/bootstrap/parser \
	src/bootstrap/eval.c -o build/eval \
	${CFLAGS} \
	-Wno-extra-semi -Wno-implicit-function-declaration -Wno-nullability-completeness -Wno-nullability-extension

configure:
	@mkdir -p build/gen
.PHONY: configure

# @${PACKCC} -v
${PARSER}: ${PACKCC} src/bootstrap/parser/kit.peg
	cd build/gen && ../../${PACKCC} -o pcc_parser ../../src/bootstrap/parser/kit.peg

${PACKCC}: build/packcc
	cd build/packcc/build/clang && make release/bin/packcc
	@echo
# https://github.com/arithy/packcc/releases/tag/v1.4.0
build/packcc:
	@rm -f /tmp/c30effc9c8.zip
	@echo "Downloading packcc…"
	@curl -L -C - https://github.com/arithy/packcc/archive/c30effc9c8dcacee1da1b66bc56c44f4f7496b11.zip -o /tmp/c30effc9c8.zip
	@echo Done.
	@echo "Unzipping packcc…"
	@unzip -q /tmp/c30effc9c8.zip -d build
	@mv build/packcc-c30effc9c8dcacee1da1b66bc56c44f4f7496b11 build/packcc
	@echo Done.
