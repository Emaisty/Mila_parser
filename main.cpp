#include "Parser.hpp"

// Use tutorials in: https://llvm.org/docs/tutorial/

int main(int argc, char *argv[]) {
    Parser parser;
    char *name;
    //printf("Interpreter of MILA language\n");
    if (argc == 1) {
        //printf("Input from keyboard, enter source code\n");
        name = NULL;
    } else {
        name = argv[1];
        //printf("Input file %s\n", name);
    }

    parser.InitLexan(name);
    parser.readTokens();
    if (!parser.Parse()) {
        return 1;
    }

    parser.Generate().print(llvm::outs(), nullptr);

    return 0;
}
