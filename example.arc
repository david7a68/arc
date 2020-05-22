
Int16   : Int -32_768 .. 32_767;
Int32   : Int -2_147_483_648 .. 2_147_483_647;

BOOL    : Int32;
DWORD   : Int32;
HANDLE  : ()*;

STD_OUTPUT_HANDLE := -11;

extern (C) GetStdHandle : DWORD -> HANDLE;
extern (C) WriteConsole : (HANDLE, Int16*, Int, Int*, ()*) -> BOOL;

main := (args: String[]) {
    str := "Hello World"w;

    stdout := GetStdHandle STD_OUTPUT_HANDLE;
    WriteConsole(stdout, &(str 0), str.length, null, null);
}

/* D equivalent
alias BOOL      = int;
alias DWORD     = int;
alias HANDLE    = void*;

enum STD_OUTPUT_HANDLE = -11;

extern (C) HANDLE GetStdHandle(DWORD);
extern (C) BOOL WriteConsole(HANDLE, wchar*, int, int*, void*);

void main(string[] args) {
    auto str = "Hello World"w;

    auto stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    WriteConsole(stdout, &str[0], str.length, null, null);
}
*/
