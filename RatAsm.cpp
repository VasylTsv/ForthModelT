#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <ctype.h>
#include <map>
#include <string>
#include <list>

using namespace std;

enum optype : int
{
    label = 1 << 6,
    value = 2 << 6,
    expression = 3 << 6,
    typemask = 3 << 6
};

map<string, int> opcodes =
{
    { "start",       1 },
    { "ldir.b",      2 + label },
    { "ldir.w",      3 + label },
    { "addr",        4 + value },
    { "sdir.b",      5 + label },
    { "sdir.w",      6 + label },
    { "sind.b",      7 },
    { "sind.w",      8 },
    { "lind.b",      9 },
    { "lind.w",     10 },
    { "call",       11 + label },
    { "scall",      12 },
    { "ujump",      13 + label },
    { "fjump",      14 + label },
    { "modstk",     15 + value },
    { "swap",       16 },
    { "limm",       17 + expression },
    { "push",       18 },
    { "pop",        19 },
    { "xchange",    20 },
    { "return",     21 },
    { "scale",      22 + value },
    { "add",        23 },
    { "sub",        24 },
    { "mult",       25 },
    { "div",        26 },
    { "mod",        27 },
    { "or",         28 },
    { "xor",        29 },
    { "and",        30 },
    { "asr",        31 },
    { "asl",        32 },
    { "neg",        33 },
    { "inc",        34 + value },
    { "dec",        35 + value },
    { "testeq",     36 },
    { "testne",     37 },
    { "testlt",     38 },
    { "testle",     39 },
    { "testgt",     40 },
    { "testge",     41 },
    { "testult",    42 },
    { "testule",    43 },
    { "testugt",    44 },
    { "testuge",    45 },
    { "end",        46 },
    { "ds",         0 + value },
    { "db",         0  },
    { "equ",        0 + label },        // Cosmetic extension for RatForth - define a symbol
    { "ref",        0 + expression }    // Mark symbol referenced
};

int main(int argc, char** argv)
{
    char* image = new char[65536];
    int memtop = 0;

    constexpr int nval = 0x10000; // all actual symbol values are 16-bit, this is used to mark symbols as undefined

    map<string, pair<int, list<int>>> symbols;

    // System vectors
    symbols["_fopen"].first = -2;
    symbols["_fclose"].first = -3;
    symbols["_getc"].first = -4;
    symbols["_gets"].first = -5;
    symbols["_putc"].first = -6;
    symbols["_putchar"].first = -7;
    symbols["_fixup"].first = -8;
    // Added for Forth system
    symbols["_getch"].first = -9;
    symbols["_remove"].first = -10;
    symbols["_rename"].first = -11;
    symbols["_ftell"].first = -12;
    symbols["_fseek"].first = -13;
    symbols["_fflush"].first = -14;
    symbols["_getsize"].first = -15;
    symbols["_setsize"].first = -16;
    symbols["_access"].first = -17;

    symbols["NULL"].first = 0;

    symbols["_fopen"].second.push_back(nval);
    symbols["_fclose"].second.push_back(nval);
    symbols["_getc"].second.push_back(nval);
    symbols["_gets"].second.push_back(nval);
    symbols["_putc"].second.push_back(nval);
    symbols["_putchar"].second.push_back(nval);
    symbols["_fixup"].second.push_back(nval);
    symbols["_getch"].second.push_back(nval);
    symbols["_remove"].second.push_back(nval);
    symbols["_rename"].second.push_back(nval);
    symbols["_ftell"].second.push_back(nval);
    symbols["_fseek"].second.push_back(nval);
    symbols["_fflush"].second.push_back(nval);
    symbols["_getsize"].second.push_back(nval);
    symbols["_setsize"].second.push_back(nval);
    symbols["_access"].second.push_back(nval);
    symbols["NULL"].second.push_back(nval);

    FILE* fin = fopen(argv[1], "rt");

    char buffer[1024];
    while (fgets(buffer, 1024, fin) != nullptr && !feof(fin))
    {
        // Only ASCII characters are really needed and supported. Using that trick to
        // work around a limitation in strtok (cannot parse comma-separated list with quoted elements).
        char quotemark = 0x00;
        for (char* ptr = buffer; *ptr; ++ptr)
        {
            if (*ptr == quotemark)
                quotemark = 0x00;
            else if (quotemark == 0x00 && (*ptr == '\'' || *ptr == '\"'))
                quotemark = *ptr;
            else if (quotemark && *ptr != '\n' && *ptr != '\r')
                *ptr |= 0x80;
        }


        for(char* ptr=buffer; *ptr; ++ptr)
            if (*ptr == ';')
            {
                *ptr = 0;
                break;
            }

        char* ptr = strtok(buffer, " \t\n\r");
        if(ptr && strlen(ptr) > 0)
        {
            if (ptr[strlen(ptr) - 1] == ':')
            {
                ptr[strlen(ptr) - 1] = 0;
                symbols[ptr].first = memtop;

                ptr = strtok(NULL, " \t\n\r");
            }

            if (ptr && strlen(ptr))
            {
                auto it = opcodes.find(ptr);
                if (it != opcodes.end())
                {
                    int opcode = it->second;
                    int t = opcode & typemask;
                    opcode = opcode & ~typemask;
                    
                    if (opcode == 0)
                    {
                        if (t == value) // ds
                        {
                            ptr = strtok(NULL, " \t\n\r");
                            if (ptr && strlen(ptr) > 0)
                            {
                                int val = atoi(ptr);
                                for(int i=0; i<val; ++i)
                                    image[memtop++] = 0;
                            }
                            else
                                printf("Warning: malformed instruction\n");
                        }
                        else if (t == label) // equ
                        {
                            char* symbol = strtok(NULL, " \t\n\r=");
                            if (symbol && strlen(symbol) > 0)
                            {
                                ptr = strtok(NULL, " \t\n\r");
                                int val = atoi(ptr);
                                symbols[symbol].first = val;
                            }
                        }
                        else if (t == expression) // ref
                        {
                            ptr = strtok(NULL, " \t\n\r");
                            auto it = symbols.find(ptr);
                            if (it == symbols.end())
                            {
                                symbols[ptr].first = nval;
                                it = symbols.find(ptr);
                            }
                            it->second.second.push_back(nval);
                        }
                        else // db
                        {
                            while ((ptr = strtok(NULL, " \t\n\r,")) != 0)
                            {
                                // RatC will never emit strings here, just bytes. But if we want to
                                // use this assembler for anything else this is the first extention to add
                                if (*ptr == '\"' || *ptr == '\'')
                                {
                                    char c = *ptr;
                                    ++ptr;
                                    while (*ptr && *ptr != c)
                                        image[memtop++] = (*ptr++) & 0x7f;
                                }
                                else if (!isdigit(*ptr) && *ptr != '-') // Second extension allows to emit addresses
                                {
                                    auto it = symbols.find(ptr);
                                    if (it == symbols.end())
                                    {
                                        symbols[ptr].first = nval;
                                        it = symbols.find(ptr);
                                    }
                                    it->second.second.push_back(memtop);
                                    image[memtop++] = 0;
                                    image[memtop++] = 0;
                                }
                                else
                                    image[memtop++] = atoi(ptr);
                            }
                        }

                    }
                    else if (t == value)
                    {
                        ptr = strtok(NULL, " \t\n\r");
                        if (ptr && strlen(ptr) > 0)
                        {
                            int val = atoi(ptr);
                            image[memtop++] = (char)(opcode & 255);
                            image[memtop++] = val&255;
                            image[memtop++] = val>>8;
                        }
                        else
                            printf("Warning: malformed instruction\n");
                    }
                    else if (t == label)
                    {
                        ptr = strtok(NULL, " \t\n\r");
                        if (ptr && strlen(ptr) > 0)
                        {
                            auto it = symbols.find(ptr);
                            if (it == symbols.end())
                            {
                                symbols[ptr].first = nval;
                                it = symbols.find(ptr);
                            }
                            it->second.second.push_back(memtop + 1);
                            image[memtop++] = (char)(opcode & 255);
                            image[memtop++] = 0;
                            image[memtop++] = 0;
                        }
                        else
                            printf("Warning: malformed instruction\n");
                    }
                    else if (t == expression)
                    {
                        ptr = strtok(NULL, " \t\n\r");
                        if (ptr && strlen(ptr) > 0)
                        {
                            char* lbl = nullptr;
                            char* val = nullptr;
                            int sign = 1;
                            char* ptrop = strpbrk(ptr, "+-");
                            if (ptrop != nullptr)
                            {
                                if (*ptrop == '-')
                                    sign = -1;
                                *ptrop = 0;
                                val = ptrop + 1;
                                if (ptrop != ptr)
                                    lbl = ptr;
                            }
                            else
                            {
                                if (isdigit(*ptr))
                                    val = ptr;
                                else
                                    lbl = ptr;
                            }

                            int value = val ? atoi(val) * sign : 0;
                            if(lbl)
                            {
                                auto it = symbols.find(ptr);
                                if (it == symbols.end())
                                {
                                    symbols[ptr].first = nval;
                                    it = symbols.find(ptr);
                                }
                                it->second.second.push_back(memtop + 1);
                            }

                            image[memtop++] = (char)(opcode & 255);
                            image[memtop++] = value & 255;
                            image[memtop++] = value >> 8;
                        }
                        else
                            printf("Warning: expected expression");
                    }
                    else
                    {
                        image[memtop++] = (char)(opcode & 255);
                    }
                }
                else
                    printf("Warning: Unknown instruction '%s'\n", ptr);
            }
        }
    }

    fclose(fin);

    // Resolve symbols
    auto mainIt = symbols.find("_main");
    if (mainIt != symbols.end())
        symbols.erase(mainIt);
    for (auto& s : symbols)
    {
        int offset = s.second.first;
        if (offset == nval)
            printf("Warning: symbol '%s' is undefined\n", s.first.c_str());
        if (s.second.second.empty())
            printf("Warning: symbol '%s' is unreferenced\n", s.first.c_str());
        for (auto& ref : s.second.second)
        {
            // Note that undefined symbols will end up being 0 here
            if(ref != nval)
                *(short*)&image[ref] += (short)offset;
        }
    }

    FILE* fout = fopen(argv[2], "wb");
    fwrite(image, memtop, 1, fout);
    fclose(fout);

    delete[] image;
}
