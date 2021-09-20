#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <conio.h>
#include <io.h>
#include <stdlib.h>
#include <sys/stat.h>

#include <vector>
using namespace std;

// memory map:
// stack 16k
// usable 48k

#define IMAGE_SIZE 65536
#define STACK_SIZE 16384
#define STACK_START (IMAGE_SIZE-STACK_SIZE)

int main(int argc, char** argv)
{
    unsigned short pc = 0;
    unsigned short sp = STACK_SIZE;
    short p = 0;
    short s = 0;
    bool running = true;

// Memory image
    char* image = new char[IMAGE_SIZE+2]; // allocate buffer for stack underflow
    char* stack = &image[STACK_START];

// Handle mapping for stdio
    vector<FILE*> handles;
    handles.push_back(0);

// Read in the program
    FILE* fin = fopen(argv[1], "rb");
    int size = _filelength(_fileno(fin));
    if(size < IMAGE_SIZE-STACK_SIZE)
        fread(image, _filelength(_fileno(fin)), 1, fin);
    else
    {
        printf("Critical error: the program is too large.");
        running = false;
    }
    fclose(fin);

// Execution loop
    while (running)
    {
        switch (image[pc++])
        {
        case 1: // start
            sp -= 2;
            *(short*)& stack[sp] = -1; // so return from main will terminate
            break;
        case 2: // ldir.b
            p = image[*(unsigned short*)& image[pc]];
            pc += 2;
            break;
        case 3: // ldir.w
            p = *(short*)& image[*(unsigned short*)& image[pc]];
            pc += 2;
            break;
        case 4: // addr
            p = STACK_START + sp + *(short*)& image[pc];
            pc += 2;
            break;
        case 5: // sdir.b
            image[*(unsigned short*)& image[pc]] = (char)p;
            pc += 2;
            break;
        case 6: // sdir.w
            *(short*)& image[*(unsigned short*)& image[pc]] = p;
            pc += 2;
            break;
        case 7: // sind.b
            image[(unsigned short)s] = (char)p;
            break;
        case 8: // sind.w
            *(short*)& image[(unsigned short)s] = p;
            break;
        case 9: // lind.b
            p = image[(unsigned short)p];
            break;
        case 10: // lind.w
            p = *(short*)& image[(unsigned short)p];
            break;
        case 11: // call
            sp -= 2;
            *(unsigned short*)& stack[sp] = pc + 2;
            pc = *(unsigned short*)& image[pc];
            break;
        case 12: // scall
            {
                unsigned short ret = pc;
                pc = *(unsigned short*)& stack[sp];
                sp += 2;
                if (sp < STACK_SIZE)
                {
                    sp -= 2;
                    *(unsigned short*)& stack[sp] = ret;
                }
            }
            break;
        case 13: // ujump
            pc = *(short*)& image[pc];
            break;
        case 14: // fjump
            if (p == 0)
                pc = *(short*)& image[pc];
            else
                pc += 2;
            break;
        case 15: // modstk
            sp += *(short*)& image[pc];
            pc += 2;
            break;
        case 16: // swap
        {
            short t = s;
            s = p;
            p = t;
        }
        break;
        case 17: // limm
            p = *(short*)& image[pc];
            pc += 2;
            break;
        case 18: // push
            sp -= 2;
            *(short*)&stack[sp] = p;
            break;
        case 19: // pop
            s = *(short*)&stack[sp];
            sp += 2;
            break;
        case 20: // xchange
            {
                short t = *(short*)&stack[sp];
                sp += 2;
                if (sp < STACK_SIZE)
                {
                    sp -= 2;
                    *(short*)& stack[sp] = p;
                }
                p = t;
            }
            break;
        case 21: // return
            pc = *(unsigned short*)& stack[sp];
            sp += 2;
            break;
        case 22: // scale
            p *= *(short*)& image[pc];
            pc += 2;
            break;
        case 23: // add
            p += s;
            break;
        case 24: // sub
            p = s - p;
            break;
        case 25: // mult
            p *= s;
            break;
        case 26: // div
            {
                short q = s / p;
                s = s % p;
                p = q;
            }
            break;
        case 27: // mod
            {
                short q = s / p;
                p = s % p;
                s = q;
            }
            break;
        case 28: // or
            p |= s;
            break;
        case 29: // xor
            p ^= s;
            break;
        case 30: // and
            p &= s;
            break;
        case 31: // asr
            p = s >> p;
            break;
        case 32: // asl
            p = s << p;
            break;
        case 33: // neg
            p = -p;
            break;
        case 34: // inc
            p += *(short*)& image[pc];
            pc += 2;
            break;
        case 35: // dec
            p -= *(short*)& image[pc];
            pc += 2;
            break;
        case 36: // testeq
            p = (s == p) ? 1 : 0;
            break;
        case 37: // testne
            p = (s != p) ? 1 : 0;
            break;
        case 38: // testlt
            p = (s < p) ? 1 : 0;
            break;
        case 39: // testle
            p = (s <= p) ? 1 : 0;
            break;
        case 40: // testgt
            p = (s > p) ? 1 : 0;
            break;
        case 41: // testgt
            p = (s >= p) ? 1 : 0;
            break;
        case 42: // testult
            p = ((unsigned short)s < (unsigned short)p) ? 1 : 0;
            break;
        case 43: // testtle
            p = ((unsigned short)s <= (unsigned short)p) ? 1 : 0;
            break;
        case 44: // testugt
            p = ((unsigned short)s > (unsigned short)p) ? 1 : 0;
            break;
        case 45: // testuge
            p = ((unsigned short)s >= (unsigned short)p) ? 1 : 0;
            break;
        case 46:
            running = false;
        }

        if (pc >= STACK_START) // syscalls
        {
            switch (pc)
            {
            case 65536 - 1: // terminate (internal)
                running = false;
                sp -= 2; // do not return from this syscall
                break;
            case 65536 - 2: // fopen
                if (sp + 6 <= STACK_SIZE)
                {
                    char* arg1 = &image[*(unsigned short*)& stack[sp+4]];
                    char* arg2 = &image[*(unsigned short*)& stack[sp+2]];
                    FILE* res = fopen(arg1, arg2);
                    if (res > 0)
                    {
                        for(short i=1; i<(short)handles.size(); ++i)
                            if (handles[i] == (FILE*)0)
                            {
                                handles[i] = res;
                                p = i;
                                break;
                            }
                        handles.push_back(res);
                        p = (short)(handles.size() - 1);
                    }
                    else
                        p = 0;
                }
                break;
            case 65536 - 3: // fclose
                if (sp + 4 <= STACK_SIZE)
                {
                    auto h = *(unsigned short*)&stack[sp + 2];
                    if (h < handles.size())
                    {
                        p = fclose(handles[h]);
                        handles[h] = (FILE*)0;
                    }
                    else
                        p = -1;
                }
                break;
            case 65536 - 4: // getc
                if (sp + 4 <= STACK_SIZE)
                {
                    auto h = *(unsigned short*)&stack[sp + 2];
                    if (h < handles.size())
                    {
                        p = getc(handles[h]);
                    }
                    else
                        p = 0;
                }
                break;
            case 65536 - 5: // gets
                if (sp + 4 <= STACK_SIZE)
                {
                    gets_s(&image[*(unsigned short*)&stack[sp+2]], 80);
                }
                break;
            case 65536 - 6: // putc
                if (sp + 6 <= STACK_SIZE)
                {
                    auto h = *(unsigned short*)&stack[sp + 2];
                    if (h < handles.size())
                    {
                        putc(stack[sp + 4], handles[h]);
                    }
                }
                break;
            case 65536 - 7: // putchar
                if (sp + 4 <= STACK_SIZE)
                {
                    putchar(stack[sp + 2]);
                }
                break;
            case 65536 - 8: // fixup - only needed for the compiler itself due to a bug
                break;
            // The following were added for Forth system but some are still usable in RatC code (those using long aren't)
            case 65536 - 9: // getch
                p = _getch();
                break;
            case 65536 - 10: // remove
                if (sp + 4 <= STACK_SIZE)
                {
                    char* arg = &image[*(unsigned short*)&stack[sp + 2]];
                    p = remove(arg);
                }
                break;
            case 65536 - 11: // rename
                if (sp + 6 <= STACK_SIZE)
                {
                    char* arg1 = &image[*(unsigned short*)&stack[sp + 4]];
                    char* arg2 = &image[*(unsigned short*)&stack[sp + 2]];
                    p = rename(arg1, arg2);
                }
                break;
            case 65536 - 12: // ftell (limiting to 2GB files)
                if (sp + 4 <= STACK_SIZE)
                {
                    auto h = *(unsigned short*)&stack[sp + 2];
                    if (h < handles.size())
                    {
                        long pos = ftell(handles[h]);
                        p = pos & 0xffff;
                        s = pos >> 16;
                    }
                    else
                        p = s = -1;
                }
                break;
            case 65536 - 13: // fseek
                if (sp + 10 <= STACK_SIZE)
                {
                    auto h = *(unsigned short*)&stack[sp + 8];
                    if (h < handles.size())
                    {
                        long offset = (long)((((unsigned long)*(unsigned short*)&stack[sp + 4])<<16) + *(unsigned short*)&stack[sp + 6]);
                        auto origin = *(unsigned short*)&stack[sp + 2];
                        p = fseek(handles[h], offset, origin);
                    }
                    else
                        p = -1;
                }
                break;
            case 65536 - 14: // fflush
                if (sp + 4 <= STACK_SIZE)
                {
                    auto h = *(unsigned short*)&stack[sp + 2];
                    if (h < handles.size())
                    {
                        p = fflush(handles[h]);
                    }
                    else
                        p = -1;
                }
                break;
            case 65536 - 15: // getsize
                if (sp + 4 <= STACK_SIZE)
                {
                    auto h = *(unsigned short*)&stack[sp + 2];
                    if (h < handles.size())
                    {
                        long size = _filelength(_fileno(handles[h]));
                        p = size & 0xffff;
                        s = size >> 16;
                    }
                    else
                        p = s = -1;
                }
                break;
            case 65536 - 16: // setsize
                if (sp + 8 <= STACK_SIZE)
                {
                    auto h = *(unsigned short*)&stack[sp + 2];
                    if (h < handles.size())
                    {
                        long size = (long)((((unsigned long)*(unsigned short*)&stack[sp + 4]) << 16) + *(unsigned short*)&stack[sp + 6]);
                        p = _chsize(_fileno(handles[h]), size);
                    }
                    else
                        p = -1;
                }
                break;
            case 65536 - 17: // access
                if (sp + 4 <= STACK_SIZE)
                {
                    char* arg = &image[*(unsigned short*)&stack[sp + 2]];

                    struct stat buffer;
                    p = stat(arg, &buffer);
                    s = buffer.st_mode;
                }
                break;
            }
            pc = *(unsigned short*)& stack[sp];
            sp += 2;
        }

        if (sp < 0)
        {
            printf("Critical: stack overflow");
            running = false;
        }
        if (sp > STACK_SIZE)
        {
            printf("Critical: stack underflow");
            running = false;
        }
    }

    delete[] image;
    return 0;
}
