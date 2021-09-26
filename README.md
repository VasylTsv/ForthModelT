# ForthModelT
Complete small Forth system on the simplest virtual machine

### What is it?
You may have heard that a completely functional Forth system can be implemented in one weekend. This is basically true, the language core is so simple and straightforward that it literally writes itself, once you understand the basics. It is the easiest language to bring up on underpowered hardware without good toolkits, making Forth still popular in the field of embedded controllers. So, one weekend I decided to try to build yet another implementation from scratch. It took a bit more than one weekend - I had the core working in one weekend but bringing it Forth 2012 compliance took a few more evenings.

### The host system
The host "machine" for this implementation is somewhat odd. There was once a book called *A Book on C* by *R. Berry* and *B. Meekings* (not to be mistaken with another book by the same name). The book had three editions, the list of authors was different every time, but the first two editions were unusual in one respect. The book included a complete source for a simple C compiler called RatC. The compiler was derived from more known SmallC but it did not compile to any existing CPU directly. Instead, it produced the output in some fictitious assembly language for equally fictitious virtual machine. Except that the author did not describe that as a virtual machine - it was supposed to be post-processed to some other assembly, like 8088 or VAX (the book was published in late 80s).

I played with the source code from that book, mainly out of curiosity, to see if it actually works (it does with small caveats, I will get to that). I did not bother translating the output to any native code, writing a virtual machine was much easier and made more sense. So, I had that part ready to go.

The assembly language consists of 48 instructions, two of which were to deal with data. I made a couple of very small additions for this project. The virtual machine was rather underspecified so I had to make some assumptions. First of all, it could be implemented as 16-bit or 32-bit - I did not try the 32-bit implementation, and I liked the simplicity of 16-bit, so I went with that. There was no explanation how system calls are supposed to work but a simple implementation with dummy addresses trapped in the VM just worked.

Overall, the "CPU" is so simple, the 6502 looks like a pinnacle of engineering next to it. It does not even have any flags, including carry (some RISC CPUs have it the same way), which makes implementing math operations very tedious. The author claims that it is similar to 8080, I failed to find any similarities. On the plus side, translating from this platform to any other should be very easy.

Now, I initially wanted to reproduce the VM specifications here but ran into some obstacles. I could get a permission to do that from the original publisher (for a price) but that would not work with any open source license. I suspect that that part of the book itself is a reproduction of a public domain work but I could not confirm that. The original programmer (R. E. Berry) passed away even before the first edition was published and I could not locate any other authors. If anybody has any leads, please let me know. All of this is not a problem - the book is not rare by any means, and the relevant section is actually available directly from the publisher's site in its entirety - look for *Back matter* at the [Springer's book page](https://link.springer.com/book/10.1007/978-1-349-10233-4)! So, I will just assume that you have access to the book in paper or electronic form, or got that PDF from Springer.

### Implementation
As I said, I went with 16-bit implementation so all four registers P, S, SP, and PC are 16-bit. That naturally defined the memory model as 64K, and I think the original spec already assumed little-endian. For RatC I placed the stack at the end of the memory space and allocated 16K for it. In the hindsight, that was an overkill, and I worked around it in this Forth implementation by giving 8K of data stack and 8K of return stack instead. It is still very generous for a 16-bit Forth machine.

I made a few small additions to the assembler. The most important one is the extension to the `db` instruction. In the original it accepted only literal bytes. I added the ability to use labels which would be interpreted as 16-bit words and strings. Another addition is the `equ` instuction that allowed to define symbolic constants. This was not required but made the code a lot more readable. The last addition is `ref` which serves the only purpose of marking a label referenced so no warnings are produced.

### Structure of the project
There are three pieces of source code in this project:
`RatAsm.cpp` - the single-pass assembler.
`RatExe.cpp` - the virtual machine.
`RatForth.asm` - the Forth Model T itself.
The implementation is compliant with [Forth 2012 standard](https://forth-standard.org/standard/words). Not all word sets are supported, see the comment in the beginning of the source code for the compliance statement.
The system successfully completes the tests from [Gerry Jackson's test suite](https://github.com/gerryjackson/forth2012-test-suite) for all supported word sets.

### About the source code
As I mentioned, this was all done in one weekend. The source code is not very clean and could use some improvements, a number of words can be rewritten to be more efficient or robust. Bugs are possible.
The system was intentionally structured the way that the core (all the way to the end_of_image) can be read-only so it can reside in ROM.

### Running Forth Model T
Visual Studio 2019 under Windows 10 was used for this project but it should compile and run in other environments with little if any changes. To build and run Forth Model T in VS command prompt, execute the following commands:
```
cl RatAsm.cpp /EHsc
cl RatExe.cpp /EHsc
RatAsm RatForth.asm RatForth.bin
RatExe RatForth.bin
```
At this point you should be greeted by the Forth prompt and you can start typing whatever you want in Forth (e.g., RC4 implementation from Wikipedia article on Forth works). If you have the mentioned testsuite and want to run it, type
```
include runtests.fth
```
There is also an autorun functionality - the system will run autorun.fth automatically when started.

### Note about RatC
The assembler and VM can be used with the original RatC as it was published in the book. If you decide to try running it though, you should be aware of a few things. First of all, OCR does not work well with that kind of text, so you may have to re-type parts of the source. There is a missing comma on line 145 (the source is identical between editions). A more serious issue is in function `outdec` on line 2073. It is broken if compiled with 32-bit compilers. As a workaround, I've inserted one line `fixup(num);` at line 2078 and defined it as `#define fixup(x) { x = (int)(short)x; }` in a forced include. It is not needed to build RatC with RatC itself but, to use the same source code, I've added a dummy `fixup` system call in VM. The provided VM implements all system calls needed, Forth requires a few more not used in RatC.

### Future plans
I don't really plan to do anything with this project but if I decide to get back to it I would probably try to get a 32-bit version of the VM working. The source code may use some cleanups, that's if I get time.
There are a few more sections of the standard that can be added to the system. Search order is the most obvious. I don't think I will ever bother with floating point, given the CPU architecture.
Other that that, there are a lot of tricks that can be done with little system modifications, like making it multitasked. 

### FAQ
- *Why "Forth Model T"?*
- Well, it's Forth, it is a model of a system, and my last name starts with T.

- *Why all filenames start with Rat?*
- It all started with RatC which stands for "Rationalized C," so it was RatForth in the beginning. I did not like the name and I still don't understand what exactly is "rationalized" there. But I left filenames intact.

- *Is it completely useless?*
- I don't think so. The core VM is so simple that it would take very little time (hours) to convert to a real-world CPU. And that would result in Forth 2012 compliant system.

- *Are the any extensions beyond Forth 2012?*
- Yes, there are a few non-standard words. I should find some time and document them. Most of them are for internal use, so not that interesting. Executing word `STRICT` will hide all non-standard words but `EXTENDED` which reenables non-standard words.
